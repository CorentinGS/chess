package chess_test

import (
	"errors"
	"io"
	"strings"
	"testing"
	"time"

	"github.com/corentings/chess/v3"
)

// failingReader serves data once and then fails with err on every read.
type failingReader struct {
	data string
	off  int
	err  error
}

func (r *failingReader) Read(p []byte) (int, error) {
	if r.off >= len(r.data) {
		return 0, r.err
	}
	n := copy(p, r.data[r.off:])
	r.off += n
	return n, nil
}

var errReadFailed = errors.New("read failed")

// A reader that keeps failing must not make the decoder loop forever: the
// read error has to reach the caller.
func TestPGNDecoderReportsReadError(t *testing.T) {
	const pgn = "[Event \"one\"]\n\n1. e4 e5\n"

	type result struct {
		game *chess.Game
		err  error
	}
	done := make(chan result, 1)
	go func() {
		dec := chess.NewPGNDecoder(&failingReader{data: pgn, err: errReadFailed})
		game, err := dec.Decode()
		done <- result{game: game, err: err}
	}()

	select {
	case res := <-done:
		if !errors.Is(res.err, errReadFailed) {
			t.Fatalf("Decode error = %v, want %v", res.err, errReadFailed)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("Decode did not return: the decoder loops on a failing reader")
	}
}

// The same failure must reach the iterator, which stops on the first error.
func TestPGNGamesReportsReadError(t *testing.T) {
	const pgn = "[Event \"one\"]\n\n1. e4 e5\n"

	type result struct {
		game *chess.Game
		err  error
	}
	done := make(chan result, 1)
	go func() {
		for _, err := range chess.PGNGames(&failingReader{data: pgn, err: errReadFailed}) {
			if err != nil {
				done <- result{err: err}
				return
			}
		}
		done <- result{}
	}()

	select {
	case res := <-done:
		if !errors.Is(res.err, errReadFailed) {
			t.Fatalf("PGNGames error = %v, want %v", res.err, errReadFailed)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("PGNGames did not return: the decoder loops on a failing reader")
	}
}

// A reader that ends cleanly still reports io.EOF rather than an error.
func TestPGNDecoderCleanEndStillReturnsEOF(t *testing.T) {
	dec := chess.NewPGNDecoder(strings.NewReader(""))
	if _, err := dec.Decode(); !errors.Is(err, io.EOF) {
		t.Fatalf("Decode error = %v, want io.EOF", err)
	}
}

// A record that was fully read before the failure is still returned; the read
// error only surfaces once the incomplete remainder is reached.
func TestPGNDecoderReturnsCompleteRecordBeforeReadError(t *testing.T) {
	const pgn = "[Event \"one\"]\n\n1. e4 e5 1-0\n\n[Event \"two\"]\n\n1. d4"
	dec := chess.NewPGNDecoder(&failingReader{data: pgn, err: errReadFailed})

	first, err := dec.Decode()
	if err != nil {
		t.Fatalf("first Decode error = %v, want nil", err)
	}
	if got := first.Tag("Event"); got != "one" {
		t.Fatalf("first Event tag = %q, want %q", got, "one")
	}

	second, err := dec.Decode()
	if !errors.Is(err, errReadFailed) {
		t.Fatalf("second Decode error = %v, want %v", err, errReadFailed)
	}
	if second != nil {
		t.Fatalf("second Decode game = %v, want nil", second)
	}
}
