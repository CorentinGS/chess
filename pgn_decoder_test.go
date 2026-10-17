package chess_test

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

const decoderGameOne = `[Event "one"]
[Site "?"]
[Date "2026.06.28"]
[Round "?"]
[White "White"]
[Black "Black"]
[Result "*"]

1. e4 *
`

const decoderGameTwo = `[Event "two"]
[Site "?"]
[Date "2026.06.28"]
[Round "?"]
[White "White"]
[Black "Black"]
[Result "*"]

1. d4 *
`

func TestParsePGNReturnsSingleGame(t *testing.T) {
	game, err := chess.ParsePGN(strings.NewReader(decoderGameOne))
	if err != nil {
		t.Fatalf("ParsePGN error: %v", err)
	}
	if got := game.GetTagPair("Event"); got != "one" {
		t.Fatalf("Event tag = %q, want %q", got, "one")
	}
	if got := len(game.Moves()); got != 1 {
		t.Fatalf("len(Moves) = %d, want 1", got)
	}
}

func TestPGNDecoderDecodesMultipleGames(t *testing.T) {
	dec := chess.NewPGNDecoder(strings.NewReader(decoderGameOne + "\n" + decoderGameTwo))

	first, err := dec.Decode()
	if err != nil {
		t.Fatalf("first Decode error: %v", err)
	}
	if got := first.GetTagPair("Event"); got != "one" {
		t.Fatalf("first Event tag = %q, want %q", got, "one")
	}
	if dec.Index() != 1 {
		t.Fatalf("Index after first Decode = %d, want 1", dec.Index())
	}

	second, err := dec.Decode()
	if err != nil {
		t.Fatalf("second Decode error: %v", err)
	}
	if got := second.GetTagPair("Event"); got != "two" {
		t.Fatalf("second Event tag = %q, want %q", got, "two")
	}
	if dec.Index() != 2 {
		t.Fatalf("Index after second Decode = %d, want 2", dec.Index())
	}

	if _, err := dec.Decode(); !errors.Is(err, io.EOF) {
		t.Fatalf("final Decode error = %v, want io.EOF", err)
	}
}

func TestPGNGamesIteratesGamesAndErrors(t *testing.T) {
	var events []string
	for game, err := range chess.PGNGames(strings.NewReader(decoderGameOne + "\n" + decoderGameTwo)) {
		if err != nil {
			t.Fatalf("PGNGames yielded error: %v", err)
		}
		events = append(events, game.GetTagPair("Event"))
	}

	if strings.Join(events, ",") != "one,two" {
		t.Fatalf("events = %v, want [one two]", events)
	}
}

func TestPGNDecoderEmptyInputReturnsEOF(t *testing.T) {
	dec := chess.NewPGNDecoder(strings.NewReader(" \n\t"))
	if _, err := dec.Decode(); !errors.Is(err, io.EOF) {
		t.Fatalf("Decode error = %v, want io.EOF", err)
	}
}

func TestPGNDecoderExpandsVariations(t *testing.T) {
	pgn := `[Event "variations"]
[Site "?"]
[Date "2026.06.28"]
[Round "?"]
[White "White"]
[Black "Black"]
[Result "*"]

1. e4 (1. d4) *
`
	dec := chess.NewPGNDecoder(strings.NewReader(pgn), chess.WithPGNExpandVariations())

	first, err := dec.Decode()
	if err != nil {
		t.Fatalf("first Decode error: %v", err)
	}
	second, err := dec.Decode()
	if err != nil {
		t.Fatalf("second Decode error: %v", err)
	}
	if _, err := dec.Decode(); !errors.Is(err, io.EOF) {
		t.Fatalf("final Decode error = %v, want io.EOF", err)
	}
	if len(first.Moves()) != 1 || len(second.Moves()) != 1 {
		t.Fatalf("expanded games should each have one move, got %d and %d", len(first.Moves()), len(second.Moves()))
	}
}
