package chess

import (
	"bytes"
	"errors"
	"io"
	"testing"
)

// pgnBenchmarkSink keeps dead values alive across iterations without
// boxing into interface{} (which would corrupt the alloc measurement).
var pgnBenchmarkSink struct {
	decoded int
	games   []*Game
}

// BenchmarkPGNDecode_RetainAll keeps every decoded Game alive for the
// lifetime of the benchmark iteration. Positions are retained, so a sync.Pool
// for *Position cannot reclaim them mid-iteration -- this shape exposes the
// upper bound (no help from pooling).
func BenchmarkPGNDecode_RetainAll(b *testing.B) {
	data := readPGNFixture("big_big.pgn")
	b.SetBytes(int64(len(data)))
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var games []*Game
		dec := NewPGNDecoder(bytes.NewReader(data))
		for {
			g, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				if isKnownInconsistentPgn(err) {
					continue
				}
				b.Fatalf("decode error: %v", err)
			}
			games = append(games, g)
		}
		pgnBenchmarkSink.games = games
	}
}

// BenchmarkPGNDecode_StreamDiscard decodes each game, reads one cheap field,
// and lets the *Game drop before the next iteration starts. Positions stay
// live within a single Game (move tree), so a pool cannot reclaim them
// mid-game in either shape; the discard shape only differs in cross-game
// reuse, which mallocgc already handles. Used to evaluate Fix C viability
// before any pooling is added.
func BenchmarkPGNDecode_StreamDiscard(b *testing.B) {
	data := readPGNFixture("big_big.pgn")
	b.SetBytes(int64(len(data)))
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var decoded int
		dec := NewPGNDecoder(bytes.NewReader(data))
		for {
			g, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				if isKnownInconsistentPgn(err) {
					continue
				}
				b.Fatalf("decode error: %v", err)
			}
			_ = g.Outcome()
			decoded++
		}
		pgnBenchmarkSink.decoded = decoded
	}
}
