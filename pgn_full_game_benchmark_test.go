package chess

import (
	"bytes"
	"context"
	"errors"
	"io"
	"runtime"
	"testing"
)

type pgnDecodeSummary struct {
	games       int
	errors      int
	moves       int
	tags        int
	comments    int
	variations  int
	withOutcome int
}

var pgnDecodeSummarySink pgnDecodeSummary

func BenchmarkPGN_FullGameDecode_BigBig(b *testing.B) {
	benchFullGameDecodeFixture(b, "big_big.pgn")
}

func BenchmarkPGN_FullGameDecode_Big(b *testing.B) {
	benchFullGameDecodeFixture(b, "big.pgn")
}

func BenchmarkPGN_FullGameDecode_RecordDecode_BigBig(b *testing.B) {
	data := readPGNFixture("big_big.pgn")
	meta := pgnMeta("big_big.pgn")
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var summary pgnDecodeSummary
		for record, err := range PGNRecords(context.Background(), bytes.NewReader(data)) {
			if err != nil {
				b.Fatalf("record error: %v", err)
			}
			game, err := record.Decode()
			if err != nil {
				if isKnownInconsistentPgn(err) {
					summary.errors++
					continue
				}
				b.Fatalf("decode error: %v", err)
			}
			accumulatePGNDecodeSummary(&summary, game)
		}
		if summary.games+summary.errors != meta.games {
			b.Fatalf("expected %d games total, parsed %d with %d errors", meta.games, summary.games, summary.errors)
		}
		pgnDecodeSummarySink = summary
	}
}

func BenchmarkPGN_FullGameDecode_Parallel_BigBig(b *testing.B) {
	data := readPGNFixture("big_big.pgn")
	meta := pgnMeta("big_big.pgn")
	workers := runtime.GOMAXPROCS(0)
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var summary pgnDecodeSummary
		results := DecodePGNGamesParallel(context.Background(), bytes.NewReader(data), PGNParallelOptions{
			Workers: workers,
			Buffer:  workers * 2,
		})
		for result := range results {
			if result.Err != nil {
				if isKnownInconsistentPgn(result.Err) {
					summary.errors++
					continue
				}
				b.Fatalf("parallel decode error: %v", result.Err)
			}
			accumulatePGNDecodeSummary(&summary, result.Game)
		}
		if summary.games+summary.errors != meta.games {
			b.Fatalf("expected %d games total, parsed %d with %d errors", meta.games, summary.games, summary.errors)
		}
		pgnDecodeSummarySink = summary
	}
}

func BenchmarkPGN_FullGameDecode_ExpandVariations(b *testing.B) {
	data := []byte(`[Event "Variation Benchmark"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 (3. Bc4 Nf6 4. d3) a6 4. Ba4 Nf6 5. O-O Be7 1-0`)

	b.SetBytes(int64(len(data)))
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var summary pgnDecodeSummary
		dec := NewPGNDecoder(bytes.NewReader(data), WithPGNExpandVariations())
		for {
			game, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				b.Fatalf("decode error: %v", err)
			}
			accumulatePGNDecodeSummary(&summary, game)
		}
		pgnDecodeSummarySink = summary
	}
}

func BenchmarkPGN_FullGameDecode_CompleteGameDetails(b *testing.B) {
	data := readPGNFixture("complete_game.pgn")
	b.SetBytes(int64(len(data)))
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		game, err := NewPGNDecoder(bytes.NewReader(data)).Decode()
		if err != nil {
			b.Fatalf("decode error: %v", err)
		}
		var summary pgnDecodeSummary
		accumulatePGNDecodeSummary(&summary, game)
		pgnDecodeSummarySink = summary
	}
}

func benchFullGameDecodeFixture(b *testing.B, name string) {
	b.Helper()
	data := readPGNFixture(name)
	meta := pgnMeta(name)
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		var summary pgnDecodeSummary
		dec := NewPGNDecoder(bytes.NewReader(data))
		for {
			game, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				if isKnownInconsistentPgn(err) {
					summary.errors++
					continue
				}
				b.Fatalf("decode error: %v", err)
			}
			accumulatePGNDecodeSummary(&summary, game)
		}
		if summary.games+summary.errors != meta.games {
			b.Fatalf("expected %d games total, parsed %d with %d errors", meta.games, summary.games, summary.errors)
		}
		pgnDecodeSummarySink = summary
	}
}

func accumulatePGNDecodeSummary(summary *pgnDecodeSummary, game *Game) {
	if game == nil {
		return
	}
	summary.games++
	summary.tags += len(game.tagPairs)
	if game.Outcome() != NoOutcome {
		summary.withOutcome++
	}
	for _, block := range game.comments {
		summary.comments += len(block)
	}
	accumulateMoveTreeSummary(summary, game.MoveTree().Root())
}

func accumulateMoveTreeSummary(summary *pgnDecodeSummary, node *MoveNode) {
	if node == nil {
		return
	}
	if node.parent != nil {
		summary.moves++
	}
	if len(node.children) > 1 {
		summary.variations += len(node.children) - 1
	}
	for _, block := range node.commentBlocks {
		summary.comments += len(block.Items)
	}
	for _, child := range node.children {
		accumulateMoveTreeSummary(summary, child)
	}
}
