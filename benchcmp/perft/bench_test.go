// Package perftbench compares the Perft performance and correctness of
// github.com/corentings/chess/v3 against github.com/dylhunn/dragontoothmg on
// the six canonical Perft positions from
// https://www.chessprogramming.org/Perft_Results.
//
// dragontoothmg is GPL v3 — linking it is acceptable here because this is a
// developer-only benchmark sandbox, not a redistributable artifact. It is
// kept in a separate Go module so the main module's permissive license is
// unaffected.
package perftbench

import (
	"fmt"
	"testing"

	chess "github.com/corentings/chess/v3"
	"github.com/dylhunn/dragontoothmg"
)

// perftCase pairs a position (as FEN) with the expected node counts at each
// depth (1-indexed). Depths included are capped per case to keep the bench
// wall-clock reasonable on commodity hardware.
type perftCase struct {
	name     string
	fen      string
	nodes    []uint64
	maxDepth int
}

var perftCases = []perftCase{
	{
		name:     "startpos",
		fen:      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		nodes:    []uint64{20, 400, 8902, 197281, 4865609, 119060324},
		maxDepth: 6,
	},
	{
		name:     "kiwipete",
		fen:      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
		nodes:    []uint64{48, 2039, 97862, 4085603, 193690690},
		maxDepth: 5,
	},
	{
		name:     "pos3",
		fen:      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
		nodes:    []uint64{14, 191, 2812, 43238, 674624, 11030083},
		maxDepth: 6,
	},
	{
		name:     "pos4",
		fen:      "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
		nodes:    []uint64{6, 264, 9467, 422333, 15833292},
		maxDepth: 5,
	},
	{
		name:     "pos5",
		fen:      "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
		nodes:    []uint64{6, 264, 9467, 422333, 15833292},
		maxDepth: 5,
	},
	{
		name:     "pos6",
		fen:      "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
		nodes:    []uint64{44, 1486, 62379, 2103487, 89941194},
		maxDepth: 5,
	},
}

// TestCorrectness verifies both libraries agree with the canonical Perft
// node counts at every included depth. This is a hard gate — if either
// library disagrees with the canonical values, the benchmarks are meaningless.
func TestCorrectness(t *testing.T) {
	for _, c := range perftCases {
		t.Run(c.name, func(t *testing.T) {
			for i, want := range c.nodes {
				depth := i + 1
				if depth > c.maxDepth {
					break
				}
				// chess/v3
				opt, err := chess.FEN(c.fen)
				if err != nil {
					t.Fatalf("chess FEN(%q): %v", c.fen, err)
				}
				g := chess.NewGame(opt)
				chessGot := g.Position().Perft(depth)

				// dragontoothmg
				dtb := dragontoothmg.ParseFen(c.fen)
				dtGot := dragontoothmg.Perft(&dtb, depth)

				if uint64(chessGot) != want {
					t.Errorf("chess depth %d: got %d, want %d", depth, chessGot, want)
				}
				if uint64(dtGot) != want {
					t.Errorf("dragontoothmg depth %d: got %d, want %d", depth, dtGot, want)
				}
				if uint64(chessGot) != uint64(dtGot) {
					t.Errorf("libraries disagree at depth %d: chess=%d, dragontoothmg=%d", depth, chessGot, dtGot)
				}
			}
		})
	}
}

// chessPerftAt is a benchmark helper that warms and resets the position
// per iteration so we measure a single Perft traversal, not allocator reuse.
func chessPerftAt(b *testing.B, fen string, depth int) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		opt, _ := chess.FEN(fen)
		g := chess.NewGame(opt)
		g.Position().Perft(depth)
	}
}

// dtPerftAt is the dragontoothmg equivalent.
func dtPerftAt(b *testing.B, fen string, depth int) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		dtb := dragontoothmg.ParseFen(fen)
		dragontoothmg.Perft(&dtb, depth)
	}
}

// Benchmark suites are emitted programmatically so the per-position × per-depth
// grid is easy to scan and stable across refactors.
func BenchmarkChessPerft(b *testing.B) {
	for _, c := range perftCases {
		for i, want := range c.nodes {
			depth := i + 1
			if depth > c.maxDepth {
				break
			}
			b.Run(fmt.Sprintf("%s/d=%d/nodes=%d", c.name, depth, want), func(b *testing.B) {
				chessPerftAt(b, c.fen, depth)
			})
		}
	}
}

func BenchmarkDragontoothPerft(b *testing.B) {
	for _, c := range perftCases {
		for i, want := range c.nodes {
			depth := i + 1
			if depth > c.maxDepth {
				break
			}
			b.Run(fmt.Sprintf("%s/d=%d/nodes=%d", c.name, depth, want), func(b *testing.B) {
				dtPerftAt(b, c.fen, depth)
			})
		}
	}
}
