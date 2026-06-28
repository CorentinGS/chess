package chess_test

import (
	"sort"
	"testing"

	"github.com/corentings/chess/v3"
)

// perftCase pairs a position with the expected node counts for each depth,
// from depth 1 up to the last entry. Values are from
// https://www.chessprogramming.org/Perft_Results.
type perftCase struct {
	name     string
	fen      string
	nodes    []uint64 // index 0 = depth 1, index i = depth (i+1)
	maxDepth int      // cap recursion at this depth to keep tests fast
}

var perftCases = []perftCase{
	{
		name: "startpos",
		fen:  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		// depths 1..4 only; depth 5 alone is 4.8M and depth 6 is 119M
		nodes:    []uint64{20, 400, 8902, 197281, 4865609},
		maxDepth: 4,
	},
	{
		name:     "kiwipete",
		fen:      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
		nodes:    []uint64{48, 2039, 97862, 4085603, 193690690},
		maxDepth: 4,
	},
	{
		name:     "pos3",
		fen:      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
		nodes:    []uint64{14, 191, 2812, 43238, 674624, 11030083, 178633661},
		maxDepth: 6,
	},
	{
		name:     "pos4",
		fen:      "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
		nodes:    []uint64{6, 264, 9467, 422333, 15833292, 706045033},
		maxDepth: 5,
	},
	{
		name:     "pos5",
		fen:      "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1",
		nodes:    []uint64{6, 264, 9467, 422333, 15833292, 706045033},
		maxDepth: 5,
	},
	{
		name:     "pos6",
		fen:      "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
		nodes:    []uint64{44, 1486, 62379, 2103487, 89941194},
		maxDepth: 4,
	},
}

func TestPerft(t *testing.T) {
	for _, c := range perftCases {
		t.Run(c.name, func(t *testing.T) {
			opt, err := chess.FEN(c.fen)
			if err != nil {
				t.Fatalf("FEN decode: %v", err)
			}
			pos := chess.NewGame(opt).Position()
			for depth, want := range c.nodes {
				if depth+1 > c.maxDepth {
					break
				}
				got := pos.Perft(depth + 1)
				if got != want {
					t.Errorf("depth %d: got %d, want %d", depth+1, got, want)
				}
			}
		})
	}
}

func TestPerftBaseCases(t *testing.T) {
	t.Run("depth_zero_is_one", func(t *testing.T) {
		pos := chess.NewGame().Position()
		if got := pos.Perft(0); got != 1 {
			t.Errorf("Perft(0) = %d, want 1", got)
		}
	})

	t.Run("depth_one_equals_legal_moves", func(t *testing.T) {
		pos := chess.NewGame().Position()
		if got, want := pos.Perft(1), uint64(len(pos.ValidMoves())); got != want {
			t.Errorf("Perft(1) = %d, want %d", got, want)
		}
	})

	t.Run("terminal_position_returns_zero", func(t *testing.T) {
		// Fool's mate: 1.f3 e5 2.g4 Qh4# -- black just delivered mate
		opt, err := chess.FEN("rnbqkbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 2")
		if err != nil {
			t.Fatalf("FEN decode: %v", err)
		}
		pos := chess.NewGame(opt).Position()
		if got := pos.Perft(1); got != 0 {
			t.Errorf("Perft(1) on checkmate position = %d, want 0", got)
		}
	})

	t.Run("nil_position_is_zero", func(t *testing.T) {
		var pos *chess.Position
		if got := pos.Perft(3); got != 0 {
			t.Errorf("Perft(3) on nil = %d, want 0", got)
		}
	})
}

func TestPerftRuleRegressions(t *testing.T) {
	tests := []struct {
		name  string
		fen   string
		nodes []uint64
	}{
		{
			name:  "castling_rights",
			fen:   "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1",
			nodes: []uint64{26, 568},
		},
		{
			name:  "en_passant",
			fen:   "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3",
			nodes: []uint64{31, 807},
		},
		{
			name:  "promotion",
			fen:   "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
			nodes: []uint64{9, 41},
		},
		{
			name:  "pinned_piece",
			fen:   "4k3/8/8/8/8/8/4R3/4K2r w - - 0 1",
			nodes: []uint64{2, 8},
		},
		{
			name:  "single_check",
			fen:   "4k3/8/8/8/8/8/4r3/4K3 w - - 0 1",
			nodes: []uint64{3, 41},
		},
		{
			name:  "double_check",
			fen:   "4k3/8/8/8/8/3b4/4r3/4K3 w - - 0 1",
			nodes: []uint64{2, 54},
		},
		{
			name:  "checkmate",
			fen:   "rnbqkbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 2",
			nodes: []uint64{0, 0},
		},
		{
			name:  "stalemate",
			fen:   "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1",
			nodes: []uint64{0, 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			opt, err := chess.FEN(tt.fen)
			if err != nil {
				t.Fatalf("FEN decode: %v", err)
			}
			pos := chess.NewGame(opt).Position()
			for depth, want := range tt.nodes {
				if got := pos.Perft(depth + 1); got != want {
					t.Errorf("Perft(%d) = %d, want %d", depth+1, got, want)
				}
			}
		})
	}
}

func TestDivide(t *testing.T) {
	opt, err := chess.FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	if err != nil {
		t.Fatalf("FEN decode: %v", err)
	}
	pos := chess.NewGame(opt).Position()
	got := pos.Divide(1)
	if len(got) != 20 {
		t.Fatalf("Divide(1) returned %d moves, want 20", len(got))
	}
	for _, count := range got {
		if count != 1 {
			t.Errorf("Divide(1) per-move count = %d, want 1", count)
		}
	}

	// depth 1 sum must equal Perft(1)
	var sum uint64
	for _, v := range got {
		sum += v
	}
	if sum != pos.Perft(1) {
		t.Errorf("sum(Divide(1)) = %d, want %d", sum, pos.Perft(1))
	}

	// depth 2: known total 400
	depth2 := pos.Divide(2)
	var sum2 uint64
	for _, v := range depth2 {
		sum2 += v
	}
	if sum2 != 400 {
		t.Errorf("sum(Divide(2)) = %d, want 400", sum2)
	}
}

func TestDivideStable(t *testing.T) {
	// Verify the map is well-defined (not a pointer map): two equal moves
	// coming from a second call produce the same key.
	pos := chess.NewGame().Position()
	first := pos.Divide(1)
	second := pos.Divide(1)
	if len(first) != len(second) {
		t.Fatalf("Divide(1) non-deterministic size: %d vs %d", len(first), len(second))
	}
	keys := make([]string, 0, len(first))
	for k := range first {
		keys = append(keys, k.String())
	}
	sort.Strings(keys)
	for i := 1; i < len(keys); i++ {
		if keys[i] == keys[i-1] {
			t.Errorf("Divide(1) returned duplicate move key %q", keys[i])
		}
	}
}

func TestPerftAndDivideDoNotChangePosition(t *testing.T) {
	tests := []struct {
		name string
		fen  string
	}{
		{
			name: "startpos",
			fen:  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		},
		{
			name: "castling_and_en_passant",
			fen:  "r3k2r/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRKR2 b kq f3 0 1",
		},
		{
			name: "promotion",
			fen:  "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
		},
		{
			name: "check_giving_moves",
			fen:  "4k3/8/8/8/4N3/8/8/4R1K1 w - - 0 1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := positionFromFEN(t, tt.fen)
			beforeFEN := pos.String()
			beforeMoves := moveStrings(pos.ValidMoves())

			_ = pos.Perft(3)
			if got := pos.String(); got != beforeFEN {
				t.Fatalf("Perft mutated position: got %q, want %q", got, beforeFEN)
			}
			if got := moveStrings(pos.ValidMoves()); !equalStrings(got, beforeMoves) {
				t.Fatalf("Perft changed legal moves: got %v, want %v", got, beforeMoves)
			}

			_ = pos.Divide(3)
			if got := pos.String(); got != beforeFEN {
				t.Fatalf("Divide mutated position: got %q, want %q", got, beforeFEN)
			}
			if got := moveStrings(pos.ValidMoves()); !equalStrings(got, beforeMoves) {
				t.Fatalf("Divide changed legal moves: got %v, want %v", got, beforeMoves)
			}
		})
	}
}

func moveStrings(moves []chess.Move) []string {
	out := make([]string, 0, len(moves))
	for _, move := range moves {
		out = append(out, move.String())
	}
	sort.Strings(out)
	return out
}

func equalStrings(a []string, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
