package chess

import (
	"testing"
)

// Common test positions
var (
	// Starting position
	startingPos = mustPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

	// Middle game position with lots of possible moves
	middlePos = mustPosition("r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 0 1")

	// Endgame position with few pieces
	endPos = mustPosition("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1")

	// Position with multiple possible pawn promotions
	promoPos = mustPosition("4k3/PPPP4/8/8/8/8/4pppp/4K3 w - - 0 1")
)

func BenchmarkStandardMoves(b *testing.B) {
	benchmarks := []struct {
		name      string
		pos       *Position
		wantFirst bool
	}{
		{"StartingPos_AllMoves", startingPos, false},
		{"StartingPos_FirstMove", startingPos, true},
		{"MiddleGame_AllMoves", middlePos, false},
		{"MiddleGame_FirstMove", middlePos, true},
		{"Endgame_AllMoves", endPos, false},
		{"Endgame_FirstMove", endPos, true},
		{"Promotions_AllMoves", promoPos, false},
		{"Promotions_FirstMove", promoPos, true},
	}

	for _, bm := range benchmarks {
		b.Run(bm.name, func(b *testing.B) {
			// Reset timer to exclude setup
			b.ResetTimer()

			// Enable allocation tracking
			b.ReportAllocs()

			for i := 0; i < b.N; i++ {
				moves := standardMoves(bm.pos, bm.wantFirst)
				// Prevent compiler optimization
				if len(moves) == 0 {
					b.Fatal("unexpected zero moves")
				}
			}
		})
	}
}

// Benchmark specific scenarios
func BenchmarkStandardMoves_PawnPromotions(b *testing.B) {
	pos := promoPos
	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		moves := standardMoves(pos, false)
		if len(moves) == 0 {
			b.Fatal("unexpected zero moves")
		}
	}
}

// Benchmark with different board sizes to understand allocation scaling
func BenchmarkStandardMoves_BoardDensity(b *testing.B) {
	positions := []struct {
		name string
		fen  string
	}{
		{"Empty", "4k3/8/8/8/8/8/8/4K3 w - - 0 1"},
		{"QuarterFull", "rnbqk3/pppp4/8/8/8/8/8/4K3 w - - 0 1"},
		{"HalfFull", "rnbqkbnr/pppp4/8/8/8/8/8/4K3 w - - 0 1"},
		{"Full", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
	}

	for _, p := range positions {
		pos := mustPosition(p.fen)
		b.Run(p.name, func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				moves := standardMoves(pos, false)
				if len(moves) == 0 && p.name != "Empty" {
					b.Fatal("unexpected zero moves")
				}
			}
		})
	}
}

func TestAddTags(t *testing.T) {
	tests := []struct {
		name string
		move Move
		want MoveTag
		fen  string
	}{
		{
			name: "move with queen side castle",
			move: Move{s1: E8, s2: C8},
			want: QueenSideCastle,
			fen:  "r3kb1r/p2nqppp/5n2/1B2p1B1/4P3/1Q6/PPP2PPP/R3K2R b KQkq - 1 12",
		},
		{
			name: "move with king side castle",
			move: Move{s1: E1, s2: G1},
			want: KingSideCastle | Check,
			fen:  "r4b1r/ppp3pp/8/4p3/2Pq4/3P4/PP2QPPP/2k1K2R w K - 0 18",
		},
		{
			name: "move with king side castle and check",
			move: Move{s1: E1, s2: G1},
			want: KingSideCastle | Check,
			fen:  "r4b1r/ppp3pp/8/4p3/2Pq4/3P1Q2/PP3PPP/1k2K2R w K - 2 19",
		},
		{
			name: "move with check",
			move: Move{s1: D7, s2: A4},
			want: Check,
			fen:  "rn2k1r1/ppqb4/4p1n1/3pPp1Q/8/P1PP4/4NPPP/R1BK1B1R b q - 0 14",
		},
		{
			name: "move leaves king in check",
			move: Move{s1: G6, s2: F8},
			want: inCheck,
			fen:  "r3k1r1/ppq5/2n1p1n1/3p1pBQ/b2P3P/P1P5/4NPP1/R3KB1R b q - 0 18",
		},
		{
			name: "capture move",
			move: Move{s1: G2, s2: G3},
			want: Capture,
			fen:  "8/7p/3k2p1/8/2p2P2/R5bP/6K1/4r3 w - - 0 44",
		},
		{
			name: "normal move without tags",
			move: Move{s1: D6, s2: D5},
			want: 0,
			fen:  "8/7p/3k2p1/8/2p2P2/R5KP/8/4r3 b - - 0 44",
		},
		{
			name: "en passant move",
			move: Move{s1: E4, s2: F3},
			want: EnPassant | Check,
			fen:  "r3k2r/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRKR2 b kq f3 0 1",
		},
		{
			name: "normal move without tags",
			move: Move{s1: B7, s2: A6},
			want: 0,
			fen:  "r3k2r/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRKR2 b kq f3 0 1",
		},
		{
			name: "en passant move with check",
			move: Move{s1: E4, s2: F3},
			want: EnPassant | Check,
			fen:  "r3k1r1/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRK1R1 b q f3 0 2",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			addTags(&test.move, mustPosition(test.fen))

			if test.move.tags != test.want {
				t.Errorf("fen: %s | move: %s\ntags(%d) == expected_tags(%d)", test.fen, test.move.String(), test.move.tags, test.want)
			}
		})
	}
}

// Helper function to convert FEN to Position
func mustPosition(fen string) *Position {
	fenObject, err := FEN(fen)
	pos := NewGame(fenObject).Position()
	if err != nil {
		panic(err)
	}
	return pos
}
