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
				moves := standardMoves(bm.pos, bm.wantFirst, false)
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
		moves := standardMoves(pos, false, false)
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
				moves := standardMoves(pos, false, false)
				if len(moves) == 0 && p.name != "Empty" {
					b.Fatal("unexpected zero moves")
				}
			}
		})
	}
}

func TestMoveTags(t *testing.T) {
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
			test.move.tags = moveTags(test.move, mustPosition(test.fen))

			if test.move.tags != test.want {
				t.Errorf("fen: %s | move: %s\ntags(%d) == expected_tags(%d)", test.fen, test.move.String(), test.move.tags, test.want)
			}
		})
	}
}

func TestUnsafeMoves_StartingPosition(t *testing.T) {
	pos := mustPosition("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	moves := engine{}.UnsafeMoves(pos)
	if len(moves) != 0 {
		t.Errorf("expected 0 unsafe moves in starting position, got %d", len(moves))
	}
}

func TestPromotionCheckTagIsolation(t *testing.T) {
	// FEN from issue #112: k7/4P3/8/8/8/8/8/K7 w - - 0 1
	// White pawn on E7 can promote to E8. Only Queen and Rook give check
	// to the black king on A8. Bishop and Knight should NOT have Check.
	pos := mustPosition("k7/4P3/8/8/8/8/8/K7 w - - 0 1")
	moves := pos.ValidMoves()

	// Verify total move count (3 king moves + 4 promotions = 7)
	if len(moves) != 7 {
		t.Fatalf("expected 7 moves, got %d", len(moves))
	}

	// Helper: find move by s1, s2, promo fields (order-independent)
	findMove := func(s1, s2 Square, promo PieceType) (Move, bool) {
		for _, m := range moves {
			if m.s1 == s1 && m.s2 == s2 && m.promo == promo {
				return m, true
			}
		}
		return Move{}, false
	}

	tests := []struct {
		name      string
		promo     PieceType
		wantCheck bool
	}{
		{"queen promotion with check", Queen, true},
		{"rook promotion with check", Rook, true},
		{"bishop promotion without check", Bishop, false},
		{"knight promotion without check", Knight, false},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			m, ok := findMove(E7, E8, test.promo)
			if !ok {
				t.Fatalf("expected to find E7-E8=%s promotion move", test.promo.String())
			}
			gotCheck := m.HasTag(Check)
			if gotCheck != test.wantCheck {
				t.Errorf("E7-E8=%s: Check=%v, want %v", test.promo.String(), gotCheck, test.wantCheck)
			}
		})
	}
}

func TestPromotionNoCheck(t *testing.T) {
	// Position where no promotion gives check against an existing black king.
	// Black king on A6 is not attacked by any promoted piece on E8.
	pos := mustPosition("8/4P3/k7/8/8/8/8/7K w - - 0 1")
	moves := pos.ValidMoves()

	for _, m := range moves {
		if m.s1 == E7 && m.s2 == E8 {
			if m.HasTag(Check) {
				t.Errorf("E7-E8=%s should NOT have Check tag", m.promo.String())
			}
		}
	}
}

func TestUnsafeMoves_PinnedKnight(t *testing.T) {
	pos := mustPosition("4k3/8/8/8/1b6/2N5/8/4K3 w - - 0 1")
	moves := engine{}.UnsafeMoves(pos)
	if len(moves) != 8 {
		t.Fatalf("expected 8 unsafe moves for pinned knight, got %d", len(moves))
	}
	for _, m := range moves {
		if m.s1 != C3 {
			t.Errorf("expected unsafe move from C3, got %s", m.s1.String())
		}
		if !m.HasTag(inCheck) {
			t.Errorf("expected unsafe move to have inCheck tag: %s", m.String())
		}
	}
}

func TestUnsafeMoves_KingIntoCheck(t *testing.T) {
	pos := mustPosition("8/8/8/8/8/3r4/8/4K3 w - - 0 1")
	moves := engine{}.UnsafeMoves(pos)
	if len(moves) != 2 {
		t.Fatalf("expected 2 unsafe moves, got %d", len(moves))
	}
	expected := map[string]bool{"d1": true, "d2": true}
	for _, m := range moves {
		if m.s1 != E1 {
			t.Errorf("expected move from E1, got %s", m.s1.String())
		}
		if !expected[m.s2.String()] {
			t.Errorf("unexpected unsafe move to %s", m.s2.String())
		}
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
