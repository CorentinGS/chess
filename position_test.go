package chess

import (
	"testing"
)

func TestPositionBinary(t *testing.T) {
	for _, fen := range validFENs {
		pos, err := decodeFEN(fen)
		if err != nil {
			t.Fatal(err)
		}
		b, err := pos.MarshalBinary()
		if err != nil {
			t.Fatal(err)
		}
		cp := &Position{}
		if err := cp.UnmarshalBinary(b); err != nil {
			t.Fatal(err)
		}
		if pos.String() != cp.String() {
			t.Fatalf("expected %s but got %s", pos.String(), cp.String())
		}
	}
}

func TestPositionUpdate(t *testing.T) {
	for _, fen := range validFENs {
		pos, err := decodeFEN(fen)
		if err != nil {
			t.Fatal(err)
		}

		{
			np := pos.Update(&pos.ValidMoves()[0])
			if pos.Turn().Other() != np.turn {
				t.Fatal("expected other turn")
			}
			if pos.halfMoveClock+1 != np.halfMoveClock {
				t.Fatal("expected half move clock increment")
			}
			if pos.board.String() == np.board.String() {
				t.Fatal("expected board update")
			}
		}

		{
			np := pos.Update(nil)
			if pos.Turn().Other() != np.turn {
				t.Fatal("expected other turn")
			}
			if pos.halfMoveClock+1 != np.halfMoveClock {
				t.Fatal("expected half move clock increment")
			}
			if pos.board.String() != np.board.String() {
				t.Fatal("expected same board")
			}
		}
	}
}

func TestPositionPly(t *testing.T) {
	tests := []struct {
		moveCount int
		turn      Color
		want      int
	}{
		{moveCount: 0, turn: White, want: 0},
		{moveCount: 1, turn: White, want: 1},
		{moveCount: 1, turn: Black, want: 2},
		{moveCount: 2, turn: White, want: 3},
		{moveCount: 2, turn: Black, want: 4},
		{moveCount: 10, turn: White, want: 19},
		{moveCount: 10, turn: Black, want: 20},
	}

	for _, tt := range tests {
		pos := &Position{
			moveCount: tt.moveCount,
			turn:      tt.turn,
		}
		got := pos.Ply()
		if got != tt.want {
			t.Errorf("Ply() with moveCount=%d, turn=%v: got %d, want %d", tt.moveCount, tt.turn, got, tt.want)
		}
	}
}

func TestSamePositionEnPassantFIDECompliance(t *testing.T) {
	// FIDE Article 9.2.2: positions are the same only if "the possible
	// moves of all the pieces are the same". An en passant square should
	// only matter when an en passant capture is actually possible.

	// Position with en passant square set but no pawn can capture:
	// White pawn on e4 (just pushed e2-e4), en passant square e3,
	// but no black pawn on d4 or f4 to capture.
	posWithIrrelevantEP, err := decodeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
	if err != nil {
		t.Fatal(err)
	}

	// Same board position but without en passant square set.
	posWithoutEP, err := decodeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
	if err != nil {
		t.Fatal(err)
	}

	// These should be considered the same position because no en passant
	// capture is possible (no black pawn on d4 or f4).
	if !posWithIrrelevantEP.samePosition(posWithoutEP) {
		t.Error("positions with irrelevant en passant square should be considered the same")
	}

	// Position where en passant IS possible:
	// White pawn on e4, black pawn on d4. En passant square e3.
	// Black pawn on d4 can capture en passant on e3.
	posWithRelevantEP, err := decodeFEN("rnbqkbnr/ppp1pppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
	if err != nil {
		t.Fatal(err)
	}

	// Same board but without en passant square.
	posWithRelevantNoEP, err := decodeFEN("rnbqkbnr/ppp1pppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
	if err != nil {
		t.Fatal(err)
	}

	// These should NOT be considered the same because the en passant
	// capture is actually possible.
	if posWithRelevantEP.samePosition(posWithRelevantNoEP) {
		t.Error("positions with relevant en passant square should be considered different")
	}

	// Test with black pawn on f4 (right side of e4 pawn).
	posWithRelevantEPRight, err := decodeFEN("rnbqkbnr/pppp1ppp/8/8/4Pp2/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
	if err != nil {
		t.Fatal(err)
	}

	posWithRelevantEPRightNoEP, err := decodeFEN("rnbqkbnr/pppp1ppp/8/8/4Pp2/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
	if err != nil {
		t.Fatal(err)
	}

	if posWithRelevantEPRight.samePosition(posWithRelevantEPRightNoEP) {
		t.Error("positions with relevant en passant square (right adjacent pawn) should be considered different")
	}
}
