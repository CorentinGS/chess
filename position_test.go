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
