package chess

import "testing"

func TestMagicHVAttackMatchesSlowAttack(t *testing.T) {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		for _, occupied := range magicOccupancies(rookMagicMasks[sq]) {
			got := hvAttack(occupied, square)
			want := slowHVAttack(occupied, square)
			if got != want {
				t.Fatalf("hvAttack(%s, %064b): got %064b, want %064b", square, occupied, got, want)
			}
		}
	}
}

func TestMagicDiaAttackMatchesSlowAttack(t *testing.T) {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		for _, occupied := range magicOccupancies(bishopMagicMasks[sq]) {
			got := diaAttack(occupied, square)
			want := slowDiaAttack(occupied, square)
			if got != want {
				t.Fatalf("diaAttack(%s, %064b): got %064b, want %064b", square, occupied, got, want)
			}
		}
	}
}
