package chess

import "testing"

func BenchmarkBoardAttacksFrom(b *testing.B) {
	pos := mustPosition(
		"r3k2r/ppp2ppp/2n1b3/3q4/3R4/2N1B3/PPP2PPP/R3K2R w KQkq - 0 1",
	)
	board := pos.Board()

	tests := []struct {
		name string
		sq   Square
	}{
		{"Pawn", A2},
		{"Knight", C3},
		{"Bishop", E3},
		{"Rook", D4},
		{"Queen", D5},
		{"King", E1},
	}

	for _, test := range tests {
		b.Run(test.name, func(b *testing.B) {
			b.ReportAllocs()

			for i := 0; i < b.N; i++ {
				_ = board.AttacksFrom(test.sq)
			}
		})
	}
}