package chess

import "testing"

func assertSameSquares(t *testing.T, got []Square, want ...Square) {
	t.Helper()

	if len(got) != len(want) {
		t.Fatalf("got %v, want %v", got, want)
	}

	gotSet := make(map[Square]bool, len(got))
	for _, sq := range got {
		gotSet[sq] = true
	}

	for _, sq := range want {
		if !gotSet[sq] {
			t.Errorf("missing expected square %s; got %v", sq, got)
		}
	}
}

func TestBoardAttacksFromWhitePawn(t *testing.T) {
	pos := mustPosition(
		"4k3/8/8/8/4P3/8/8/4K3 w - - 0 1",
	)

	got := pos.Board().AttacksFrom(E4)

	assertSameSquares(t, got, D5, F5)
}

func TestBoardAttacksFromBlackPawn(t *testing.T) {
	pos := mustPosition(
		"4k3/8/8/4p3/8/8/8/4K3 b - - 0 1",
	)

	got := pos.Board().AttacksFrom(E5)

	assertSameSquares(t, got, D4, F4)
}

func TestBoardAttacksFromPawnOnAFile(t *testing.T) {
	pos := mustPosition(
		"4k3/8/8/8/P7/8/8/4K3 w - - 0 1",
	)

	got := pos.Board().AttacksFrom(A4)

	assertSameSquares(t, got, B5)
}

func TestBoardAttacksFromRookStopsAfterBlockers(t *testing.T) {
	pos := mustPosition(
		"4k3/8/3P4/8/1n1R1b2/8/3p4/4K3 w - - 0 1",
	)

	got := pos.Board().AttacksFrom(D4)

	assertSameSquares(
		t,
		got,
		D5, D6,
		D3, D2,
		C4, B4,
		E4, F4,
	)
}

func TestBoardAttacksFromEmptySquare(t *testing.T) {
	board := StartingPosition().Board()

	got := board.AttacksFrom(E4)

	if len(got) != 0 {
		t.Fatalf("expected no attacks, got %v", got)
	}
}

func TestBoardAttacksFromInvalidSquare(t *testing.T) {
	board := StartingPosition().Board()

	for _, sq := range []Square{NoSquare, Square(64)} {
		if got := board.AttacksFrom(sq); len(got) != 0 {
			t.Errorf("AttacksFrom(%d) = %v, want empty", sq, got)
		}
	}
}

func TestBoardAttacksFromIgnoresPins(t *testing.T) {
	pos := mustPosition(
		"4r1k1/8/8/8/8/8/4R3/4K3 w - - 0 1",
	)

	got := pos.Board().AttacksFrom(E2)

	// The rook is pinned against the white king, but it still attacks
	// horizontally for attack-map purposes.
	for _, expected := range []Square{A2, B2, C2, D2, F2, G2, H2} {
		found := false
		for _, actual := range got {
			if actual == expected {
				found = true
				break
			}
		}

		if !found {
			t.Errorf("expected pinned rook to attack %s", expected)
		}
	}
}

func TestBoardAttacksFromRemainingPiecesAndPawnHEdge(t *testing.T) {
	tests := []struct {
		name string
		fen  string
		from Square
		want []Square
	}{
		{
			name: "knight",
			fen:  "4k3/8/8/8/4N3/8/8/4K3 w - - 0 1",
			from: E4,
			want: []Square{C3, C5, D2, D6, F2, F6, G3, G5},
		},
		{
			name: "bishop",
			fen:  "4k3/8/8/8/4B3/8/8/4K3 w - - 0 1",
			from: E4,
			want: []Square{A8, B1, B7, C2, C6, D3, D5, F3, F5, G2, G6, H1, H7},
		},
		{
			name: "queen",
			fen:  "4k3/8/8/8/3Q4/8/8/4K3 w - - 0 1",
			from: D4,
			want: []Square{A1, A4, A7, B2, B4, B6, C3, C4, C5, D1, D2, D3, D5, D6, D7, D8, E3, E4, E5, F2, F4, F6, G1, G4, G7, H4, H8},
		},
		{
			name: "king",
			fen:  "4k3/8/8/8/4K3/8/8/8 w - - 0 1",
			from: E4,
			want: []Square{D3, D4, D5, E3, E5, F3, F4, F5},
		},
		{
			name: "white pawn H file",
			fen:  "4k3/8/8/8/7P/8/8/4K3 w - - 0 1",
			from: H4,
			want: []Square{G5},
		},
		{
			name: "black pawn H file",
			fen:  "4k3/8/8/7p/8/8/8/4K3 b - - 0 1",
			from: H5,
			want: []Square{G4},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			assertSameSquares(t, mustPosition(test.fen).Board().AttacksFrom(test.from), test.want...)
		})
	}
}
