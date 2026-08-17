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
