package chess

import "testing"

func TestPublicAttacks(t *testing.T) {
	// Knight on e4 attacks d6, f6, g5, g3, f2, d2, c3, c5.
	knight := KnightAttacks(E4)
	for _, sq := range []Square{D6, F6, G5, G3, F2, D2, C3, C5} {
		if !Bitboard(bbForSquare(sq)).Intersects(Bitboard(knight)) {
			t.Fatalf("expected knight on E4 to attack %s", sq)
		}
	}
	if knight.Popcount() != 8 {
		t.Fatalf("expected 8 knight attacks, got %d", knight.Popcount())
	}

	// King on e4 attacks 8 surrounding squares.
	king := KingAttacks(E4)
	if king.Popcount() != 8 {
		t.Fatalf("expected 8 king attacks, got %d", king.Popcount())
	}

	// White pawn on e4 attacks d5 and f5.
	wp := PawnAttacks(E4, White)
	if wp.Popcount() != 2 {
		t.Fatalf("expected 2 white pawn attacks, got %d", wp.Popcount())
	}
	if !Bitboard(bbForSquare(D5)).Intersects(Bitboard(wp)) || !Bitboard(bbForSquare(F5)).Intersects(Bitboard(wp)) {
		t.Fatalf("white pawn on E4 should attack D5 and F5, got %s", wp.Draw())
	}

	// Black pawn on e4 attacks d3 and f3.
	bp := PawnAttacks(E4, Black)
	if bp.Popcount() != 2 {
		t.Fatalf("expected 2 black pawn attacks, got %d", bp.Popcount())
	}
	if !Bitboard(bbForSquare(D3)).Intersects(Bitboard(bp)) || !Bitboard(bbForSquare(F3)).Intersects(Bitboard(bp)) {
		t.Fatalf("black pawn on E4 should attack D3 and F3, got %s", bp.Draw())
	}

	// Rook on d4 with starting occupancy: attacks along rank 4 and file d.
	occ := StartingPosition().Board().Occupied()
	rook := RookAttacks(D4, occ)
	if rook.IsEmpty() {
		t.Fatal("rook on d4 should attack squares")
	}
}
