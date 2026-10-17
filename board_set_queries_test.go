package chess

import "testing"

func TestBoardBitboardQueries(t *testing.T) {
	b := StartingPosition().Board()

	if b.White().Popcount() != 16 {
		t.Fatalf("expected 16 white pieces, got %d", b.White().Popcount())
	}
	if b.Black().Popcount() != 16 {
		t.Fatalf("expected 16 black pieces, got %d", b.Black().Popcount())
	}
	if b.Occupied().Popcount() != 32 {
		t.Fatalf("expected 32 occupied squares, got %d", b.Occupied().Popcount())
	}
	if b.Empty().Popcount() != 32 {
		t.Fatalf("expected 32 empty squares, got %d", b.Empty().Popcount())
	}
	if b.Pieces(Pawn, White).Popcount() != 8 {
		t.Fatalf("expected 8 white pawns, got %d", b.Pieces(Pawn, White).Popcount())
	}
	if b.Color(Black).Popcount() != 16 {
		t.Fatalf("expected 16 black pieces via Color, got %d", b.Color(Black).Popcount())
	}
}
