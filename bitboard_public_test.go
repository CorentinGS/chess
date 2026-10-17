package chess

import (
	"slices"
	"testing"
)

func TestBitboardFromSquares(t *testing.T) {
	bb := NewBitboardFromSquares(E4, D4)
	if bb.Popcount() != 2 {
		t.Fatalf("expected popcount 2, got %d", bb.Popcount())
	}
	var got []Square
	for sq := range bb.Squares {
		got = append(got, sq)
	}
	want := []Square{E4, D4}
	if !slices.Equal(got, want) {
		t.Fatalf("expected squares %v, got %v", want, got)
	}
}

func TestBitboardSetOps(t *testing.T) {
	bb := NewBitboardFromSquares(E4, D4)
	bb2 := NewBitboardFromSquares(D4, C4)

	if bb.And(bb2).Popcount() != 1 {
		t.Fatal("expected intersection popcount 1")
	}
	if bb.Or(bb2).Popcount() != 3 {
		t.Fatal("expected union popcount 3")
	}
	if bb.Subtract(bb2).Popcount() != 1 {
		t.Fatal("expected subtract popcount 1")
	}
	if !bb.Intersects(bb2) {
		t.Fatal("expected bb and bb2 to intersect")
	}
	if !NewBitboardFromSquares().IsEmpty() {
		t.Fatal("expected empty bitboard")
	}
}

func TestBitboardStringRoundTrip(t *testing.T) {
	bb := NewBitboardFromSquares(A1, H8)
	s := bb.String()
	var rt Bitboard
	if err := rt.UnmarshalText([]byte(s)); err != nil {
		t.Fatal(err)
	}
	if rt != bb {
		t.Fatalf("round-trip failed: %v != %v", rt, bb)
	}
}
