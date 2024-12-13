package chess

import (
	"reflect"
	"testing"
)

type bitboardTestPair struct {
	initial  uint64
	reversed uint64
}

type bitboardTestNew struct {
	smap map[Square]bool
	bits uint64
}

var (
	tests = []bitboardTestPair{
		{
			uint64(1),
			uint64(9223372036854775808),
		},
		{
			uint64(18446744073709551615),
			uint64(18446744073709551615),
		},
		{
			uint64(0),
			uint64(0),
		},
	}
	newBitboardTests = []bitboardTestNew{
		{
			map[Square]bool{},
			0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
			// all zeroes
		},
		{
			map[Square]bool{
				A1: true,
			},
			0b_10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
			// ^
		},
		{
			map[Square]bool{
				B1: true,
			},
			0b_01000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000,
			//  ^
		},
		{
			map[Square]bool{
				B3: true,
			},
			0b_00000000_00000000_01000000_00000000_00000000_00000000_00000000_00000000,
			//                    ^
		},
		{
			map[Square]bool{
				H8: true,
			},
			0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001,
			//                                                                       ^
		},
		{
			map[Square]bool{
				A1: true,
				B3: true,
				H8: true,
			},
			0b_10000000_00000000_01000000_00000000_00000000_00000000_00000000_00000001,
			// ^                  ^                                                  ^
		},
	}
)

func TestBitboardReverse(t *testing.T) {
	for _, p := range tests {
		r := uint64(bitboard(p.initial).Reverse())
		if r != p.reversed {
			t.Fatalf("bitboard reverse of %s expected %s but got %s", intStr(p.initial), intStr(p.reversed), intStr(r))
		}
	}
}

func TestBitboardOccupied(t *testing.T) {
	m := map[Square]bool{
		B3: true,
	}
	bb := newBitboard(m)
	if bb.Occupied(B3) != true {
		t.Fatalf("bitboard occupied of %s expected %t but got %t", bb, true, false)
	}
}

func BenchmarkBitboardReverse(b *testing.B) {
	for i := 0; i < b.N; i++ {
		u := uint64(9223372036854775807)
		bitboard(u).Reverse()
	}
}

func intStr(i uint64) string {
	return bitboard(i).String()
}

func TestBitboardNew(t *testing.T) {
	for _, c := range newBitboardTests {
		bb := newBitboard(c.smap)
		if uint64(bb) != c.bits {
			t.Fatalf("new bitboard from %v expected %s but got %s", c.smap, intStr(c.bits), bb)
		}
	}
}

func TestMappingEmptyBitboard(t *testing.T) {
	bb := bitboard(0)
	expected := map[Square]bool{}
	result := bb.Mapping()
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf("expected %v but got %v", expected, result)
	}
}

func TestMappingSingleSquare(t *testing.T) {
	bb := bitboard(0b_10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000)
	expected := map[Square]bool{A1: true}
	result := bb.Mapping()
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf("expected %v but got %v", expected, result)
	}
}

func TestMappingMultipleSquares(t *testing.T) {
	bb := bitboard(0b_10000000_00000000_01000000_00000000_00000000_00000000_00000000_00000001)
	expected := map[Square]bool{A1: true, B3: true, H8: true}
	result := bb.Mapping()
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf("expected %v but got %v", expected, result)
	}
}

func TestMappingFullBitboard(t *testing.T) {
	bb := bitboard(0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111)
	expected := map[Square]bool{}
	for sq := range numOfSquaresInBoard {
		expected[Square(sq)] = true
	}
	result := bb.Mapping()
	if !reflect.DeepEqual(result, expected) {
		t.Fatalf("expected %v but got %v", expected, result)
	}
}
