package chess

import (
	"math/bits"
	"strconv"
	"strings"
)

// Bitboard is a public 64-bit set of squares. It is backed by a uint64 where
// the most significant bit represents A1 and the least significant bit
// represents H8, matching the internal bitboard layout.
type Bitboard uint64

// NewBitboardFromSquares returns a Bitboard with the given squares set.
func NewBitboardFromSquares(squares ...Square) Bitboard {
	var bb Bitboard
	for _, sq := range squares {
		bb |= Bitboard(bbForSquare(sq))
	}
	return bb
}

// And returns the intersection of bb and other.
func (bb Bitboard) And(other Bitboard) Bitboard {
	return bb & other
}

// Or returns the union of bb and other.
func (bb Bitboard) Or(other Bitboard) Bitboard {
	return bb | other
}

// Xor returns the symmetric difference of bb and other.
func (bb Bitboard) Xor(other Bitboard) Bitboard {
	return bb ^ other
}

// Not returns the complement of bb (all 64 squares, flipped).
func (bb Bitboard) Not() Bitboard {
	return ^bb
}

// Subtract returns the squares in bb that are not in other.
func (bb Bitboard) Subtract(other Bitboard) Bitboard {
	return bb &^ other
}

// Intersects reports whether bb and other share at least one square.
func (bb Bitboard) Intersects(other Bitboard) bool {
	return (bb & other) != 0
}

// IsEmpty reports whether bb contains no squares.
func (bb Bitboard) IsEmpty() bool {
	return bb == 0
}

// Popcount returns the number of set squares.
func (bb Bitboard) Popcount() int {
	return bits.OnesCount64(uint64(bb))
}

// FirstSquare returns the most significant set square. It panics if bb is empty.
func (bb Bitboard) FirstSquare() Square {
	if bb == 0 {
		panic("chess: FirstSquare called on empty Bitboard")
	}
	return squareFromBit(bitboard(bb))
}

// LastSquare returns the least significant set square. It panics if bb is empty.
func (bb Bitboard) LastSquare() Square {
	if bb == 0 {
		panic("chess: LastSquare called on empty Bitboard")
	}
	return Square(63 - bits.TrailingZeros64(uint64(bb)))
}

// Squares yields every set square using Go 1.23's range-over-func pattern.
//
// Example:
//
//	for sq := range bb.Squares {
//	    // process square
//	}
func (bb Bitboard) Squares(yield func(Square) bool) {
	for bb != 0 {
		lsb := bb & -bb
		if !yield(squareFromBit(bitboard(lsb))) {
			return
		}
		bb &^= lsb
	}
}

// String returns a 64-character string of '1's and '0's starting with the most
// significant bit (A1). This matches the internal bitboard.String() format and
// round-trips unambiguously.
func (bb Bitboard) String() string {
	s := strconv.FormatUint(uint64(bb), 2)
	return strings.Repeat("0", numOfSquaresInBoard-len(s)) + s
}

// Draw returns an 8x8 visual representation of the bitboard useful for debugging.
func (bb Bitboard) Draw() string {
	var sb strings.Builder
	sb.Grow(154)
	sb.WriteString("\n A B C D E F G H\n")
	for r := 7; r >= 0; r-- {
		sb.WriteString(Rank(r).String())
		for f := range numOfSquaresInRow {
			sq := NewSquare(File(f), Rank(r))
			if Bitboard(bbForSquare(sq))&bb != 0 {
				sb.WriteByte('1')
			} else {
				sb.WriteByte('0')
			}
			sb.WriteByte(' ')
		}
		sb.WriteByte('\n')
	}
	return sb.String()
}

// MarshalText implements encoding.TextMarshaler using the 64-character binary
// string representation.
func (bb Bitboard) MarshalText() ([]byte, error) {
	return []byte(bb.String()), nil
}

// UnmarshalText implements encoding.TextUnmarshaler from the 64-character
// binary string representation.
func (bb *Bitboard) UnmarshalText(text []byte) error {
	s := string(text)
	if len(s) != numOfSquaresInBoard {
		return strconv.ErrRange
	}
	v, err := strconv.ParseUint(s, 2, 64)
	if err != nil {
		return err
	}
	*bb = Bitboard(v)
	return nil
}
