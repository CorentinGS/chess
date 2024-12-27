/*
Package chess provides a chess engine implementation using bitboard representation.

The package uses bitboards (64-bit integers) to represent the chess board state,
where each bit corresponds to a square on the board. The squares are numbered
from 0 to 63, starting from the most significant bit (A1) to the least
significant bit (H8):

	8 | 56 57 58 59 60 61 62 63
	7 | 48 49 50 51 52 53 54 55
	6 | 40 41 42 43 44 45 46 47
	5 | 32 33 34 35 36 37 38 39
	4 | 24 25 26 27 28 29 30 31
	3 | 16 17 18 19 20 21 22 23
	2 | 08 09 10 11 12 13 14 15
	1 | 00 01 02 03 04 05 06 07
	  -------------------------
	    A  B  C  D  E  F  G  H

A bit value of 1 indicates the presence of a piece, while 0 indicates an empty square.

Usage:

	// Create a new bitboard with pieces on A1 and E4
	squares := map[Square]bool{
	    NewSquare(FileA, Rank1): true,
	    NewSquare(FileE, Rank4): true,
	}
	bb := newBitboard(squares)

	// Check if E4 is occupied
	if bb.Occupied(NewSquare(FileE, Rank4)) {
	    fmt.Println("E4 is occupied")
	}

	// Print board representation
	fmt.Println(bb.Draw())
*/
package chess

import (
	"math/bits"
	"strconv"
	"strings"
)

// bitboard represents a chess board as a 64-bit integer. Each bit corresponds
// to a square on the board, with the most significant bit representing A1
// and the least significant bit representing H8.
type bitboard uint64

// newBitboard creates a bitboard from a map of squares. The map keys are Square
// values and the boolean values indicate whether each square is occupied.
//
// Example:
//
//	squares := map[Square]bool{
//	    NewSquare(FileA, Rank1): true,
//	    NewSquare(FileE, Rank4): true,
//	}
//	bb := newBitboard(squares)
func newBitboard(m map[Square]bool) bitboard {
	var bb uint64
	for sq := range numOfSquaresInBoard {
		bb <<= 1
		if m[Square(sq)] {
			bb |= 1
		}
	}
	return bitboard(bb)
}

// Mapping returns a map where the keys are Square values and the values
// indicate whether each square is occupied on the bitboard.
//
// The returned map can be used to iterate over occupied squares or convert
// the bitboard to other board representations.
func (b bitboard) Mapping() map[Square]bool {
	m := map[Square]bool{}
	for sq := range numOfSquaresInBoard {
		if b&bbForSquare(Square(sq)) > 0 {
			m[Square(sq)] = true
		}
	}
	return m
}

// String returns a 64 character string of 1s and 0s starting with the most significant bit.
func (b bitboard) String() string {
	s := strconv.FormatUint(uint64(b), 2)
	return strings.Repeat("0", numOfSquaresInBoard-len(s)) + s
}

// Draw returns visual representation of the bitboard useful for debugging.
func (b bitboard) Draw() string {
	s := "\n A B C D E F G H\n"
	for r := 7; r >= 0; r-- {
		s += Rank(r).String()
		for f := range numOfSquaresInRow {
			sq := NewSquare(File(f), Rank(r))
			if b.Occupied(sq) {
				s += "1"
			} else {
				s += "0"
			}
			s += " "
		}
		s += "\n"
	}
	return s
}

// Reverse returns a new bitboard with the bit order reversed, which can be
// useful for operations that require working with the board from the opposite
// perspective.
//
// Example:
//
//	bb := bitboard(0x8000000000000001)  // Pieces on A1 and H8
//	reversed := bb.Reverse()             // Pieces on A8 and H1
func (b bitboard) Reverse() bitboard {
	return bitboard(bits.Reverse64(uint64(b)))
}

// Occupied returns true if the given square's corresponding bit is set to 1
// on the bitboard.
//
// Example:
//
//	sq := NewSquare(FileE, Rank4)
//	if bb.Occupied(sq) {
//	    fmt.Printf("Square %v is occupied\n", sq)
//	}
func (b bitboard) Occupied(sq Square) bool {
	return (bits.RotateLeft64(uint64(b), int(sq)+1) & 1) == 1
}
