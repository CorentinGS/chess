/*
Package chess provides a chess engine implementation using bitboard representation for board state.

The package uses a combination of bitboards for piece positions and convenience lookups,
allowing for efficient move generation and position analysis.

Board Layout:

    8 | r n b q k b n r
    7 | p p p p p p p p
    6 | - - - - - - - -
    5 | - - - - - - - -
    4 | - - - - - - - -
    3 | - - - - - - - -
    2 | P P P P P P P P
    1 | R N B Q K B N R
      ---------------
        A B C D E F G H

Usage:

    // Create a new board with starting position
    squares := map[Square]Piece{
        NewSquare(FileE, Rank1): WhiteKing,
        NewSquare(FileD, Rank8): BlackQueen,
    }
    board := NewBoard(squares)

    // Check piece at square
    piece := board.Piece(NewSquare(FileE, Rank1))

    // Get all piece positions.
    positions := board.SquareMap()
*/

package chess

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
)

// Board represents a chess board and its relationship between squares and pieces.
// It maintains separate bitboards for each piece type and color, along with
// convenience bitboards for quick position analysis.
type Board struct {
	bbWhiteKing   bitboard
	bbWhiteQueen  bitboard
	bbWhiteRook   bitboard
	bbWhiteBishop bitboard
	bbWhiteKnight bitboard
	bbWhitePawn   bitboard
	bbBlackKing   bitboard
	bbBlackQueen  bitboard
	bbBlackRook   bitboard
	bbBlackBishop bitboard
	bbBlackKnight bitboard
	bbBlackPawn   bitboard
	whiteSqs      bitboard                   // all white pieces
	blackSqs      bitboard                   // all black pieces
	emptySqs      bitboard                   // all empty squares
	whiteKingSq   Square                     // cached white king square
	blackKingSq   Square                     // cached black king square
	mailbox       [numOfSquaresInBoard]Piece // O(1) piece lookup per square
}

// kingSquare returns the cached king square for the given color.
// Callers must ensure the board's convenience bitboards are up to date
// (i.e., calcConvienceBBs has been called after any mutation).
func (b *Board) kingSquare(c Color) Square {
	if c == White {
		return b.whiteKingSq
	}
	return b.blackKingSq
}

// NewBoard returns a board from a square to piece mapping.
// The map should contain only occupied squares.
//
// Example:
//
//	squares := map[Square]Piece{
//	    NewSquare(FileE, Rank1): WhiteKing,
//	    NewSquare(FileE, Rank8): BlackKing,
//	}
//	board, err := NewBoard(squares)
func NewBoard(m map[Square]Piece) (*Board, error) {
	b := &Board{}
	for sq := range numOfSquaresInBoard {
		b.mailbox[sq] = NoPiece
	}
	for _, p1 := range allPieces {
		var bb uint64
		for sq := range numOfSquaresInBoard {
			bb <<= 1
			if p2, exists := m[Square(sq)]; exists && p1 == p2 {
				bb |= 1
				b.mailbox[sq] = p1
			}
		}
		if err := b.setBBForPiece(p1, bitboard(bb)); err != nil {
			return nil, err
		}
	}
	b.calcConvienceBBs(nil)
	return b, nil
}

// SquareMap returns a mapping of squares to pieces.
// A square is only added to the map if it is occupied.
func (b *Board) SquareMap() map[Square]Piece {
	m := map[Square]Piece{}
	for sq := range numOfSquaresInBoard {
		p := b.Piece(Square(sq))
		if p != NoPiece {
			m[Square(sq)] = p
		}
	}
	return m
}

// Rotate rotates the board 90 degrees clockwise.
func (b *Board) Rotate() (*Board, error) {
	flipped, err := b.Flip(UpDown)
	if err != nil {
		return nil, err
	}
	return flipped.Transpose()
}

// FlipDirection is the direction for the Board.Flip method.
type FlipDirection int

const (
	// UpDown flips the board's rank values.
	UpDown FlipDirection = iota
	// LeftRight flips the board's file values.
	LeftRight
)

// Flip returns a new board flipped over the specified axis.
// For UpDown, pieces are mirrored across the horizontal center line.
// For LeftRight, pieces are mirrored across the vertical center line.
func (b *Board) Flip(fd FlipDirection) (*Board, error) {
	m := map[Square]Piece{}
	for sq := range numOfSquaresInBoard {
		var mv Square
		switch fd {
		case UpDown:
			file := Square(sq).File()
			rank := 7 - Square(sq).Rank()
			mv = NewSquare(file, rank)
		case LeftRight:
			file := 7 - Square(sq).File()
			rank := Square(sq).Rank()
			mv = NewSquare(file, rank)
		}
		m[mv] = b.Piece(Square(sq))
	}
	return NewBoard(m)
}

// Transpose flips the board over the A8 to H1 diagonal.
func (b *Board) Transpose() (*Board, error) {
	m := map[Square]Piece{}
	for sq := range numOfSquaresInBoard {
		file := File(7 - Square(sq).Rank())
		rank := Rank(7 - Square(sq).File())
		mv := NewSquare(file, rank)
		m[mv] = b.Piece(Square(sq))
	}
	return NewBoard(m)
}

// Draw returns a visual ASCII representation of the board.
// Capital letters represent white pieces, lowercase represent black pieces.
// Empty squares are shown as "-".
//
// Example output:
//
//	  A B C D E F G H
//	8 r n b q k b n r
//	7 p p p p p p p p
//	6 - - - - - - - -
//	5 - - - - - - - -
//	4 - - - - - - - -
//	3 - - - - - - - -
//	2 P P P P P P P P
//	1 R N B Q K B N R
func (b *Board) Draw() string {
	return b.drawForWhite(false)
}

// Draw2 returns visual representation of the board useful for debugging.
// It is similar to Draw() except allows the caller to specify perspective
// and dark mode options
func (b *Board) Draw2(perspective Color, darkMode bool) string {
	if perspective == Black {
		return b.drawForBlack(darkMode)
	} // else

	return b.drawForWhite(darkMode)
}

// drawForWhite returns visual representation of the board from white's perspective
func (b *Board) drawForWhite(darkMode bool) string {
	s := "\n A B C D E F G H\n"
	for r := 7; r >= 0; r-- {
		s += Rank(r).String()
		for f := range numOfSquaresInRow {
			p := b.Piece(NewSquare(File(f), Rank(r)))
			if p == NoPiece {
				s += "-"
			} else {
				if darkMode {
					s += p.DarkString()
				} else {
					s += p.String()
				}
			}
			s += " "
		}
		s += "\n"
	}
	return s
}

// drawForBlack returns visual representation of the board from black's perspective
func (b *Board) drawForBlack(darkMode bool) string {
	s := "\n H G F E D C B A\n"
	for r := 0; r <= 7; r++ {
		s += Rank(r).String()
		for f := numOfSquaresInRow - 1; f >= 0; f-- {
			p := b.Piece(NewSquare(File(f), Rank(r)))
			if p == NoPiece {
				s += "-"
			} else {
				if darkMode {
					s += p.DarkString()
				} else {
					s += p.String()
				}
			}
			s += " "
		}
		s += "\n"
	}
	return s
}

// String implements the fmt.Stringer interface and returns
// a string in the FEN board format: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR.
func (b *Board) String() string {
	const maxRankValue = 7
	const numOfFiles = 8

	// Use a buffer to build the string
	buf := make([]byte, 0, 71)

	// Buffer to count empty squares
	emptyCount := 0

	// Process each rank
	for r := maxRankValue; r >= 0; r-- {
		// Add rank separator except for first rank
		if r < maxRankValue {
			buf = append(buf, '/')
		}

		// Process each file in the rank
		for f := range numOfFiles {
			sq := NewSquare(File(f), Rank(r))
			p := b.Piece(sq)

			if p == NoPiece {
				emptyCount++
				continue
			}

			// If we had empty squares before this piece, write the count
			if emptyCount > 0 {
				buf = append(buf, byte('0'+emptyCount))
				emptyCount = 0
			}

			// Write the piece character
			buf = append(buf, p.getFENChar())
		}

		// Handle empty squares at end of rank
		if emptyCount > 0 {
			buf = append(buf, byte('0'+emptyCount))
			emptyCount = 0
		}
	}

	// Convert to string once at the end
	return string(buf)
}

// Piece returns the piece for the given square.
// Returns NoPiece if the square is empty.
func (b *Board) Piece(sq Square) Piece {
	return b.mailbox[sq]
}

// MarshalText implements the encoding.TextMarshaler interface and returns
// a string in the FEN board format: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR.
func (b *Board) MarshalText() ([]byte, error) {
	return []byte(b.String()), nil
}

// UnmarshalText implements the encoding.TextUnarshaler interface and takes
// a string in the FEN board format: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR.
func (b *Board) UnmarshalText(text []byte) error {
	cp, err := fenBoard(string(text))
	if err != nil {
		return err
	}
	*b = *cp
	return nil
}

// MarshalBinary implements the encoding.BinaryMarshaler interface and returns
// the bitboard representations as a array of bytes.  Bitboads are encoded
// in the following order: WhiteKing, WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight
// WhitePawn, BlackKing, BlackQueen, BlackRook, BlackBishop, BlackKnight, BlackPawn.
func (b *Board) MarshalBinary() ([]byte, error) {
	bbs := []bitboard{
		b.bbWhiteKing, b.bbWhiteQueen, b.bbWhiteRook, b.bbWhiteBishop, b.bbWhiteKnight, b.bbWhitePawn,
		b.bbBlackKing, b.bbBlackQueen, b.bbBlackRook, b.bbBlackBishop, b.bbBlackKnight, b.bbBlackPawn,
	}
	buf := new(bytes.Buffer)
	err := binary.Write(buf, binary.BigEndian, bbs)
	return buf.Bytes(), err
}

// UnmarshalBinary implements the encoding.BinaryUnmarshaler interface and parses
// the bitboard representations as a array of bytes.  Bitboads are decoded
// in the following order: WhiteKing, WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight
// WhitePawn, BlackKing, BlackQueen, BlackRook, BlackBishop, BlackKnight, BlackPawn.
func (b *Board) UnmarshalBinary(data []byte) error {
	const expectedSize = 96

	if len(data) != expectedSize {
		return errors.New("chess: invalid number of bytes for board unmarshal binary")
	}
	b.bbWhiteKing = bitboard(binary.BigEndian.Uint64(data[:8]))
	b.bbWhiteQueen = bitboard(binary.BigEndian.Uint64(data[8:16]))
	b.bbWhiteRook = bitboard(binary.BigEndian.Uint64(data[16:24]))
	b.bbWhiteBishop = bitboard(binary.BigEndian.Uint64(data[24:32]))
	b.bbWhiteKnight = bitboard(binary.BigEndian.Uint64(data[32:40]))
	b.bbWhitePawn = bitboard(binary.BigEndian.Uint64(data[40:48]))
	b.bbBlackKing = bitboard(binary.BigEndian.Uint64(data[48:56]))
	b.bbBlackQueen = bitboard(binary.BigEndian.Uint64(data[56:64]))
	b.bbBlackRook = bitboard(binary.BigEndian.Uint64(data[64:72]))
	b.bbBlackBishop = bitboard(binary.BigEndian.Uint64(data[72:80]))
	b.bbBlackKnight = bitboard(binary.BigEndian.Uint64(data[80:88]))
	b.bbBlackPawn = bitboard(binary.BigEndian.Uint64(data[88:96]))
	b.calcConvienceBBs(nil)
	b.rebuildMailbox()
	return nil
}

//nolint:mnd // magic number is used for bitboard size.
func (b *Board) update(m Move) {
	p1 := b.Piece(m.s1)
	s1BB := bbForSquare(m.s1)
	s2BB := bbForSquare(m.s2)

	// move s1 piece to s2
	for _, p := range allPieces {
		bb := b.bbForPiece(p)
		// remove what was at s2
		if err := b.setBBForPiece(p, bb & ^s2BB); err != nil {
			panic(fmt.Sprintf("chess: invariant violation in board update: %v", err))
		}
		// move what was at s1 to s2
		if bb.Occupied(m.s1) {
			bb = b.bbForPiece(p)
			if err := b.setBBForPiece(p, (bb & ^s1BB)|s2BB); err != nil {
				panic(fmt.Sprintf("chess: invariant violation in board update: %v", err))
			}
		}
	}
	// check promotion
	if m.promo != NoPieceType && p1 != NoPiece {
		newPiece := NewPiece(m.promo, p1.Color())
		// remove pawn
		bbPawn := b.bbForPiece(p1)
		if err := b.setBBForPiece(p1, bbPawn & ^s1BB & ^s2BB); err != nil {
			panic(fmt.Sprintf("chess: invariant violation in board update: %v", err))
		}
		// add promo piece
		bbPromo := b.bbForPiece(newPiece)
		if err := b.setBBForPiece(newPiece, bbPromo|s2BB); err != nil {
			panic(fmt.Sprintf("chess: invariant violation in board update: %v", err))
		}
		b.mailbox[m.s1] = NoPiece
		b.mailbox[m.s2] = newPiece
	} else {
		b.mailbox[m.s1] = NoPiece
		b.mailbox[m.s2] = p1
	}
	// remove captured en passant piece
	if m.HasTag(EnPassant) {
		if p1.Color() == White {
			b.bbBlackPawn = ^(bbForSquare(m.s2) << 8) & b.bbBlackPawn
			b.mailbox[m.s2-8] = NoPiece
		} else {
			b.bbWhitePawn = ^(bbForSquare(m.s2) >> 8) & b.bbWhitePawn
			b.mailbox[m.s2+8] = NoPiece
		}
	}
	// move rook for castle
	switch {
	case p1.Color() == White && m.HasTag(KingSideCastle):
		b.bbWhiteRook = b.bbWhiteRook & ^bbForSquare(H1) | bbForSquare(F1)
		b.mailbox[H1] = NoPiece
		b.mailbox[F1] = WhiteRook
	case p1.Color() == White && m.HasTag(QueenSideCastle):
		b.bbWhiteRook = (b.bbWhiteRook & ^bbForSquare(A1)) | bbForSquare(D1)
		b.mailbox[A1] = NoPiece
		b.mailbox[D1] = WhiteRook
	case p1.Color() == Black && m.HasTag(KingSideCastle):
		b.bbBlackRook = b.bbBlackRook & ^bbForSquare(H8) | bbForSquare(F8)
		b.mailbox[H8] = NoPiece
		b.mailbox[F8] = BlackRook
	case p1.Color() == Black && m.HasTag(QueenSideCastle):
		b.bbBlackRook = (b.bbBlackRook & ^bbForSquare(A8)) | bbForSquare(D8)
		b.mailbox[A8] = NoPiece
		b.mailbox[D8] = BlackRook
	}

	b.calcConvienceBBs(&m)
}

func (b *Board) calcConvienceBBs(m *Move) {
	whiteSqs := b.bbWhiteKing | b.bbWhiteQueen | b.bbWhiteRook | b.bbWhiteBishop | b.bbWhiteKnight | b.bbWhitePawn
	blackSqs := b.bbBlackKing | b.bbBlackQueen | b.bbBlackRook | b.bbBlackBishop | b.bbBlackKnight | b.bbBlackPawn
	emptySqs := ^(whiteSqs | blackSqs)
	b.whiteSqs = whiteSqs
	b.blackSqs = blackSqs
	b.emptySqs = emptySqs
	switch {
	case m == nil:
		b.whiteKingSq = NoSquare
		b.blackKingSq = NoSquare

		for sq := range numOfSquaresInBoard {
			sqr := Square(sq)
			if b.bbWhiteKing.Occupied(sqr) {
				b.whiteKingSq = sqr
			} else if b.bbBlackKing.Occupied(sqr) {
				b.blackKingSq = sqr
			}
		}
	case m.s1 == b.whiteKingSq:
		b.whiteKingSq = m.s2
	case m.s1 == b.blackKingSq:
		b.blackKingSq = m.s2
	}
}

func (b *Board) copy() *Board {
	cp := &Board{
		whiteSqs:      b.whiteSqs,
		blackSqs:      b.blackSqs,
		emptySqs:      b.emptySqs,
		whiteKingSq:   b.whiteKingSq,
		blackKingSq:   b.blackKingSq,
		bbWhiteKing:   b.bbWhiteKing,
		bbWhiteQueen:  b.bbWhiteQueen,
		bbWhiteRook:   b.bbWhiteRook,
		bbWhiteBishop: b.bbWhiteBishop,
		bbWhiteKnight: b.bbWhiteKnight,
		bbWhitePawn:   b.bbWhitePawn,
		bbBlackKing:   b.bbBlackKing,
		bbBlackQueen:  b.bbBlackQueen,
		bbBlackRook:   b.bbBlackRook,
		bbBlackBishop: b.bbBlackBishop,
		bbBlackKnight: b.bbBlackKnight,
		bbBlackPawn:   b.bbBlackPawn,
	}
	cp.mailbox = b.mailbox
	return cp
}

// rebuildMailbox reconstructs the mailbox from the current bitboard state.
// Used after deserialization or when consistency is otherwise needed.
func (b *Board) rebuildMailbox() {
	for sq := range numOfSquaresInBoard {
		b.mailbox[sq] = NoPiece
	}
	for _, p := range allPieces {
		bb := b.bbForPiece(p)
		for sq := range numOfSquaresInBoard {
			if bb.Occupied(Square(sq)) {
				b.mailbox[sq] = p
			}
		}
	}
}

func (b *Board) isOccupied(sq Square) bool {
	return !b.emptySqs.Occupied(sq)
}

func (b *Board) hasSufficientMaterial() bool {
	// queen, rook, or pawn exist
	if (b.bbWhiteQueen | b.bbWhiteRook | b.bbWhitePawn |
		b.bbBlackQueen | b.bbBlackRook | b.bbBlackPawn) > 0 {
		return true
	}
	// if king is missing then it is a test
	if b.bbWhiteKing == 0 || b.bbBlackKing == 0 {
		return true
	}
	count := map[PieceType]int{}
	pieceMap := b.SquareMap()
	for _, p := range pieceMap {
		count[p.Type()]++
	}
	// 	king versus king
	if count[Bishop] == 0 && count[Knight] == 0 {
		return false
	}
	// king and bishop versus king
	if count[Bishop] == 1 && count[Knight] == 0 {
		return false
	}
	// king and knight versus king
	if count[Bishop] == 0 && count[Knight] == 1 {
		return false
	}
	// king and bishop(s) versus king and bishop(s) with the bishops on the same colour.
	if count[Knight] == 0 {
		whiteCount := 0
		blackCount := 0
		for sq, p := range pieceMap {
			if p.Type() == Bishop {
				switch sq.color() {
				case White:
					whiteCount++
				case Black:
					blackCount++
				}
			}
		}
		if whiteCount == 0 || blackCount == 0 {
			return false
		}
	}
	return true
}

func (b *Board) bbForPiece(p Piece) bitboard {
	switch p {
	case WhiteKing:
		return b.bbWhiteKing
	case WhiteQueen:
		return b.bbWhiteQueen
	case WhiteRook:
		return b.bbWhiteRook
	case WhiteBishop:
		return b.bbWhiteBishop
	case WhiteKnight:
		return b.bbWhiteKnight
	case WhitePawn:
		return b.bbWhitePawn
	case BlackKing:
		return b.bbBlackKing
	case BlackQueen:
		return b.bbBlackQueen
	case BlackRook:
		return b.bbBlackRook
	case BlackBishop:
		return b.bbBlackBishop
	case BlackKnight:
		return b.bbBlackKnight
	case BlackPawn:
		return b.bbBlackPawn
	}
	return bitboard(0)
}

func (b *Board) setBBForPiece(p Piece, bb bitboard) error {
	switch p {
	case WhiteKing:
		b.bbWhiteKing = bb
	case WhiteQueen:
		b.bbWhiteQueen = bb
	case WhiteRook:
		b.bbWhiteRook = bb
	case WhiteBishop:
		b.bbWhiteBishop = bb
	case WhiteKnight:
		b.bbWhiteKnight = bb
	case WhitePawn:
		b.bbWhitePawn = bb
	case BlackKing:
		b.bbBlackKing = bb
	case BlackQueen:
		b.bbBlackQueen = bb
	case BlackRook:
		b.bbBlackRook = bb
	case BlackBishop:
		b.bbBlackBishop = bb
	case BlackKnight:
		b.bbBlackKnight = bb
	case BlackPawn:
		b.bbBlackPawn = bb
	default:
		return fmt.Errorf("invalid piece %s", p)
	}
	return nil
}
