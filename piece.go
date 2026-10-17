package chess

import (
	"fmt"
	"strings"
)

// Color represents the color of a chess piece.
type Color int8

const (
	// NoColor represents no color.
	NoColor Color = iota
	// White represents the color white.
	White
	// Black represents the color black.
	Black
)

// ParseColor converts a FEN side-to-move character ("w" or "b") to a Color.
// It returns an error for any other input.
func ParseColor(s string) (Color, error) {
	switch strings.ToLower(s) {
	case "w":
		return White, nil
	case "b":
		return Black, nil
	}
	return NoColor, fmt.Errorf("chess: invalid color %q", s)
}

// ColorFromString converts a FEN side-to-move character ("w" or "b") to a Color.
// Returns NoColor for any other input.
//
// Deprecated: use ParseColor for new code; it distinguishes parse failures from
// the NoColor sentinel.
func ColorFromString(s string) Color {
	c, _ := ParseColor(s)
	return c
}

// Other returns the opposite color of the receiver.
func (c Color) Other() Color {
	switch c {
	case White:
		return Black
	case Black:
		return White
	}
	return NoColor
}

// String implements the fmt.Stringer interface and returns.
// the color's FEN compatible notation.
func (c Color) String() string {
	switch c {
	case White:
		return "w"
	case Black:
		return "b"
	}
	return "-"
}

// Name returns a display friendly name.
func (c Color) Name() string {
	switch c {
	case White:
		return "White"
	case Black:
		return "Black"
	}
	return "No Color"
}

// PieceType is the type of a piece.
type PieceType int8

const (
	// NoPieceType represents a lack of piece type.
	NoPieceType PieceType = iota
	// King represents a king.
	King
	// Queen represents a queen.
	Queen
	// Rook represents a rook.
	Rook
	// Bishop represents a bishop.
	Bishop
	// Knight represents a knight.
	Knight
	// Pawn represents a pawn.
	Pawn
)

// PieceTypes returns a slice of all piece types.
func PieceTypes() [6]PieceType {
	return [6]PieceType{King, Queen, Rook, Bishop, Knight, Pawn}
}

// ParsePieceTypeFromByte parses a FEN piece-type character (case-insensitive)
// into a PieceType. It returns an error for any other byte.
func ParsePieceTypeFromByte(b byte) (PieceType, error) {
	switch b {
	case 'k', 'K':
		return King, nil
	case 'q', 'Q':
		return Queen, nil
	case 'r', 'R':
		return Rook, nil
	case 'b', 'B':
		return Bishop, nil
	case 'n', 'N':
		return Knight, nil
	case 'p', 'P':
		return Pawn, nil
	}
	return NoPieceType, fmt.Errorf("chess: invalid piece type byte %q", b)
}

// PieceTypeFromByte parses a FEN piece-type character (case-insensitive) into a
// PieceType. Returns NoPieceType for any other byte.
//
// Deprecated: use ParsePieceTypeFromByte for new code; it distinguishes parse
// failures from the NoPieceType sentinel.
func PieceTypeFromByte(b byte) PieceType {
	pt, _ := ParsePieceTypeFromByte(b)
	return pt
}

// ParsePieceType parses a single-character piece notation into a PieceType.
// It returns an error if the input is empty or longer than one character, or
// if the character is not a valid piece type.
func ParsePieceType(s string) (PieceType, error) {
	if len(s) != 1 {
		return NoPieceType, fmt.Errorf("chess: invalid piece type %q", s)
	}
	return ParsePieceTypeFromByte(strings.ToLower(s)[0])
}

// PieceTypeFromString parses a single-character piece notation into a
// PieceType. Returns NoPieceType for invalid input.
//
// Deprecated: use ParsePieceType for new code; it distinguishes parse failures
// from the NoPieceType sentinel.
func PieceTypeFromString(s string) PieceType {
	pt, _ := ParsePieceType(s)
	return pt
}

func (p PieceType) String() string {
	switch p {
	case King:
		return "k"
	case Queen:
		return "q"
	case Rook:
		return "r"
	case Bishop:
		return "b"
	case Knight:
		return "n"
	case Pawn:
		return "p"
	}
	return ""
}

func (p PieceType) Bytes() []byte {
	switch p {
	case King:
		return []byte{'k'}
	case Queen:
		return []byte{'q'}
	case Rook:
		return []byte{'r'}
	case Bishop:
		return []byte{'b'}
	case Knight:
		return []byte{'n'}
	case Pawn:
		return []byte{'p'}
	case NoPieceType:
		return []byte{}
	}
	return []byte{}
}

func (p PieceType) ToPolyglotPromotionValue() int {
	switch p {
	case Knight:
		return 1
	case Bishop:
		return 2
	case Rook:
		return 3
	case Queen:
		return 4
	default:
		return 0
	}
}

// Piece is a piece type with a color.
type Piece int8

const (
	// NoPiece represents no piece.
	NoPiece Piece = iota
	// WhiteKing is a white king.
	WhiteKing
	// WhiteQueen is a white queen.
	WhiteQueen
	// WhiteRook is a white rook.
	WhiteRook
	// WhiteBishop is a white bishop.
	WhiteBishop
	// WhiteKnight is a white knight.
	WhiteKnight
	// WhitePawn is a white pawn.
	WhitePawn
	// BlackKing is a black king.
	BlackKing
	// BlackQueen is a black queen.
	BlackQueen
	// BlackRook is a black rook.
	BlackRook
	// BlackBishop is a black bishop.
	BlackBishop
	// BlackKnight is a black knight.
	BlackKnight
	// BlackPawn is a black pawn.
	BlackPawn
)

// allPieces is an immutable array of all piece types.
// Treat as read-only; do not modify elements.
//
//nolint:gochecknoglobals // Immutable lookup table.
var allPieces = [12]Piece{
	WhiteKing, WhiteQueen, WhiteRook, WhiteBishop, WhiteKnight, WhitePawn,
	BlackKing, BlackQueen, BlackRook, BlackBishop, BlackKnight, BlackPawn,
}

//nolint:gochecknoglobals // Immutable lookup tables.
var (
	pieceTypes = [13]PieceType{
		NoPieceType,
		King, Queen, Rook, Bishop, Knight, Pawn,
		King, Queen, Rook, Bishop, Knight, Pawn,
	}
	pieceColors = [13]Color{
		NoColor,
		White, White, White, White, White, White,
		Black, Black, Black, Black, Black, Black,
	}
)

// NewPiece returns the piece matching the PieceType and Color.
// NoPiece is returned if the PieceType or Color isn't valid.
func NewPiece(t PieceType, c Color) Piece {
	if t < King || t > Pawn {
		return NoPiece
	}
	switch c {
	case White:
		return Piece(t)
	case Black:
		return Piece(int(t) + 6)
	}
	return NoPiece
}

// Type returns the type of the piece.
func (p Piece) Type() PieceType {
	if p < NoPiece || int(p) >= len(pieceTypes) {
		return NoPieceType
	}
	return pieceTypes[p]
}

// Color returns the color of the piece.
func (p Piece) Color() Color {
	if p < NoPiece || int(p) >= len(pieceColors) {
		return NoColor
	}
	return pieceColors[p]
}

// String implements the fmt.Stringer interface.
func (p Piece) String() string {
	if p == NoPiece {
		return "-"
	}
	return pieceUnicodes[int(p)]
}

// DarkString is equivalent to String() except colors reversed for terminal
// windows in dark mode.
func (p Piece) DarkString() string {
	return pieceDarkUnicodes[int(p)]
}

// pieceUnicodes and pieceDarkUnicodes are immutable lookup tables for piece unicode symbols.
// Treat as read-only; do not modify elements.
//
//nolint:gochecknoglobals // Immutable lookup tables.
var (
	pieceUnicodes     = [13]string{" ", "♔", "♕", "♖", "♗", "♘", "♙", "♚", "♛", "♜", "♝", "♞", "♟"}
	pieceDarkUnicodes = [13]string{" ", "♚", "♛", "♜", "♝", "♞", "♟", "♔", "♕", "♖", "♗", "♘", "♙"}
)

// getFENChar returns the FEN character representation of a piece
// Returns a single byte representing the piece.
func (p Piece) getFENChar() byte {
	pieceType := p.Type()
	if pieceType < 0 || pieceType > 6 {
		return 0 // Invalid piece type
	}

	if p.Color() == White {
		return whitePiecesToFEN[pieceType]
	}
	return blackPiecesToFEN[pieceType]
}
