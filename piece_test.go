package chess_test

import (
	"bytes"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPieceType_FENString(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		pt   chess.PieceType
		want string
	}{
		{"king", chess.King, "k"},
		{"queen", chess.Queen, "q"},
		{"rook", chess.Rook, "r"},
		{"bishop", chess.Bishop, "b"},
		{"knight", chess.Knight, "n"},
		{"pawn", chess.Pawn, "p"},
		{"none", chess.NoPieceType, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := tt.pt.String(); got != tt.want {
				t.Errorf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestPieceType_Bytes(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		pt   chess.PieceType
		want []byte
	}{
		{"king", chess.King, []byte{'k'}},
		{"queen", chess.Queen, []byte{'q'}},
		{"rook", chess.Rook, []byte{'r'}},
		{"bishop", chess.Bishop, []byte{'b'}},
		{"knight", chess.Knight, []byte{'n'}},
		{"pawn", chess.Pawn, []byte{'p'}},
		{"none", chess.NoPieceType, []byte{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if !bytes.Equal(tt.pt.Bytes(), tt.want) {
				t.Errorf("Bytes() = %v, want %v", tt.pt.Bytes(), tt.want)
			}
		})
	}
}

func TestPieceType_ToPolyglotPromotionValue(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		pt   chess.PieceType
		want int
	}{
		{"knight", chess.Knight, 1},
		{"bishop", chess.Bishop, 2},
		{"rook", chess.Rook, 3},
		{"queen", chess.Queen, 4},
		{"king_is_zero", chess.King, 0},
		{"pawn_is_zero", chess.Pawn, 0},
		{"none_is_zero", chess.NoPieceType, 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := tt.pt.ToPolyglotPromotionValue(); got != tt.want {
				t.Errorf("ToPolyglotPromotionValue() = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestPieceType_StringRoundTrip(t *testing.T) {
	t.Parallel()
	for _, pt := range chess.PieceTypes() {
		t.Run(pt.String(), func(t *testing.T) {
			t.Parallel()
			if got := chess.PieceTypeFromString(pt.String()); got != pt {
				t.Errorf("PieceTypeFromString(%q) = %v, want %v", pt.String(), got, pt)
			}
			if got := chess.PieceTypeFromByte(pt.Bytes()[0]); got != pt {
				t.Errorf("PieceTypeFromByte(%q) = %v, want %v", pt.Bytes(), got, pt)
			}
		})
	}
}

func TestPieceTypeFromByte(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		in   byte
		want chess.PieceType
	}{
		{"lower_k", 'k', chess.King},
		{"lower_q", 'q', chess.Queen},
		{"lower_r", 'r', chess.Rook},
		{"lower_b", 'b', chess.Bishop},
		{"lower_n", 'n', chess.Knight},
		{"lower_p", 'p', chess.Pawn},
		{"upper_K", 'K', chess.NoPieceType},
		{"digit", '1', chess.NoPieceType},
		{"symbol", '?', chess.NoPieceType},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := chess.PieceTypeFromByte(tt.in); got != tt.want {
				t.Errorf("PieceTypeFromByte(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
}

func TestPieceTypeFromString(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		in   string
		want chess.PieceType
	}{
		{"lower_k", "k", chess.King},
		{"upper_K", "K", chess.King},
		{"upper_Q", "Q", chess.Queen},
		{"upper_N", "N", chess.Knight},
		{"empty", "", chess.NoPieceType},
		{"multi_char", "kk", chess.NoPieceType},
		{"invalid", "?", chess.NoPieceType},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if got := chess.PieceTypeFromString(tt.in); got != tt.want {
				t.Errorf("PieceTypeFromString(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
}

func TestPieceTypes_ReturnsSixTypesInCanonicalOrder(t *testing.T) {
	want := [6]chess.PieceType{chess.King, chess.Queen, chess.Rook, chess.Bishop, chess.Knight, chess.Pawn}
	if got := chess.PieceTypes(); got != want {
		t.Errorf("PieceTypes() = %v, want %v", got, want)
	}
}

func TestColor_FENString(t *testing.T) {
	tests := []struct {
		name string
		c    chess.Color
		want string
	}{
		{"white", chess.White, "w"},
		{"black", chess.Black, "b"},
		{"none", chess.NoColor, "-"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.c.String(); got != tt.want {
				t.Errorf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestColor_Name(t *testing.T) {
	tests := []struct {
		name string
		c    chess.Color
		want string
	}{
		{"white", chess.White, "White"},
		{"black", chess.Black, "Black"},
		{"none", chess.NoColor, "No Color"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.c.Name(); got != tt.want {
				t.Errorf("Name() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestColor_Other(t *testing.T) {
	tests := []struct {
		name string
		c    chess.Color
		want chess.Color
	}{
		{"white_reverses_to_black", chess.White, chess.Black},
		{"black_reverses_to_white", chess.Black, chess.White},
		{"none_stays_none", chess.NoColor, chess.NoColor},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.c.Other(); got != tt.want {
				t.Errorf("Other() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestColorFromString(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want chess.Color
	}{
		{"lower_w", "w", chess.White},
		{"lower_b", "b", chess.Black},
		{"upper_W", "W", chess.White},
		{"upper_B", "B", chess.Black},
		{"invalid", "x", chess.NoColor},
		{"empty", "", chess.NoColor},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := chess.ColorFromString(tt.in); got != tt.want {
				t.Errorf("ColorFromString(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
}

func TestPiece_TypeColorAndConstruction(t *testing.T) {
	tests := []struct {
		name  string
		piece chess.Piece
		pt    chess.PieceType
		c     chess.Color
	}{
		{"white_king", chess.WhiteKing, chess.King, chess.White},
		{"white_queen", chess.WhiteQueen, chess.Queen, chess.White},
		{"white_rook", chess.WhiteRook, chess.Rook, chess.White},
		{"white_bishop", chess.WhiteBishop, chess.Bishop, chess.White},
		{"white_knight", chess.WhiteKnight, chess.Knight, chess.White},
		{"white_pawn", chess.WhitePawn, chess.Pawn, chess.White},
		{"black_king", chess.BlackKing, chess.King, chess.Black},
		{"black_queen", chess.BlackQueen, chess.Queen, chess.Black},
		{"black_rook", chess.BlackRook, chess.Rook, chess.Black},
		{"black_bishop", chess.BlackBishop, chess.Bishop, chess.Black},
		{"black_knight", chess.BlackKnight, chess.Knight, chess.Black},
		{"black_pawn", chess.BlackPawn, chess.Pawn, chess.Black},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.piece.Type(); got != tt.pt {
				t.Errorf("Type() = %v, want %v", got, tt.pt)
			}
			if got := tt.piece.Color(); got != tt.c {
				t.Errorf("Color() = %v, want %v", got, tt.c)
			}
			if got := chess.NewPiece(tt.pt, tt.c); got != tt.piece {
				t.Errorf("NewPiece(%v,%v) = %v, want %v", tt.pt, tt.c, got, tt.piece)
			}
		})
	}
}

func TestPiece_StringAndDarkString(t *testing.T) {
	tests := []struct {
		name      string
		piece     chess.Piece
		str, dark string
	}{
		{"white_king", chess.WhiteKing, "♔", "♚"},
		{"white_queen", chess.WhiteQueen, "♕", "♛"},
		{"white_rook", chess.WhiteRook, "♖", "♜"},
		{"white_bishop", chess.WhiteBishop, "♗", "♝"},
		{"white_knight", chess.WhiteKnight, "♘", "♞"},
		{"white_pawn", chess.WhitePawn, "♙", "♟"},
		{"black_king", chess.BlackKing, "♚", "♔"},
		{"black_queen", chess.BlackQueen, "♛", "♕"},
		{"black_rook", chess.BlackRook, "♜", "♖"},
		{"black_bishop", chess.BlackBishop, "♝", "♗"},
		{"black_knight", chess.BlackKnight, "♞", "♘"},
		{"black_pawn", chess.BlackPawn, "♟", "♙"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.piece.String(); got != tt.str {
				t.Errorf("String() = %q, want %q", got, tt.str)
			}
			if got := tt.piece.DarkString(); got != tt.dark {
				t.Errorf("DarkString() = %q, want %q", got, tt.dark)
			}
		})
	}
}

func TestPiece_DarkStringReversesColors(t *testing.T) {
	for _, pt := range chess.PieceTypes() {
		t.Run(pt.String(), func(t *testing.T) {
			white := chess.NewPiece(pt, chess.White)
			black := chess.NewPiece(pt, chess.Black)
			if white.DarkString() != black.String() {
				t.Errorf("white DarkString() = %q, want black String() = %q", white.DarkString(), black.String())
			}
			if white.String() != black.DarkString() {
				t.Errorf("white String() = %q, want black DarkString() = %q", white.String(), black.DarkString())
			}
		})
	}
}

func TestNoPiece_IsEmptySentinel(t *testing.T) {
	if got := chess.NoPiece.String(); got != "-" {
		t.Errorf("NoPiece.String() = %q, want %q", got, "-")
	}
	if got := chess.NoPiece.Type(); got != chess.NoPieceType {
		t.Errorf("NoPiece.Type() = %v, want %v", got, chess.NoPieceType)
	}
	if got := chess.NoPiece.Color(); got != chess.NoColor {
		t.Errorf("NoPiece.Color() = %v, want %v", got, chess.NoColor)
	}
}

func TestNewPiece_ReturnsNoPieceForInvalidInputs(t *testing.T) {
	tests := []struct {
		name string
		pt   chess.PieceType
		c    chess.Color
	}{
		{"invalid_type", chess.NoPieceType, chess.White},
		{"invalid_color", chess.King, chess.NoColor},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := chess.NewPiece(tt.pt, tt.c); got != chess.NoPiece {
				t.Errorf("NewPiece(%v,%v) = %v, want NoPiece", tt.pt, tt.c, got)
			}
		})
	}
}
