package chess_test

import (
	"bytes"
	"fmt"
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
			if got, err := chess.ParsePieceType(pt.String()); err != nil || got != pt {
				t.Errorf("ParsePieceType(%q) = %v, %v, want %v, nil", pt.String(), got, err, pt)
			}
			if got, err := chess.ParsePieceTypeFromByte(pt.Bytes()[0]); err != nil || got != pt {
				t.Errorf("ParsePieceTypeFromByte(%q) = %v, %v, want %v, nil", pt.Bytes(), got, err, pt)
			}
		})
	}
}

func TestParsePieceTypeFromByte(t *testing.T) {
	t.Parallel()
	valid := []struct {
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
		{"upper_K", 'K', chess.King},
		{"upper_Q", 'Q', chess.Queen},
	}
	for _, tt := range valid {
		t.Run("valid/"+tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := chess.ParsePieceTypeFromByte(tt.in)
			if err != nil {
				t.Fatalf("ParsePieceTypeFromByte(%q): %v", tt.in, err)
			}
			if got != tt.want {
				t.Errorf("ParsePieceTypeFromByte(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
	invalid := []byte{'1', '?', 'x'}
	for _, in := range invalid {
		t.Run(fmt.Sprintf("invalid/%q", in), func(t *testing.T) {
			t.Parallel()
			if got, err := chess.ParsePieceTypeFromByte(in); err == nil {
				t.Errorf("ParsePieceTypeFromByte(%q) = %v, want error", in, got)
			}
		})
	}
}

func TestParsePieceType(t *testing.T) {
	t.Parallel()
	valid := []struct {
		name string
		in   string
		want chess.PieceType
	}{
		{"lower_k", "k", chess.King},
		{"upper_K", "K", chess.King},
		{"upper_Q", "Q", chess.Queen},
		{"upper_N", "N", chess.Knight},
	}
	for _, tt := range valid {
		t.Run("valid/"+tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := chess.ParsePieceType(tt.in)
			if err != nil {
				t.Fatalf("ParsePieceType(%q): %v", tt.in, err)
			}
			if got != tt.want {
				t.Errorf("ParsePieceType(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
	invalid := []string{"", "kk", "?"}
	for _, in := range invalid {
		t.Run("invalid/"+in, func(t *testing.T) {
			t.Parallel()
			if got, err := chess.ParsePieceType(in); err == nil {
				t.Errorf("ParsePieceType(%q) = %v, want error", in, got)
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

func TestParseColor(t *testing.T) {
	valid := []struct {
		name string
		in   string
		want chess.Color
	}{
		{"lower_w", "w", chess.White},
		{"lower_b", "b", chess.Black},
		{"upper_W", "W", chess.White},
		{"upper_B", "B", chess.Black},
	}
	for _, tt := range valid {
		t.Run("valid/"+tt.name, func(t *testing.T) {
			got, err := chess.ParseColor(tt.in)
			if err != nil {
				t.Fatalf("ParseColor(%q): %v", tt.in, err)
			}
			if got != tt.want {
				t.Errorf("ParseColor(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
	invalid := []string{"x", ""}
	for _, in := range invalid {
		t.Run("invalid/"+in, func(t *testing.T) {
			if got, err := chess.ParseColor(in); err == nil {
				t.Errorf("ParseColor(%q) = %v, want error", in, got)
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
