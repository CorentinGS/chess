package chess_test

import (
	"testing"

	"github.com/corentings/chess/v3"
)

func TestNewSquare_FromFileAndRank(t *testing.T) {
	tests := []struct {
		name string
		f    chess.File
		r    chess.Rank
		want chess.Square
	}{
		{"a1", chess.FileA, chess.Rank1, chess.A1},
		{"a8", chess.FileA, chess.Rank8, chess.A8},
		{"h1", chess.FileH, chess.Rank1, chess.H1},
		{"h8", chess.FileH, chess.Rank8, chess.H8},
		{"b4", chess.FileB, chess.Rank4, chess.B4},
		{"e8", chess.FileE, chess.Rank8, chess.E8},
		{"h3", chess.FileH, chess.Rank3, chess.H3},
		{"d7", chess.FileD, chess.Rank7, chess.D7},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := chess.NewSquare(tt.f, tt.r); got != tt.want {
				t.Errorf("NewSquare(%v,%v) = %s, want %s", tt.f, tt.r, got.String(), tt.want.String())
			}
		})
	}
}

func TestNewSquare_RoundTripsThroughFileAndRank(t *testing.T) {
	for _, sq := range allSquares() {
		t.Run(sq.String(), func(t *testing.T) {
			if got := chess.NewSquare(sq.File(), sq.Rank()); got != sq {
				t.Errorf("NewSquare(%s.File(), %s.Rank()) = %s, want %s", sq, sq, got, sq)
			}
		})
	}
}

func TestSquare_StringRoundTrip(t *testing.T) {
	for _, sq := range allSquares() {
		t.Run(sq.String(), func(t *testing.T) {
			if got := chess.SquareFromString(sq.String()); got != sq {
				t.Errorf("SquareFromString(%q) = %v, want %v", sq.String(), got, sq)
			}
		})
	}
}

func TestSquare_Bytes(t *testing.T) {
	for _, sq := range allSquares() {
		t.Run(sq.String(), func(t *testing.T) {
			b := sq.Bytes()
			if len(b) != 2 {
				t.Fatalf("%s.Bytes() len = %d, want 2", sq, len(b))
			}
			if string(b) != sq.String() {
				t.Errorf("%s.Bytes() = %q, want %q", sq, string(b), sq.String())
			}
		})
	}
}

func TestSquareFromString(t *testing.T) {
	valid := []struct {
		in   string
		want chess.Square
	}{
		{"a1", chess.A1},
		{"a8", chess.A8},
		{"h1", chess.H1},
		{"h8", chess.H8},
		{"e4", chess.E4},
		{"d1", chess.D1},
		{"g5", chess.G5},
	}
	for _, tt := range valid {
		t.Run("valid/"+tt.in, func(t *testing.T) {
			if got := chess.SquareFromString(tt.in); got != tt.want {
				t.Errorf("SquareFromString(%q) = %v, want %v", tt.in, got, tt.want)
			}
		})
	}
	invalid := []string{"", "a", "a9", "a0", "i1", "z9", "A1", "e44", " e4", "e4 ", "##", "e"}
	for _, in := range invalid {
		t.Run("invalid/"+in, func(t *testing.T) {
			if got := chess.SquareFromString(in); got != chess.NoSquare {
				t.Errorf("SquareFromString(%q) = %v, want NoSquare", in, got)
			}
		})
	}
}

func TestFile_StringAndByte(t *testing.T) {
	tests := []struct {
		name string
		f    chess.File
		str  string
		b    byte
	}{
		{"file_a", chess.FileA, "a", 'a'},
		{"file_b", chess.FileB, "b", 'b'},
		{"file_c", chess.FileC, "c", 'c'},
		{"file_d", chess.FileD, "d", 'd'},
		{"file_e", chess.FileE, "e", 'e'},
		{"file_f", chess.FileF, "f", 'f'},
		{"file_g", chess.FileG, "g", 'g'},
		{"file_h", chess.FileH, "h", 'h'},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.f.String(); got != tt.str {
				t.Errorf("String() = %q, want %q", got, tt.str)
			}
			if got := tt.f.Byte(); got != tt.b {
				t.Errorf("Byte() = %q, want %q", got, tt.b)
			}
		})
	}
}

func TestRank_StringAndByte(t *testing.T) {
	tests := []struct {
		name string
		r    chess.Rank
		str  string
		b    byte
	}{
		{"rank_1", chess.Rank1, "1", '1'},
		{"rank_2", chess.Rank2, "2", '2'},
		{"rank_3", chess.Rank3, "3", '3'},
		{"rank_4", chess.Rank4, "4", '4'},
		{"rank_5", chess.Rank5, "5", '5'},
		{"rank_6", chess.Rank6, "6", '6'},
		{"rank_7", chess.Rank7, "7", '7'},
		{"rank_8", chess.Rank8, "8", '8'},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.r.String(); got != tt.str {
				t.Errorf("String() = %q, want %q", got, tt.str)
			}
			if got := tt.r.Byte(); got != tt.b {
				t.Errorf("Byte() = %q, want %q", got, tt.b)
			}
		})
	}
}

func TestNoSquare_IsMinusOne(t *testing.T) {
	if chess.NoSquare != -1 {
		t.Errorf("NoSquare = %d, want -1", chess.NoSquare)
	}
	if got := chess.SquareFromString("not-a-square"); got != chess.NoSquare {
		t.Errorf("SquareFromString(invalid) = %v, want NoSquare", got)
	}
}

func allSquares() []chess.Square {
	squares := make([]chess.Square, 0, 64)
	for r := chess.Rank1; r <= chess.Rank8; r++ {
		for f := chess.FileA; f <= chess.FileH; f++ {
			squares = append(squares, chess.NewSquare(f, r))
		}
	}
	return squares
}
