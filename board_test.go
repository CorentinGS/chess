package chess_test

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func boardFromFEN(t *testing.T, fen string) *chess.Board {
	t.Helper()
	b := &chess.Board{}
	if err := b.UnmarshalText([]byte(fen)); err != nil {
		t.Fatal(err)
	}
	return b
}

var boardFENs = []struct {
	name string
	fen  string
}{
	{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"},
	{"empty", "8/8/8/8/8/8/8/8"},
	{"mid", "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R"},
	{"kiwipete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R"},
}

func TestBoard_TextRoundTrip(t *testing.T) {
	for _, tt := range boardFENs {
		t.Run(tt.name, func(t *testing.T) {
			b := boardFromFEN(t, tt.fen)
			txt, err := b.MarshalText()
			if err != nil {
				t.Fatal(err)
			}
			if string(txt) != tt.fen {
				t.Errorf("MarshalText = %q, want %q", string(txt), tt.fen)
			}
		})
	}
}

func TestBoard_BinaryRoundTrip(t *testing.T) {
	for _, tt := range boardFENs {
		t.Run(tt.name, func(t *testing.T) {
			b := boardFromFEN(t, tt.fen)
			data, err := b.MarshalBinary()
			if err != nil {
				t.Fatal(err)
			}
			if len(data) != 96 {
				t.Fatalf("MarshalBinary len = %d, want 96 (12 bitboards * 8 bytes)", len(data))
			}
			cp := &chess.Board{}
			if err := cp.UnmarshalBinary(data); err != nil {
				t.Fatal(err)
			}
			if cp.String() != tt.fen {
				t.Errorf("binary round-trip = %q, want %q", cp.String(), tt.fen)
			}
			redata, err := cp.MarshalBinary()
			if err != nil {
				t.Fatal(err)
			}
			if !bytes.Equal(data, redata) {
				t.Errorf("MarshalBinary not byte-order stable: %v != %v", data, redata)
			}
		})
	}
}

func TestBoard_SquareMapRoundTrip(t *testing.T) {
	for _, tt := range boardFENs {
		t.Run(tt.name, func(t *testing.T) {
			b := boardFromFEN(t, tt.fen)
			rebuilt, err := chess.NewBoard(b.SquareMap())
			if err != nil {
				t.Fatal(err)
			}
			if rebuilt.String() != b.String() {
				t.Errorf("NewBoard(SquareMap()) = %q, want %q", rebuilt.String(), b.String())
			}
		})
	}
}

func TestBoard_PieceAtKnownSquares(t *testing.T) {
	b := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	tests := []struct {
		sq   chess.Square
		want chess.Piece
	}{
		{chess.A1, chess.WhiteRook},
		{chess.E1, chess.WhiteKing},
		{chess.D1, chess.WhiteQueen},
		{chess.A2, chess.WhitePawn},
		{chess.E8, chess.BlackKing},
		{chess.D8, chess.BlackQueen},
		{chess.H8, chess.BlackRook},
		{chess.A7, chess.BlackPawn},
		{chess.E4, chess.NoPiece},
		{chess.D5, chess.NoPiece},
	}
	for _, tt := range tests {
		t.Run(tt.sq.String(), func(t *testing.T) {
			if got := b.Piece(tt.sq); got != tt.want {
				t.Errorf("Piece(%s) = %v, want %v", tt.sq, got, tt.want)
			}
		})
	}
}

func TestBoard_RotateNinetyDegreesClockwise(t *testing.T) {
	want := []string{
		"RP4pr/NP4pn/BP4pb/QP4pq/KP4pk/BP4pb/NP4pn/RP4pr",
		"RNBKQBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbkqbnr",
		"rp4PR/np4PN/bp4PB/kp4PK/qp4PQ/bp4PB/np4PN/rp4PR",
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
	}
	start := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	board := start
	for i, w := range want {
		t.Run(rotatedLabel(i+1), func(t *testing.T) {
			var err error
			board, err = board.Rotate()
			if err != nil {
				t.Fatalf("Rotate() error = %v", err)
			}
			if board.String() != w {
				t.Errorf("Rotate() = %q, want %q", board.String(), w)
			}
		})
	}
	rotated, err := start.Rotate()
	if err != nil {
		t.Fatal(err)
	}
	flipped, err := start.Flip(chess.UpDown)
	if err != nil {
		t.Fatal(err)
	}
	transposed, err := flipped.Transpose()
	if err != nil {
		t.Fatal(err)
	}
	if rotated.String() != transposed.String() {
		t.Errorf("Rotate() = %q, want Flip(UpDown).Transpose() = %q", rotated.String(), transposed.String())
	}
}

func rotatedLabel(n int) string {
	switch n {
	case 1:
		return "rotated_90cw"
	case 2:
		return "rotated_180"
	case 3:
		return "rotated_270cw"
	default:
		return "rotated_360_identity"
	}
}

func TestBoard_FlipIsInvolution(t *testing.T) {
	t.Run("UpDown_is_involution", func(t *testing.T) {
		start := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
		once, err := start.Flip(chess.UpDown)
		if err != nil {
			t.Fatal(err)
		}
		if want := "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr"; once.String() != want {
			t.Errorf("Flip(UpDown) = %q, want %q", once.String(), want)
		}
		twice, err := once.Flip(chess.UpDown)
		if err != nil {
			t.Fatal(err)
		}
		if twice.String() != start.String() {
			t.Errorf("Flip(UpDown).Flip(UpDown) = %q, want identity %q", twice.String(), start.String())
		}
	})
	t.Run("LeftRight_is_involution", func(t *testing.T) {
		start := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
		once, err := start.Flip(chess.LeftRight)
		if err != nil {
			t.Fatal(err)
		}
		if want := "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"; once.String() != want {
			t.Errorf("Flip(LeftRight) = %q, want %q", once.String(), want)
		}
		twice, err := once.Flip(chess.LeftRight)
		if err != nil {
			t.Fatal(err)
		}
		if twice.String() != start.String() {
			t.Errorf("Flip(LeftRight).Flip(LeftRight) = %q, want identity %q", twice.String(), start.String())
		}
	})
}

func TestBoard_TransposeIsInvolution(t *testing.T) {
	start := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	once, err := start.Transpose()
	if err != nil {
		t.Fatal(err)
	}
	if want := "rp4PR/np4PN/bp4PB/qp4PQ/kp4PK/bp4PB/np4PN/rp4PR"; once.String() != want {
		t.Errorf("Transpose() = %q, want %q", once.String(), want)
	}
	twice, err := once.Transpose()
	if err != nil {
		t.Fatal(err)
	}
	if twice.String() != start.String() {
		t.Errorf("Transpose().Transpose() = %q, want identity %q", twice.String(), start.String())
	}
}

func TestBoard_DrawReturnsASCIIArt(t *testing.T) {
	b := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	s := b.Draw()
	if !strings.Contains(s, " A B C D E F G H") {
		t.Errorf("Draw() missing file header, got:\n%s", s)
	}
	if !strings.Contains(s, "8") || !strings.Contains(s, "1") {
		t.Errorf("Draw() missing rank markers, got:\n%s", s)
	}
	if !strings.Contains(s, "♚") {
		t.Errorf("Draw() missing black king glyph, got:\n%s", s)
	}
	if !strings.Contains(s, "-") {
		t.Errorf("Draw() missing empty-square marker, got:\n%s", s)
	}
}

func TestBoard_Draw2AllPerspectives(t *testing.T) {
	b := boardFromFEN(t, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	for _, tc := range []struct {
		name     string
		persp    chess.Color
		darkMode bool
	}{
		{"white_light", chess.White, false},
		{"white_dark", chess.White, true},
		{"black_light", chess.Black, false},
		{"black_dark", chess.Black, true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			s := b.Draw2(tc.persp, tc.darkMode)
			if !strings.Contains(s, "♔") {
				t.Errorf("Draw2(%v,%v) missing white king glyph, got:\n%s", tc.persp, tc.darkMode, s)
			}
			if !strings.Contains(s, "-") {
				t.Errorf("Draw2(%v,%v) missing empty-square marker, got:\n%s", tc.persp, tc.darkMode, s)
			}
		})
	}
}

func TestBoard_UnmarshalTextErrors(t *testing.T) {
	tests := []struct {
		name string
		fen  string
	}{
		{"missing_rank", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP"},
		{"bad_char", "rnbqkbnZ/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"},
		{"empty", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			b := &chess.Board{}
			if err := b.UnmarshalText([]byte(tt.fen)); err == nil {
				t.Errorf("UnmarshalText(%q) = nil, want error", tt.fen)
			}
		})
	}
}

func TestBoard_UnmarshalBinaryErrors(t *testing.T) {
	for _, n := range []int{0, 95, 97} {
		t.Run(fmt.Sprintf("len_%d", n), func(t *testing.T) {
			b := &chess.Board{}
			if err := b.UnmarshalBinary(make([]byte, n)); err == nil {
				t.Errorf("UnmarshalBinary(len=%d) = nil, want error", n)
			}
		})
	}
}

func BenchmarkPieceMailbox(b *testing.B) {
	g := chess.NewGame()
	board := g.Position().Board()

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = board.Piece(chess.A1)
		_ = board.Piece(chess.E4)
		_ = board.Piece(chess.H8)
	}
}
