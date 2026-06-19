package chess_test

import (
	"testing"

	"github.com/corentings/chess/v3"
)

func TestBoardTextSerialization(t *testing.T) {
	fen := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	b := &chess.Board{}
	if err := b.UnmarshalText([]byte(fen)); err != nil {
		t.Fatal("recieved unexpected error", err)
	}
	txt, err := b.MarshalText()
	if err != nil {
		t.Fatal("recieved unexpected error", err)
	}
	if fen != string(txt) {
		t.Fatalf("fen expected board string %s but got %s", fen, string(txt))
	}
}

func TestBoardBinarySerialization(t *testing.T) {
	g := chess.NewGame()
	board := g.Position().Board()
	b, err := board.MarshalBinary()
	if err != nil {
		t.Fatal("recieved unexpected error", err)
	}
	cpBoard := &chess.Board{}
	err = cpBoard.UnmarshalBinary(b)
	if err != nil {
		t.Fatal("recieved unexpected error", err)
	}
	s := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	if s != cpBoard.String() {
		t.Fatalf("expected board string %s but got %s", s, cpBoard.String())
	}
}

func TestBoardRotation(t *testing.T) {
	fens := []string{
		"RP4pr/NP4pn/BP4pb/QP4pq/KP4pk/BP4pb/NP4pn/RP4pr",
		"RNBKQBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbkqbnr",
		"rp4PR/np4PN/bp4PB/kp4PK/qp4PQ/bp4PB/np4PN/rp4PR",
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
	}
	g := chess.NewGame()
	board := g.Position().Board()
	for i := range fens {
		var err error
		board, err = board.Rotate()
		if err != nil {
			t.Fatalf("Rotate() error = %v", err)
		}
		if fens[i] != board.String() {
			t.Fatalf("expected board string %s but got %s", fens[i], board.String())
		}
	}
}

func TestBoardFlip(t *testing.T) {
	g := chess.NewGame()
	board := g.Position().Board()
	var err error
	board, err = board.Flip(chess.UpDown)
	if err != nil {
		t.Fatalf("Flip(UpDown) error = %v", err)
	}
	b := "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(chess.UpDown)
	if err != nil {
		t.Fatalf("Flip(UpDown) error = %v", err)
	}
	b = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(chess.LeftRight)
	if err != nil {
		t.Fatalf("Flip(LeftRight) error = %v", err)
	}
	b = "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(chess.LeftRight)
	if err != nil {
		t.Fatalf("Flip(LeftRight) error = %v", err)
	}
	b = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
}

func TestBoardTranspose(t *testing.T) {
	g := chess.NewGame()
	board := g.Position().Board()
	var err error
	board, err = board.Transpose()
	if err != nil {
		t.Fatalf("Transpose() error = %v", err)
	}
	b := "rp4PR/np4PN/bp4PB/qp4PQ/kp4PK/bp4PB/np4PN/rp4PR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
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
