package chess

import (
	"fmt"
	"testing"
)

func TestBoardTextSerialization(t *testing.T) {
	fen := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	b := &Board{}
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
	g := NewGame()
	board := g.Position().Board()
	b, err := board.MarshalBinary()
	if err != nil {
		t.Fatal("recieved unexpected error", err)
	}
	cpBoard := &Board{}
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
	g := NewGame()
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
	g := NewGame()
	board := g.Position().Board()
	var err error
	board, err = board.Flip(UpDown)
	if err != nil {
		t.Fatalf("Flip(UpDown) error = %v", err)
	}
	b := "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(UpDown)
	if err != nil {
		t.Fatalf("Flip(UpDown) error = %v", err)
	}
	b = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(LeftRight)
	if err != nil {
		t.Fatalf("Flip(LeftRight) error = %v", err)
	}
	b = "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
	board, err = board.Flip(LeftRight)
	if err != nil {
		t.Fatalf("Flip(LeftRight) error = %v", err)
	}
	b = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
	if b != board.String() {
		t.Fatalf("expected board string %s but got %s", b, board.String())
	}
}

func TestBoardTranspose(t *testing.T) {
	g := NewGame()
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

// TestMailboxConsistency verifies that the mailbox array stays in sync with bitboards
// across various board operations including moves, flips, transposes, and rotations.
func TestMailboxConsistency(t *testing.T) {
	g := NewGame()
	pos := g.Position()

	// Test 1: Starting position
	if err := verifyMailboxConsistency(pos.Board()); err != nil {
		t.Fatalf("starting position mailbox inconsistent: %v", err)
	}

	// Test 2: After several moves
	moves := []string{"e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "g8f6", "d2d3", "d7d5"}
	for _, moveStr := range moves {
		move, err := UCINotation{}.Decode(g.Position(), moveStr)
		if err != nil {
			t.Fatalf("failed to parse move %s: %v", moveStr, err)
		}
		if err := g.Move(move, nil); err != nil {
			t.Fatalf("failed to play move %s: %v", moveStr, err)
		}
	}
	if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
		t.Fatalf("after moves mailbox inconsistent: %v", err)
	}

	// Test 3: Board transformations
	board := g.Position().Board()

	flipped, err := board.Flip(UpDown)
	if err != nil {
		t.Fatalf("Flip error: %v", err)
	}
	if err := verifyMailboxConsistency(flipped); err != nil {
		t.Fatalf("flipped board mailbox inconsistent: %v", err)
	}

	transposed, err := board.Transpose()
	if err != nil {
		t.Fatalf("Transpose error: %v", err)
	}
	if err := verifyMailboxConsistency(transposed); err != nil {
		t.Fatalf("transposed board mailbox inconsistent: %v", err)
	}

	rotated, err := board.Rotate()
	if err != nil {
		t.Fatalf("Rotate error: %v", err)
	}
	if err := verifyMailboxConsistency(rotated); err != nil {
		t.Fatalf("rotated board mailbox inconsistent: %v", err)
	}
}

// verifyMailboxConsistency checks that every square in the mailbox matches the
// piece found by scanning bitboards.
func verifyMailboxConsistency(b *Board) error {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		mailboxPiece := b.Piece(square)
		bitboardPiece := NoPiece

		// Find which piece (if any) occupies this square in bitboards
		for _, p := range allPieces {
			if b.bbForPiece(p).Occupied(square) {
				bitboardPiece = p
				break
			}
		}

		if mailboxPiece != bitboardPiece {
			return fmt.Errorf("mailbox/bitboard mismatch at %s: mailbox=%s, bitboard=%s",
				square.String(), mailboxPiece.String(), bitboardPiece.String())
		}
	}
	return nil
}

func BenchmarkPieceMailbox(b *testing.B) {
	g := NewGame()
	board := g.Position().Board()

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = board.Piece(A1)
		_ = board.Piece(E4)
		_ = board.Piece(H8)
	}
}
