package chess

import (
	"fmt"
	"testing"
)

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
