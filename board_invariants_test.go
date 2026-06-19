package chess

import (
	"fmt"
	"testing"
)

func TestMailboxConsistency(t *testing.T) {
	t.Run("starting_position", func(t *testing.T) {
		g := NewGame()
		if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("after_quiet_moves", func(t *testing.T) {
		g := NewGame()
		moves := []string{"e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "g8f6", "d2d3", "d7d5"}
		for _, moveStr := range moves {
			move, err := UCINotation{}.Decode(g.Position(), moveStr)
			if err != nil {
				t.Fatalf("failed to parse move %s: %v", moveStr, err)
			}
			if err := g.Move(move, nil); err != nil {
				t.Fatalf("failed to play move %s: %v", moveStr, err)
			}
			if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
				t.Fatalf("mailbox inconsistent after %s: %v", moveStr, err)
			}
		}
	})

	t.Run("castling", func(t *testing.T) {
		g := NewGame()
		moves := []string{"e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "g8f6", "e1g1", "f8c5", "d2d3", "e8g8"}
		for _, moveStr := range moves {
			move, err := UCINotation{}.Decode(g.Position(), moveStr)
			if err != nil {
				t.Fatalf("failed to parse move %s: %v", moveStr, err)
			}
			if err := g.Move(move, nil); err != nil {
				t.Fatalf("failed to play move %s: %v", moveStr, err)
			}
			if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
				t.Fatalf("mailbox inconsistent after castling %s: %v", moveStr, err)
			}
		}
	})

	t.Run("en_passant", func(t *testing.T) {
		g := NewGame()
		moves := []string{"e2e4", "a7a6", "e4e5", "d7d5", "e5d6"}
		for _, moveStr := range moves {
			move, err := UCINotation{}.Decode(g.Position(), moveStr)
			if err != nil {
				t.Fatalf("failed to parse move %s: %v", moveStr, err)
			}
			if err := g.Move(move, nil); err != nil {
				t.Fatalf("failed to play move %s: %v", moveStr, err)
			}
			if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
				t.Fatalf("mailbox inconsistent after en passant %s: %v", moveStr, err)
			}
		}
	})

	t.Run("promotion", func(t *testing.T) {
		opt, err := FEN("4k3/P7/8/8/8/8/8/4K3 w - - 0 1")
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(opt)
		moves := []string{"a7a8q"}
		for _, moveStr := range moves {
			move, err := UCINotation{}.Decode(g.Position(), moveStr)
			if err != nil {
				t.Fatalf("failed to parse move %s: %v", moveStr, err)
			}
			if err := g.Move(move, nil); err != nil {
				t.Fatalf("failed to play move %s: %v", moveStr, err)
			}
			if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
				t.Fatalf("mailbox inconsistent after promotion %s: %v", moveStr, err)
			}
		}
	})

	t.Run("perft_positions", func(t *testing.T) {
		fens := []string{
			"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
			"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
			"r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
			"rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
			"r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
		}
		for _, fen := range fens {
			t.Run(fen, func(t *testing.T) {
				opt, err := FEN(fen)
				if err != nil {
					t.Fatal(err)
				}
				g := NewGame(opt)
				if err := verifyMailboxConsistency(g.Position().Board()); err != nil {
					t.Fatal(err)
				}
			})
		}
	})

	t.Run("empty_board", func(t *testing.T) {
		b, err := NewBoard(map[Square]Piece{})
		if err != nil {
			t.Fatal(err)
		}
		if err := verifyMailboxConsistency(b); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("binary_round_trip", func(t *testing.T) {
		g := NewGame()
		board := g.Position().Board()
		data, err := board.MarshalBinary()
		if err != nil {
			t.Fatal(err)
		}
		cp := &Board{}
		if err := cp.UnmarshalBinary(data); err != nil {
			t.Fatal(err)
		}
		if err := verifyMailboxConsistency(cp); err != nil {
			t.Fatalf("unmarshaled board mailbox inconsistent: %v", err)
		}
		if cp.String() != board.String() {
			t.Errorf("binary round-trip changed board: got %q, want %q", cp.String(), board.String())
		}
	})

	t.Run("transformations", func(t *testing.T) {
		board := NewGame().Position().Board()

		flipped, err := board.Flip(UpDown)
		if err != nil {
			t.Fatal(err)
		}
		if err := verifyMailboxConsistency(flipped); err != nil {
			t.Fatalf("flipped board mailbox inconsistent: %v", err)
		}

		transposed, err := board.Transpose()
		if err != nil {
			t.Fatal(err)
		}
		if err := verifyMailboxConsistency(transposed); err != nil {
			t.Fatalf("transposed board mailbox inconsistent: %v", err)
		}

		rotated, err := board.Rotate()
		if err != nil {
			t.Fatal(err)
		}
		if err := verifyMailboxConsistency(rotated); err != nil {
			t.Fatalf("rotated board mailbox inconsistent: %v", err)
		}
	})
}

func verifyMailboxConsistency(b *Board) error {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		mailboxPiece := b.Piece(square)
		bitboardPiece := NoPiece

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
