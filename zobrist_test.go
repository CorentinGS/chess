package chess_test

import (
	"testing"

	"github.com/corentings/chess/v3"
)

func TestHashFromFEN(t *testing.T) {
	t.Run("Known Position Hashes", func(t *testing.T) {
		knownPositions := []struct {
			name string
			fen  string
			hash uint64
		}{
			{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 0x463b96181691fc9c},
			{"after_e4", "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", 0x823c9b50fd114196},
			{"after_e4_e5", "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2", 0x0844931a6ef4b9a0},
			{"empty", "8/8/8/8/8/8/8/8 w - - 0 1", 0xf8d626aaaf278509},
		}
		for _, tc := range knownPositions {
			t.Run(tc.name, func(t *testing.T) {
				hash, err := chess.HashFromFEN(tc.fen)
				if err != nil {
					t.Fatalf("Expected no error, got %v", err)
				}
				if hash != tc.hash {
					t.Errorf("Expected hash %016x, got %016x", tc.hash, hash)
				}
			})
		}
	})

	t.Run("Error Handling", func(t *testing.T) {
		invalidFENs := []struct {
			name string
			fen  string
		}{
			{"missing_fields", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"},
			{"bad_piece_count", "rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
			{"bad_ranks_count", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1"},
			{"bad_side_to_move", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1"},
			{"bad_piece_char", "rnbqkbnZ/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
			{"empty_string", ""},
		}
		for _, tc := range invalidFENs {
			t.Run(tc.name, func(t *testing.T) {
				if _, err := chess.HashFromFEN(tc.fen); err == nil {
					t.Errorf("Expected error for %q, got nil", tc.fen)
				}
			})
		}
	})

	t.Run("Color Handling", func(t *testing.T) {
		positionWhite := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
		positionBlack := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

		hashWhite, err1 := chess.HashFromFEN(positionWhite)
		hashBlack, err2 := chess.HashFromFEN(positionBlack)

		if err1 != nil || err2 != nil {
			t.Fatalf("Unexpected errors: %v, %v", err1, err2)
		}
		if hashWhite == hashBlack {
			t.Error("Expected different hashes for white and black to move")
		}
	})

	t.Run("Castling Rights", func(t *testing.T) {
		withAllCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
		withoutCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"
		onlyWhiteCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1"

		hashAll, _ := chess.HashFromFEN(withAllCastling)
		hashNone, _ := chess.HashFromFEN(withoutCastling)
		hashWhite, _ := chess.HashFromFEN(onlyWhiteCastling)

		if hashAll == hashNone || hashAll == hashWhite || hashWhite == hashNone {
			t.Error("Expected different hashes for different castling rights")
		}
	})

	t.Run("En Passant", func(t *testing.T) {
		afterE4E5 := "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
		afterE4E6 := "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

		hashWithEP, err1 := chess.HashFromFEN(afterE4E5)
		hashWithoutEP, err2 := chess.HashFromFEN(afterE4E6)

		if err1 != nil || err2 != nil {
			t.Fatalf("Unexpected errors: %v, %v", err1, err2)
		}
		if hashWithEP == hashWithoutEP {
			t.Error("Expected different hashes for positions with and without en passant")
		}
		if hashWithEP != 0x0844931a6ef4b9a0 {
			t.Errorf("Expected hash 0844931a6ef4b9a0, got %016x", hashWithEP)
		}
		if hashWithoutEP != 0xf44b6961e533d1c4 {
			t.Errorf("Expected hash f44b6961e533d1c4, got %016x", hashWithoutEP)
		}
	})

	t.Run("Piece Placement", func(t *testing.T) {
		positions := []string{
			"4k3/8/8/8/8/8/8/4K3 w - - 0 1",
			"4k3/8/8/8/8/8/8/R3K3 w - - 0 1",
			"4k3/8/8/8/8/8/8/B3K3 w - - 0 1",
			"4k3/8/8/8/8/8/8/N3K3 w - - 0 1",
			"4k3/8/8/8/8/8/8/Q3K3 w - - 0 1",
			"4k3/8/8/8/8/8/8/3K2rr w - - 0 1",
		}

		hashes := make([]uint64, len(positions))
		for i, pos := range positions {
			hash, err := chess.HashFromFEN(pos)
			if err != nil {
				t.Fatalf("Unexpected error for position %s: %v", pos, err)
			}
			hashes[i] = hash
		}

		for i := 0; i < len(hashes); i++ {
			for j := i + 1; j < len(hashes); j++ {
				if hashes[i] == hashes[j] {
					t.Errorf("Expected different hashes for positions %s and %s",
						positions[i], positions[j])
				}
			}
		}
	})
}

func BenchmarkHashFromFEN(b *testing.B) {
	fens := []string{
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		"8/8/8/8/8/8/8/8 w - - 0 1",
		"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2",
	}
	for _, fen := range fens {
		b.Run(fen, func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				_, _ = chess.HashFromFEN(fen)
			}
		})
	}
}
