package chess

import (
	"strings"
	"testing"
)

func TestChessHasher(t *testing.T) {
	t.Run("Basic Position Validation", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should correctly hash the starting position", func(t *testing.T) {
			startPos := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
			hash, err := hasher.HashPosition(startPos)

			if err != nil {
				t.Errorf("Expected no error, got %v", err)
			}
			if len(hash) != 16 {
				t.Errorf("Expected hash length of 16, got %d", len(hash))
			}
			if hash != "463b96181691fc9c" {
				t.Errorf("Expected hash 463b96181691fc9c, got %s", hash)
			}
		})

		t.Run("should handle empty board position", func(t *testing.T) {
			emptyPos := "8/8/8/8/8/8/8/8 w - - 0 1"
			hash, err := hasher.HashPosition(emptyPos)

			if err != nil {
				t.Errorf("Expected no error, got %v", err)
			}
			if hash != "f8d626aaaf278509" {
				t.Errorf("Expected hash f8d626aaaf278509, got %s", hash)
			}
		})
	})

	t.Run("Known Position Hashes", func(t *testing.T) {
		hasher := NewZobristHasher()
		knownPositions := []struct {
			fen  string
			hash string
		}{
			{
				"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
				"463b96181691fc9c",
			},
			{
				"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
				"823c9b50fd114196",
			},
			{
				"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2",
				"0844931a6ef4b9a0",
			},
			{
				"rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
				"0756b94461c50fb0",
			},
			{
				"8/8/8/8/8/8/8/8 w - - 0 1",
				"f8d626aaaf278509",
			},
		}

		for _, tc := range knownPositions {
			t.Run(tc.fen, func(t *testing.T) {
				hash, err := hasher.HashPosition(tc.fen)
				if err != nil {
					t.Errorf("Expected no error, got %v", err)
				}
				if strings.ToLower(hash) != strings.ToLower(tc.hash) {
					t.Errorf("Expected hash %s, got %s", tc.hash, hash)
				}
			})
		}
	})

	t.Run("Error Handling", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should return error for invalid FEN format", func(t *testing.T) {
			invalidFen := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
			_, err := hasher.HashPosition(invalidFen)
			if err == nil {
				t.Error("Expected error for invalid FEN format")
			}
		})

		t.Run("should detect invalid piece placement", func(t *testing.T) {
			invalidPieces := "rnbqkbnr/ppppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
			_, err := hasher.HashPosition(invalidPieces)
			if err == nil {
				t.Error("Expected error for invalid piece placement")
			}
		})

		t.Run("should detect invalid ranks count", func(t *testing.T) {
			invalidRanks := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1"
			_, err := hasher.HashPosition(invalidRanks)
			if err == nil {
				t.Error("Expected error for invalid ranks count")
			}
		})
	})

	t.Run("Color Handling", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should produce different hashes for white and black to move", func(t *testing.T) {
			positionWhite := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
			positionBlack := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

			hashWhite, err1 := hasher.HashPosition(positionWhite)
			hashBlack, err2 := hasher.HashPosition(positionBlack)

			if err1 != nil || err2 != nil {
				t.Errorf("Unexpected errors: %v, %v", err1, err2)
			}
			if hashWhite == hashBlack {
				t.Error("Expected different hashes for white and black to move")
			}
			if len(hashWhite) != 16 || len(hashBlack) != 16 {
				t.Error("Expected hash length of 16")
			}
		})
	})

	t.Run("Castling Rights", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should produce different hashes for different castling rights", func(t *testing.T) {
			withAllCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
			withoutCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"
			onlyWhiteCastling := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1"

			hashAll, _ := hasher.HashPosition(withAllCastling)
			hashNone, _ := hasher.HashPosition(withoutCastling)
			hashWhite, _ := hasher.HashPosition(onlyWhiteCastling)

			if hashAll == hashNone || hashAll == hashWhite || hashWhite == hashNone {
				t.Error("Expected different hashes for different castling rights")
			}
		})
	})

	t.Run("En Passant", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should handle en passant squares correctly", func(t *testing.T) {
			afterE4E5 := "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
			afterE4E6 := "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

			hashWithEP, err1 := hasher.HashPosition(afterE4E5)
			hashWithoutEP, err2 := hasher.HashPosition(afterE4E6)

			if err1 != nil || err2 != nil {
				t.Errorf("Unexpected errors: %v, %v", err1, err2)
			}
			if hashWithEP == hashWithoutEP {
				t.Error("Expected different hashes for positions with and without en passant")
			}
			if hashWithEP != "0844931a6ef4b9a0" {
				t.Errorf("Expected hash 0844931a6ef4b9a0, got %s", hashWithEP)
			}
			if hashWithoutEP != "f44b6961e533d1c4" {
				t.Errorf("Expected hash f44b6961e533d1c4, got %s", hashWithoutEP)
			}
		})

		t.Run("should validate en passant square format", func(t *testing.T) {
			invalidEP := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e99 0 1"
			_, err := hasher.HashPosition(invalidEP)
			if err == nil {
				t.Error("Expected error for invalid en passant square")
			}
		})
	})

	t.Run("Position Equality", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should produce identical hashes for identical positions", func(t *testing.T) {
			position := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
			hash1, _ := hasher.HashPosition(position)
			hash2, _ := hasher.HashPosition(position)

			if hash1 != hash2 {
				t.Error("Expected identical hashes for identical positions")
			}
		})

		t.Run("should produce different hashes for different positions", func(t *testing.T) {
			position1 := "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
			position2 := "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"

			hash1, _ := hasher.HashPosition(position1)
			hash2, _ := hasher.HashPosition(position2)

			if hash1 == hash2 {
				t.Error("Expected different hashes for different positions")
			}
		})
	})

	t.Run("Piece Placement", func(t *testing.T) {
		hasher := NewZobristHasher()

		t.Run("should handle individual piece placement correctly", func(t *testing.T) {
			positions := []string{
				"4k3/8/8/8/8/8/8/4K3 w - - 0 1",  // Kings only
				"4k3/8/8/8/8/8/8/R3K3 w - - 0 1", // White rook added
				"4k3/8/8/8/8/8/8/B3K3 w - - 0 1", // White bishop instead
				"4k3/8/8/8/8/8/8/N3K3 w - - 0 1", // White knight instead
				"4k3/8/8/8/8/8/8/Q3K3 w - - 0 1", // White queen instead
			}

			hashes := make([]string, len(positions))
			for i, pos := range positions {
				hash, err := hasher.HashPosition(pos)
				if err != nil {
					t.Errorf("Unexpected error for position %s: %v", pos, err)
				}
				hashes[i] = hash
			}

			// Check that all hashes are different
			for i := 0; i < len(hashes); i++ {
				for j := i + 1; j < len(hashes); j++ {
					if hashes[i] == hashes[j] {
						t.Errorf("Expected different hashes for positions %s and %s",
							positions[i], positions[j])
					}
				}
			}
		})
	})
}

func TestZobristHashToUint64(t *testing.T) {
	t.Run("valid 16-digit hexadecimal hash is converted correctly", func(t *testing.T) {
		hash := "463b96181691fc9c"
		expected := uint64(0x463b96181691fc9c)
		result := ZobristHashToUint64(hash)

		if result != expected {
			t.Fatalf("expected value %v but got %v", expected, result)
		}
	})

	t.Run("invalid hash format returns error", func(t *testing.T) {
		hash := "invalidhash"
		_ = ZobristHashToUint64(hash)
	})

	t.Run("empty hash returns error", func(t *testing.T) {
		hash := ""
		_ = ZobristHashToUint64(hash)

	})
}

func BenchmarkHashPosition(b *testing.B) {
	hasher := NewZobristHasher()
	fen := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		_, _ = hasher.HashPosition(fen)
	}
}
