package chess_test

import (
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestZobristHasher(t *testing.T) {
	hasher := chess.NewZobristHasher()

	t.Run("Known Position Hashes", func(t *testing.T) {
		knownPositions := []struct {
			name string
			fen  string
			hash string
		}{
			{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "463b96181691fc9c"},
			{"after_e4", "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", "823c9b50fd114196"},
			{"after_e4_e5", "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2", "0844931a6ef4b9a0"},
			{"after_e4_e5_d5", "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", "0756b94461c50fb0"},
			{"empty", "8/8/8/8/8/8/8/8 w - - 0 1", "f8d626aaaf278509"},
		}
		for _, tc := range knownPositions {
			t.Run(tc.name, func(t *testing.T) {
				hash, err := hasher.HashPosition(tc.fen)
				if err != nil {
					t.Fatalf("Expected no error, got %v", err)
				}
				if len(hash) != 16 {
					t.Errorf("Expected hash length of 16, got %d", len(hash))
				}
				if !strings.EqualFold(hash, tc.hash) {
					t.Errorf("Expected hash %s, got %s", tc.hash, hash)
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
				if _, err := hasher.HashPosition(tc.fen); err == nil {
					t.Errorf("Expected error for %q, got nil", tc.fen)
				}
			})
		}
	})

	t.Run("Color Handling", func(t *testing.T) {
		positionWhite := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
		positionBlack := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

		hashWhite, err1 := hasher.HashPosition(positionWhite)
		hashBlack, err2 := hasher.HashPosition(positionBlack)

		if err1 != nil || err2 != nil {
			t.Fatalf("Unexpected errors: %v, %v", err1, err2)
		}
		if hashWhite == hashBlack {
			t.Error("Expected different hashes for white and black to move")
		}
		if len(hashWhite) != 16 || len(hashBlack) != 16 {
			t.Error("Expected hash length of 16")
		}
	})

	t.Run("Castling Rights", func(t *testing.T) {
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

	t.Run("En Passant", func(t *testing.T) {
		afterE4E5 := "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"
		afterE4E6 := "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

		hashWithEP, err1 := hasher.HashPosition(afterE4E5)
		hashWithoutEP, err2 := hasher.HashPosition(afterE4E6)

		if err1 != nil || err2 != nil {
			t.Fatalf("Unexpected errors: %v, %v", err1, err2)
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

		invalidEP := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e99 0 1"
		if _, err := hasher.HashPosition(invalidEP); err == nil {
			t.Error("Expected error for invalid en passant square")
		}
	})

	t.Run("Position Equality", func(t *testing.T) {
		position := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
		hash1, _ := hasher.HashPosition(position)
		hash2, _ := hasher.HashPosition(position)

		if hash1 != hash2 {
			t.Error("Expected identical hashes for identical positions")
		}

		position1 := "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
		position2 := "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"

		hash1, _ = hasher.HashPosition(position1)
		hash2, _ = hasher.HashPosition(position2)

		if hash1 == hash2 {
			t.Error("Expected different hashes for different positions")
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

		hashes := make([]string, len(positions))
		for i, pos := range positions {
			hash, err := hasher.HashPosition(pos)
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

func TestZobristHashToUint64(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want uint64
	}{
		{"valid_16_hex", "463b96181691fc9c", 0x463b96181691fc9c},
		{"uppercase_hex", "463B96181691FC9C", 0x463b96181691fc9c},
		{"invalid_text", "invalidhash", 0},
		{"empty", "", 0},
		{"too_short", "463b", 0},
		{"too_long", "463b96181691fc9c00", 0},
		{"odd_length", "abc", 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := chess.ZobristHashToUint64(tt.in); got != tt.want {
				t.Errorf("ZobristHashToUint64(%q) = %#x, want %#x", tt.in, got, tt.want)
			}
		})
	}
}

func TestZobristHashToUint64_NonZeroForValidHash(t *testing.T) {
	hasher := chess.NewZobristHasher()
	fens := []struct {
		name string
		fen  string
	}{
		{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
		{"after_e4_e5", "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"},
		{"empty", "8/8/8/8/8/8/8/8 w - - 0 1"},
	}
	for _, tt := range fens {
		t.Run(tt.name, func(t *testing.T) {
			hash, err := hasher.HashPosition(tt.fen)
			if err != nil {
				t.Fatalf("HashPosition(%q): %v", tt.fen, err)
			}
			asUint := chess.ZobristHashToUint64(hash)
			if asUint == 0 {
				t.Errorf("ZobristHashToUint64(%q) = 0 for a valid hash", hash)
			}
		})
	}
}

func TestDeprecatedNewChessHasher_MatchesNewZobristHasher(t *testing.T) {
	fens := []struct {
		name string
		fen  string
	}{
		{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
		{"after_e4_e5", "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"},
	}
	deprecated := chess.NewChessHasher()
	current := chess.NewZobristHasher()
	for _, tt := range fens {
		t.Run(tt.name, func(t *testing.T) {
			h1, err1 := deprecated.HashPosition(tt.fen)
			h2, err2 := current.HashPosition(tt.fen)
			if err1 != nil || err2 != nil {
				t.Fatalf("unexpected errors: %v %v", err1, err2)
			}
			if h1 != h2 {
				t.Errorf("NewChessHasher and NewZobristHasher disagree on %q: %s vs %s", tt.fen, h1, h2)
			}
		})
	}
}

func BenchmarkHashPosition(b *testing.B) {
	hasher := chess.NewZobristHasher()
	fens := []string{
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		"8/8/8/8/8/8/8/8 w - - 0 1",
		"rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2",
	}
	for _, fen := range fens {
		b.Run(fen, func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				_, _ = hasher.HashPosition(fen)
			}
		})
	}
}
