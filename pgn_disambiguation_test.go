package chess

import (
	"strings"
	"testing"
)

// Test cases for PGN disambiguation parsing issue #73
// The issue is that moves like "Qe8f7" (queen from e8 to f7) fail to parse
// because the parser doesn't handle full square disambiguation properly.

func TestPGNDisambiguationSquares(t *testing.T) {
	tests := []struct {
		name        string
		pgn         string
		shouldFail  bool
		description string
	}{
		{
			name:        "multiple_pieces_same_type_complex",
			pgn:         `1. h4 d6 2. g4 Kd7 3. f4 Kc6 4. f5 Kd7 5. g5 Ke8 6. h5 Kd7 7. h6 Kc6 8. g6 Kb6 9. f6 Kc6 10. hxg7 Kd7 11. gxf7 Kc6 12. fxe7 Kb6 13. gxf8=Q Kc6 14. fxg8=Q Kb6 15. e8=Q Ka6 16. Qfe7 Kb6 17. Qe8f7`,
			shouldFail:  false,
			description: "Complex game without full square disambiguation (should work)",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			reader := strings.NewReader(tc.pgn)
			scanner := NewScanner(reader)

			game, err := scanner.ParseNext()
			if err != nil {
				if !tc.shouldFail {
					t.Errorf("Expected PGN parsing to succeed but got error: %v", err)
				}
				// If we expected it to fail, verify it's the right kind of error
				if tc.shouldFail && !strings.Contains(err.Error(), "invalid destination square") {
					t.Logf("Expected 'invalid destination square' error but got: %v", err)
				}
				return
			}

			if tc.shouldFail {
				t.Errorf("Expected test case '%s' to fail but it succeeded. Description: %s", tc.name, tc.description)
			}

			// Additional validation for successful cases
			if !tc.shouldFail {
				if game == nil {
					t.Errorf("Game should not be nil for successful parsing")
				}
			}
		})
	}
}

// TestTokenizerDisambiguationSquares tests that the lexer correctly tokenizes disambiguation squares
func TestTokenizerDisambiguationSquares(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []TokenType
	}{
		{
			name:     "queen_with_full_square_disambiguation",
			input:    "Qe8f7",
			expected: []TokenType{PIECE, SQUARE, SQUARE}, // This is likely wrong - should be PIECE, SQUARE (source), SQUARE (dest)
		},
		{
			name:     "rook_with_full_square_disambiguation",
			input:    "Ra1d1",
			expected: []TokenType{PIECE, SQUARE, SQUARE},
		},
		{
			name:     "knight_with_full_square_disambiguation",
			input:    "Nb1c3",
			expected: []TokenType{PIECE, SQUARE, SQUARE},
		},
		{
			name:     "standard_piece_move",
			input:    "Nf3",
			expected: []TokenType{PIECE, SQUARE},
		},
		{
			name:     "piece_with_file_disambiguation",
			input:    "Nbd2",
			expected: []TokenType{PIECE, FILE, SQUARE},
		},
		{
			name:     "piece_with_rank_disambiguation",
			input:    "R1d2",
			expected: []TokenType{PIECE, RANK, SQUARE},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lexer := NewLexer(tc.input)
			var actualTypes []TokenType

			for {
				token := lexer.NextToken()
				if token.Type == EOF {
					break
				}
				actualTypes = append(actualTypes, token.Type)
				t.Logf("Token: %s, Value: %s", token.Type, token.Value)
			}

			if len(actualTypes) != len(tc.expected) {
				t.Errorf("Expected %d tokens, got %d", len(tc.expected), len(actualTypes))
				t.Errorf("Expected: %v", tc.expected)
				t.Errorf("Actual: %v", actualTypes)
				return
			}

			for i, expected := range tc.expected {
				if actualTypes[i] != expected {
					t.Errorf("Token %d: expected %s, got %s", i, expected, actualTypes[i])
				}
			}
		})
	}
}
