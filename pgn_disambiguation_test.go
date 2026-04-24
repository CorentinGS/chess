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
			expected: []TokenType{PIECE, DeambiguationSquare, SQUARE},
		},
		{
			name:     "rook_with_full_square_disambiguation",
			input:    "Ra1d1",
			expected: []TokenType{PIECE, DeambiguationSquare, SQUARE},
		},
		{
			name:     "knight_with_full_square_disambiguation",
			input:    "Nb1c3",
			expected: []TokenType{PIECE, DeambiguationSquare, SQUARE},
		},
		{
			name:     "queen_with_full_square_disambiguation_capture",
			input:    "Qg8xg7",
			expected: []TokenType{PIECE, DeambiguationSquare, CAPTURE, SQUARE},
		},
		{
			name:     "rook_with_full_square_disambiguation_capture",
			input:    "Ra1xa8",
			expected: []TokenType{PIECE, DeambiguationSquare, CAPTURE, SQUARE},
		},
		{
			name:     "knight_with_full_square_disambiguation_capture",
			input:    "Nb1xc3",
			expected: []TokenType{PIECE, DeambiguationSquare, CAPTURE, SQUARE},
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

func TestPGNFullSquareDisambiguationCapture(t *testing.T) {
	pgn := `[Event "rated blitz game"]
[Site "https://lichess.org/FaQvH6Iq"]
[Result "1-0"]

1. c4 Nf6 2. Nc3 Ng8 3. e4 Nf6 4. e5 Ng8 5. d4 e6 6. f4 Ne7 7. Nf3 Ng8 8. d5 Ne7 9. Be3 Nf5 10. Bf2 Ne7 11. Bd3 Ng8 12. O-O Nh6 13. h3 Ng8 14. g4 h5 15. g5 d6 16. Qc2 Qf6 17. exf6 Nc6 18. fxg7 Bd7 19. gxh8=Q O-O-O 20. g6 Bg7 21. gxf7 Kb8 22. fxg8=Q Ka8 23. f5 Rf8 24. f6 Be8 25. f7 Nd8 26. fxe8=Q Kb8 27. Qhxh5 Ka8 28. Qg4 Kb8 29. h4 Ka8 30. h5 Kb8 31. h6 Ka8 32. h7 Kb8 33. h8=Q Ka8 34. dxe6 Kb8 35. e7 Ka8 36. exf8=Q Kb8 37. Qg8xg7 Ka8 38. Qgg8 Kb8 39. Q4g7 Ka8 40. c5 Kb8 41. cxd6 Ka8 42. dxc7 b6 43. cxd8=Q# 1-0`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("expected PGN with full-square disambiguation capture to parse: %v", err)
	}
	if game == nil {
		t.Fatal("expected game to be parsed")
	}

	// Verify the specific problematic move (Qg8xg7) was parsed correctly.
	// Move 37 is at index 72 (0-based) in the moves slice.
	moves := game.Moves()
	if len(moves) < 73 {
		t.Fatalf("expected at least 73 moves, got %d", len(moves))
	}

	move37 := moves[72]
	if move37.String() != "g8g7" {
		t.Errorf("expected move 37 to be g8g7, got %s", move37.String())
	}
	if !move37.HasTag(Capture) {
		t.Errorf("expected move 37 to have Capture tag")
	}
}
