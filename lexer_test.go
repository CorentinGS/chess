package chess

import (
	"os"
	"testing"
)

func TestLexer_TokenizesFullGame(t *testing.T) {
	input := `[Event "Example"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 {This is the Ruy Lopez.} 3... a6 1-0`

	lexer := newPGNLexer(input)
	expectedTokens := []struct {
		typ   pgnTokenType
		value string
	}{
		{tokTagStart, "["},
		{tokTagKey, "Event"},
		{tokTagValue, "Example"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Site"},
		{tokTagValue, "Internet"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Date"},
		{tokTagValue, "2023.12.06"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Round"},
		{tokTagValue, "1"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "White"},
		{tokTagValue, "Player1"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Black"},
		{tokTagValue, "Player2"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Result"},
		{tokTagValue, "1-0"},
		{tokTagEnd, "]"},
		{tokMoveNumber, "1"},
		{tokDot, "."},
		{tokSquare, "e4"},
		{tokSquare, "e5"},
		{tokMoveNumber, "2"},
		{tokDot, "."},
		{tokPiece, "N"},
		{tokSquare, "f3"},
		{tokPiece, "N"},
		{tokSquare, "c6"},
		{tokMoveNumber, "3"},
		{tokDot, "."},
		{tokPiece, "B"},
		{tokSquare, "b5"},
		{tokCommentStart, "{"},
		{tokComment, "This is the Ruy Lopez."},
		{tokCommentEnd, "}"},
		{tokMoveNumber, "3"},
		{tokEllipsis, "..."},
		{tokSquare, "a6"},
		{tokResult, "1-0"},
	}

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}

	// Test EOF
	token := lexer.NextToken()
	if token.Type != tokEOF {
		t.Errorf("Expected EOF token, got %v", token.Type)
	}
}

func TestLexer_ParsesTagValueWithApostrophe(t *testing.T) {
	input := "[Opening \"King's Indian Attack, General\"]"
	lexer := newPGNLexer(input)

	expectedTokens := []struct {
		typ   pgnTokenType
		value string
	}{
		{tokTagStart, "["},
		{tokTagKey, "Opening"},
		{tokTagValue, "King's Indian Attack, General"},
		{tokTagEnd, "]"},
	}

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}
}

func TestLexer_ParsesTagValueWithEscapedBackslash(t *testing.T) {
	lexer := newPGNLexer(`[Path "C:\\Games"]`)
	lexer.NextToken()
	lexer.NextToken()

	token := lexer.NextToken()
	if token.Type != tokTagValue || token.Value != `C:\Games` {
		t.Fatalf("TagValue = {%v, %q}, want {%v, %q}", token.Type, token.Value, tokTagValue, `C:\Games`)
	}
}

func TestLexer_CheckAndCheckmateSuffixes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Check",
			input: "e5+",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e5"},
				{Type: tokCheck, Value: "+"},
			},
		},
		{
			name:  "Checkmate",
			input: "e5#",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e5"},
				{Type: tokCheckmate, Value: "#"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Promotion(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Promotion",
			input: "e8=Q",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e8"},
				{Type: tokPromotion, Value: "="},
				{Type: tokPromotionPiece, Value: "Q"},
			},
		},
		{
			name:  "Promotion in game",
			input: "1. e8=Q e1=N 2. exd8=R",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e8"},
				{Type: tokPromotion, Value: "="},
				{Type: tokPromotionPiece, Value: "Q"},
				{Type: tokSquare, Value: "e1"},
				{Type: tokPromotion, Value: "="},
				{Type: tokPromotionPiece, Value: "N"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokFile, Value: "e"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "d8"},
				{Type: tokPromotion, Value: "="},
				{Type: tokPromotionPiece, Value: "R"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_NAG(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "NAG",
			input: "e5 $1",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e5"},
				{Type: tokNAG, Value: "$1"},
			},
		},
		{
			name:  "NAG in game",
			input: "1. e5 $1 e6 $2 2. Nf3 $3",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e5"},
				{Type: tokNAG, Value: "$1"},
				{Type: tokSquare, Value: "e6"},
				{Type: tokNAG, Value: "$2"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
				{Type: tokNAG, Value: "$3"},
			},
		},
		{
			name:  "NAG and comment after move",
			input: "e4 $1 {Good move}",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e4"},
				{Type: tokNAG, Value: "$1"},
				{Type: tokCommentStart, Value: "{"},
				{Type: tokComment, Value: "Good move"},
				{Type: tokCommentEnd, Value: "}"},
			},
		},
		{
			name:  "Comment and NAG after move",
			input: "e4 {Good move} $1",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e4"},
				{Type: tokCommentStart, Value: "{"},
				{Type: tokComment, Value: "Good move"},
				{Type: tokCommentEnd, Value: "}"},
				{Type: tokNAG, Value: "$1"},
			},
		},
		{
			name:  "Move with multiple NAGs and comment",
			input: "e4 $1 $2 {Excellent move}",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e4"},
				{Type: tokNAG, Value: "$1"},
				{Type: tokNAG, Value: "$2"},
				{Type: tokCommentStart, Value: "{"},
				{Type: tokComment, Value: "Excellent move"},
				{Type: tokCommentEnd, Value: "}"},
			},
		},
		{
			name:  "Move with comment and multiple NAGs",
			input: "e4 {Excellent move} $1 $2",
			expected: []pgnToken{
				{Type: tokSquare, Value: "e4"},
				{Type: tokCommentStart, Value: "{"},
				{Type: tokComment, Value: "Excellent move"},
				{Type: tokCommentEnd, Value: "}"},
				{Type: tokNAG, Value: "$1"},
				{Type: tokNAG, Value: "$2"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Captures(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Pawn capture",
			input: "exf3",
			expected: []pgnToken{
				{Type: tokFile, Value: "e"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "f3"},
			},
		},
		{
			name:  "Piece capture",
			input: "Nxc6",
			expected: []pgnToken{
				{Type: tokPiece, Value: "N"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with file disambiguation",
			input: "Nbxc6",
			expected: []pgnToken{
				{Type: tokPiece, Value: "N"},
				{Type: tokFile, Value: "b"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with rank disambiguation",
			input: "N4xd5",
			expected: []pgnToken{
				{Type: tokPiece, Value: "N"},
				{Type: tokRank, Value: "4"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "d5"},
			},
		},
		{
			name:  "Complex position with captures",
			input: "1. e4 d5 2. Nf3 Nc6 3. Nbxd5",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokSquare, Value: "d5"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "c6"},
				{Type: tokMoveNumber, Value: "3"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokFile, Value: "b"},
				{Type: tokCapture, Value: "x"},
				{Type: tokSquare, Value: "d5"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

// Test captures in context
func TestLexer_CapturesInGameContext(t *testing.T) {
	input := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Bxc6"

	expectedTokens := []struct {
		typ   pgnTokenType
		value string
	}{
		{tokMoveNumber, "1"},
		{tokDot, "."},
		{tokSquare, "e4"},
		{tokSquare, "e5"},
		{tokMoveNumber, "2"},
		{tokDot, "."},
		{tokPiece, "N"},
		{tokSquare, "f3"},
		{tokPiece, "N"},
		{tokSquare, "c6"},
		{tokMoveNumber, "3"},
		{tokDot, "."},
		{tokPiece, "B"},
		{tokSquare, "b5"},
		{tokSquare, "a6"},
		{tokMoveNumber, "4"},
		{tokDot, "."},
		{tokPiece, "B"},
		{tokCapture, "x"},
		{tokSquare, "c6"},
	}

	lexer := newPGNLexer(input)

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}
}

func TestLexer_Commands(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Command",
			input: "{[%clk 12:34:56]}",
			expected: []pgnToken{
				{Type: tokCommentStart, Value: "{"},
				{Type: tokCommandStart, Value: "[%"},
				{Type: tokCommandName, Value: "clk"},
				{Type: tokCommandParam, Value: "12:34:56"},
				{Type: tokCommandEnd, Value: "]"},
				{Type: tokCommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command in game",
			input: "1. e4 {[%clk 12:34:56]} e5",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokCommentStart, Value: "{"},
				{Type: tokCommandStart, Value: "[%"},
				{Type: tokCommandName, Value: "clk"},
				{Type: tokCommandParam, Value: "12:34:56"},
				{Type: tokCommandEnd, Value: "]"},
				{Type: tokCommentEnd, Value: "}"},
				{Type: tokSquare, Value: "e5"},
			},
		},
		{ // Test multiple commands
			name:  "Multiple commands",
			input: "{[%clk 0:00:07][%eval -6.05] White is toast}",
			expected: []pgnToken{
				{Type: tokCommentStart, Value: "{"},
				{Type: tokCommandStart, Value: "[%"},
				{Type: tokCommandName, Value: "clk"},
				{Type: tokCommandParam, Value: "0:00:07"},
				{Type: tokCommandEnd, Value: "]"},
				{Type: tokCommandStart, Value: "[%"},
				{Type: tokCommandName, Value: "eval"},
				{Type: tokCommandParam, Value: "-6.05"},
				{Type: tokCommandEnd, Value: "]"},
				{Type: tokComment, Value: "White is toast"},
				{Type: tokCommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command with multiple parameters",
			input: "{[%command 1:45:12,Nf6,\"very interesting, but wrong\"]}",
			expected: []pgnToken{
				{Type: tokCommentStart, Value: "{"},
				{Type: tokCommandStart, Value: "[%"},
				{Type: tokCommandName, Value: "command"},
				{Type: tokCommandParam, Value: "1:45:12"},
				{Type: tokCommandParam, Value: "Nf6"},
				{Type: tokCommandParam, Value: "very interesting, but wrong"},
				{Type: tokCommandEnd, Value: "]"},
				{Type: tokCommentEnd, Value: "}"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Variations(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Variation start",
			input: "1. e4 (1. d4) e5",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "d4"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokSquare, Value: "e5"},
			},
		},
		{
			name:  "Variation in game",
			input: "1. e4 (1. d4) e5 2. Nf3 (2. Nc3) Nc6",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "d4"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokSquare, Value: "e5"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "c3"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "c6"},
			},
		},
		{
			name:  "Nested variations",
			input: "1. e4 (1. d4 (1. c4)) 1... e5",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "d4"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "c4"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokEllipsis, Value: "..."},
				{Type: tokSquare, Value: "e5"},
			},
		},

		{
			name:  "Another variation",
			input: "1. e4 e5 (1... e6 2. d4 d5) 2. Nf3",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokSquare, Value: "e5"},
				{Type: tokVariationStart, Value: "("},
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokEllipsis, Value: "..."},
				{Type: tokSquare, Value: "e6"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "d4"},
				{Type: tokSquare, Value: "d5"},
				{Type: tokVariationEnd, Value: ")"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Castling(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []pgnToken
	}{
		{
			name:  "Short castle",
			input: "O-O",
			expected: []pgnToken{
				{Type: tokKingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle",
			input: "O-O-O",
			expected: []pgnToken{
				{Type: tokQueensideCastle, Value: "O-O-O"},
			},
		},
		{
			name:  "Short castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokSquare, Value: "e5"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "c6"},
				{Type: tokMoveNumber, Value: "3"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "B"},
				{Type: tokSquare, Value: "c4"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f6"},
				{Type: tokMoveNumber, Value: "4"},
				{Type: tokDot, Value: "."},
				{Type: tokKingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O-O",
			expected: []pgnToken{
				{Type: tokMoveNumber, Value: "1"},
				{Type: tokDot, Value: "."},
				{Type: tokSquare, Value: "e4"},
				{Type: tokSquare, Value: "e5"},
				{Type: tokMoveNumber, Value: "2"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f3"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "c6"},
				{Type: tokMoveNumber, Value: "3"},
				{Type: tokDot, Value: "."},
				{Type: tokPiece, Value: "B"},
				{Type: tokSquare, Value: "c4"},
				{Type: tokPiece, Value: "N"},
				{Type: tokSquare, Value: "f6"},
				{Type: tokMoveNumber, Value: "4"},
				{Type: tokDot, Value: "."},
				{Type: tokQueensideCastle, Value: "O-O-O"},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := newPGNLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != tokEOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestFuzzRegressions(t *testing.T) {
	seeds := []string{"y", "a", "{[%0", "{[%,\""}
	for _, input := range seeds {
		t.Run(input, func(t *testing.T) {
			lexer := newPGNLexer(input)
			defer func() {
				if r := recover(); r != nil {
					t.Errorf("Lexer panicked on input %q: %v", input, r)
				}
			}()
			count := 0
			for {
				token := lexer.NextToken()
				count++
				if token.Type == tokEOF {
					break
				}
				if count > len(input)*3 {
					t.Errorf("Too many tokens generated for input %q", input)
					break
				}
			}
		})
	}
}

func FuzzLexer(f *testing.F) {
	// Add seeds covering all possible token types
	seeds := []string{
		// Basic moves and numbers
		"1. e4 e5 2. Nf3",

		// Tags
		"[Event \"Test\"][Site \"Chess.com\"]",

		// Comments with commands
		"{Normal comment} {[%clk 1:23:45]} {[%eval +1.2]}",

		// Pieces and squares with disambiguation
		"Nbd7 R1e2 Qh4xe4",

		// Castle both sides
		"O-O O-O-O",

		// Promotion with checks
		"e8=Q+ f1=N#",

		// NAGs
		"$1 $20 $123",

		// Variations
		"1. e4 e5 (1... c5 2. Nf3 (2. c3 d5)) 2. Nf3",

		// Complex combinations
		"1. e4 $1 {Great move} (1... e5?! {Dubious [%clk 0:30:00]}) 1... c5",

		// Full game with metadata
		`[Event "Test"]
         1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {Ruy Lopez} 4. Ba4 Nf6 5. O-O
         Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 1-0`,

		// Various special characters
		"+ #",

		// Results
		"1-0 0-1 1/2-1/2",

		// Edge cases
		"a1 h8 a8 h1", // Corner squares
		"Qa1xb2",      // Capture with full disambiguation
		"e7e8=Q",      // Promotion without capture
		"exd8=Q+",     // Promotion with capture and check
	}

	for _, seed := range seeds {
		f.Add(seed)
	}

	f.Fuzz(func(t *testing.T, input string) {
		// Prevent excessively long inputs
		if len(input) > 1000 {
			return
		}

		defer func() {
			if r := recover(); r != nil {
				t.Errorf("Lexer panicked on input %q: %v", input, r)
			}
		}()

		lexer := newPGNLexer(input)
		var tokens []pgnToken

		t.Log("Input:", input)

		// Read all tokens
		for {
			token := lexer.NextToken()
			tokens = append(tokens, token)

			// Stop at EOF
			if token.Type == tokEOF {
				break
			}

			// Prevent infinite loops
			if len(tokens) > len(input)*3 {
				t.Log("Tokens:", tokens)
				t.Errorf("Too many tokens generated for input length")
				break
			}
		}

		// Validate token sequence
		validateTokens(t, tokens)
	})
}

func validateTokens(t *testing.T, tokens []pgnToken) {
	for i, token := range tokens {
		// Validate specific token values
		switch token.Type {
		case tokFile:
			if len(token.Value) != 1 || token.Value[0] < 'a' || token.Value[0] > 'h' {
				t.Errorf("Invalid file at token %d: %v", i, token.Value)
			}
		case tokRank:
			if len(token.Value) != 1 || (!(token.Value[0] >= '1' && token.Value[0] <= '8') && token.Error == nil) {
				t.Errorf("Invalid rank at token %d: %v", i, token.Value)
			}
		case tokPiece:
			if len(token.Value) != 1 || (!(token.Value[0] == 'N' || token.Value[0] == 'B' || token.Value[0] == 'R' || token.Value[0] == 'Q' || token.Value[0] == 'K') && token.Error == nil) {
				t.Errorf("Invalid piece at token %d: %v", i, token.Value)
			}
		case tokPromotionPiece:
			if len(token.Value) != 1 || (!(token.Value[0] == 'N' || token.Value[0] == 'B' || token.Value[0] == 'R' || token.Value[0] == 'Q' || token.Value[0] == 'K') && token.Error == nil) {
				t.Errorf("Invalid promotion piece at token %d: %v", i, token.Value)
			}
		}
	}
}

func TestLexer_FromPositionFixture(t *testing.T) {
	// Read fixture
	data, err := os.ReadFile("fixtures/pgns/single_frompos.pgn")
	if err != nil {
		t.Fatalf("Failed to read fixture: %v", err)
	}

	lexer := newPGNLexer(string(data))

	expected := []struct {
		typ   pgnTokenType
		value string
	}{
		// Tags
		{tokTagStart, "["},
		{tokTagKey, "Event"},
		{tokTagValue, "Slav Defense: Chebanenko Variation, 5. cxd5"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Site"},
		{tokTagValue, ""},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Date"},
		{tokTagValue, "2025.07.12"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Round"},
		{tokTagValue, "1"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "White"},
		{tokTagValue, ""},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Black"},
		{tokTagValue, ""},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Result"},
		{tokTagValue, "*"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "UTCDate"},
		{tokTagValue, "2025.07.12"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "UTCTime"},
		{tokTagValue, "15:51:01"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "Variant"},
		{tokTagValue, "Standard"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "ECO"},
		{tokTagValue, "D15"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "FEN"},
		{tokTagValue, "rnbqkb1r/1p2pppp/p1p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5"},
		{tokTagEnd, "]"},
		{tokTagStart, "["},
		{tokTagKey, "SetUp"},
		{tokTagValue, "1"},
		{tokTagEnd, "]"},
		// Moves
		{tokMoveNumber, "5"},
		{tokDot, "."},
		{tokFile, "c"},
		{tokCapture, "x"},
		{tokSquare, "d5"},
		{tokVariationStart, "("},
		{tokMoveNumber, "5"},
		{tokDot, "."},
		{tokSquare, "e3"},
		{tokSquare, "e6"},
		{tokMoveNumber, "6"},
		{tokDot, "."},
		{tokFile, "c"},
		{tokCapture, "x"},
		{tokSquare, "d5"},
		{tokFile, "c"},
		{tokCapture, "x"},
		{tokSquare, "d5"},
		{tokVariationEnd, ")"},
		{tokMoveNumber, "5"},
		{tokEllipsis, "..."},
		{tokFile, "c"},
		{tokCapture, "x"},
		{tokSquare, "d5"},
		{tokMoveNumber, "6"},
		{tokDot, "."},
		{tokSquare, "e3"},
		{tokSquare, "e6"},
		{tokCommentStart, "{"},
		{tokCommandStart, "[%"},
		{tokCommandName, "eval"},
		{tokCommandParam, "0.11"},
		{tokCommandEnd, "]"},
		{tokCommentEnd, "}"},
		{tokResult, "*"},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Value != exp.value {
			t.Errorf("Token %d: expected {%v, %q}, got {%v, %q}",
				i, exp.typ, exp.value, tok.Type, tok.Value)
		}
	}
	// final EOF
	if tok := lexer.NextToken(); tok.Type != tokEOF {
		t.Errorf("Expected EOF, got %v", tok.Type)
	}
}

func assertTokenSequence(t *testing.T, l *pgnLexer, expected []pgnToken) {
	t.Helper()
	for i, exp := range expected {
		tok := l.NextToken()
		if tok.Type != exp.Type || tok.Value != exp.Value {
			t.Errorf("Token %d: expected {%v, %q}, got {%v, %q}",
				i, exp.Type, exp.Value, tok.Type, tok.Value)
		}
		if exp.Error != nil && tok.Error == nil {
			t.Errorf("Token %d: expected error %q, got nil", i, exp.Error)
		}
	}
	if tok := l.NextToken(); tok.Type != tokEOF {
		t.Errorf("expected EOF after sequence, got {%v, %q}", tok.Type, tok.Value)
	}
}

func TestLexer_EmptyInput(t *testing.T) {
	for _, in := range []string{"", "   ", " \t\n  "} {
		l := newPGNLexer(in)
		tok := l.NextToken()
		if tok.Type != tokEOF {
			t.Errorf("NewLexer(%q).NextToken() = %v, want EOF", in, tok.Type)
		}
	}
}

func TestLexer_UndefinedToken(t *testing.T) {
	for _, in := range []string{"@", ";", "~"} {
		l := newPGNLexer(in)
		tok := l.NextToken()
		if tok.Type != tokUndefined || tok.Value != in {
			t.Errorf("NewLexer(%q).NextToken() = {%v, %q}, want {Undefined, %q}", in, tok.Type, tok.Value, in)
		}
		if eof := l.NextToken(); eof.Type != tokEOF {
			t.Errorf("expected EOF after undefined, got %v", eof.Type)
		}
	}
}

func TestLexer_DeambiguationSquare(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []pgnToken
	}{
		{"full_square", "Qe8f7", []pgnToken{
			{Type: tokPiece, Value: "Q"},
			{Type: tokDisambiguationSquare, Value: "e8"},
			{Type: tokSquare, Value: "f7"},
		}},
		{"full_square_capture", "Qg8xg7", []pgnToken{
			{Type: tokPiece, Value: "Q"},
			{Type: tokDisambiguationSquare, Value: "g8"},
			{Type: tokCapture, Value: "x"},
			{Type: tokSquare, Value: "g7"},
		}},
		{"file_only", "Nbd7", []pgnToken{
			{Type: tokPiece, Value: "N"},
			{Type: tokFile, Value: "b"},
			{Type: tokSquare, Value: "d7"},
		}},
		{"rank_only", "N4d7", []pgnToken{
			{Type: tokPiece, Value: "N"},
			{Type: tokRank, Value: "4"},
			{Type: tokSquare, Value: "d7"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, newPGNLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_ResultSpellings(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []pgnToken
	}{
		{"white_wins", "1-0", []pgnToken{{Type: tokResult, Value: "1-0"}}},
		{"black_wins", "0-1", []pgnToken{{Type: tokResult, Value: "0-1"}}},
		{"ongoing", "*", []pgnToken{{Type: tokResult, Value: "*"}}},
		{"draw", "1/2-1/2", []pgnToken{
			{Type: tokResult, Value: "1/2-1/2"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, newPGNLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_NAGSymbols(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []pgnToken
	}{
		{"double_bang", "e4!!", []pgnToken{
			{Type: tokSquare, Value: "e4"},
			{Type: tokNAG, Value: "!!"},
		}},
		{"bang", "e4!", []pgnToken{
			{Type: tokSquare, Value: "e4"},
			{Type: tokNAG, Value: "!"},
		}},
		{"question", "e4?", []pgnToken{
			{Type: tokSquare, Value: "e4"},
			{Type: tokNAG, Value: "?"},
		}},
		{"dubious", "e4?!", []pgnToken{
			{Type: tokSquare, Value: "e4"},
			{Type: tokNAG, Value: "?!"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, newPGNLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_NAGMaximalRun(t *testing.T) {
	// A run of more than two '!'/'?' characters must be a single NAG token,
	// not split into a valid token plus a trailing symbol (regression for the
	// "!!!" bug that produced two NAG tokens).
	assertTokenSequence(t, newPGNLexer("e4!!!"), []pgnToken{
		{Type: tokSquare, Value: "e4"},
		{Type: tokNAG, Value: "!!!"},
	})
}

func TestLexer_ErrorPaths(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		wantMsg string
	}{
		{"unterminated_comment", "{unterm", "unterminated comment"},
		{"unterminated_quote", `{[%clk "12:34]}`, "unterminated quote"},
		{"invalid_command", `{[%clk }]`, "invalid command in comment"},
		{"invalid_piece", "Ze4", "invalid piece"},
		{"invalid_square", "z9", "invalid square"},
		{"invalid_rank", "N9", "invalid rank"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			l := newPGNLexer(tt.input)
			var errToken pgnToken
			for {
				tok := l.NextToken()
				if tok.Error != nil {
					errToken = tok
					break
				}
				if tok.Type == tokEOF {
					t.Fatalf("reached EOF without an error token for input %q", tt.input)
				}
			}
			if errToken.Error == nil {
				t.Fatalf("expected error for input %q, got nil", tt.input)
			}
			if errToken.Error.Error() != tt.wantMsg {
				t.Errorf("error = %q, want %q", errToken.Error.Error(), tt.wantMsg)
			}
		})
	}
}
