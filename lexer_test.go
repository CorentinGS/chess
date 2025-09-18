package chess

import (
	"os"
	"testing"
)

func TestLexer(t *testing.T) {
	input := `[Event "Example"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 {This is the Ruy Lopez.} 3... a6 1-0`

	lexer := NewLexer(input)
	expectedTokens := []struct {
		typ   TokenType
		value string
	}{
		{TagStart, "["},
		{TagKey, "Event"},
		{TagValue, "Example"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Site"},
		{TagValue, "Internet"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Date"},
		{TagValue, "2023.12.06"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Round"},
		{TagValue, "1"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "White"},
		{TagValue, "Player1"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Black"},
		{TagValue, "Player2"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Result"},
		{TagValue, "1-0"},
		{TagEnd, "]"},
		{MoveNumber, "1"},
		{DOT, "."},
		{SQUARE, "e4"},
		{SQUARE, "e5"},
		{MoveNumber, "2"},
		{DOT, "."},
		{PIECE, "N"},
		{SQUARE, "f3"},
		{PIECE, "N"},
		{SQUARE, "c6"},
		{MoveNumber, "3"},
		{DOT, "."},
		{PIECE, "B"},
		{SQUARE, "b5"},
		{CommentStart, "{"},
		{COMMENT, "This is the Ruy Lopez."},
		{CommentEnd, "}"},
		{MoveNumber, "3"},
		{ELLIPSIS, "..."},
		{SQUARE, "a6"},
		{RESULT, "1-0"},
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
	if token.Type != EOF {
		t.Errorf("Expected EOF token, got %v", token.Type)
	}
}

func Test_TagKey(t *testing.T) {
	input := "[Opening \"King's Indian Attack, General\"]"
	lexer := NewLexer(input)

	expectedTokens := []struct {
		typ   TokenType
		value string
	}{
		{TagStart, "["},
		{TagKey, "Opening"},
		{TagValue, "King's Indian Attack, General"},
		{TagEnd, "]"},
	}

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}
}

func TestCheck(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Check",
			input: "e5+",
			expected: []Token{
				{Type: SQUARE, Value: "e5"},
				{Type: CHECK, Value: "+"},
			},
		},
		{
			name:  "Checkmate",
			input: "e5#",
			expected: []Token{
				{Type: SQUARE, Value: "e5"},
				{Type: CHECKMATE, Value: "#"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestDisambiguation(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Disambiguation by file",
			input: "Nbd7",
			expected: []Token{
				{Type: PIECE, Value: "N"},
				{Type: FILE, Value: "b"},
				{Type: SQUARE, Value: "d7"},
			},
		},
		{
			name:  "Disambiguation by rank",
			input: "N4d7",
			expected: []Token{
				{Type: PIECE, Value: "N"},
				{Type: RANK, Value: "4"},
				{Type: SQUARE, Value: "d7"},
			},
		},

		{
			name:  "Disambiguation in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Nbd7",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: SQUARE, Value: "e5"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c6"},
				{Type: MoveNumber, Value: "3"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: FILE, Value: "b"},
				{Type: SQUARE, Value: "d7"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestPromotion(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Promotion",
			input: "e8=Q",
			expected: []Token{
				{Type: SQUARE, Value: "e8"},
				{Type: PROMOTION, Value: "="},
				{Type: PromotionPiece, Value: "Q"},
			},
		},
		{
			name:  "Promotion in game",
			input: "1. e8=Q e1=N 2. exd8=R",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e8"},
				{Type: PROMOTION, Value: "="},
				{Type: PromotionPiece, Value: "Q"},
				{Type: SQUARE, Value: "e1"},
				{Type: PROMOTION, Value: "="},
				{Type: PromotionPiece, Value: "N"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: FILE, Value: "e"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "d8"},
				{Type: PROMOTION, Value: "="},
				{Type: PromotionPiece, Value: "R"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestNAG(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "NAG",
			input: "e5 $1",
			expected: []Token{
				{Type: SQUARE, Value: "e5"},
				{Type: NAG, Value: "$1"},
			},
		},
		{
			name:  "NAG in game",
			input: "1. e5 $1 e6 $2 2. Nf3 $3",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e5"},
				{Type: NAG, Value: "$1"},
				{Type: SQUARE, Value: "e6"},
				{Type: NAG, Value: "$2"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: NAG, Value: "$3"},
			},
		},
		{
			name:  "NAG and comment after move",
			input: "e4 $1 {Good move}",
			expected: []Token{
				{Type: SQUARE, Value: "e4"},
				{Type: NAG, Value: "$1"},
				{Type: CommentStart, Value: "{"},
				{Type: COMMENT, Value: "Good move"},
				{Type: CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Comment and NAG after move",
			input: "e4 {Good move} $1",
			expected: []Token{
				{Type: SQUARE, Value: "e4"},
				{Type: CommentStart, Value: "{"},
				{Type: COMMENT, Value: "Good move"},
				{Type: CommentEnd, Value: "}"},
				{Type: NAG, Value: "$1"},
			},
		},
		{
			name:  "Move with multiple NAGs and comment",
			input: "e4 $1 $2 {Excellent move}",
			expected: []Token{
				{Type: SQUARE, Value: "e4"},
				{Type: NAG, Value: "$1"},
				{Type: NAG, Value: "$2"},
				{Type: CommentStart, Value: "{"},
				{Type: COMMENT, Value: "Excellent move"},
				{Type: CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Move with comment and multiple NAGs",
			input: "e4 {Excellent move} $1 $2",
			expected: []Token{
				{Type: SQUARE, Value: "e4"},
				{Type: CommentStart, Value: "{"},
				{Type: COMMENT, Value: "Excellent move"},
				{Type: CommentEnd, Value: "}"},
				{Type: NAG, Value: "$1"},
				{Type: NAG, Value: "$2"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestCaptures(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Pawn capture",
			input: "exf3",
			expected: []Token{
				{Type: FILE, Value: "e"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "f3"},
			},
		},
		{
			name:  "Piece capture",
			input: "Nxc6",
			expected: []Token{
				{Type: PIECE, Value: "N"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with file disambiguation",
			input: "Nbxc6",
			expected: []Token{
				{Type: PIECE, Value: "N"},
				{Type: FILE, Value: "b"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with rank disambiguation",
			input: "N4xd5",
			expected: []Token{
				{Type: PIECE, Value: "N"},
				{Type: RANK, Value: "4"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "d5"},
			},
		},
		{
			name:  "Complex position with captures",
			input: "1. e4 d5 2. Nf3 Nc6 3. Nbxd5",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: SQUARE, Value: "d5"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c6"},
				{Type: MoveNumber, Value: "3"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: FILE, Value: "b"},
				{Type: CAPTURE, Value: "x"},
				{Type: SQUARE, Value: "d5"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

// Test captures in context
func TestCapturesInGame(t *testing.T) {
	input := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Bxc6"

	expectedTokens := []struct {
		typ   TokenType
		value string
	}{
		{MoveNumber, "1"},
		{DOT, "."},
		{SQUARE, "e4"},
		{SQUARE, "e5"},
		{MoveNumber, "2"},
		{DOT, "."},
		{PIECE, "N"},
		{SQUARE, "f3"},
		{PIECE, "N"},
		{SQUARE, "c6"},
		{MoveNumber, "3"},
		{DOT, "."},
		{PIECE, "B"},
		{SQUARE, "b5"},
		{SQUARE, "a6"},
		{MoveNumber, "4"},
		{DOT, "."},
		{PIECE, "B"},
		{CAPTURE, "x"},
		{SQUARE, "c6"},
	}

	lexer := NewLexer(input)

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}
}

func TestCommands(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Command",
			input: "{[%clk 12:34:56]}",
			expected: []Token{
				{Type: CommentStart, Value: "{"},
				{Type: CommandStart, Value: "[%"},
				{Type: CommandName, Value: "clk"},
				{Type: CommandParam, Value: "12:34:56"},
				{Type: CommandEnd, Value: "]"},
				{Type: CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command in game",
			input: "1. e4 {[%clk 12:34:56]} e5",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: CommentStart, Value: "{"},
				{Type: CommandStart, Value: "[%"},
				{Type: CommandName, Value: "clk"},
				{Type: CommandParam, Value: "12:34:56"},
				{Type: CommandEnd, Value: "]"},
				{Type: CommentEnd, Value: "}"},
				{Type: SQUARE, Value: "e5"},
			},
		},
		{ // Test multiple commands
			name:  "Multiple commands",
			input: "{[%clk 0:00:07][%eval -6.05] White is toast}",
			expected: []Token{
				{Type: CommentStart, Value: "{"},
				{Type: CommandStart, Value: "[%"},
				{Type: CommandName, Value: "clk"},
				{Type: CommandParam, Value: "0:00:07"},
				{Type: CommandEnd, Value: "]"},
				{Type: CommandStart, Value: "[%"},
				{Type: CommandName, Value: "eval"},
				{Type: CommandParam, Value: "-6.05"},
				{Type: CommandEnd, Value: "]"},
				{Type: COMMENT, Value: "White is toast"},
				{Type: CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command with multiple parameters",
			input: "{[%command 1:45:12,Nf6,\"very interesting, but wrong\"]}",
			expected: []Token{
				{Type: CommentStart, Value: "{"},
				{Type: CommandStart, Value: "[%"},
				{Type: CommandName, Value: "command"},
				{Type: CommandParam, Value: "1:45:12"},
				{Type: CommandParam, Value: "Nf6"},
				{Type: CommandParam, Value: "very interesting, but wrong"},
				{Type: CommandEnd, Value: "]"},
				{Type: CommentEnd, Value: "}"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestVariations(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Variation start",
			input: "1. e4 (1. d4) e5",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "d4"},
				{Type: VariationEnd, Value: ")"},
				{Type: SQUARE, Value: "e5"},
			},
		},
		{
			name:  "Variation in game",
			input: "1. e4 (1. d4) e5 2. Nf3 (2. Nc3) Nc6",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "d4"},
				{Type: VariationEnd, Value: ")"},
				{Type: SQUARE, Value: "e5"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c3"},
				{Type: VariationEnd, Value: ")"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Nested variations",
			input: "1. e4 (1. d4 (1. c4)) 1... e5",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "d4"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "c4"},
				{Type: VariationEnd, Value: ")"},
				{Type: VariationEnd, Value: ")"},
				{Type: MoveNumber, Value: "1"},
				{Type: ELLIPSIS, Value: "..."},
				{Type: SQUARE, Value: "e5"},
			},
		},

		{
			name:  "Another variation",
			input: "1. e4 e5 (1... e6 2. d4 d5) 2. Nf3",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: SQUARE, Value: "e5"},
				{Type: VariationStart, Value: "("},
				{Type: MoveNumber, Value: "1"},
				{Type: ELLIPSIS, Value: "..."},
				{Type: SQUARE, Value: "e6"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "d4"},
				{Type: SQUARE, Value: "d5"},
				{Type: VariationEnd, Value: ")"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestCaslting(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []Token
	}{
		{
			name:  "Short castle",
			input: "O-O",
			expected: []Token{
				{Type: KingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle",
			input: "O-O-O",
			expected: []Token{
				{Type: QueensideCastle, Value: "O-O-O"},
			},
		},
		{
			name:  "Short castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: SQUARE, Value: "e5"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c6"},
				{Type: MoveNumber, Value: "3"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "B"},
				{Type: SQUARE, Value: "c4"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f6"},
				{Type: MoveNumber, Value: "4"},
				{Type: DOT, Value: "."},
				{Type: KingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O-O",
			expected: []Token{
				{Type: MoveNumber, Value: "1"},
				{Type: DOT, Value: "."},
				{Type: SQUARE, Value: "e4"},
				{Type: SQUARE, Value: "e5"},
				{Type: MoveNumber, Value: "2"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f3"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "c6"},
				{Type: MoveNumber, Value: "3"},
				{Type: DOT, Value: "."},
				{Type: PIECE, Value: "B"},
				{Type: SQUARE, Value: "c4"},
				{Type: PIECE, Value: "N"},
				{Type: SQUARE, Value: "f6"},
				{Type: MoveNumber, Value: "4"},
				{Type: DOT, Value: "."},
				{Type: QueensideCastle, Value: "O-O-O"},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != EOF {
				t.Errorf("Expected EOF token after capture, got %v", token.Type)
			}
		})
	}
}

func TestFuzzRepro_b41648629adb0a5d_y(t *testing.T) {
	input := "y"
	lexer := NewLexer(input)

	var tokens []Token
	for {
		token := lexer.NextToken()
		tokens = append(tokens, token)

		if token.Type == EOF {
			break
		}

		if len(tokens) > len(input)*2 { // Arbitrary limit based on input length
			t.Errorf("Too many tokens generated for input length")
			break
		}
	}
}

func TestFuzzRepro_ff9f899cf2252ff1_a(t *testing.T) {
	input := "a"
	lexer := NewLexer(input)

	var tokens []Token
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("Lexer panicked on input %q: %v", input, r)
		}
	}()
	for {
		token := lexer.NextToken()
		tokens = append(tokens, token)

		if token.Type == EOF {
			break
		}

		if len(tokens) > len(input)*2 { // Arbitrary limit based on input length
			t.Errorf("Too many tokens generated for input length")
			break
		}
	}
}

func TestFuzzRepro_167803a88524c396(t *testing.T) {
	input := "{[%0"
	lexer := NewLexer(input)

	var tokens []Token
	for {
		token := lexer.NextToken()
		tokens = append(tokens, token)

		if token.Type == EOF {
			break
		}

		if len(tokens) > len(input)*2 { // Arbitrary limit based on input length
			t.Errorf("Too many tokens generated for input length")
			break
		}
	}
}

func TestFuzzRepro_b68c42fa4236bdd7(t *testing.T) {
	input := "{[%,\""
	lexer := NewLexer(input)

	var tokens []Token
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("Lexer panicked on input %q: %v", input, r)
		}
	}()
	for {
		token := lexer.NextToken()
		tokens = append(tokens, token)

		if token.Type == EOF {
			break
		}

		if len(tokens) > len(input)*2 { // Arbitrary limit based on input length
			t.Errorf("Too many tokens generated for input length")
			break
		}
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

		lexer := NewLexer(input)
		var tokens []Token

		t.Log("Input:", input)

		// Read all tokens
		for {
			token := lexer.NextToken()
			tokens = append(tokens, token)

			// Stop at EOF
			if token.Type == EOF {
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

func validateTokens(t *testing.T, tokens []Token) {
	for i, token := range tokens {
		// Validate specific token values
		switch token.Type {
		case FILE:
			if len(token.Value) != 1 || token.Value[0] < 'a' || token.Value[0] > 'h' {
				t.Errorf("Invalid file at token %d: %v", i, token.Value)
			}
		case RANK:
			if len(token.Value) != 1 || (!isRank(token.Value[0]) && token.Error == nil) {
				t.Errorf("Invalid rank at token %d: %v", i, token.Value)
			}
		case PIECE:
			if len(token.Value) != 1 || (!isPiece(token.Value[0]) && token.Error == nil) {
				t.Errorf("Invalid piece at token %d: %v", i, token.Value)
			}
		case PromotionPiece:
			if len(token.Value) != 1 || (!isPiece(token.Value[0]) && token.Error == nil) {
				t.Errorf("Invalid promotion piece at token %d: %v", i, token.Value)
			}
		}
	}
}

func TestSingleFromPosPGN(t *testing.T) {
	// Read fixture
	data, err := os.ReadFile("fixtures/pgns/single_frompos.pgn")
	if err != nil {
		t.Fatalf("Failed to read fixture: %v", err)
	}

	lexer := NewLexer(string(data))

	expected := []struct {
		typ   TokenType
		value string
	}{
		// Tags
		{TagStart, "["},
		{TagKey, "Event"},
		{TagValue, "Slav Defense: Chebanenko Variation, 5. cxd5"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Site"},
		{TagValue, ""},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Date"},
		{TagValue, "2025.07.12"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Round"},
		{TagValue, "1"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "White"},
		{TagValue, ""},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Black"},
		{TagValue, ""},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Result"},
		{TagValue, "*"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "UTCDate"},
		{TagValue, "2025.07.12"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "UTCTime"},
		{TagValue, "15:51:01"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Variant"},
		{TagValue, "Standard"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "ECO"},
		{TagValue, "D15"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "FEN"},
		{TagValue, "rnbqkb1r/1p2pppp/p1p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "SetUp"},
		{TagValue, "1"},
		{TagEnd, "]"},
		// Moves
		{MoveNumber, "5"},
		{DOT, "."},
		{FILE, "c"},
		{CAPTURE, "x"},
		{SQUARE, "d5"},
		{VariationStart, "("},
		{MoveNumber, "5"},
		{DOT, "."},
		{SQUARE, "e3"},
		{SQUARE, "e6"},
		{MoveNumber, "6"},
		{DOT, "."},
		{FILE, "c"},
		{CAPTURE, "x"},
		{SQUARE, "d5"},
		{FILE, "c"},
		{CAPTURE, "x"},
		{SQUARE, "d5"},
		{VariationEnd, ")"},
		{MoveNumber, "5"},
		{ELLIPSIS, "..."},
		{FILE, "c"},
		{CAPTURE, "x"},
		{SQUARE, "d5"},
		{MoveNumber, "6"},
		{DOT, "."},
		{SQUARE, "e3"},
		{SQUARE, "e6"},
		{CommentStart, "{"},
		{CommandStart, "[%"},
		{CommandName, "eval"},
		{CommandParam, "0.11"},
		{CommandEnd, "]"},
		{CommentEnd, "}"},
		{RESULT, "*"},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Value != exp.value {
			t.Errorf("Token %d: expected {%v, %q}, got {%v, %q}",
				i, exp.typ, exp.value, tok.Type, tok.Value)
		}
	}
	// final EOF
	if tok := lexer.NextToken(); tok.Type != EOF {
		t.Errorf("Expected EOF, got %v", tok.Type)
	}
}
