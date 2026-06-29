package chess_test

import (
	"os"
	"testing"

	"github.com/corentings/chess/v3"
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

	lexer := chess.NewLexer(input)
	expectedTokens := []struct {
		typ   chess.TokenType
		value string
	}{
		{chess.TagStart, "["},
		{chess.TagKey, "Event"},
		{chess.TagValue, "Example"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Site"},
		{chess.TagValue, "Internet"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Date"},
		{chess.TagValue, "2023.12.06"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Round"},
		{chess.TagValue, "1"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "White"},
		{chess.TagValue, "Player1"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Black"},
		{chess.TagValue, "Player2"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Result"},
		{chess.TagValue, "1-0"},
		{chess.TagEnd, "]"},
		{chess.MoveNumber, "1"},
		{chess.DOT, "."},
		{chess.SQUARE, "e4"},
		{chess.SQUARE, "e5"},
		{chess.MoveNumber, "2"},
		{chess.DOT, "."},
		{chess.PIECE, "N"},
		{chess.SQUARE, "f3"},
		{chess.PIECE, "N"},
		{chess.SQUARE, "c6"},
		{chess.MoveNumber, "3"},
		{chess.DOT, "."},
		{chess.PIECE, "B"},
		{chess.SQUARE, "b5"},
		{chess.CommentStart, "{"},
		{chess.COMMENT, "This is the Ruy Lopez."},
		{chess.CommentEnd, "}"},
		{chess.MoveNumber, "3"},
		{chess.ELLIPSIS, "..."},
		{chess.SQUARE, "a6"},
		{chess.RESULT, "1-0"},
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
	if token.Type != chess.EOF {
		t.Errorf("Expected EOF token, got %v", token.Type)
	}
}

func TestLexer_ParsesTagValueWithApostrophe(t *testing.T) {
	input := "[Opening \"King's Indian Attack, General\"]"
	lexer := chess.NewLexer(input)

	expectedTokens := []struct {
		typ   chess.TokenType
		value string
	}{
		{chess.TagStart, "["},
		{chess.TagKey, "Opening"},
		{chess.TagValue, "King's Indian Attack, General"},
		{chess.TagEnd, "]"},
	}

	for i, expected := range expectedTokens {
		token := lexer.NextToken()
		if token.Type != expected.typ || token.Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, token.Type, token.Value)
		}
	}
}

func TestLexer_CheckAndCheckmateSuffixes(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "Check",
			input: "e5+",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.CHECK, Value: "+"},
			},
		},
		{
			name:  "Checkmate",
			input: "e5#",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.CHECKMATE, Value: "#"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Promotion(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "Promotion",
			input: "e8=Q",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e8"},
				{Type: chess.PROMOTION, Value: "="},
				{Type: chess.PromotionPiece, Value: "Q"},
			},
		},
		{
			name:  "Promotion in game",
			input: "1. e8=Q e1=N 2. exd8=R",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e8"},
				{Type: chess.PROMOTION, Value: "="},
				{Type: chess.PromotionPiece, Value: "Q"},
				{Type: chess.SQUARE, Value: "e1"},
				{Type: chess.PROMOTION, Value: "="},
				{Type: chess.PromotionPiece, Value: "N"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.FILE, Value: "e"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "d8"},
				{Type: chess.PROMOTION, Value: "="},
				{Type: chess.PromotionPiece, Value: "R"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_NAG(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "NAG",
			input: "e5 $1",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.NAG, Value: "$1"},
			},
		},
		{
			name:  "NAG in game",
			input: "1. e5 $1 e6 $2 2. Nf3 $3",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.NAG, Value: "$1"},
				{Type: chess.SQUARE, Value: "e6"},
				{Type: chess.NAG, Value: "$2"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
				{Type: chess.NAG, Value: "$3"},
			},
		},
		{
			name:  "NAG and comment after move",
			input: "e4 $1 {Good move}",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.NAG, Value: "$1"},
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.COMMENT, Value: "Good move"},
				{Type: chess.CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Comment and NAG after move",
			input: "e4 {Good move} $1",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.COMMENT, Value: "Good move"},
				{Type: chess.CommentEnd, Value: "}"},
				{Type: chess.NAG, Value: "$1"},
			},
		},
		{
			name:  "Move with multiple NAGs and comment",
			input: "e4 $1 $2 {Excellent move}",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.NAG, Value: "$1"},
				{Type: chess.NAG, Value: "$2"},
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.COMMENT, Value: "Excellent move"},
				{Type: chess.CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Move with comment and multiple NAGs",
			input: "e4 {Excellent move} $1 $2",
			expected: []chess.Token{
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.COMMENT, Value: "Excellent move"},
				{Type: chess.CommentEnd, Value: "}"},
				{Type: chess.NAG, Value: "$1"},
				{Type: chess.NAG, Value: "$2"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Captures(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "Pawn capture",
			input: "exf3",
			expected: []chess.Token{
				{Type: chess.FILE, Value: "e"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "f3"},
			},
		},
		{
			name:  "Piece capture",
			input: "Nxc6",
			expected: []chess.Token{
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with file disambiguation",
			input: "Nbxc6",
			expected: []chess.Token{
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.FILE, Value: "b"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Piece capture with rank disambiguation",
			input: "N4xd5",
			expected: []chess.Token{
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.RANK, Value: "4"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "d5"},
			},
		},
		{
			name:  "Complex position with captures",
			input: "1. e4 d5 2. Nf3 Nc6 3. Nbxd5",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.SQUARE, Value: "d5"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "c6"},
				{Type: chess.MoveNumber, Value: "3"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.FILE, Value: "b"},
				{Type: chess.CAPTURE, Value: "x"},
				{Type: chess.SQUARE, Value: "d5"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

// Test captures in context
func TestLexer_CapturesInGameContext(t *testing.T) {
	input := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Bxc6"

	expectedTokens := []struct {
		typ   chess.TokenType
		value string
	}{
		{chess.MoveNumber, "1"},
		{chess.DOT, "."},
		{chess.SQUARE, "e4"},
		{chess.SQUARE, "e5"},
		{chess.MoveNumber, "2"},
		{chess.DOT, "."},
		{chess.PIECE, "N"},
		{chess.SQUARE, "f3"},
		{chess.PIECE, "N"},
		{chess.SQUARE, "c6"},
		{chess.MoveNumber, "3"},
		{chess.DOT, "."},
		{chess.PIECE, "B"},
		{chess.SQUARE, "b5"},
		{chess.SQUARE, "a6"},
		{chess.MoveNumber, "4"},
		{chess.DOT, "."},
		{chess.PIECE, "B"},
		{chess.CAPTURE, "x"},
		{chess.SQUARE, "c6"},
	}

	lexer := chess.NewLexer(input)

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
		expected []chess.Token
	}{
		{
			name:  "Command",
			input: "{[%clk 12:34:56]}",
			expected: []chess.Token{
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.CommandStart, Value: "[%"},
				{Type: chess.CommandName, Value: "clk"},
				{Type: chess.CommandParam, Value: "12:34:56"},
				{Type: chess.CommandEnd, Value: "]"},
				{Type: chess.CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command in game",
			input: "1. e4 {[%clk 12:34:56]} e5",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.CommandStart, Value: "[%"},
				{Type: chess.CommandName, Value: "clk"},
				{Type: chess.CommandParam, Value: "12:34:56"},
				{Type: chess.CommandEnd, Value: "]"},
				{Type: chess.CommentEnd, Value: "}"},
				{Type: chess.SQUARE, Value: "e5"},
			},
		},
		{ // Test multiple commands
			name:  "Multiple commands",
			input: "{[%clk 0:00:07][%eval -6.05] White is toast}",
			expected: []chess.Token{
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.CommandStart, Value: "[%"},
				{Type: chess.CommandName, Value: "clk"},
				{Type: chess.CommandParam, Value: "0:00:07"},
				{Type: chess.CommandEnd, Value: "]"},
				{Type: chess.CommandStart, Value: "[%"},
				{Type: chess.CommandName, Value: "eval"},
				{Type: chess.CommandParam, Value: "-6.05"},
				{Type: chess.CommandEnd, Value: "]"},
				{Type: chess.COMMENT, Value: "White is toast"},
				{Type: chess.CommentEnd, Value: "}"},
			},
		},
		{
			name:  "Command with multiple parameters",
			input: "{[%command 1:45:12,Nf6,\"very interesting, but wrong\"]}",
			expected: []chess.Token{
				{Type: chess.CommentStart, Value: "{"},
				{Type: chess.CommandStart, Value: "[%"},
				{Type: chess.CommandName, Value: "command"},
				{Type: chess.CommandParam, Value: "1:45:12"},
				{Type: chess.CommandParam, Value: "Nf6"},
				{Type: chess.CommandParam, Value: "very interesting, but wrong"},
				{Type: chess.CommandEnd, Value: "]"},
				{Type: chess.CommentEnd, Value: "}"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Variations(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "Variation start",
			input: "1. e4 (1. d4) e5",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "d4"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.SQUARE, Value: "e5"},
			},
		},
		{
			name:  "Variation in game",
			input: "1. e4 (1. d4) e5 2. Nf3 (2. Nc3) Nc6",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "d4"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "c3"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "c6"},
			},
		},
		{
			name:  "Nested variations",
			input: "1. e4 (1. d4 (1. c4)) 1... e5",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "d4"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "c4"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.ELLIPSIS, Value: "..."},
				{Type: chess.SQUARE, Value: "e5"},
			},
		},

		{
			name:  "Another variation",
			input: "1. e4 e5 (1... e6 2. d4 d5) 2. Nf3",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.VariationStart, Value: "("},
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.ELLIPSIS, Value: "..."},
				{Type: chess.SQUARE, Value: "e6"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "d4"},
				{Type: chess.SQUARE, Value: "d5"},
				{Type: chess.VariationEnd, Value: ")"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestLexer_Castling(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{
			name:  "Short castle",
			input: "O-O",
			expected: []chess.Token{
				{Type: chess.KingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle",
			input: "O-O-O",
			expected: []chess.Token{
				{Type: chess.QueensideCastle, Value: "O-O-O"},
			},
		},
		{
			name:  "Short castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "c6"},
				{Type: chess.MoveNumber, Value: "3"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "B"},
				{Type: chess.SQUARE, Value: "c4"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f6"},
				{Type: chess.MoveNumber, Value: "4"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.KingsideCastle, Value: "O-O"},
			},
		},
		{
			name:  "Long castle in game",
			input: "1. e4 e5 2. Nf3 Nc6 3. Bc4 Nf6 4. O-O-O",
			expected: []chess.Token{
				{Type: chess.MoveNumber, Value: "1"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.SQUARE, Value: "e4"},
				{Type: chess.SQUARE, Value: "e5"},
				{Type: chess.MoveNumber, Value: "2"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f3"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "c6"},
				{Type: chess.MoveNumber, Value: "3"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.PIECE, Value: "B"},
				{Type: chess.SQUARE, Value: "c4"},
				{Type: chess.PIECE, Value: "N"},
				{Type: chess.SQUARE, Value: "f6"},
				{Type: chess.MoveNumber, Value: "4"},
				{Type: chess.DOT, Value: "."},
				{Type: chess.QueensideCastle, Value: "O-O-O"},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lexer := chess.NewLexer(tt.input)

			for i, expected := range tt.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}

			// Verify we get EOF after all tokens
			token := lexer.NextToken()
			if token.Type != chess.EOF {
				t.Errorf("expected EOF after sequence, got %v", token.Type)
			}
		})
	}
}

func TestFuzzRegressions(t *testing.T) {
	seeds := []string{"y", "a", "{[%0", "{[%,\""}
	for _, input := range seeds {
		t.Run(input, func(t *testing.T) {
			lexer := chess.NewLexer(input)
			defer func() {
				if r := recover(); r != nil {
					t.Errorf("Lexer panicked on input %q: %v", input, r)
				}
			}()
			count := 0
			for {
				token := lexer.NextToken()
				count++
				if token.Type == chess.EOF {
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

		lexer := chess.NewLexer(input)
		var tokens []chess.Token

		t.Log("Input:", input)

		// Read all tokens
		for {
			token := lexer.NextToken()
			tokens = append(tokens, token)

			// Stop at EOF
			if token.Type == chess.EOF {
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

func validateTokens(t *testing.T, tokens []chess.Token) {
	for i, token := range tokens {
		// Validate specific token values
		switch token.Type {
		case chess.FILE:
			if len(token.Value) != 1 || token.Value[0] < 'a' || token.Value[0] > 'h' {
				t.Errorf("Invalid file at token %d: %v", i, token.Value)
			}
		case chess.RANK:
			if len(token.Value) != 1 || (!(token.Value[0] >= '1' && token.Value[0] <= '8') && token.Error == nil) {
				t.Errorf("Invalid rank at token %d: %v", i, token.Value)
			}
		case chess.PIECE:
			if len(token.Value) != 1 || (!(token.Value[0] == 'N' || token.Value[0] == 'B' || token.Value[0] == 'R' || token.Value[0] == 'Q' || token.Value[0] == 'K') && token.Error == nil) {
				t.Errorf("Invalid piece at token %d: %v", i, token.Value)
			}
		case chess.PromotionPiece:
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

	lexer := chess.NewLexer(string(data))

	expected := []struct {
		typ   chess.TokenType
		value string
	}{
		// Tags
		{chess.TagStart, "["},
		{chess.TagKey, "Event"},
		{chess.TagValue, "Slav Defense: Chebanenko Variation, 5. cxd5"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Site"},
		{chess.TagValue, ""},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Date"},
		{chess.TagValue, "2025.07.12"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Round"},
		{chess.TagValue, "1"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "White"},
		{chess.TagValue, ""},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Black"},
		{chess.TagValue, ""},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Result"},
		{chess.TagValue, "*"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "UTCDate"},
		{chess.TagValue, "2025.07.12"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "UTCTime"},
		{chess.TagValue, "15:51:01"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "Variant"},
		{chess.TagValue, "Standard"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "ECO"},
		{chess.TagValue, "D15"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "FEN"},
		{chess.TagValue, "rnbqkb1r/1p2pppp/p1p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5"},
		{chess.TagEnd, "]"},
		{chess.TagStart, "["},
		{chess.TagKey, "SetUp"},
		{chess.TagValue, "1"},
		{chess.TagEnd, "]"},
		// Moves
		{chess.MoveNumber, "5"},
		{chess.DOT, "."},
		{chess.FILE, "c"},
		{chess.CAPTURE, "x"},
		{chess.SQUARE, "d5"},
		{chess.VariationStart, "("},
		{chess.MoveNumber, "5"},
		{chess.DOT, "."},
		{chess.SQUARE, "e3"},
		{chess.SQUARE, "e6"},
		{chess.MoveNumber, "6"},
		{chess.DOT, "."},
		{chess.FILE, "c"},
		{chess.CAPTURE, "x"},
		{chess.SQUARE, "d5"},
		{chess.FILE, "c"},
		{chess.CAPTURE, "x"},
		{chess.SQUARE, "d5"},
		{chess.VariationEnd, ")"},
		{chess.MoveNumber, "5"},
		{chess.ELLIPSIS, "..."},
		{chess.FILE, "c"},
		{chess.CAPTURE, "x"},
		{chess.SQUARE, "d5"},
		{chess.MoveNumber, "6"},
		{chess.DOT, "."},
		{chess.SQUARE, "e3"},
		{chess.SQUARE, "e6"},
		{chess.CommentStart, "{"},
		{chess.CommandStart, "[%"},
		{chess.CommandName, "eval"},
		{chess.CommandParam, "0.11"},
		{chess.CommandEnd, "]"},
		{chess.CommentEnd, "}"},
		{chess.RESULT, "*"},
	}

	for i, exp := range expected {
		tok := lexer.NextToken()
		if tok.Type != exp.typ || tok.Value != exp.value {
			t.Errorf("Token %d: expected {%v, %q}, got {%v, %q}",
				i, exp.typ, exp.value, tok.Type, tok.Value)
		}
	}
	// final EOF
	if tok := lexer.NextToken(); tok.Type != chess.EOF {
		t.Errorf("Expected EOF, got %v", tok.Type)
	}
}

func assertTokenSequence(t *testing.T, l *chess.Lexer, expected []chess.Token) {
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
	if tok := l.NextToken(); tok.Type != chess.EOF {
		t.Errorf("expected EOF after sequence, got {%v, %q}", tok.Type, tok.Value)
	}
}

func TestLexer_EmptyInput(t *testing.T) {
	for _, in := range []string{"", "   ", " \t\n  "} {
		l := chess.NewLexer(in)
		tok := l.NextToken()
		if tok.Type != chess.EOF {
			t.Errorf("NewLexer(%q).NextToken() = %v, want EOF", in, tok.Type)
		}
	}
}

func TestLexer_UndefinedToken(t *testing.T) {
	for _, in := range []string{"@", ";", "~"} {
		l := chess.NewLexer(in)
		tok := l.NextToken()
		if tok.Type != chess.Undefined || tok.Value != in {
			t.Errorf("NewLexer(%q).NextToken() = {%v, %q}, want {Undefined, %q}", in, tok.Type, tok.Value, in)
		}
		if eof := l.NextToken(); eof.Type != chess.EOF {
			t.Errorf("expected EOF after undefined, got %v", eof.Type)
		}
	}
}

func TestLexer_DeambiguationSquare(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []chess.Token
	}{
		{"full_square", "Qe8f7", []chess.Token{
			{Type: chess.PIECE, Value: "Q"},
			{Type: chess.DeambiguationSquare, Value: "e8"},
			{Type: chess.SQUARE, Value: "f7"},
		}},
		{"full_square_capture", "Qg8xg7", []chess.Token{
			{Type: chess.PIECE, Value: "Q"},
			{Type: chess.DeambiguationSquare, Value: "g8"},
			{Type: chess.CAPTURE, Value: "x"},
			{Type: chess.SQUARE, Value: "g7"},
		}},
		{"file_only", "Nbd7", []chess.Token{
			{Type: chess.PIECE, Value: "N"},
			{Type: chess.FILE, Value: "b"},
			{Type: chess.SQUARE, Value: "d7"},
		}},
		{"rank_only", "N4d7", []chess.Token{
			{Type: chess.PIECE, Value: "N"},
			{Type: chess.RANK, Value: "4"},
			{Type: chess.SQUARE, Value: "d7"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, chess.NewLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_ResultSpellings(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []chess.Token
	}{
		{"white_wins", "1-0", []chess.Token{{Type: chess.RESULT, Value: "1-0"}}},
		{"black_wins", "0-1", []chess.Token{{Type: chess.RESULT, Value: "0-1"}}},
		{"ongoing", "*", []chess.Token{{Type: chess.RESULT, Value: "*"}}},
		{"draw", "1/2-1/2", []chess.Token{
			{Type: chess.RESULT, Value: "1/2-1/2"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, chess.NewLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_NAGSymbols(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  []chess.Token
	}{
		{"double_bang", "e4!!", []chess.Token{
			{Type: chess.SQUARE, Value: "e4"},
			{Type: chess.NAG, Value: "!!"},
		}},
		{"bang", "e4!", []chess.Token{
			{Type: chess.SQUARE, Value: "e4"},
			{Type: chess.NAG, Value: "!"},
		}},
		{"question", "e4?", []chess.Token{
			{Type: chess.SQUARE, Value: "e4"},
			{Type: chess.NAG, Value: "?"},
		}},
		{"dubious", "e4?!", []chess.Token{
			{Type: chess.SQUARE, Value: "e4"},
			{Type: chess.NAG, Value: "?!"},
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			assertTokenSequence(t, chess.NewLexer(tt.input), tt.want)
		})
	}
}

func TestLexer_NAGMaximalRun(t *testing.T) {
	// A run of more than two '!'/'?' characters must be a single NAG token,
	// not split into a valid token plus a trailing symbol (regression for the
	// "!!!" bug that produced two NAG tokens).
	assertTokenSequence(t, chess.NewLexer("e4!!!"), []chess.Token{
		{Type: chess.SQUARE, Value: "e4"},
		{Type: chess.NAG, Value: "!!!"},
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
			l := chess.NewLexer(tt.input)
			var errToken chess.Token
			for {
				tok := l.NextToken()
				if tok.Error != nil {
					errToken = tok
					break
				}
				if tok.Type == chess.EOF {
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
