package chess_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGN_ResolvesOutcomeFromTagTokenAndBoard(t *testing.T) {
	moves := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6"
	scholarMate := "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6?? 4. Qxf7#"

	tests := []struct {
		name        string
		pgn         string
		wantErr     bool
		wantOutcome chess.Outcome
		wantMethod  chess.Method
	}{
		{"tag_white_won", "[Result \"1-0\"]\n\n" + moves + " *", false, chess.WhiteWon, chess.NoMethod},
		{"tag_draw", "[Result \"1/2-1/2\"]\n\n" + moves + " *", false, chess.Draw, chess.NoMethod},
		{"tag_black_won", "[Result \"0-1\"]\n\n" + moves + " *", false, chess.BlackWon, chess.NoMethod},
		{"token_white_won", moves + " 1-0", false, chess.WhiteWon, chess.NoMethod},
		{"token_black_won", moves + " 0-1", false, chess.BlackWon, chess.NoMethod},
		{"token_no_outcome", moves + " *", false, chess.NoOutcome, chess.NoMethod},
		{"tag_token_agree", "[Result \"1-0\"]\n\n" + moves + " 1-0", false, chess.WhiteWon, chess.NoMethod},
		{"tag_token_conflict", "[Result \"0-1\"]\n\n" + moves + " 1-0", true, chess.NoOutcome, chess.NoMethod},
		{"board_checkmate_conflicts_with_tag", "[Result \"0-1\"]\n\n" + scholarMate + " 1-0", true, chess.NoOutcome, chess.NoMethod},
		{"board_checkmate_agrees_with_tag", "[Result \"1-0\"]\n\n" + scholarMate + " 1-0", false, chess.WhiteWon, chess.Checkmate},
		{"tag_overrides_star_token", "[Result \"0-1\"]\n\n" + moves + " *", false, chess.BlackWon, chess.NoMethod},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g, err := chess.ParsePGN(strings.NewReader(tt.pgn))
			if tt.wantErr {
				var pe *chess.ParserError
				if !errors.As(err, &pe) {
					t.Fatalf("expected *ParserError, got %T (%v)", err, err)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if got := g.Outcome(); got != tt.wantOutcome {
				t.Errorf("Outcome() = %v, want %v", got, tt.wantOutcome)
			}
			if got := g.Method(); got != tt.wantMethod {
				t.Errorf("Method() = %v, want %v", got, tt.wantMethod)
			}
		})
	}
}

func TestPGNDrawMovetextTokenNotRecognized(t *testing.T) {
	g, err := chess.ParsePGN(strings.NewReader("1. e4 e5 1/2-1/2"))
	if err != nil {
		t.Fatal(err)
	}
	if got := g.Outcome(); got != chess.NoOutcome {
		t.Errorf("draw movetext token currently yields %v; lexer does not recognize 1/2-1/2 as a RESULT (only the Result tag produces draws). If this changed, update this test", got)
	}
}

func TestPGNTerminalVariationDoesNotSetMainLineOutcome(t *testing.T) {
	g, err := chess.ParsePGN(strings.NewReader(`
[Result "*"]

(1. f3 e5 2. g4 Qh4#) 1. e4 e5 *`))
	if err != nil {
		t.Fatal(err)
	}
	if got := g.Outcome(); got != chess.NoOutcome {
		t.Fatalf("Outcome() = %v, want %v", got, chess.NoOutcome)
	}
	if got := g.Method(); got != chess.NoMethod {
		t.Fatalf("Method() = %v, want %v", got, chess.NoMethod)
	}
}
