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
		{"token_draw", moves + " 1/2-1/2", false, chess.Draw, chess.NoMethod},
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

func TestPGNDrawMovetextTokenRecognized(t *testing.T) {
	g, err := chess.ParsePGN(strings.NewReader("1. e4 e5 1/2-1/2"))
	if err != nil {
		t.Fatal(err)
	}
	if got := g.Outcome(); got != chess.Draw {
		t.Errorf("Outcome() = %v, want %v", got, chess.Draw)
	}
	if got := g.Method(); got != chess.NoMethod {
		t.Errorf("Method() = %v, want %v", got, chess.NoMethod)
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

func TestIllegalMoveErrorsWrapErrIllegalMove(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.Move(chess.NewMove(chess.E2, chess.E5), nil); !errors.Is(err, chess.ErrIllegalMove) {
		t.Errorf("Game.Move(e2e5) error = %v, want ErrIllegalMove", err)
	}
	if _, err := g.MoveText("Qxh7", chess.SAN(), nil); !errors.Is(err, chess.ErrIllegalMove) {
		t.Errorf("Game.MoveText(Qxh7) error = %v, want ErrIllegalMove", err)
	}
	if _, err := g.MoveText("e2e5", chess.UCI(), nil); !errors.Is(err, chess.ErrIllegalMove) {
		t.Errorf("Game.MoveText(e2e5, UCI) error = %v, want ErrIllegalMove", err)
	}
	if _, err := chess.SAN().Decode(g.Position(), "Nf6"); !errors.Is(err, chess.ErrIllegalMove) {
		t.Errorf("SAN().Decode(Nf6) error = %v, want ErrIllegalMove", err)
	}

	_, err := chess.ParsePGN(strings.NewReader("1. e4 e5 2. Qxh7 *"))
	if !errors.Is(err, chess.ErrInvalidPGN) || !errors.Is(err, chess.ErrIllegalMove) {
		t.Errorf("ParsePGN(illegal) error = %v, want ErrInvalidPGN and ErrIllegalMove", err)
	}
	var pe *chess.ParserError
	if !errors.As(err, &pe) {
		t.Fatalf("ParsePGN(illegal) error = %v, want *ParserError", err)
	}
	if strings.Count(err.Error(), "invalid PGN") != 1 {
		t.Errorf("error repeats its prefix: %v", err)
	}

	_, err = chess.ParsePGN(strings.NewReader("[Event \"x"))
	if strings.Count(err.Error(), "invalid PGN") != 1 {
		t.Errorf("header error repeats its prefix: %v", err)
	}
}

func TestParsePGNWithoutTagsKeepsCommentCommands(t *testing.T) {
	for _, in := range []string{
		"1. e4 {good [%clk 0:01:00] move} *",
		"{opening} 1. e4 { [%clk 0:01:00] } e5 *",
	} {
		game, err := chess.ParsePGN(strings.NewReader(in))
		if err != nil {
			t.Fatalf("ParsePGN(%q): %v", in, err)
		}
		if clk, ok := game.MoveTree().MainLine()[0].Command("clk"); !ok || clk != "0:01:00" {
			t.Errorf("ParsePGN(%q): Command(clk) = %q, %v", in, clk, ok)
		}
	}
	var n int
	for _, err := range chess.PGNGames(strings.NewReader("1. e4 {[%clk 0:01:00]} e5 1-0\n\n1. d4 d5 0-1\n")) {
		if err != nil {
			t.Fatal(err)
		}
		n++
	}
	if n != 2 {
		t.Errorf("PGNGames over two tagless games yielded %d games", n)
	}
}

func TestPGNTerminationTagSetsMethod(t *testing.T) {
	cases := []struct {
		termination string
		result      string
		want        chess.Method
	}{
		{"Time forfeit", "1-0", chess.TimeForfeit},
		{"time forfeit", "1/2-1/2", chess.TimeForfeit},
		{"Abandoned", "0-1", chess.Abandoned},
		{"Adjudication", "1/2-1/2", chess.Adjudication},
		{"Rules infraction", "1-0", chess.RulesInfraction},
		{"Normal", "1-0", chess.NoMethod},
		{"Abandoned", "1/2-1/2", chess.NoMethod}, // not a valid draw method
	}
	for _, tc := range cases {
		pgn := "[Result \"" + tc.result + "\"]\n[Termination \"" + tc.termination + "\"]\n\n1. e4 e5 " + tc.result + "\n"
		g, err := chess.ParsePGN(strings.NewReader(pgn))
		if err != nil {
			t.Fatal(err)
		}
		if g.Method() != tc.want {
			t.Errorf("Termination %q, Result %s: Method = %s, want %s", tc.termination, tc.result, g.Method(), tc.want)
		}
	}
	g := chess.NewGame()
	if err := g.SetOutcome(chess.WhiteWon, chess.TimeForfeit); err != nil {
		t.Errorf("SetOutcome(WhiteWon, TimeForfeit): %v", err)
	}
}
