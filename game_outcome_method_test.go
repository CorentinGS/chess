package chess_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func ExampleGame_SetOutcome() {
	g := chess.NewGame()
	if err := g.SetOutcome(chess.WhiteWon, chess.Resignation); err != nil {
		fmt.Println("Error:", err)
		return
	}
	fmt.Println(g.Outcome(), g.Method())
	// Output: 1-0 Resignation
}

func TestSetOutcome(t *testing.T) {
	tests := []struct {
		name    string
		outcome chess.Outcome
		method  chess.Method
		wantErr bool
	}{
		{"NoOutcome/NoMethod", chess.NoOutcome, chess.NoMethod, false},
		{"NoOutcome/Checkmate", chess.NoOutcome, chess.Checkmate, true},
		{"NoOutcome/Stalemate", chess.NoOutcome, chess.Stalemate, true},
		{"NoOutcome/Resignation", chess.NoOutcome, chess.Resignation, true},
		{"WhiteWon/NoMethod", chess.WhiteWon, chess.NoMethod, false},
		{"WhiteWon/Checkmate", chess.WhiteWon, chess.Checkmate, false},
		{"WhiteWon/Resignation", chess.WhiteWon, chess.Resignation, false},
		{"WhiteWon/Stalemate", chess.WhiteWon, chess.Stalemate, true},
		{"WhiteWon/DrawOffer", chess.WhiteWon, chess.DrawOffer, true},
		{"WhiteWon/FiftyMoveRule", chess.WhiteWon, chess.FiftyMoveRule, true},
		{"BlackWon/NoMethod", chess.BlackWon, chess.NoMethod, false},
		{"BlackWon/Checkmate", chess.BlackWon, chess.Checkmate, false},
		{"BlackWon/Resignation", chess.BlackWon, chess.Resignation, false},
		{"BlackWon/Stalemate", chess.BlackWon, chess.Stalemate, true},
		{"BlackWon/InsufficientMaterial", chess.BlackWon, chess.InsufficientMaterial, true},
		{"Draw/NoMethod", chess.Draw, chess.NoMethod, false},
		{"Draw/DrawOffer", chess.Draw, chess.DrawOffer, false},
		{"Draw/Stalemate", chess.Draw, chess.Stalemate, false},
		{"Draw/ThreefoldRepetition", chess.Draw, chess.ThreefoldRepetition, false},
		{"Draw/FivefoldRepetition", chess.Draw, chess.FivefoldRepetition, false},
		{"Draw/FiftyMoveRule", chess.Draw, chess.FiftyMoveRule, false},
		{"Draw/SeventyFiveMoveRule", chess.Draw, chess.SeventyFiveMoveRule, false},
		{"Draw/InsufficientMaterial", chess.Draw, chess.InsufficientMaterial, false},
		{"Draw/Checkmate", chess.Draw, chess.Checkmate, true},
		{"Draw/Resignation", chess.Draw, chess.Resignation, true},
		{"NoOutcome/NoMethod", chess.NoOutcome, chess.NoMethod, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g := chess.NewGame()
			err := g.SetOutcome(tt.outcome, tt.method)
			if tt.wantErr {
				if err == nil {
					t.Fatalf("expected error, got nil")
				}
				if g.Outcome() != chess.NoOutcome {
					t.Errorf("Outcome = %v after rejection, want NoOutcome", g.Outcome())
				}
				if g.Method() != chess.NoMethod {
					t.Errorf("Method = %v after rejection, want NoMethod", g.Method())
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if g.Outcome() != tt.outcome {
				t.Errorf("Outcome = %v, want %v", g.Outcome(), tt.outcome)
			}
			if g.Method() != tt.method {
				t.Errorf("Method = %v, want %v", g.Method(), tt.method)
			}
		})
	}
}

func TestTerminalOutcomeGuards(t *testing.T) {
	setups := map[string]func(t *testing.T, g *chess.Game){
		"via_SetOutcome": func(t *testing.T, g *chess.Game) {
			t.Helper()
			if err := g.SetOutcome(chess.WhiteWon, chess.Checkmate); err != nil {
				t.Fatal(err)
			}
		},
		"via_Resign": func(t *testing.T, g *chess.Game) {
			t.Helper()
			if err := g.Resign(chess.White); err != nil {
				t.Fatal(err)
			}
		},
		"via_Draw": func(t *testing.T, g *chess.Game) {
			t.Helper()
			if err := g.Draw(chess.DrawOffer); err != nil {
				t.Fatal(err)
			}
		},
	}
	actions := []struct {
		name string
		call func(g *chess.Game) error
	}{
		{"Move", func(g *chess.Game) error { _, err := g.Move(g.LegalMoves()[0], nil); return err }},
		{"UnsafeMove", func(g *chess.Game) error { _, err := g.UnsafeMove(g.LegalMoves()[0], nil); return err }},
		{"MoveText", func(g *chess.Game) error {
			_, err := g.MoveText("e4", chess.SAN(), nil)
			return err
		}},
		{"UnsafeMoveText", func(g *chess.Game) error {
			_, err := g.UnsafeMoveText("e2e4", chess.UCI(), nil)
			return err
		}},
		{"Resign", func(g *chess.Game) error { return g.Resign(chess.Black) }},
		{"Draw", func(g *chess.Game) error { return g.Draw(chess.DrawOffer) }},
	}
	for setupName, setup := range setups {
		t.Run(setupName, func(t *testing.T) {
			for _, act := range actions {
				t.Run(act.name, func(t *testing.T) {
					g := chess.NewGame()
					setup(t, g)
					err := act.call(g)
					if !errors.Is(err, chess.ErrGameAlreadyEnded) {
						t.Errorf("%s after terminal: error = %v, want ErrGameAlreadyEnded", act.name, err)
					}
				})
			}
		})
	}
}

func TestResignWithNoColorReturnsError(t *testing.T) {
	g := chess.NewGame()
	err := g.Resign(chess.NoColor)
	if err == nil {
		t.Fatal("expected error when resigning with NoColor, got nil")
	}
	if g.Outcome() != chess.NoOutcome {
		t.Errorf("Outcome = %s, want NoOutcome", g.Outcome())
	}
}

func TestResignSetsOpponentWinner(t *testing.T) {
	g := chess.NewGame()
	if err := g.Resign(chess.White); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() != chess.BlackWon {
		t.Errorf("Outcome = %v, want BlackWon", g.Outcome())
	}
	if g.Method() != chess.Resignation {
		t.Errorf("Method = %v, want Resignation", g.Method())
	}
}

func TestFinishedGameAcceptsVariationsButNotMainLineMoves(t *testing.T) {
	g := chess.NewGame()
	for _, san := range []string{"f3", "e5", "g4", "Qh4#"} {
		if _, err := g.MoveText(san, chess.SAN(), nil); err != nil {
			t.Fatal(err)
		}
	}
	if g.Outcome() != chess.BlackWon {
		t.Fatalf("outcome = %s, want 0-1", g.Outcome())
	}

	// At the end of the main line the game is over.
	if _, err := g.NullMove(nil); !errors.Is(err, chess.ErrGameAlreadyEnded) {
		t.Fatalf("move after mate: error = %v, want ErrGameAlreadyEnded", err)
	}

	// From an earlier node a move is analysis: it adds a variation and the
	// game keeps its result.
	if !g.MoveTree().GoBack() {
		t.Fatal("GoBack failed")
	}
	node, err := g.MoveText("Qe7", chess.SAN(), nil)
	if err != nil {
		t.Fatalf("variation after GoBack: %v", err)
	}
	if g.Outcome() != chess.BlackWon || g.Method() != chess.Checkmate {
		t.Fatalf("variation changed the outcome to %s %s", g.Outcome(), g.Method())
	}
	if len(node.Parent().Variations()) != 1 {
		t.Fatalf("Qe7 should be a variation, got %d variations", len(node.Parent().Variations()))
	}
	// Extending the variation is allowed too.
	if _, err := g.MoveText("Nc3", chess.SAN(), nil); err != nil {
		t.Fatalf("extending the variation: %v", err)
	}

	// Promoting a line to the main line re-evaluates the outcome.
	g.MoveTree().GoBack()
	g.MoveTree().GoBack()
	if _, err := g.MoveText("Qe7", chess.SAN(), &chess.MoveInsertOptions{PromoteToMainLine: true}); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() != chess.NoOutcome {
		t.Fatalf("after promoting an unfinished line, outcome = %s, want *", g.Outcome())
	}
}

func TestAddVariationAllowedAfterTerminalOutcome(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.MoveText("e4", chess.SAN(), nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcome(chess.WhiteWon, chess.Checkmate); err != nil {
		t.Fatal(err)
	}
	if !g.MoveTree().GoBack() {
		t.Fatal("GoBack failed")
	}
	variation := g.LegalMoves()[0]
	g.MoveTree().AddVariation(g.MoveTree().Root(), variation)
	if len(g.MoveTree().Root().Continuations()) == 0 {
		t.Error("AddVariation did not append child after terminal outcome")
	}
}

func TestClearOutcome(t *testing.T) {
	g := chess.NewGame()
	if err := g.SetOutcome(chess.WhiteWon, chess.Checkmate); err != nil {
		t.Fatal(err)
	}
	g.ClearOutcome()
	if g.Outcome() != chess.NoOutcome {
		t.Errorf("Outcome after Clear = %s, want NoOutcome", g.Outcome())
	}
	if g.Method() != chess.NoMethod {
		t.Errorf("Method after Clear = %s, want NoMethod", g.Method())
	}
	g.ClearOutcome()
	if g.Outcome() != chess.NoOutcome || g.Method() != chess.NoMethod {
		t.Error("ClearOutcome should be idempotent")
	}
}

func TestClearOutcomeDoesNotModifyResultTag(t *testing.T) {
	g := chess.NewGame()
	g.SetTag("Result", "1-0")
	if err := g.SetOutcome(chess.WhiteWon, chess.Checkmate); err != nil {
		t.Fatal(err)
	}
	g.ClearOutcome()
	if got := g.Tag("Result"); got != "1-0" {
		t.Errorf("Result tag after ClearOutcome = %q, want 1-0", got)
	}
}

func TestTagPairLifecycle(t *testing.T) {
	g := chess.NewGame()
	g.SetTag("Event", "E1")
	if got := g.Tag("Event"); got != "E1" {
		t.Errorf("Tag = %q, want E1", got)
	}
	g.SetTag("Event", "E2")
	tags := g.Tags()
	if tags["Event"] != "E2" {
		t.Errorf("Tags()[Event] = %q, want E2", tags["Event"])
	}
	tags["Event"] = "mutated"
	if got := g.Tag("Event"); got != "E2" {
		t.Errorf("mutating Tags() changed the game: Tag = %q", got)
	}
	if got := g.Tag("Event"); got != "E2" {
		t.Errorf("Tag after overwrite = %q, want E2", got)
	}
	if !g.RemoveTag("Event") {
		t.Error("RemoveTag on existing key should return true")
	}
	if got := g.Tag("Event"); got != "" {
		t.Errorf("Tag after remove = %q, want empty", got)
	}
	if g.RemoveTag("Event") {
		t.Error("RemoveTag on absent key should return false")
	}
	if g.RemoveTag("NeverExisted") {
		t.Error("RemoveTag on never-existing key should return false")
	}
}

func TestEligibleDraws_FreshGameReturnsOnlyDrawOffer(t *testing.T) {
	g := chess.NewGame()
	draws := g.EligibleDraws()
	if len(draws) != 1 || draws[0] != chess.DrawOffer {
		t.Errorf("fresh game EligibleDraws = %v, want [DrawOffer]", draws)
	}
}

func TestEligibleDraws_IncludesThreefoldRepetitionWhenConditionsMet(t *testing.T) {
	g := chess.NewGame()
	for _, mv := range []string{"Nf3", "Nf6", "Ng1", "Ng8", "Nf3", "Nf6", "Ng1", "Ng8"} {
		if _, err := g.MoveText(mv, chess.SAN(), nil); err != nil {
			t.Fatal(err)
		}
	}
	draws := g.EligibleDraws()
	found := false
	for _, d := range draws {
		if d == chess.ThreefoldRepetition {
			found = true
		}
	}
	if !found {
		t.Errorf("EligibleDraws = %v, want to include ThreefoldRepetition", draws)
	}
}

func TestDraw_RejectsInvalidMethod(t *testing.T) {
	tests := []struct {
		name    string
		method  chess.Method
		wantSub string
	}{
		{"no_method", chess.NoMethod, "invalid draw method"},
		{"checkmate", chess.Checkmate, "invalid draw method"},
		{"resignation", chess.Resignation, "invalid draw method"},
		{"stalemate", chess.Stalemate, "invalid draw method"},
		{"fivefold_repetition", chess.FivefoldRepetition, "invalid draw method"},
		{"seventyfive_move_rule", chess.SeventyFiveMoveRule, "invalid draw method"},
		{"insufficient_material", chess.InsufficientMaterial, "invalid draw method"},
		{"threefold_repetition_unmet", chess.ThreefoldRepetition, "ThreefoldRepetition"},
		{"fifty_move_rule_unmet", chess.FiftyMoveRule, "FiftyMoveRule"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g := chess.NewGame()
			err := g.Draw(tt.method)
			if err == nil {
				t.Fatalf("Draw(%s) = nil, want error", tt.method)
			}
			if !strings.Contains(err.Error(), tt.wantSub) {
				t.Errorf("Draw(%s) error = %q, want substring %q", tt.method, err.Error(), tt.wantSub)
			}
			if g.Outcome() != chess.NoOutcome {
				t.Errorf("Outcome = %v after rejected Draw, want NoOutcome", g.Outcome())
			}
		})
	}
}

func TestDrawOfferSucceeds(t *testing.T) {
	g := chess.NewGame()
	if err := g.Draw(chess.DrawOffer); err != nil {
		t.Fatalf("Draw(DrawOffer) = %v, want nil", err)
	}
	if g.Outcome() != chess.Draw {
		t.Errorf("Outcome = %v, want Draw", g.Outcome())
	}
	if g.Method() != chess.DrawOffer {
		t.Errorf("Method = %v, want DrawOffer", g.Method())
	}
}
