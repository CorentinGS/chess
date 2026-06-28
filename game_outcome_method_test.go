package chess_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func ExampleGame_SetOutcomeMethod() {
	g := chess.NewGame()
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{
		Outcome: chess.WhiteWon,
		Method:  chess.Resignation,
	}); err != nil {
		fmt.Println("Error:", err)
		return
	}
	fmt.Println(g.Outcome(), g.Method())
	// Output: 1-0 Resignation
}

func TestSetOutcomeMethod(t *testing.T) {
	tests := []struct {
		name    string
		pair    chess.OutcomeMethodPair
		wantErr bool
	}{
		{"NoOutcome/NoMethod", chess.OutcomeMethodPair{chess.NoOutcome, chess.NoMethod}, false},
		{"NoOutcome/Checkmate", chess.OutcomeMethodPair{chess.NoOutcome, chess.Checkmate}, true},
		{"NoOutcome/Stalemate", chess.OutcomeMethodPair{chess.NoOutcome, chess.Stalemate}, true},
		{"NoOutcome/Resignation", chess.OutcomeMethodPair{chess.NoOutcome, chess.Resignation}, true},
		{"WhiteWon/NoMethod", chess.OutcomeMethodPair{chess.WhiteWon, chess.NoMethod}, false},
		{"WhiteWon/Checkmate", chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}, false},
		{"WhiteWon/Resignation", chess.OutcomeMethodPair{chess.WhiteWon, chess.Resignation}, false},
		{"WhiteWon/Stalemate", chess.OutcomeMethodPair{chess.WhiteWon, chess.Stalemate}, true},
		{"WhiteWon/DrawOffer", chess.OutcomeMethodPair{chess.WhiteWon, chess.DrawOffer}, true},
		{"WhiteWon/FiftyMoveRule", chess.OutcomeMethodPair{chess.WhiteWon, chess.FiftyMoveRule}, true},
		{"BlackWon/NoMethod", chess.OutcomeMethodPair{chess.BlackWon, chess.NoMethod}, false},
		{"BlackWon/Checkmate", chess.OutcomeMethodPair{chess.BlackWon, chess.Checkmate}, false},
		{"BlackWon/Resignation", chess.OutcomeMethodPair{chess.BlackWon, chess.Resignation}, false},
		{"BlackWon/Stalemate", chess.OutcomeMethodPair{chess.BlackWon, chess.Stalemate}, true},
		{"BlackWon/InsufficientMaterial", chess.OutcomeMethodPair{chess.BlackWon, chess.InsufficientMaterial}, true},
		{"Draw/NoMethod", chess.OutcomeMethodPair{chess.Draw, chess.NoMethod}, false},
		{"Draw/DrawOffer", chess.OutcomeMethodPair{chess.Draw, chess.DrawOffer}, false},
		{"Draw/Stalemate", chess.OutcomeMethodPair{chess.Draw, chess.Stalemate}, false},
		{"Draw/ThreefoldRepetition", chess.OutcomeMethodPair{chess.Draw, chess.ThreefoldRepetition}, false},
		{"Draw/FivefoldRepetition", chess.OutcomeMethodPair{chess.Draw, chess.FivefoldRepetition}, false},
		{"Draw/FiftyMoveRule", chess.OutcomeMethodPair{chess.Draw, chess.FiftyMoveRule}, false},
		{"Draw/SeventyFiveMoveRule", chess.OutcomeMethodPair{chess.Draw, chess.SeventyFiveMoveRule}, false},
		{"Draw/InsufficientMaterial", chess.OutcomeMethodPair{chess.Draw, chess.InsufficientMaterial}, false},
		{"Draw/Checkmate", chess.OutcomeMethodPair{chess.Draw, chess.Checkmate}, true},
		{"Draw/Resignation", chess.OutcomeMethodPair{chess.Draw, chess.Resignation}, true},
		{"UnknownOutcome/NoMethod", chess.OutcomeMethodPair{chess.UnknownOutcome, chess.NoMethod}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g := chess.NewGame()
			err := g.SetOutcomeMethod(tt.pair)
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
			if g.Outcome() != tt.pair.Outcome {
				t.Errorf("Outcome = %v, want %v", g.Outcome(), tt.pair.Outcome)
			}
			if g.Method() != tt.pair.Method {
				t.Errorf("Method = %v, want %v", g.Method(), tt.pair.Method)
			}
		})
	}
}

func TestTerminalOutcomeGuards(t *testing.T) {
	setups := map[string]func(t *testing.T, g *chess.Game){
		"via_SetOutcomeMethod": func(t *testing.T, g *chess.Game) {
			t.Helper()
			if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
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
		{"Move", func(g *chess.Game) error { return g.Move(g.ValidMoves()[0], nil) }},
		{"UnsafeMove", func(g *chess.Game) error { return g.UnsafeMove(g.ValidMoves()[0], nil) }},
		{"PushMove", func(g *chess.Game) error { return g.PushMove("e4", nil) }},
		{"PushNotationMove", func(g *chess.Game) error {
			return g.PushNotationMove("e4", chess.AlgebraicNotation{}, nil)
		}},
		{"UnsafePushNotationMove", func(g *chess.Game) error {
			return g.UnsafePushNotationMove("e4", chess.AlgebraicNotation{}, nil)
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

func TestMoveAfterGoBackFromTerminalStillRejected(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	if !g.GoBack() {
		t.Fatal("GoBack failed")
	}
	if g.Outcome() != chess.WhiteWon {
		t.Fatalf("outcome lost after GoBack: %s", g.Outcome())
	}
	move := g.ValidMoves()[0]
	err := g.Move(move, nil)
	if !errors.Is(err, chess.ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
}

func TestAddVariationAllowedAfterTerminalOutcome(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	if !g.GoBack() {
		t.Fatal("GoBack failed")
	}
	variation := g.ValidMoves()[0]
	g.AddVariation(g.GetRootMove(), variation)
	if len(g.GetRootMove().Children()) == 0 {
		t.Error("AddVariation did not append child after terminal outcome")
	}
}

func TestClearOutcome(t *testing.T) {
	g := chess.NewGame()
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
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
	g.AddTagPair("Result", "1-0")
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	g.ClearOutcome()
	if got := g.GetTagPair("Result"); got != "1-0" {
		t.Errorf("Result tag after ClearOutcome = %q, want 1-0", got)
	}
}

func TestTagPairLifecycle(t *testing.T) {
	g := chess.NewGame()
	if g.AddTagPair("Event", "E1") {
		t.Error("first AddTagPair should return false (no prior value)")
	}
	if got := g.GetTagPair("Event"); got != "E1" {
		t.Errorf("GetTagPair = %q, want E1", got)
	}
	if !g.AddTagPair("Event", "E2") {
		t.Error("overwriting AddTagPair should return true")
	}
	if got := g.GetTagPair("Event"); got != "E2" {
		t.Errorf("GetTagPair after overwrite = %q, want E2", got)
	}
	if !g.RemoveTagPair("Event") {
		t.Error("RemoveTagPair on existing key should return true")
	}
	if got := g.GetTagPair("Event"); got != "" {
		t.Errorf("GetTagPair after remove = %q, want empty", got)
	}
	if g.RemoveTagPair("Event") {
		t.Error("RemoveTagPair on absent key should return false")
	}
	if g.RemoveTagPair("NeverExisted") {
		t.Error("RemoveTagPair on never-existing key should return false")
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
		if err := g.PushMove(mv, nil); err != nil {
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
