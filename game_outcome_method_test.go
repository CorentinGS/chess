package chess_test

import (
	"errors"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestSetOutcomeMethodAcceptsValidPair(t *testing.T) {
	g := chess.NewGame()
	pair := chess.OutcomeMethodPair{Outcome: chess.WhiteWon, Method: chess.Checkmate}
	if err := g.SetOutcomeMethod(pair); err != nil {
		t.Fatalf("SetOutcomeMethod returned error: %v", err)
	}
	if g.Outcome() != chess.WhiteWon {
		t.Errorf("Outcome = %s, want WhiteWon", g.Outcome())
	}
	if g.Method() != chess.Checkmate {
		t.Errorf("Method = %s, want Checkmate", g.Method())
	}
}

func TestSetOutcomeMethodRejectsInvalidPair(t *testing.T) {
	g := chess.NewGame()
	pair := chess.OutcomeMethodPair{Outcome: chess.WhiteWon, Method: chess.Stalemate}
	err := g.SetOutcomeMethod(pair)
	if err == nil {
		t.Fatal("expected error for WhiteWon/Stalemate, got nil")
	}
	if g.Outcome() != chess.NoOutcome {
		t.Errorf("Outcome = %s, want NoOutcome (rejected)", g.Outcome())
	}
	if g.Method() != chess.NoMethod {
		t.Errorf("Method = %s, want NoMethod (rejected)", g.Method())
	}
}

func TestSetOutcomeMethodRejectsUnknownOutcome(t *testing.T) {
	g := chess.NewGame()
	pair := chess.OutcomeMethodPair{Outcome: chess.UnknownOutcome, Method: chess.NoMethod}
	err := g.SetOutcomeMethod(pair)
	if err == nil {
		t.Fatal("expected error for UnknownOutcome, got nil")
	}
}

func TestClearOutcomeResetsToNoOutcomeNoMethod(t *testing.T) {
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

func TestMoveRejectedAfterTerminalOutcome(t *testing.T) {
	g := chess.NewGame()
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	move := g.ValidMoves()[0]
	err := g.Move(move, nil)
	if err == nil {
		t.Fatal("expected error when moving after terminal outcome, got nil")
	}
	if !errors.Is(err, chess.ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
}

func TestResignAfterTerminalReturnsErrGameAlreadyEnded(t *testing.T) {
	g := chess.NewGame()
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	err := g.Resign(chess.White)
	if err == nil {
		t.Fatal("expected error when resigning after terminal outcome, got nil")
	}
	if !errors.Is(err, chess.ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
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
	if err == nil {
		t.Fatal("expected error when moving after GoBack from terminal, got nil")
	}
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

func TestDrawRejectedAfterTerminalOutcome(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(chess.OutcomeMethodPair{chess.WhiteWon, chess.Checkmate}); err != nil {
		t.Fatal(err)
	}
	err := g.Draw(chess.DrawOffer)
	if err == nil {
		t.Fatal("expected error when calling Draw after terminal, got nil")
	}
	if !errors.Is(err, chess.ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
	if g.Outcome() != chess.WhiteWon {
		t.Errorf("Draw overwrote terminal outcome: got %s, want %s", g.Outcome(), chess.WhiteWon)
	}
}
