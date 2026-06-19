package chess

import (
	"errors"
	"testing"
)

func TestSetOutcomeMethodAcceptsValidPair(t *testing.T) {
	g := NewGame()
	pair := OutcomeMethodPair{Outcome: WhiteWon, Method: Checkmate}
	if err := g.SetOutcomeMethod(pair); err != nil {
		t.Fatalf("SetOutcomeMethod returned error: %v", err)
	}
	if g.Outcome() != WhiteWon {
		t.Errorf("Outcome = %s, want WhiteWon", g.Outcome())
	}
	if g.Method() != Checkmate {
		t.Errorf("Method = %s, want Checkmate", g.Method())
	}
}

func TestSetOutcomeMethodRejectsInvalidPair(t *testing.T) {
	g := NewGame()
	pair := OutcomeMethodPair{Outcome: WhiteWon, Method: Stalemate}
	err := g.SetOutcomeMethod(pair)
	if err == nil {
		t.Fatal("expected error for WhiteWon/Stalemate, got nil")
	}
	if g.Outcome() != NoOutcome {
		t.Errorf("Outcome = %s, want NoOutcome (rejected)", g.Outcome())
	}
	if g.Method() != NoMethod {
		t.Errorf("Method = %s, want NoMethod (rejected)", g.Method())
	}
}

func TestSetOutcomeMethodRejectsUnknownOutcome(t *testing.T) {
	g := NewGame()
	pair := OutcomeMethodPair{Outcome: UnknownOutcome, Method: NoMethod}
	err := g.SetOutcomeMethod(pair)
	if err == nil {
		t.Fatal("expected error for UnknownOutcome, got nil")
	}
}

func TestClearOutcomeResetsToNoOutcomeNoMethod(t *testing.T) {
	g := NewGame()
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}
	g.ClearOutcome()
	if g.Outcome() != NoOutcome {
		t.Errorf("Outcome after Clear = %s, want NoOutcome", g.Outcome())
	}
	if g.Method() != NoMethod {
		t.Errorf("Method after Clear = %s, want NoMethod", g.Method())
	}
}

func TestClearOutcomeDoesNotModifyResultTag(t *testing.T) {
	g := NewGame()
	g.tagPairs["Result"] = "1-0"
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}

	g.ClearOutcome()

	if got := g.tagPairs["Result"]; got != "1-0" {
		t.Errorf("Result tag after ClearOutcome = %q, want 1-0", got)
	}
}

func TestMoveRejectedAfterTerminalOutcome(t *testing.T) {
	g := NewGame()
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}
	move := g.ValidMoves()[0]
	err := g.Move(move, nil)
	if err == nil {
		t.Fatal("expected error when moving after terminal outcome, got nil")
	}
	if !errors.Is(err, ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
}

func TestResignAfterTerminalReturnsErrGameAlreadyEnded(t *testing.T) {
	g := NewGame()
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}
	err := g.Resign(White)
	if err == nil {
		t.Fatal("expected error when resigning after terminal outcome, got nil")
	}
	if !errors.Is(err, ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
}

func TestResignWithNoColorReturnsError(t *testing.T) {
	g := NewGame()
	err := g.Resign(NoColor)
	if err == nil {
		t.Fatal("expected error when resigning with NoColor, got nil")
	}
	if g.Outcome() != NoOutcome {
		t.Errorf("Outcome = %s, want NoOutcome", g.Outcome())
	}
}

func TestMoveAfterGoBackFromTerminalStillRejected(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}
	if !g.GoBack() {
		t.Fatal("GoBack failed")
	}
	if g.outcome != WhiteWon {
		t.Fatalf("outcome lost after GoBack: %s", g.outcome)
	}
	move := g.ValidMoves()[0]
	err := g.Move(move, nil)
	if err == nil {
		t.Fatal("expected error when moving after GoBack from terminal, got nil")
	}
	if !errors.Is(err, ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
}

func TestAddVariationAllowedAfterTerminalOutcome(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
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
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.SetOutcomeMethod(OutcomeMethodPair{WhiteWon, Checkmate}); err != nil {
		t.Fatal(err)
	}
	err := g.Draw(DrawOffer)
	if err == nil {
		t.Fatal("expected error when calling Draw after terminal, got nil")
	}
	if !errors.Is(err, ErrGameAlreadyEnded) {
		t.Errorf("error = %v, want ErrGameAlreadyEnded", err)
	}
	if g.Outcome() != WhiteWon {
		t.Errorf("Draw overwrote terminal outcome: got %s, want %s", g.Outcome(), WhiteWon)
	}
}
