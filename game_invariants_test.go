package chess

import (
	"strings"
	"testing"
)

func assertGameCurrentPositionInvariant(t *testing.T, g *Game) {
	t.Helper()

	if g == nil {
		t.Fatal("game is nil")
	}
	if g.pos == nil {
		t.Fatal("game current position is nil")
	}
	if g.Position() == nil {
		t.Fatal("Position() is nil")
	}
	if g.CurrentPosition() == nil {
		t.Fatal("CurrentPosition() is nil")
	}
	if g.Position().String() != g.pos.String() {
		t.Fatalf("Position() = %q, want game current position %q", g.Position(), g.pos)
	}
	if g.CurrentPosition().String() != g.pos.String() {
		t.Fatalf("CurrentPosition() = %q, want game current position %q", g.CurrentPosition(), g.pos)
	}
	if g.currentMove != nil && g.currentMove.position != nil && g.currentMove.position.String() != g.pos.String() {
		t.Fatalf("current move position = %q, want game current position %q", g.currentMove.position, g.pos)
	}
}

func TestGameCurrentPositionInvariantAfterClonePreservesCursor(t *testing.T) {
	g := NewGame()
	for _, move := range []string{"e4", "e5", "Nf3"} {
		if err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
	}

	if !g.GoBack() {
		t.Fatal("expected to navigate back")
	}
	want := g.CurrentPosition().String()

	clone := g.Clone()

	assertGameCurrentPositionInvariant(t, clone)
	if got := clone.CurrentPosition().String(); got != want {
		t.Fatalf("clone current position = %q, want %q", got, want)
	}
}

func TestGameCurrentPositionInvariantAfterDirectGameOperations(t *testing.T) {
	g := NewGame()
	assertGameCurrentPositionInvariant(t, g)

	for _, move := range []string{"e4", "e5", "Nf3"} {
		if err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
		assertGameCurrentPositionInvariant(t, g)
	}

	if !g.GoBack() {
		t.Fatal("expected to navigate back")
	}
	assertGameCurrentPositionInvariant(t, g)

	if !g.GoForward() {
		t.Fatal("expected to navigate forward")
	}
	assertGameCurrentPositionInvariant(t, g)
}

func TestGameCurrentPositionInvariantAfterPGNParse(t *testing.T) {
	opt, err := PGN(strings.NewReader("1. e4 e5 2. Nf3 *"))
	if err != nil {
		t.Fatal(err)
	}

	g := NewGame(opt)

	assertGameCurrentPositionInvariant(t, g)
}

func TestGameCurrentPositionInvariantAfterSplitUsesLineLeaf(t *testing.T) {
	g := NewGame()
	for _, move := range []string{"e4", "e5", "Nf3"} {
		if err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
	}

	if !g.GoBack() {
		t.Fatal("expected to navigate back before adding a variation")
	}
	if err := g.PushMove("Nc3", nil); err != nil {
		t.Fatal(err)
	}

	splitGames := g.Split()
	if len(splitGames) != 2 {
		t.Fatalf("split game count = %d, want 2", len(splitGames))
	}
	for _, splitGame := range splitGames {
		assertGameCurrentPositionInvariant(t, splitGame)
		if !splitGame.IsAtEnd() {
			t.Fatalf("split game current position = %q, want leaf position", splitGame.CurrentPosition())
		}
	}
}
