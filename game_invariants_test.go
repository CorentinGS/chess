package chess

import (
	"strings"
	"testing"
)

func assertGameTreeCurrentPositionInvariant(t *testing.T, g *Game) {
	t.Helper()

	if g == nil {
		t.Fatal("game is nil")
	}
	if g.currentPosition() == nil {
		t.Fatal("tree current position is nil")
	}
	if g.Position() == nil {
		t.Fatal("Position() is nil")
	}
	if g.MoveTree().Current().Position() == nil {
		t.Fatal("MoveTree().Current().Position() is nil")
	}
	if g.Position().String() != g.currentPosition().String() {
		t.Fatalf("Position() = %q, want tree current position %q", g.Position(), g.currentPosition())
	}
	if g.MoveTree().Current().Position().String() != g.currentPosition().String() {
		t.Fatalf("MoveTree().Current().Position() = %q, want tree current position %q", g.MoveTree().Current().Position(), g.currentPosition())
	}
	if g.MoveTree().Current() != nil && g.MoveTree().Current().position != nil && g.MoveTree().Current().position.String() != g.currentPosition().String() {
		t.Fatalf("current move position = %q, want tree current position %q", g.MoveTree().Current().position, g.currentPosition())
	}
}

func TestGameTreeCurrentPositionInvariantAfterClonePreservesCursor(t *testing.T) {
	g := NewGame()
	for _, move := range []string{"e4", "e5", "Nf3"} {
		if _, err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
	}

	if !g.MoveTree().GoBack() {
		t.Fatal("expected to navigate back")
	}
	want := g.MoveTree().Current().Position().String()

	clone := g.Clone()

	assertGameTreeCurrentPositionInvariant(t, clone)
	if got := clone.MoveTree().Current().Position().String(); got != want {
		t.Fatalf("clone current position = %q, want %q", got, want)
	}
}

func TestGameTreeCurrentPositionInvariantAfterDirectGameOperations(t *testing.T) {
	g := NewGame()
	assertGameTreeCurrentPositionInvariant(t, g)

	for _, move := range []string{"e4", "e5", "Nf3"} {
		if _, err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
		assertGameTreeCurrentPositionInvariant(t, g)
	}

	if !g.MoveTree().GoBack() {
		t.Fatal("expected to navigate back")
	}
	assertGameTreeCurrentPositionInvariant(t, g)

	if !g.MoveTree().GoForward() {
		t.Fatal("expected to navigate forward")
	}
	assertGameTreeCurrentPositionInvariant(t, g)
}

func TestPositionReturnsDefensiveCopyAfterGoForward(t *testing.T) {
	g := NewGame()
	for _, move := range []string{"e4", "e5"} {
		if _, err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
	}

	if !g.MoveTree().GoBack() {
		t.Fatal("expected to navigate back")
	}
	if !g.MoveTree().GoForward() {
		t.Fatal("expected to navigate forward")
	}
	if g.Position() == g.MoveTree().Current().position {
		t.Fatal("Position shared current position pointer with move node")
	}
	assertGameTreeCurrentPositionInvariant(t, g)
}

func TestAddVariationStoresPosition(t *testing.T) {
	g := NewGame()
	move, err := algebraicNotation{}.Decode(g.Position(), "e4")
	if err != nil {
		t.Fatal(err)
	}

	if _, err := g.MoveTree().AddVariation(nil, move); err != nil {
		t.Fatal(err)
	}
	children := g.MoveTree().Root().Children()
	if len(children) != 1 {
		t.Fatalf("root children = %d, want 1", len(children))
	}
	if children[0].Position() == nil {
		t.Fatal("variation position is nil")
	}
	if got, want := children[0].Position().String(), g.Position().Update(move).String(); got != want {
		t.Fatalf("variation position = %q, want %q", got, want)
	}
	if !g.MoveTree().GoForward() {
		t.Fatal("expected to navigate into variation")
	}
	assertGameTreeCurrentPositionInvariant(t, g)
}

func TestGameTreeCurrentPositionInvariantAfterPGNParse(t *testing.T) {
	opt, err := PGN(strings.NewReader("1. e4 e5 2. Nf3 *"))
	if err != nil {
		t.Fatal(err)
	}

	g := NewGame(opt)

	assertGameTreeCurrentPositionInvariant(t, g)
}

func TestGameTreeCurrentPositionInvariantAfterSplitUsesLineLeaf(t *testing.T) {
	g := NewGame()
	for _, move := range []string{"e4", "e5", "Nf3"} {
		if _, err := g.PushMove(move, nil); err != nil {
			t.Fatal(err)
		}
	}

	if !g.MoveTree().GoBack() {
		t.Fatal("expected to navigate back before adding a variation")
	}
	if _, err := g.PushMove("Nc3", nil); err != nil {
		t.Fatal(err)
	}

	splitGames := g.Split()
	if len(splitGames) != 2 {
		t.Fatalf("split game count = %d, want 2", len(splitGames))
	}
	for _, splitGame := range splitGames {
		assertGameTreeCurrentPositionInvariant(t, splitGame)
		if !splitGame.IsAtEnd() {
			t.Fatalf("split tree current position = %q, want leaf position", splitGame.MoveTree().Current().Position())
		}
	}
}
