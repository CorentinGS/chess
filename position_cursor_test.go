package chess

import (
	"testing"
)

func newCursorGame() *Game {
	g := NewGame()
	// Main line: 1. e4 e5 2. Nf3
	g.PushMove("e4", nil)
	g.PushMove("e5", nil)
	g.PushMove("Nf3", nil)
	return g
}

func TestPositionCursorPeekReturnsStartingPosition(t *testing.T) {
	g := NewGame()
	c := g.tree.Cursor()
	if c == nil {
		t.Fatal("nil cursor")
	}
	peek := c.Peek()
	if peek == nil {
		t.Fatal("nil peek")
	}
	if got := peek.String(); got != StartingPosition().String() {
		t.Fatalf("Peek = %q, want starting position", got)
	}
}

func TestPositionCursorForwardMainAdvances(t *testing.T) {
	g := newCursorGame()
	c := g.tree.Cursor()
	c.Reset()
	if !c.ForwardMain() {
		t.Fatal("ForwardMain from root failed")
	}
	want := StartingPosition().Update(*mustSAN(StartingPosition(), "e4")).String()
	if got := c.Peek().String(); got != want {
		t.Fatalf("after ForwardMain, Peek = %q, want post-e4 %q", got, want)
	}
}

func TestPositionCursorBackAndForwardRoundTrip(t *testing.T) {
	g := newCursorGame()
	c := g.tree.Cursor()
	leaf := c.Peek().String()

	if !c.Back() {
		t.Fatal("Back failed")
	}
	if !c.Back() {
		t.Fatal("Back failed")
	}
	if !c.Back() {
		t.Fatal("Back failed")
	}
	if c.Back() {
		t.Fatal("Back from root should fail")
	}
	if got := c.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("after 3 Backs, Peek = %q, want starting position", got)
	}

	if !c.ForwardMain() {
		t.Fatal("ForwardMain 1 failed")
	}
	if !c.ForwardMain() {
		t.Fatal("ForwardMain 2 failed")
	}
	if !c.ForwardMain() {
		t.Fatal("ForwardMain 3 failed")
	}
	if got := c.Peek().String(); got != leaf {
		t.Fatalf("round-trip Peek = %q, want %q", got, leaf)
	}
}

func TestPositionCursorForwardIndex(t *testing.T) {
	g := NewGame()
	root := g.tree.Root()
	a := &MoveNode{move: *mustSAN(StartingPosition(), "e4"), parent: root}
	b := &MoveNode{move: *mustSAN(StartingPosition(), "d4"), parent: root}
	cc := &MoveNode{move: *mustSAN(StartingPosition(), "Nf3"), parent: root}
	root.children = []*MoveNode{a, b, cc}

	c := g.tree.Cursor()
	c.Reset()

	if c.Forward(3) {
		t.Fatal("Forward(3) should fail (out of range)")
	}
	if c.Forward(-1) {
		t.Fatal("Forward(-1) should fail")
	}
	if !c.Forward(1) {
		t.Fatal("Forward(1) failed")
	}
	want := StartingPosition().Update(b.move).String()
	if got := c.Peek().String(); got != want {
		t.Fatalf("after Forward(1), Peek = %q, want %q", got, want)
	}
}

func TestPositionCursorGotoDeepDescendant(t *testing.T) {
	g := newCursorGame()
	leaf := g.tree.Current()    // deepest node after 3 main-line moves
	want := g.tree.pos.String() // capture leaf position BEFORE reset
	c := g.tree.Cursor()
	c.Reset()
	if !c.Goto(leaf) {
		t.Fatal("Goto failed")
	}
	if got := c.Peek().String(); got != want {
		t.Fatalf("Goto+Peek = %q, want %q", got, want)
	}
}

func TestPositionCursorResetReturnsToRoot(t *testing.T) {
	g := newCursorGame()
	c := g.tree.Cursor()
	c.Reset()
	if got := c.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("after Reset, Peek = %q, want starting position", got)
	}
}

func TestPositionCursorPositionReturnsDefensiveCopy(t *testing.T) {
	g := newCursorGame()
	c := g.tree.Cursor()
	p1 := c.Position()
	p2 := c.Position()
	if p1 == p2 {
		t.Fatal("Position() should return a fresh copy each call")
	}
	p1.halfMoveClock = 999
	if c.Peek().halfMoveClock == 999 {
		t.Fatal("Position() mutation leaked into cursor")
	}
}

func TestPositionCursorMultipleCursorsShareState(t *testing.T) {
	g := newCursorGame()
	c1 := g.tree.Cursor()
	c2 := g.tree.Cursor()

	want := c1.Peek().String()
	if got := c2.Peek().String(); got != want {
		t.Fatalf("c2.Peek = %q, want %q (c1.Peek)", got, want)
	}

	c1.Reset()
	if got := c2.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("after c1.Reset, c2.Peek = %q, want starting position", got)
	}
}

func TestPositionCursorNilSafe(t *testing.T) {
	var c *PositionCursor
	if c.Peek() != nil {
		t.Fatal("nil cursor Peek should return nil")
	}
	if c.Position() != nil {
		t.Fatal("nil cursor Position should return nil")
	}
	if c.ForwardMain() {
		t.Fatal("nil cursor ForwardMain should return false")
	}
	if c.Forward(0) {
		t.Fatal("nil cursor Forward should return false")
	}
	if c.Back() {
		t.Fatal("nil cursor Back should return false")
	}
	if c.Goto(nil) {
		t.Fatal("nil cursor Goto should return false")
	}
	c.Reset()
}

func mustSAN(pos *Position, san string) *Move {
	m, err := SAN().Decode(pos, san)
	if err != nil {
		panic(err)
	}
	return &m
}
