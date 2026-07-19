package chess

import (
	"testing"
)

func mustSAN(pos *Position, san string) *Move {
	m, err := SAN().Decode(pos, san)
	if err != nil {
		panic(err)
	}
	return &m
}

func newMoveTreeTestGame() *Game {
	g := NewGame()
	g.PushMove("e4", nil)
	g.PushMove("e5", nil)
	g.PushMove("Nf3", nil)
	return g
}

func TestMoveTreePeekReturnsStartingPosition(t *testing.T) {
	g := NewGame()
	if got := g.tree.Peek(); got == nil {
		t.Fatal("Peek on nil-active game returned nil")
	}
	if got := g.tree.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("Peek = %q, want starting position", got)
	}
}

func TestMoveTreeGoForwardAdvances(t *testing.T) {
	g := newMoveTreeTestGame()
	g.tree.Reset()
	if !g.tree.GoForward() {
		t.Fatal("GoForward from root failed")
	}
	want := StartingPosition().Update(*mustSAN(StartingPosition(), "e4")).String()
	if got := g.tree.Peek().String(); got != want {
		t.Fatalf("after GoForward, Peek = %q, want post-e4 %q", got, want)
	}
}

func TestMoveTreeBackAndForwardRoundTrip(t *testing.T) {
	g := newMoveTreeTestGame()
	leaf := g.tree.Peek().String()

	for i := range 3 {
		if !g.tree.GoBack() {
			t.Fatalf("GoBack #%d failed", i)
		}
	}
	if g.tree.GoBack() {
		t.Fatal("GoBack from root should fail")
	}
	if got := g.tree.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("after 3 GoBacks, Peek = %q, want starting position", got)
	}

	for i := range 3 {
		if !g.tree.GoForward() {
			t.Fatalf("GoForward #%d failed", i)
		}
	}
	if got := g.tree.Peek().String(); got != leaf {
		t.Fatalf("round-trip Peek = %q, want %q", got, leaf)
	}
}

func TestMoveTreeForwardIndex(t *testing.T) {
	g := NewGame()
	root := g.tree.Root()
	a := &MoveNode{move: *mustSAN(StartingPosition(), "e4"), parent: root}
	b := &MoveNode{move: *mustSAN(StartingPosition(), "d4"), parent: root}
	cc := &MoveNode{move: *mustSAN(StartingPosition(), "Nf3"), parent: root}
	root.children = []*MoveNode{a, b, cc}

	g.tree.Reset()

	if g.tree.Forward(3) {
		t.Fatal("Forward(3) should fail (out of range)")
	}
	if g.tree.Forward(-1) {
		t.Fatal("Forward(-1) should fail")
	}
	if !g.tree.Forward(1) {
		t.Fatal("Forward(1) failed")
	}
	want := StartingPosition().Update(b.move).String()
	if got := g.tree.Peek().String(); got != want {
		t.Fatalf("after Forward(1), Peek = %q, want %q", got, want)
	}
}

func TestMoveTreeGotoDeepDescendant(t *testing.T) {
	g := newMoveTreeTestGame()
	leaf := g.tree.Current()
	want := g.tree.pos.String()
	g.tree.Reset()
	if !g.tree.Goto(leaf) {
		t.Fatal("Goto failed")
	}
	if got := g.tree.Peek().String(); got != want {
		t.Fatalf("Goto+Peek = %q, want %q", got, want)
	}
}

func TestMoveTreeResetReturnsToRoot(t *testing.T) {
	g := newMoveTreeTestGame()
	g.tree.Reset()
	if got := g.tree.Peek().String(); got != StartingPosition().String() {
		t.Fatalf("after Reset, Peek = %q, want starting position", got)
	}
}

func TestMoveTreePeekIsLive(t *testing.T) {
	g := newMoveTreeTestGame()
	g.tree.Reset()
	peeked := g.tree.Peek()
	if peeked != g.tree.Peek() {
		t.Fatal("Peek should alias the same pointer across reads with no nav")
	}
	g.tree.GoForward()
	if peeked.String() != g.tree.Peek().String() {
		t.Fatalf("Peek alias lost its live updates: aliased=%q fresh=%q", peeked.String(), g.tree.Peek().String())
	}
}

func TestMoveTreeGotoCurrentIsNoop(t *testing.T) {
	g := newMoveTreeTestGame()
	leaf := g.tree.Current()
	beforeUndos := len(g.tree.undos)
	if !g.tree.Goto(leaf) {
		t.Fatal("Goto(current) should succeed")
	}
	if len(g.tree.undos) != beforeUndos {
		t.Fatalf("Goto(current) mutated undo stack: %d -> %d", beforeUndos, len(g.tree.undos))
	}
}

func TestMoveTreeGotoRootViaLCA(t *testing.T) {
	g := newMoveTreeTestGame()
	g.tree.Reset()
	if !g.tree.Goto(g.tree.Root()) {
		t.Fatal("Goto(root) failed")
	}
	if len(g.tree.undos) != 0 {
		t.Fatalf("Goto(root) left %d undos, want 0", len(g.tree.undos))
	}
	if g.tree.Peek().String() != StartingPosition().String() {
		t.Fatal("Goto(root) did not reset position")
	}
}

func TestMoveTreeGotoSiblingSubtree(t *testing.T) {
	// Pin the LCA path through setCurrent: jump between sibling subtrees
	// from the same parent. The setCurrent slow path walks to the LCA
	// (the shared parent) and then advances along the target's chain.
	g := NewGame()
	root := g.tree.Root()

	// Build a 3-ply "e4 c5 Nf3" subtree and a 2-ply "d4 Nc6" sibling
	// subtree, both children of root.
	buildSubtree := func(sanChain []string) *MoveNode {
		parent := root
		var last *MoveNode
		for _, san := range sanChain {
			pos := parent.Position()
			if pos == nil {
				pos = StartingPosition()
			}
			child := &MoveNode{move: *mustSAN(pos, san), parent: parent, tree: g.tree}
			parent.children = append(parent.children, child)
			last = child
			parent = child
		}
		return last
	}

	aLeaf := buildSubtree([]string{"e4", "c5", "Nf3"})
	bLeaf := buildSubtree([]string{"d4", "Nc6"})

	if !g.tree.Goto(aLeaf) {
		t.Fatal("Goto(aLeaf) failed")
	}
	if len(g.tree.undos) != 3 {
		t.Fatalf("Goto(aLeaf) undos = %d, want 3", len(g.tree.undos))
	}

	if !g.tree.Goto(bLeaf) {
		t.Fatal("Goto(bLeaf) failed")
	}
	if len(g.tree.undos) != 2 {
		t.Fatalf("Goto(bLeaf) undos = %d, want 2 (LCA retreat then forward)", len(g.tree.undos))
	}
	if g.tree.Current() != bLeaf {
		t.Fatal("Goto(bLeaf) left cursor on wrong node")
	}
}

func TestMoveTreeGotoRejectsForeignTree(t *testing.T) {
	g1 := newMoveTreeTestGame()
	g2 := newMoveTreeTestGame()

	foreignNode := g2.tree.Current()
	before := g1.tree.Current()

	if g1.tree.Goto(foreignNode) {
		t.Fatal("Goto with foreign-tree node should return false")
	}
	if g1.tree.Current() != before {
		t.Fatal("Goto(foreign) mutated active cursor")
	}
}

func TestMoveTreeNilSafe(t *testing.T) {
	var t1 *MoveTree
	if t1.Peek() != nil {
		t.Fatal("Peek on nil tree should be nil")
	}
	if t1.Forward(0) {
		t.Fatal("Forward on nil tree should be false")
	}
	if t1.Goto(nil) {
		t.Fatal("Goto on nil tree should be false")
	}
	t1.Reset()

	g := NewGame()
	if g.tree.Goto(nil) {
		t.Fatal("Goto(nil) on live tree should be false")
	}
}

func TestMoveNodePositionRestoresOnPanic(t *testing.T) {
	// MoveNode.Position's save/restore is a bare `defer setCurrent(current)`;
	// defers always run, including on panic, by construction. This test
	// pins the broader contract: the cursor returned by Position() is
	// observationally pure (the read leaves the tree state unchanged) even
	// when the caller panics after the call returns.
	g := newMoveTreeTestGame()
	before := g.tree.Current()
	leaf := g.tree.Current()

	func() {
		defer func() {
			recover()
			if g.tree.Current() != before {
				t.Fatal("cursor moved after MoveNode.Position() returned")
			}
		}()
		_ = leaf.Position()
		panic("boom")
	}()
}

func TestAddVariationErrorRestoresCursor(t *testing.T) {
	g := newMoveTreeTestGame()
	root := g.tree.Root()
	root.children = nil
	before := g.tree.Current()
	bogusMove := Move{s1: A1, s2: H8, promo: NoPieceType}

	if _, err := g.tree.AddVariation(root, bogusMove); err == nil {
		t.Fatal("AddVariation should error on invalid move")
	}
	if g.tree.Current() != before {
		t.Fatal("AddVariation error left cursor moved")
	}
}

func TestParseVariationErrorRestoresCursor(t *testing.T) {
	// Drives parseVariation directly via NewParser (exported via
	// export_test.go). Token sequence: open variation, parse a legal move
	// that advances the cursor into the abandoned subtree, then trigger
	// AddNAG on an invalid NAG value. Pre-fix, parseVariation's err return
	// left the cursor on the abandoned subtree node. Post-fix, the defer
	// restores oldCurrent.
	parser := NewParser([]Token{
		{Type: VariationStart, Value: "("},
		{Type: SQUARE, Value: "e4"},
		{Type: NAG, Value: "!!!"}, // not in nagSymbolToNumeric and not "$N" — ParseNAG errors
		{Type: VariationEnd, Value: ")"},
	})

	before := parser.game.tree.Current()
	if err := parser.parseVariation(1, 1); err == nil {
		t.Fatal("expected parse error from invalid NAG")
	}
	after := parser.game.tree.Current()

	if before == nil || after == nil {
		t.Fatal("nil cursor before/after parseVariation")
	}
	if after != before {
		t.Fatalf("cursor moved: before=%p after=%p (after parent=%p)",
			before, after, after.parent)
	}
	if isInsideAbandonedVariation(parser.game.tree) {
		t.Fatal("cursor stranded inside abandoned variation subtree")
	}
}

// isInsideAbandonedVariation reports whether the tree's active cursor sits
// inside a non-mainline subtree (i.e. it is not reachable from the root
// via children[0] only).
func isInsideAbandonedVariation(t *MoveTree) bool {
	if t == nil || t.Root() == nil || t.current == nil {
		return false
	}
	if t.current == t.Root() {
		return false
	}
	node := t.current
	for node != nil && node != t.Root() {
		parent := node.parent
		if parent == nil {
			return false
		}
		idx := -1
		for i, c := range parent.children {
			if c == node {
				idx = i
				break
			}
		}
		if idx > 0 {
			return true
		}
		node = parent
	}
	return false
}

func TestMoveTreePeekAcrossReads(t *testing.T) {
	// Pinned by TestMoveTreePeekIsLive (same pointer across no-nav reads);
	// kept as a smoke test for the zero-state game.
	g := NewGame()
	if g.tree.Peek() != g.tree.Peek() {
		t.Fatal("Peek returned different pointers across two reads with no nav")
	}
}
