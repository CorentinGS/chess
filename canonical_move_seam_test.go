package chess

import (
	"testing"
)

// TestMoveCanonicalizesStalenessForSpecialMove guards the original report
// friction: a legal Move with missing or false derived tags must surface as
// the canonical generated Move through the safe seam (resolveCanonicalMove),
// not as the caller-supplied value. Without this guard, a legal e1g1 without
// its KingSideCastle tag would advance the cursor while leaving the rook
// unmoved.
func TestMoveCanonicalizesStalenessForSpecialMove(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMoveText("e4", SAN(), nil); err != nil {
		t.Fatalf("setup e4: %v", err)
	}
	if _, err := g.PushMoveText("e5", SAN(), nil); err != nil {
		t.Fatalf("setup e5: %v", err)
	}
	if _, err := g.PushMoveText("Nf3", SAN(), nil); err != nil {
		t.Fatalf("setup Nf3: %v", err)
	}
	if _, err := g.PushMoveText("Nc6", SAN(), nil); err != nil {
		t.Fatalf("setup Nc6: %v", err)
	}
	if _, err := g.PushMoveText("Bc4", SAN(), nil); err != nil {
		t.Fatalf("setup Bc4: %v", err)
	}
	if _, err := g.PushMoveText("Bc5", SAN(), nil); err != nil {
		t.Fatalf("setup Bc5: %v", err)
	}

	goodCastle := Move{s1: E1, s2: G1, tags: 0}
	if _, err := g.Move(goodCastle, nil); err != nil {
		t.Fatalf("safe castle without KingSideCastle tag should be canonicalised: %v", err)
	}
	if !g.MoveTree().Current().Move().HasTag(KingSideCastle) {
		t.Fatalf("stored Move must carry the KingSideCastle tag after canonicalisation; got %v",
			g.MoveTree().Current().Move())
	}
	pos := g.Position()
	if pos.Board().Piece(F1) != WhiteRook || pos.Board().Piece(H1) != NoPiece {
		t.Fatalf("rook must have moved h1→f1 after canonicalised castle, got F1=%v H1=%v",
			pos.Board().Piece(F1), pos.Board().Piece(H1))
	}
}

// TestMoveRejectsNullPinnedByUnsafeContract preserves the existing user-visible
// contract: Game.Move rejects Null-tagged values. AddVariation normalises them
// instead — see TestAddVariationNormalisesNull.
func TestMoveRejectsNullPinnedByUnsafeContract(t *testing.T) {
	g := NewGame()
	if _, err := g.Move(NewNullMove(), nil); err == nil {
		t.Fatal("Game.Move must reject Null-tagged Move")
	}
}

// TestAddVariationNormalisesNull exercises the chosen policy: a Null-tagged
// Move passed to AddVariation is stored as the canonical Null representation
// regardless of stray origin, destination, or promotion.
func TestAddVariationNormalisesNull(t *testing.T) {
	g := NewGame()
	root := g.MoveTree().Root()
	spurious := Move{s1: E2, s2: E4, promo: Queen, tags: Null}
	node, err := g.MoveTree().AddVariation(root, spurious)
	if err != nil {
		t.Fatalf("AddVariation(NewNullMove form) error: %v", err)
	}
	stored := node.Move()
	if !stored.HasTag(Null) {
		t.Fatalf("stored Move must carry Null tag; got %v", stored)
	}
}

// TestSafeMoveRejectsRepairOnExistingSubtree pins the no-mutation contract:
// when an existing Move occurrence has continuations and stale tags, safe
// insertion refuses rather than risking an inconsistent subtree. The setup
// injects a stale-tagged MoveNode (alongside a child) directly into the
// MoveTree to demonstrate the guard; without the repair-or-reject check
// the safe path would either silently overwrite the stored Move (corrupting
// the subtree) or leave stale tags in place.
func TestSafeMoveRejectsRepairOnExistingSubtree(t *testing.T) {
	g := NewGame()
	tree := g.MoveTree()
	root := tree.Root()

	staleMove := Move{s1: E2, s2: E4, tags: Capture}
	continuation := Move{s1: E7, s2: E5, tags: Capture}
	staleNode := &MoveNode{move: staleMove, parent: root, tree: tree}
	childNode := &MoveNode{move: continuation, parent: staleNode, tree: tree}
	staleNode.children = append(staleNode.children, childNode)
	root.children = append(root.children, staleNode)

	beforeRootLen := len(root.children)
	beforeStaleChildren := len(staleNode.children)
	beforeStaleMove := staleNode.move
	beforeChildMove := childNode.move
	beforeUndoLen := len(tree.undos)
	beforeCurrent := tree.Current()

	_, err := g.Move(Move{s1: E2, s2: E4}, nil)
	if err == nil {
		t.Fatal("safe Move on a stale Move with continuations must be refused")
	}
	if err.Error() != "chess: cannot canonicalise existing Move occurrence with continuations" {
		t.Fatalf("unexpected rejection error: %v", err)
	}

	if len(root.children) != beforeRootLen {
		t.Fatalf("topology mutated on reject: root.children %d → %d",
			beforeRootLen, len(root.children))
	}
	if len(staleNode.children) != beforeStaleChildren {
		t.Fatalf("subtree mutated on reject: staleNode.children %d → %d",
			beforeStaleChildren, len(staleNode.children))
	}
	if staleNode.move != beforeStaleMove {
		t.Fatalf("stale Move mutated on reject: %v → %v", beforeStaleMove, staleNode.move)
	}
	if childNode.move != beforeChildMove {
		t.Fatalf("continuation mutated on reject: %v → %v", beforeChildMove, childNode.move)
	}
	if len(tree.undos) != beforeUndoLen {
		t.Fatalf("undo stack mutated on reject: %d → %d", beforeUndoLen, len(tree.undos))
	}
	if tree.Current() != beforeCurrent {
		t.Fatal("active cursor mutated on reject")
	}
}

