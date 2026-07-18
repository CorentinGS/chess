package chess

// PositionCursor is a read-mostly handle onto a [MoveTree]'s active position.
//
// The cursor does not own a separate position: it delegates every navigation
// to the tree, which advances its internal in-place [Position] via
// makeMove / unmakeMove (see perft.go). Multiple cursors on the same tree
// share state — calls to [PositionCursor.ForwardMain] on one cursor are
// visible to every other cursor rooted at that tree, mirroring the
// single-active-cursor invariant in ADR-016.
//
// Read paths:
//
//   - [PositionCursor.Peek] returns the live [Position] pointer for zero-copy
//     reads. Callers MUST NOT mutate the returned position; doing so corrupts
//     the cursor and the tree.
//   - [PositionCursor.Position] returns a defensive copy for callers that
//     need a value they can mutate. This is the same semantics as
//     [Game.Position] before the lazy-position refactor.
type PositionCursor struct {
	tree *MoveTree
}

// Cursor returns a fresh [PositionCursor] pinned at the tree's current node.
// The cursor shares state with the tree — navigation methods on either are
// reflected in the other.
func (t *MoveTree) Cursor() *PositionCursor {
	if t == nil {
		return nil
	}
	return &PositionCursor{tree: t}
}

// Peek returns the cursor's current [Position] without copying. The returned
// pointer aliases the tree's internal position state; callers MUST NOT mutate
// it. Use [PositionCursor.Position] when a mutable value is required.
//
// Returns nil if the cursor or its tree is nil.
func (c *PositionCursor) Peek() *Position {
	if c == nil || c.tree == nil {
		return nil
	}
	return c.tree.pos
}

// Position returns a defensive copy of the cursor's current [Position]. The
// caller may mutate the returned value freely; the cursor is unaffected.
//
// Returns nil if the cursor or its tree is nil.
func (c *PositionCursor) Position() *Position {
	if c == nil || c.tree == nil || c.tree.pos == nil {
		return nil
	}
	return c.tree.pos.copy()
}

// ForwardMain advances the cursor to the main-line continuation
// (children[0]) of the current node. Returns true on success; false if the
// cursor is nil, the current node has no children, or the underlying tree
// rejected the advance.
func (c *PositionCursor) ForwardMain() bool {
	if c == nil || c.tree == nil {
		return false
	}
	return c.tree.GoForward()
}

// Forward advances the cursor to the idx-th child of the current node.
// Forward(0) is equivalent to [PositionCursor.ForwardMain]. Returns true on
// success; false if the cursor is nil, idx is out of range, or the advance
// otherwise failed.
func (c *PositionCursor) Forward(idx int) bool {
	if c == nil || c.tree == nil || c.tree.current == nil {
		return false
	}
	if idx < 0 || idx >= len(c.tree.current.children) {
		return false
	}
	// Route through setCurrent so the direct-child fast path owns the
	// makeMove/undo append invariant. Forward(0) then matches ForwardMain.
	c.tree.setCurrent(c.tree.current.children[idx])
	return true
}

// Back retreats the cursor to the current node's parent. Returns true on
// success; false if the cursor is nil or already at the synthetic root.
func (c *PositionCursor) Back() bool {
	if c == nil || c.tree == nil {
		return false
	}
	return c.tree.GoBack()
}

// Goto jumps the cursor to the given node. The node MUST belong to the same
// tree as the cursor; passing a node from another tree produces undefined
// behavior. Goto uses the same fast paths as internal cursor navigation:
// direct-child, strict-ancestor, and full replay-from-root.
//
// Returns true if the cursor moved; false if the cursor is nil or node is
// nil.
func (c *PositionCursor) Goto(node *MoveNode) bool {
	if c == nil || c.tree == nil || node == nil {
		return false
	}
	c.tree.setCurrent(node)
	return true
}

// Reset returns the cursor to the synthetic root, restoring the starting
// position. Safe to call on a nil cursor.
func (c *PositionCursor) Reset() {
	if c == nil || c.tree == nil {
		return
	}
	c.tree.resetCursor()
}
