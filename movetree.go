package chess

import (
	"cmp"
	"errors"
	"fmt"
)

// MoveInsertOptions contains options for inserting a move into a MoveTree.
type MoveInsertOptions struct {
	// PromoteToMainLine makes the inserted or selected continuation the main line.
	PromoteToMainLine bool
}

// MoveTree owns the move topology and active cursor for a game.
//
// The root is a synthetic position node, not a move occurrence. The cursor's
// current position is tracked in pos alongside current: every navigation
// (addMove, GoForward, GoBack, setCurrent) advances pos via the in-place
// makeMoveCursor / unmakeMoveCursor pair (move_applier.go), with one entry
// in undos per level between root and current. rootPos holds the starting
// position so the cursor can be reset without re-deriving it from the
// synthetic root MoveNode.
type MoveTree struct {
	root    *MoveNode
	current *MoveNode
	rootPos *Position
	pos     *Position
	undos   []cursorUndo
}

func newMoveTree(pos *Position) *MoveTree {
	root := &MoveNode{}
	t := &MoveTree{
		root:    root,
		current: root,
		rootPos: pos,
		pos:     pos.copy(),
		undos:   nil,
	}
	t.setTree(root)
	return t
}

// setTree recursively patches n.tree = t on every node reachable from n. Used
// after Clone / Split where clone() copies topology but not the tree pointer.
func (t *MoveTree) setTree(n *MoveNode) {
	if n == nil {
		return
	}
	n.tree = t
	for _, c := range n.children {
		t.setTree(c)
	}
}

// Root returns the root position node.
func (t *MoveTree) Root() *MoveNode {
	if t == nil {
		return nil
	}
	return t.root
}

// Current returns the active cursor node.
func (t *MoveTree) Current() *MoveNode {
	if t == nil {
		return nil
	}
	return t.current
}

// MainLine returns the main-line move nodes, excluding the root position node.
func (t *MoveTree) MainLine() []*MoveNode {
	if t == nil || t.root == nil {
		return nil
	}

	nodes := make([]*MoveNode, 0)
	for current := t.MainChild(t.root); current != nil; current = t.MainChild(current) {
		nodes = append(nodes, current)
	}
	return nodes
}

// MainChild returns the main-line continuation from parent.
func (t *MoveTree) MainChild(parent *MoveNode) *MoveNode {
	if parent == nil || len(parent.children) == 0 {
		return nil
	}
	return parent.children[0]
}

// Continuations returns all continuations from parent.
func (t *MoveTree) Continuations(parent *MoveNode) []*MoveNode {
	if parent == nil {
		return nil
	}
	return append([]*MoveNode{}, parent.children...)
}

// Variations returns all non-main-line continuations from parent.
func (t *MoveTree) Variations(parent *MoveNode) []*MoveNode {
	if parent == nil || len(parent.children) <= 1 {
		return nil
	}
	return append([]*MoveNode{}, parent.children[1:]...)
}

func (t *MoveTree) addMove(move Move, options *MoveInsertOptions) (*MoveNode, error) {
	if t == nil || t.current == nil {
		return nil, errors.New("chess: move tree has no current position")
	}
	options = cmp.Or(options, &MoveInsertOptions{})

	if existing := t.findExistingMove(move); existing != nil {
		if options.PromoteToMainLine {
			t.promoteToMainLine(existing)
		}
		// Advance the cursor in place rather than re-deriving the post-move
		// position from existing.position. The undo record keeps undos in
		// sync with the depth between root and current.
		t.undos = append(t.undos, t.pos.makeMoveCursor(existing.move))
		t.current = existing
		return existing, nil
	}

	node := &MoveNode{move: move, parent: t.current, tree: t}
	// Advance the cursor in place; t.pos is now post-move. The node carries
	// no per-node position — readers route through the cursor via
	// MoveNode.Position() (ADR-016).
	t.undos = append(t.undos, t.pos.makeMoveCursor(move))

	if options.PromoteToMainLine {
		t.current.children = append(t.current.children, nil)
		copy(t.current.children[1:], t.current.children[:len(t.current.children)-1])
		t.current.children[0] = node
	} else {
		t.current.children = append(t.current.children, node)
	}
	t.current = node
	return node, nil
}

// AddVariation validates and appends move as a variation from parent. The
// cursor is left on parent for the duration of the validation lookup; callers
// that need to preserve cursor state should snapshot and restore via
// [PositionCursor.Goto] before/after.
func (t *MoveTree) AddVariation(parent *MoveNode, move Move) (*MoveNode, error) {
	if t == nil || t.root == nil {
		return nil, errors.New("chess: move tree has no root position")
	}
	if parent == nil {
		parent = t.root
	}
	// Resolve parent's position via the cursor — variations attach to an
	// arbitrary node, not the cursor's current node, so we navigate. Save
	// and restore the cursor so the caller's active position is unchanged.
	defer t.setCurrent(t.current)
	c := t.Cursor()
	if !c.Goto(parent) {
		return nil, errors.New("chess: variation parent is not reachable from cursor")
	}
	if c.Peek() == nil {
		return nil, errors.New("chess: variation parent has no position")
	}
	if err := validatePositionMove(c.Peek(), move); err != nil {
		return nil, err
	}
	node := t.addVariationUnchecked(parent, move)
	return node, nil
}

func (t *MoveTree) addVariationUnchecked(parent *MoveNode, move Move) *MoveNode {
	if parent == nil {
		parent = t.root
	}
	node := &MoveNode{move: move, parent: parent, tree: t}
	parent.children = append(parent.children, node)
	return node
}

// GoBack moves the active cursor to its parent.
func (t *MoveTree) GoBack() bool {
	if t == nil || t.current == nil || t.current.parent == nil {
		return false
	}
	if len(t.undos) == 0 {
		// Cursor invariant broken; refuse to advance rather than panic.
		return false
	}
	undo := t.undos[len(t.undos)-1]
	t.undos = t.undos[:len(t.undos)-1]
	// The undo at the top of the stack reverses the move that led to
	// current; read it before current retreats.
	t.pos.unmakeMoveCursor(t.current.move, undo)
	t.current = t.current.parent
	return true
}

// GoForward moves the active cursor to the main continuation.
func (t *MoveTree) GoForward() bool {
	if t == nil || t.current == nil || len(t.current.children) == 0 {
		return false
	}
	child := t.current.children[0]
	t.undos = append(t.undos, t.pos.makeMoveCursor(child.move))
	t.current = child
	return true
}

// NavigateToMainLine moves the active cursor to the first main-line move.
func (t *MoveTree) NavigateToMainLine() {
	if t == nil || t.root == nil {
		return
	}
	t.resetCursor()
	if len(t.root.children) == 0 {
		return
	}
	t.GoForward()
}

// Lines returns every root-to-leaf line in the tree.
func (t *MoveTree) Lines() [][]*MoveNode {
	if t == nil || t.root == nil {
		return nil
	}
	var paths [][]*MoveNode
	for _, child := range t.root.children {
		paths = append(paths, collectPaths(child)...)
	}
	return paths
}

// Clone returns a deep copy of the move tree with the cursor preserved.
func (t *MoveTree) Clone() *MoveTree {
	if t == nil || t.root == nil {
		return nil
	}
	ret := &MoveTree{
		root:    t.root.clone(),
		rootPos: t.rootPos.copy(),
	}
	// Patch tree back-pointers on the freshly cloned topology.
	ret.setTree(ret.root)
	ret.pos = ret.rootPos.copy()
	ret.undos = nil
	ret.current = ret.root
	// Find the cloned equivalent of the original cursor and replay forward
	// so pos / undos match the new current.
	var target *MoveNode
	if t.current == nil || t.current == t.root {
		target = ret.root
	} else {
		target = findClonedMove(t.root, ret.root, t.current)
		if target == nil {
			target = ret.root
		}
	}
	ret.setCurrent(target)
	return ret
}

func (t *MoveTree) position() *Position {
	if t == nil || t.pos == nil {
		return nil
	}
	return t.pos
}

func (t *MoveTree) setRootPosition(pos *Position) {
	if t == nil || t.root == nil {
		return
	}
	t.rootPos = pos
	t.resetCursor()
}

// resetCursor rewinds pos to rootPos and clears the undo stack. current lands
// on the synthetic root, ready for addMove / GoForward to push forward again.
func (t *MoveTree) resetCursor() {
	if t == nil {
		return
	}
	t.pos = t.rootPos.copy()
	t.undos = t.undos[:0]
	t.current = t.root
}

func (t *MoveTree) setCurrent(node *MoveNode) {
	if t == nil {
		return
	}
	if node == nil {
		node = t.root
	}
	if node == t.current {
		return
	}
	// Direct child of current: advance one step. This is the hot path for
	// linear parsing (PGN addMove, GoForward after a findExisting).
	if node.parent == t.current {
		t.undos = append(t.undos, t.pos.makeMoveCursor(node.move))
		t.current = node
		return
	}
	// Anything else — ancestor, sibling subtree, root: retreat to the lowest
	// common ancestor by popping undos, then advance along node's chain.
	// This is the variation-entry/exit hot path in PGN parsing; the LCA turns
	// an O(root-depth) reset+replay (with a rootPos.copy allocation) into an
	// O(distance) walk with no allocation.
	ancestor := lcaNode(t.current, node)
	for t.current != ancestor {
		if !t.GoBack() {
			break
		}
	}
	t.replayToFrom(node, ancestor)
	t.current = node
}

// replayToFrom advances pos from stop's position to node's, pushing one undo
// per move along the stop-to-node chain. Recursive so the chain stays on the
// call stack (no heap alloc per variation).
func (t *MoveTree) replayToFrom(node, stop *MoveNode) {
	if node == nil || node == stop {
		return
	}
	t.replayToFrom(node.parent, stop)
	t.undos = append(t.undos, t.pos.makeMoveCursor(node.move))
}

// nodeDepth returns the number of moves between the synthetic root and n.
func nodeDepth(n *MoveNode) int {
	d := 0
	for cur := n; cur != nil && cur.parent != nil; cur = cur.parent {
		d++
	}
	return d
}

// lcaNode returns the lowest common ancestor of a and b — the root at worst
// for same-tree nodes (the only kind setCurrent mixes, per ADR-016).
// Pointer-walk only: no position mutation, no allocation.
func lcaNode(a, b *MoveNode) *MoveNode {
	da, db := nodeDepth(a), nodeDepth(b)
	for da > db {
		a = a.parent
		da--
	}
	for db > da {
		b = b.parent
		db--
	}
	for a != nil && b != nil && a != b {
		a = a.parent
		b = b.parent
	}
	return a
}

func (t *MoveTree) findExistingMove(move Move) *MoveNode {
	if t == nil || t.current == nil {
		return nil
	}
	for _, child := range t.current.children {
		if sameMove(child.move, move) {
			return child
		}
	}
	return nil
}

func (t *MoveTree) promoteToMainLine(move *MoveNode) {
	if move == nil || move.parent == nil {
		return
	}
	children := move.parent.children
	for i, child := range children {
		if child == move {
			for ; i > 0; i-- {
				children[i] = children[i-1]
			}
			children[0] = move
			return
		}
	}
}

func sameMove(a, b Move) bool {
	return a.s1 == b.s1 && a.s2 == b.s2 && a.promo == b.promo
}

func validatePositionMove(pos *Position, move Move) error {
	if pos == nil {
		return errors.New("no current position")
	}
	if move.HasTag(Null) {
		return nil
	}
	for _, validMove := range pos.ValidMovesUnsafe() {
		if sameMove(validMove, move) {
			return nil
		}
	}
	return fmt.Errorf("move %s is not valid for the current position", move.String())
}

// collectPaths returns all paths from the given move to each leaf node.
// Each path is represented as a slice of *MoveNode, starting with the given node
// and ending with a leaf (a move with no children).
func collectPaths(node *MoveNode) [][]*MoveNode {
	if node == nil {
		return nil
	}
	// If leaf, return a single path containing this node
	if len(node.children) == 0 {
		return [][]*MoveNode{{node}}
	}
	// Otherwise, collect paths from each child and prepend this node
	var paths [][]*MoveNode
	for _, c := range node.children {
		childPaths := collectPaths(c)
		for _, p := range childPaths {
			path := append([]*MoveNode{node}, p...)
			paths = append(paths, path)
		}
	}
	return paths
}

func findClonedMove(original, clone, target *MoveNode) *MoveNode {
	if original == nil || clone == nil || target == nil {
		return nil
	}
	if original == target {
		return clone
	}
	for i, child := range original.children {
		if i >= len(clone.children) {
			return nil
		}
		if found := findClonedMove(child, clone.children[i], target); found != nil {
			return found
		}
	}
	return nil
}
