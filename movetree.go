package chess

import (
	"cmp"
	"fmt"
)

// MoveInsertOptions contains options for inserting a move into a MoveTree.
type MoveInsertOptions struct {
	// PromoteToMainLine makes the inserted or selected continuation the main line.
	PromoteToMainLine bool
}

// MoveTree owns the move topology and active cursor for a game.
//
// The root is a synthetic position node, not a move occurrence.
type MoveTree struct {
	root    *MoveNode
	current *MoveNode
}

func newMoveTree(pos *Position) *MoveTree {
	root := &MoveNode{position: pos}
	return &MoveTree{root: root, current: root}
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
		return nil, fmt.Errorf("chess: move tree has no current position")
	}
	options = cmp.Or(options, &MoveInsertOptions{})

	if existing := t.findExistingMove(move); existing != nil {
		if options.PromoteToMainLine {
			t.promoteToMainLine(existing)
		}
		t.current = existing
		return existing, nil
	}

	node := &MoveNode{move: move, parent: t.current}
	if t.current.position != nil {
		node.position = t.current.position.Update(move)
	}
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

// AddVariation validates and appends move as a variation from parent.
func (t *MoveTree) AddVariation(parent *MoveNode, move Move) (*MoveNode, error) {
	if t == nil || t.root == nil {
		return nil, fmt.Errorf("chess: move tree has no root position")
	}
	if parent == nil {
		parent = t.root
	}
	if parent.position == nil {
		return nil, fmt.Errorf("chess: variation parent has no position")
	}
	if err := validatePositionMove(parent.position, move); err != nil {
		return nil, err
	}
	node := t.addVariationUnchecked(parent, move)
	return node, nil
}

func (t *MoveTree) addVariationUnchecked(parent *MoveNode, move Move) *MoveNode {
	if parent == nil {
		parent = t.root
	}
	node := &MoveNode{move: move, parent: parent}
	if parent != nil && parent.position != nil {
		node.position = parent.position.Update(move)
	}
	parent.children = append(parent.children, node)
	return node
}

// GoBack moves the active cursor to its parent.
func (t *MoveTree) GoBack() bool {
	if t == nil || t.current == nil || t.current.parent == nil {
		return false
	}
	t.current = t.current.parent
	return true
}

// GoForward moves the active cursor to the main continuation.
func (t *MoveTree) GoForward() bool {
	if t == nil || t.current == nil || len(t.current.children) == 0 {
		return false
	}
	t.current = t.current.children[0]
	return true
}

// NavigateToMainLine moves the active cursor to the first main-line move.
func (t *MoveTree) NavigateToMainLine() {
	if t == nil || t.root == nil {
		return
	}
	if len(t.root.children) == 0 {
		t.current = t.root
		return
	}
	t.current = t.root.children[0]
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
	ret := &MoveTree{root: t.root.clone()}
	if t.current == nil {
		ret.current = ret.root
	} else {
		ret.current = findClonedMove(t.root, ret.root, t.current)
		if ret.current == nil {
			ret.current = ret.root
		}
	}
	return ret
}

func (t *MoveTree) position() *Position {
	if t == nil || t.current == nil {
		return nil
	}
	return t.current.position
}

func (t *MoveTree) setRootPosition(pos *Position) {
	if t == nil || t.root == nil {
		return
	}
	t.root.position = pos
	if t.current == nil {
		t.current = t.root
	}
	if t.current == t.root {
		t.current.position = pos
	}
}

func (t *MoveTree) setCurrent(node *MoveNode) {
	if t != nil {
		t.current = node
	}
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
		return fmt.Errorf("no current position")
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
