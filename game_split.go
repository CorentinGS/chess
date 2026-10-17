package chess

// Split takes a Game with a main line and 0 or more variations and returns a
// slice of Games (one for each variation), each containing exactly only a main
// line and 0 variations.
func (g *Game) Split() []*Game {
	// Build a Game for each path
	var games []*Game
	for _, path := range g.tree.Lines() {
		newG := g.buildOneGameFromPath(path)
		games = append(games, newG)
	}

	return games
}

func (g *Game) buildOneGameFromPath(path []*MoveNode) *Game {
	rootPos := g.tree.rootPos.copy()
	rootMove := &MoveNode{}
	cur := rootMove

	for _, m := range path {
		child := &MoveNode{
			move:          m.move,
			number:        m.number,
			nags:          append([]string(nil), m.nags...),
			commentBlocks: copyCommentBlocks(m.commentBlocks),
		}
		child.parent = cur

		cur.children = []*MoveNode{child}
		cur = child
	}

	newG := &Game{}
	newG.copy(g)
	// Build a fresh single-line tree; copy() skips tree. Seed rootPos from
	// the path's starting position and let setCurrent replay the move chain
	// so pos / undos / current are all consistent with the leaf cursor.
	// current is left on root so setCurrent's "node == current" early-return
	// does not skip the replay.
	newG.tree = &MoveTree{
		root:    rootMove,
		current: rootMove,
		rootPos: rootPos,
		pos:     rootPos.copy(),
	}
	newG.tree.setTree(rootMove)
	newG.tree.setCurrent(cur)

	// Discard any manual outcome inherited from the parent game and recompute
	// from the leaf using the Full policy (all automatic draws, honouring the
	// ignore flags). Split is the only path that syncs the Result tag, because
	// each split game is built from scratch and must carry a self-consistent
	// tag for PGN serialization.
	newG.outcome, newG.method = classifyOutcome(newG.currentPosition(), newG.numOfRepetitions(), fullOutcomeRules(newG))
	newG.syncResultTag()

	return newG
}

func (g *Game) syncResultTag() {
	if g.outcome == NoOutcome {
		delete(g.tagPairs, "Result")
		return
	}
	g.tagPairs["Result"] = g.outcome.String()
}
