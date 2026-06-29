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
	rootMove := &MoveNode{position: g.tree.Root().position.copy()}
	cur := rootMove

	for _, m := range path {
		child := &MoveNode{
			move:          m.move,
			position:      m.position.copy(),
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
	newG.tree = &MoveTree{root: rootMove, current: cur}

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
	g.tagPairs["Result"] = string(g.outcome)
}
