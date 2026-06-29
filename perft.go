package chess

// Perft (performance test) counts the number of legal move sequences of the
// given length reachable from the receiver. It is a standard correctness and
// performance test for chess move generators.
//
// A depth of 0 counts the current position itself. A depth of 1 counts the
// number of legal moves. Otherwise each legal move is applied in turn and the
// counts are summed recursively.
//
// Perft is purely a node counter; it does not collect, store, or annotate the
// move sequences. For a per-root-move breakdown use Divide.
//
// Example:
//
//	pos := chess.StartingPosition()
//	nodes := pos.Perft(5) // 4 865 609 for the start position
//
// The traversal uses the same legality rules that govern normal play, but it
// applies moves through an internal make/unmake path to avoid allocating a new
// Position at each ply. A position that is checkmate or stalemate returns 0 for
// any depth greater than 0 (no legal moves to count).
func (pos *Position) Perft(depth int) uint64 {
	if pos == nil {
		return 0
	}
	return pos.perftInPlace(depth)
}

func (pos *Position) perftInPlace(depth int) uint64 {
	if depth <= 0 {
		return 1
	}
	var nodes uint64
	visitLegalMoves(pos, generateLegalOnly, func(m Move) bool {
		if depth == 1 {
			nodes++
			return false
		}
		undo := pos.makeMove(m)
		nodes += pos.perftInPlace(depth - 1)
		pos.unmakeMove(undo)
		return false
	})
	return nodes
}

// Divide returns the per-root-move Perft breakdown at the given depth: a map
// from each legal move in the current position to the number of leaf positions
// reachable in exactly depth-1 further plies after that move. It is the
// per-move form of Perft and is typically used to localize a divergence
// between two move generators.
//
// Like Perft, a depth of 0 returns a single-entry map for the current position
// (the move is the zero Move, count 1). A position with no legal moves returns
// an empty map for any depth greater than 0.
//
// The map iteration order is unspecified; sort by key if a stable display is
// required.
//
// Example:
//
//	pos := chess.StartingPosition()
//	results := pos.Divide(1)
//	for _, m := range sortedMoves(results) {
//	    fmt.Printf("%s: %d\n", m, results[m])
//	}
func (pos *Position) Divide(depth int) map[Move]uint64 {
	if pos == nil {
		return nil
	}
	if depth <= 0 {
		return map[Move]uint64{Move{}: 1}
	}
	out := make(map[Move]uint64)
	visitLegalMoves(pos, generateLegalOnly, func(m Move) bool {
		if depth == 1 {
			out[m] = 1
			return false
		}
		undo := pos.makeMove(m)
		out[m] = pos.perftInPlace(depth - 1)
		pos.unmakeMove(undo)
		return false
	})
	return out
}

type positionUndo struct {
	board           Board
	castleRights    CastleRights
	validMoves      []Move
	halfMoveClock   int
	moveCount       int
	turn            Color
	enPassantSquare Square
	inCheck         bool
	hash            uint64
	status          Method
	statusCached    bool
}

func (pos *Position) makeMove(m Move) positionUndo {
	undo := positionUndo{
		board:           pos.board,
		castleRights:    pos.castleRights,
		validMoves:      pos.validMoves,
		halfMoveClock:   pos.halfMoveClock,
		moveCount:       pos.moveCount,
		turn:            pos.turn,
		enPassantSquare: pos.enPassantSquare,
		inCheck:         pos.inCheck,
		hash:            pos.hash,
		status:          pos.status,
		statusCached:    pos.statusCached,
	}

	if m.HasTag(Null) {
		next := pos.nullUpdate()
		*pos = *next
		return undo
	}

	pos.applyMove(m)
	// Perft and Divide never inspect intermediate hashes. The undo record
	// restores the caller-visible hash exactly after traversal, so applyMove
	// leaving the hash untouched is safe here.
	return undo
}

func (pos *Position) unmakeMove(undo positionUndo) {
	pos.board = undo.board
	pos.castleRights = undo.castleRights
	pos.validMoves = undo.validMoves
	pos.halfMoveClock = undo.halfMoveClock
	pos.moveCount = undo.moveCount
	pos.turn = undo.turn
	pos.enPassantSquare = undo.enPassantSquare
	pos.inCheck = undo.inCheck
	pos.hash = undo.hash
	pos.status = undo.status
	pos.statusCached = undo.statusCached
}
