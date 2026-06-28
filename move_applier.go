package chess

// This file is the single source of truth for "what changes about a Position
// when a non-null Move is played": the incremental bookkeeping rule
// (moveCount, halfMoveClock, castleRights, enPassant, board, turn, inCheck) and
// the primitives it shares. The copy-on-write applier (Position.Update) and the
// in-place applier (Position.makeMove) both delegate here to applyMove, so the
// rule cannot drift between them.
//
// The Zobrist hash is deliberately NOT part of the core: Update computes it
// incrementally, while makeMove skips it entirely (perft never inspects
// intermediate hashes and restores the original from the positionUndo record).
// See docs/adr/0001-single-move-application-core.md.

// nextMoveCount returns the full-move number after the side to move has moved.
// The number advances when Black is to move. This is the single source for the
// moveCount rule; it was previously inlined in Update, makeMove, and nullUpdate.
func (pos *Position) nextMoveCount() int {
	if pos.turn == Black {
		return pos.moveCount + 1
	}
	return pos.moveCount
}

// applyMove applies the non-null bookkeeping rule to pos in place. It updates
// the board, side to move, castling rights, en-passant square, half-move clock,
// full-move number, the in-check flag, and invalidates the cached legal-move
// and status values.
//
// The hash is intentionally left untouched: callers own the hash. Update
// computes it from the pre-move state via updateHash; makeMove leaves it stale
// and restores the original on unmakeMove.
//
// Pre-move-derived values (castle rights, en passant, half-move clock) are
// computed before board.update mutates the board, since updateCastleRights and
// updateEnPassantSquare both inspect the origin square on the pre-move board.
// isInCheck is computed after, once the new side to move and board are in place.
func (pos *Position) applyMove(m Move) {
	moveCount := pos.nextMoveCount()
	ncr := pos.updateCastleRights(m)
	p := pos.board.Piece(m.s1)
	halfMove := 0
	if p.Type() != Pawn && !m.HasTag(Capture) {
		halfMove = pos.halfMoveClock + 1
	}
	ep := pos.updateEnPassantSquare(m)

	pos.board.update(m)
	pos.turn = pos.turn.Other()
	pos.castleRights = ncr
	pos.enPassantSquare = ep
	pos.halfMoveClock = halfMove
	pos.moveCount = moveCount
	pos.validMoves = nil
	pos.statusCached = false
	if m.HasTag(Check) {
		pos.inCheck = true
	} else {
		pos.inCheck = isInCheck(pos)
	}
}

// updateCastleRights returns the castling rights after m is played. It inspects
// the origin square on the pre-move board, so it must be called before
// board.update.
func (pos *Position) updateCastleRights(m Move) CastleRights {
	removeK := false
	removeQ := false
	removek := false
	removeq := false
	p := pos.board.Piece(m.s1)
	if p == WhiteKing || m.s1 == H1 || m.s2 == H1 {
		removeK = true
	}
	if p == WhiteKing || m.s1 == A1 || m.s2 == A1 {
		removeQ = true
	}
	if p == BlackKing || m.s1 == H8 || m.s2 == H8 {
		removek = true
	}
	if p == BlackKing || m.s1 == A8 || m.s2 == A8 {
		removeq = true
	}
	var buf [4]byte
	n := 0
	for i := range pos.castleRights {
		c := pos.castleRights[i]
		if (c == 'K' && removeK) || (c == 'Q' && removeQ) || (c == 'k' && removek) || (c == 'q' && removeq) || c == '-' {
			continue
		}
		buf[n] = c
		n++
	}
	if n == 0 {
		return "-"
	}
	return CastleRights(string(buf[:n]))
}

// updateEnPassantSquare returns the en-passant target square created by m, or
// NoSquare. It inspects the pre-move board and side to move, so it must be
// called before board.update and the side-to-move flip.
func (pos *Position) updateEnPassantSquare(m Move) Square {
	const squaresPerRank = 8
	p := pos.board.Piece(m.s1)
	if p.Type() != Pawn {
		return NoSquare
	}
	if pos.turn == White &&
		(bbForSquare(m.s1)&bbRank2) != 0 &&
		(bbForSquare(m.s2)&bbRank4) != 0 {
		return m.s2 - squaresPerRank
	} else if pos.turn == Black &&
		(bbForSquare(m.s1)&bbRank7) != 0 &&
		(bbForSquare(m.s2)&bbRank5) != 0 {
		return m.s2 + squaresPerRank
	}
	return NoSquare
}
