package chess

// This file is the single source of truth for "what changes about a Position
// when a non-null Move is played": the incremental bookkeeping rule
// (moveCount, halfMoveClock, castleRights, enPassant, board, turn, inCheck,
// hash) and the shared physical-fact descriptor (moveEffect) for the board
// mutation and the Zobrist hash delta. The copy-on-write applier
// (Position.Update) and the in-place applier (Position.makeMove) both delegate
// here to applyMove, so the rule cannot drift between them. applyMove also
// computes the post-move Zobrist hash from the pre-move state via updateHash,
// so the board mutation and the hash delta read one interpretation of
// en-passant squares, castle rook squares, and capture targets.
//
// Null moves are a separate concern (nullUpdate) and intentionally do not share
// the applyMove body: a null move never touches the board, castling rights, or
// pieces, so folding it in would force applyMove to skip a board copy it can
// otherwise avoid. They share only the moveCount rule via nextMoveCount.
//
// updateHash is called while pos still holds pre-move state, so its reads of
// pos.board / pos.hash / pos.castleRights / pos.enPassantSquare are the
// pre-move values; ncr, ep, and eff carry the post-move bookkeeping and the
// physical descriptor. See docs/adr/0001-single-move-application-core.md and
// docs/adr/0017-zobrist-hash-consolidation.md.

// nullUpdate returns a new position that is identical to the receiver except
// for the side to move, the half-move clock, the full-move clock, and the
// en-passant square. The half-move clock is incremented as for a quiet move,
// the full-move clock advances when Black passed, and the en-passant capture
// right is cleared. The board, castling rights, and pieces are unchanged.
// The Zobrist hash is recomputed incrementally.
func (pos *Position) nullUpdate() *Position {
	newPos := &Position{
		board:           pos.board,
		turn:            pos.turn.Other(),
		castleRights:    pos.castleRights,
		enPassantSquare: NoSquare,
		halfMoveClock:   pos.halfMoveClock + 1,
		moveCount:       pos.nextMoveCount(),
	}

	// Recompute inCheck for the new side to move. The board is unchanged,
	// so the new side may now be in check if a piece attacks their king.
	newPos.inCheck = isInCheck(newPos)
	newPos.hash = pos.nullUpdateHash(newPos.enPassantSquare)
	return newPos
}

// nextMoveCount returns the full-move number after the side to move has moved.
// The number advances when Black is to move. This is the single source for the
// moveCount rule; it was previously inlined in Update, makeMove, and nullUpdate.
func (pos *Position) nextMoveCount() int {
	if pos.turn == Black {
		return pos.moveCount + 1
	}
	return pos.moveCount
}

// moveEffect is the single interpretation of a move's physical facts. The board
// mutation (Board.update) and the Zobrist delta (updateHash) both read it
// instead of re-deriving en-passant squares, castle rook squares, and capture
// targets from MoveTags, so the two cannot drift.
//
// A zero value (moving == NoPiece) is returned for degenerate moves with no
// piece on the origin square; Board.update treats this as a no-op, as it did
// before the descriptor existed.
type moveEffect struct {
	moving   Piece  // piece on s1 before the move
	landing  Piece  // piece that ends on s2 (promo piece, else moving)
	capPiece Piece  // captured piece, NoPiece if none
	capSq    Square // capture square (s2, or s2±8 for en passant)
	rookFrom Square // castle rook origin, NoSquare if not a castle
	rookTo   Square // castle rook destination, NoSquare if not a castle
}

// computeMoveEffect derives the physical facts of m from the pre-move board.
// It is the only place that knows how the special-move tags (EnPassant,
// KingSideCastle, QueenSideCastle) map to physical effects; both Board.update
// and updateHash consume the returned struct.
//
// Every effect (captured square, castle rook squares) is derived from the
// moving piece's color so unsafe moves with a mismatched turn cannot apply
// effects for the wrong side. Returns the zero effect (every optional field
// at NoSquare / NoPiece) when m.s1 is empty.
func computeMoveEffect(b *Board, m Move) moveEffect {
	moving := b.Piece(m.s1)
	if moving == NoPiece {
		return moveEffect{capSq: NoSquare, rookFrom: NoSquare, rookTo: NoSquare}
	}

	eff := moveEffect{
		moving:   moving,
		landing:  moving,
		capSq:    NoSquare,
		rookFrom: NoSquare,
		rookTo:   NoSquare,
	}
	if m.promo != NoPieceType {
		eff.landing = NewPiece(m.promo, moving.Color())
	}

	switch {
	case m.HasTag(EnPassant) && !b.isOccupied(m.s2):
		// Real e.p.: m.s2 is empty, so capture the pawn on the adjacent
		// rank. If m.s2 is occupied the EnPassant tag is being misused on
		// a normal capture; fall through to handle it as such and remove
		// the piece actually on m.s2 rather than ghosting it in the
		// bitboards.
		if moving.Color() == White {
			eff.capSq = m.s2 - 8
		} else {
			eff.capSq = m.s2 + 8
		}
		eff.capPiece = b.Piece(eff.capSq)
	case b.isOccupied(m.s2):
		eff.capPiece = b.Piece(m.s2)
		eff.capSq = m.s2
	}

	eff.rookFrom, eff.rookTo = castleRookMove(m, moving.Color())
	return eff
}

// castleRookMove returns the origin and destination squares of the rook in a
// castling move, or (NoSquare, NoSquare) when m carries no castle tag. The
// chosen rank is determined by the moving piece's color: white castles along
// the first rank, black along the eighth.
func castleRookMove(m Move, c Color) (Square, Square) {
	switch {
	case m.HasTag(KingSideCastle):
		if c == White {
			return H1, F1
		}
		return H8, F8
	case m.HasTag(QueenSideCastle):
		if c == White {
			return A1, D1
		}
		return A8, D8
	}
	return NoSquare, NoSquare
}

// applyMove applies the non-null bookkeeping rule to pos in place. It updates
// the board, side to move, castling rights, en-passant square, half-move clock,
// full-move number, the in-check flag, the Zobrist hash, and invalidates the
// cached legal-move and status values.
//
// updateHash is called while pos still holds pre-move state, so its reads of
// pos.board / pos.hash / pos.castleRights / pos.enPassantSquare are the
// pre-move values; ncr, ep, and eff carry the post-move bookkeeping and the
// physical descriptor. The new hash is assigned alongside the other post-move
// fields after the board mutation.
//
// Pre-move-derived values (castle rights, en passant, half-move clock) are
// computed before board.update mutates the board, since updateCastleRights and
// updateEnPassantSquare both inspect the origin square on the pre-move board.
// isInCheck is computed after, once the new side to move and board are in
// place. The computed effect is returned so undo-style callers (the MoveTree
// cursor's makeMoveCursor) can record the inverse without recomputing it;
// statement-style callers (perft's makeMove) simply ignore it.
func (pos *Position) applyMove(m Move) moveEffect {
	eff := computeMoveEffect(&pos.board, m)
	moveCount := pos.nextMoveCount()
	ncr := pos.updateCastleRights(m)
	p := eff.moving
	halfMove := 0
	if p.Type() != Pawn && eff.capPiece == NoPiece {
		halfMove = pos.halfMoveClock + 1
	}
	ep := pos.updateEnPassantSquare(m)

	// Compute the hash delta while pos still holds pre-move state. updateHash
	// reads pos.board / pos.hash / pos.castleRights / pos.enPassantSquare as
	// the pre-move values; ncr, ep, and eff describe the post-move state.
	newHash := pos.updateHash(m, ncr, ep, eff)

	pos.board.update(m, eff)
	pos.turn = pos.turn.Other()
	pos.castleRights = ncr
	pos.enPassantSquare = ep
	pos.halfMoveClock = halfMove
	pos.moveCount = moveCount
	pos.hash = newHash
	pos.validMoves = nil
	pos.statusCached = false
	if m.HasTag(Check) {
		pos.inCheck = true
	} else {
		pos.inCheck = isInCheck(pos)
	}
	return eff
}

// cursorUndo is the MoveTree cursor's slim undo record: the inverse of the
// move plus scalar state, ~56 bytes versus ~264 for perft's full-state
// positionUndo. The tree retains one entry per played level for the game's
// lifetime, so size matters here; perft's undo is stack-allocated on a hot
// path and stays untouched (ADR-0001). validMoves/status caches are not
// captured: applyMove invalidates both on the forward path, so restoring
// them as invalid matches the forward state exactly.
type cursorUndo struct {
	hash            uint64
	moveCount       int
	halfMoveClock   int
	castleRights    CastleRights
	eff             moveEffect
	enPassantSquare Square
	turn            Color
	inCheck         bool
}

// makeMoveCursor applies m in place and returns a slim undo record.
func (pos *Position) makeMoveCursor(m Move) cursorUndo {
	u := cursorUndo{
		hash:            pos.hash,
		moveCount:       pos.moveCount,
		halfMoveClock:   pos.halfMoveClock,
		castleRights:    pos.castleRights,
		enPassantSquare: pos.enPassantSquare,
		turn:            pos.turn,
		inCheck:         pos.inCheck,
	}
	if m.HasTag(Null) {
		next := pos.nullUpdate()
		*pos = *next
		return u
	}
	u.eff = pos.applyMove(m)
	return u
}

// unmakeMoveCursor restores the pre-move position captured by makeMoveCursor.
// m must be the move that produced u. eff.moving == NoPiece (null move)
// makes board.unapply a no-op, leaving only the scalar restore.
func (pos *Position) unmakeMoveCursor(m Move, u cursorUndo) {
	pos.board.unapply(m, u.eff)
	pos.hash = u.hash
	pos.moveCount = u.moveCount
	pos.halfMoveClock = u.halfMoveClock
	pos.castleRights = u.castleRights
	pos.enPassantSquare = u.enPassantSquare
	pos.turn = u.turn
	pos.inCheck = u.inCheck
	pos.validMoves = nil
	pos.statusCached = false
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
	if !removeK && !removeQ && !removek && !removeq {
		return pos.castleRights
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
