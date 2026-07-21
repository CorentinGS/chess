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
		variant:         pos.variant,
	}

	// Recompute inCheck and checkers for the new side to move. The board is
	// unchanged, so the new side may now be in check if a piece attacks their king.
	newPos.inCheck, newPos.checkers = checkState(newPos)
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

	eff.rookFrom, eff.rookTo = castleRookMove(b, m, moving.Color())
	return eff
}

// castleRookMove returns the origin and destination squares of the rook in a
// castling move, or (NoSquare, NoSquare) when m carries no castle tag. The
// destination is always the standard square (f for king side, d for queen
// side); the origin is derived from the board: the nearest friendly rook to the
// king's right (king side) or left (queen side) on the back rank. m.s1 is the
// king's origin in a castling move. This serves both standard chess (rook on
// a1/h1/a8/h8) and Chess960 (rook anywhere on the back rank).
func castleRookMove(b *Board, m Move, c Color) (Square, Square) {
	kingSide := m.HasTag(KingSideCastle)
	queenSide := m.HasTag(QueenSideCastle)
	if !kingSide && !queenSide {
		return NoSquare, NoSquare
	}
	var (
		rank   Rank
		rook   Piece
		rookTo Square
	)
	if c == White {
		rank, rook = Rank1, WhiteRook
		if kingSide {
			rookTo = F1
		} else {
			rookTo = D1
		}
	} else {
		rank, rook = Rank8, BlackRook
		if kingSide {
			rookTo = F8
		} else {
			rookTo = D8
		}
	}
	kingFile := int(m.s1.File())
	if kingSide {
		for f := kingFile + 1; f < 8; f++ {
			if b.Piece(NewSquare(File(f), rank)) == rook {
				return NewSquare(File(f), rank), rookTo
			}
		}
	} else {
		for f := kingFile - 1; f >= 0; f-- {
			if b.Piece(NewSquare(File(f), rank)) == rook {
				return NewSquare(File(f), rank), rookTo
			}
		}
	}
	return NoSquare, rookTo
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
// applyMove serves both COW callers (Position.Update) and in-place
// callers (the cursor's makeMoveCursor). See ADR-016 for the
// single-active-cursor reasoning.
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
	pos.inCheck, pos.checkers = checkState(pos)
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
	checkers        bitboard
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
		checkers:        pos.checkers,
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
	pos.checkers = u.checkers
	pos.validMoves = nil
	pos.statusCached = false
}

// updateCastleRights returns the castling rights after m is played. It inspects
// the origin square on the pre-move board, so it must be called before
// board.update.
func (pos *Position) updateCastleRights(m Move) CastleRights {
	if pos.variant == Chess960 {
		return pos.updateCastleRightsChess960(m)
	}
	removeWK := false
	removeWQ := false
	removeBK := false
	removeBQ := false
	p := pos.board.Piece(m.s1)
	if p == WhiteKing || m.s1 == H1 || m.s2 == H1 {
		removeWK = true
	}
	if p == WhiteKing || m.s1 == A1 || m.s2 == A1 {
		removeWQ = true
	}
	if p == BlackKing || m.s1 == H8 || m.s2 == H8 {
		removeBK = true
	}
	if p == BlackKing || m.s1 == A8 || m.s2 == A8 {
		removeBQ = true
	}
	if !removeWK && !removeWQ && !removeBK && !removeBQ {
		return pos.castleRights
	}
	ncr := pos.castleRights
	if removeWK {
		ncr.White.KingSide = false
	}
	if removeWQ {
		ncr.White.QueenSide = false
	}
	if removeBK {
		ncr.Black.KingSide = false
	}
	if removeBQ {
		ncr.Black.QueenSide = false
	}
	return ncr
}

// updateCastleRightsChess960 returns the castling rights after m is played in a
// Chess960 position. A held right is lost when the king moves, or when the rook
// granting that right moves or is captured. The granting rook is located on the
// back rank relative to the king, so this must be called before board.update.
func (pos *Position) updateCastleRightsChess960(m Move) CastleRights {
	ncr := pos.castleRights
	if ncr.White.KingSide && pos.chess960RightLost(White, KingSide, m) {
		ncr.White.KingSide = false
	}
	if ncr.White.QueenSide && pos.chess960RightLost(White, QueenSide, m) {
		ncr.White.QueenSide = false
	}
	if ncr.Black.KingSide && pos.chess960RightLost(Black, KingSide, m) {
		ncr.Black.KingSide = false
	}
	if ncr.Black.QueenSide && pos.chess960RightLost(Black, QueenSide, m) {
		ncr.Black.QueenSide = false
	}
	return ncr
}

// chess960RightLost reports whether the castling right (c, side) is lost by
// playing m: the king moved off its square, or the granting rook moved or was
// captured. Returns false if the king or rook cannot be located.
func (pos *Position) chess960RightLost(c Color, side Side, m Move) bool {
	kingFile := backRankKingFile(&pos.board, c)
	if kingFile < 0 {
		return false
	}
	rank := Rank1
	if c == Black {
		rank = Rank8
	}
	kingSq := NewSquare(File(kingFile), rank)
	rf := rookFileForSide(&pos.board, c, kingFile, side)
	if rf < 0 {
		return false
	}
	rookSq := NewSquare(File(rf), rank)
	return m.s1 == kingSq || m.s1 == rookSq || m.s2 == rookSq
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
