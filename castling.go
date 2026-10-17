package chess

// castleMoves returns all legal castling moves for the current position.
//
// A castling move is legal if:
//   - The king has castling rights in that direction
//   - The squares between king and rook are empty
//   - The king is not in check
//   - The king does not pass through check
func hasCastleMove(pos *Position) bool {
	var castles [2]Move
	return castleMovesInto(pos, &castles, generateLegalOnly) > 0
}

func castleMovesInto(pos *Position, moves *[2]Move, mode moveGenerationMode) int {
	if pos.variant == Chess960 {
		return chess960CastleMovesInto(pos, moves, mode)
	}
	return standardCastleMovesInto(pos, moves, mode)
}

// standardCastleMovesInto generates castling moves for the standard variant,
// where the king starts on e1/e8 and the rooks on a1/h1/a8/h8.
func standardCastleMovesInto(pos *Position, moves *[2]Move, mode moveGenerationMode) int {
	count := 0

	kingSide := pos.castleRights.CanCastle(pos.Turn(), KingSide)
	queenSide := pos.castleRights.CanCastle(pos.Turn(), QueenSide)

	// white king side
	if pos.turn == White && kingSide &&
		(^pos.board.emptySqs&(bbForSquare(F1)|bbForSquare(G1))) == 0 &&
		!squaresAreAttacked(pos, F1, G1) &&
		!pos.inCheck {
		m := Move{s1: E1, s2: G1}
		tag, _ := newLegality(pos, mode).legal(m)
		m.tags = tag
		moves[count] = m
		count++
	}

	// white queen side
	if pos.turn == White && queenSide &&
		(^pos.board.emptySqs&(bbForSquare(B1)|bbForSquare(C1)|bbForSquare(D1))) == 0 &&
		!squaresAreAttacked(pos, C1, D1) &&
		!pos.inCheck {
		m := Move{s1: E1, s2: C1}
		tag, _ := newLegality(pos, mode).legal(m)
		m.tags = tag
		moves[count] = m
		count++
	}

	// black king side
	if pos.turn == Black && kingSide &&
		(^pos.board.emptySqs&(bbForSquare(F8)|bbForSquare(G8))) == 0 &&
		!squaresAreAttacked(pos, F8, G8) &&
		!pos.inCheck {
		m := Move{s1: E8, s2: G8}
		tag, _ := newLegality(pos, mode).legal(m)
		m.tags = tag
		moves[count] = m
		count++
	}

	// black queen side
	if pos.turn == Black && queenSide &&
		(^pos.board.emptySqs&(bbForSquare(B8)|bbForSquare(C8)|bbForSquare(D8))) == 0 &&
		!squaresAreAttacked(pos, C8, D8) &&
		!pos.inCheck {
		m := Move{s1: E8, s2: C8}
		tag, _ := newLegality(pos, mode).legal(m)
		m.tags = tag
		moves[count] = m
		count++
	}

	return count
}

// chess960CastleMovesInto generates castling moves for the Chess960 variant.
// Castling destinations are the standard squares (king to g/c, rook to f/d),
// but the king and rook origins are derived from the board: the king is on its
// back rank, and the castling rook is the nearest friendly rook to the king's
// right (king side) or left (queen side). The king and rook paths must be clear
// (each allowed to stand in the other's way), and the king must not be in check
// or pass through or land on an attacked square.
func chess960CastleMovesInto(pos *Position, moves *[2]Move, mode moveGenerationMode) int {
	count := 0
	c := pos.turn
	var rank Rank
	var kSideKingTo, qSideKingTo, kSideRookTo, qSideRookTo Square
	if c == White {
		rank = Rank1
		kSideKingTo, qSideKingTo, kSideRookTo, qSideRookTo = G1, C1, F1, D1
	} else {
		rank = Rank8
		kSideKingTo, qSideKingTo, kSideRookTo, qSideRookTo = G8, C8, F8, D8
	}
	kingFile := backRankKingFile(&pos.board, c)
	if kingFile < 0 {
		return 0
	}
	kingFrom := NewSquare(File(kingFile), rank)

	tryCastle := func(side Side, kingTo, rookTo Square) {
		rookFile := rookFileForSide(&pos.board, c, kingFile, side)
		if rookFile < 0 {
			return
		}
		rookFrom := NewSquare(File(rookFile), rank)
		if !chess960CastlePathClear(&pos.board, kingFrom, kingTo, rookFrom, rookTo) {
			return
		}
		if !chess960KingTransitSafe(pos, kingFrom, kingTo) {
			return
		}
		m := Move{s1: kingFrom, s2: kingTo}
		tag, _ := newLegality(pos, mode).legal(m)
		m.tags = tag
		moves[count] = m
		count++
	}
	if pos.castleRights.CanCastle(c, KingSide) {
		tryCastle(KingSide, kSideKingTo, kSideRookTo)
	}
	if pos.castleRights.CanCastle(c, QueenSide) {
		tryCastle(QueenSide, qSideKingTo, qSideRookTo)
	}
	return count
}

// chess960CastlePathClear reports whether all squares the king and rook traverse
// during a Chess960 castle are clear of pieces other than the castling king and
// rook themselves. The squares strictly between each piece's start and end must
// be empty, except that the rook may stand in the king's path and the king may
// stand in the rook's path (each moves out of the other's way). This covers the
// stationary-piece and crossing-path cases.
func chess960CastlePathClear(b *Board, kingFrom, kingTo, rookFrom, rookTo Square) bool {
	return pathBetweenClear(b, kingFrom, kingTo, rookFrom) &&
		pathBetweenClear(b, rookFrom, rookTo, kingFrom)
}

// pathBetweenClear reports whether every square strictly between from and to
// (exclusive) on their shared rank is empty, ignoring the exempt square (the
// other castling piece, which is permitted to occupy the path).
func pathBetweenClear(b *Board, from, to, exempt Square) bool {
	lo, hi := int(from.File()), int(to.File())
	if lo > hi {
		lo, hi = hi, lo
	}
	rank := from.Rank()
	for f := lo + 1; f < hi; f++ {
		sq := NewSquare(File(f), rank)
		if sq == exempt {
			continue
		}
		if b.isOccupied(sq) {
			return false
		}
	}
	return true
}

// chess960KingTransitSafe reports whether the king can castle from kingFrom to
// kingTo: it must not be in check, and no square from kingFrom through kingTo
// (inclusive) may be attacked by the enemy.
func chess960KingTransitSafe(pos *Position, kingFrom, kingTo Square) bool {
	if pos.inCheck {
		return false
	}
	lo, hi := int(kingFrom.File()), int(kingTo.File())
	if lo > hi {
		lo, hi = hi, lo
	}
	rank := kingFrom.Rank()
	var transit []Square
	for f := lo; f <= hi; f++ {
		transit = append(transit, NewSquare(File(f), rank))
	}
	return !squaresAreAttacked(pos, transit...)
}
