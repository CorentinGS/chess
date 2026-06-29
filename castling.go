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
	count := 0

	kingSide := pos.castleRights.CanCastle(pos.Turn(), KingSide)
	queenSide := pos.castleRights.CanCastle(pos.Turn(), QueenSide)

	// white king side
	if pos.turn == White && kingSide &&
		(^pos.board.emptySqs&(bbForSquare(F1)|bbForSquare(G1))) == 0 &&
		!squaresAreAttacked(pos, F1, G1) &&
		!pos.inCheck {
		m := Move{s1: E1, s2: G1}
		m.tags = moveTagsForMode(m, pos, mode)
		moves[count] = m
		count++
	}

	// white queen side
	if pos.turn == White && queenSide &&
		(^pos.board.emptySqs&(bbForSquare(B1)|bbForSquare(C1)|bbForSquare(D1))) == 0 &&
		!squaresAreAttacked(pos, C1, D1) &&
		!pos.inCheck {
		m := Move{s1: E1, s2: C1}
		m.tags = moveTagsForMode(m, pos, mode)
		moves[count] = m
		count++
	}

	// black king side
	if pos.turn == Black && kingSide &&
		(^pos.board.emptySqs&(bbForSquare(F8)|bbForSquare(G8))) == 0 &&
		!squaresAreAttacked(pos, F8, G8) &&
		!pos.inCheck {
		m := Move{s1: E8, s2: G8}
		m.tags = moveTagsForMode(m, pos, mode)
		moves[count] = m
		count++
	}

	// black queen side
	if pos.turn == Black && queenSide &&
		(^pos.board.emptySqs&(bbForSquare(B8)|bbForSquare(C8)|bbForSquare(D8))) == 0 &&
		!squaresAreAttacked(pos, C8, D8) &&
		!pos.inCheck {
		m := Move{s1: E8, s2: C8}
		m.tags = moveTagsForMode(m, pos, mode)
		moves[count] = m
		count++
	}

	return count
}
