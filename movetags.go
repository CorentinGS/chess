package chess

// moveTags computes all tags for a move from scratch based on the resulting position.
// Tags include:
//   - Capture: The move captures an opponent's piece
//   - EnPassant: The move is an en passant capture
//   - Check: The move puts the opponent in check
//   - inCheck: The move leaves the moving side's king in check (illegal)
//   - KingSideCastle: The move is a king-side castle
//   - QueenSideCastle: The move is a queen-side castle
func moveTags(m Move, pos *Position) MoveTag {
	return moveTagsForMode(m, pos, generateLegalAnnotated)
}

// moveTagsForMode computes the tags required by the selected generation mode.
// Full public move generation needs exported annotations such as Check. Fast
// existence checks only need enough information to reject moves that leave the
// moving side in check, so they skip the opponent-check test.
func moveTagsForMode(m Move, pos *Position, mode moveGenerationMode) MoveTag {
	return moveTagsForPiece(m, pos, mode, pos.board.Piece(m.s1), false)
}

func moveTagsForPiece(m Move, pos *Position, mode moveGenerationMode, p Piece, ownKingSafe bool) MoveTag {
	var tags MoveTag
	if pos.board.isOccupied(m.s2) {
		tags |= Capture
	} else if m.s2 == pos.enPassantSquare && p.Type() == Pawn {
		tags |= EnPassant
	}
	// determine if move is castle
	if (p == WhiteKing && m.s1 == E1) || (p == BlackKing && m.s1 == E8) {
		switch m.s2 {
		case C1, C8:
			tags |= QueenSideCastle
		case G1, G8:
			tags |= KingSideCastle
		}
	}
	if mode == generateLegalOnly || mode == generateUnsafeOnly {
		if ownKingSafe {
			return tags
		}
		if !pos.inCheck && p.Type() != King && tags&EnPassant == 0 {
			if moveFromAlignedWithOwnKing(m, pos) {
				if exposesOwnKingToSlider(m, pos) {
					tags |= inCheck
				}
				return tags
			}
			return tags
		}
		if !requiresOwnKingCheckSimulation(m, pos, p, tags) {
			return tags
		}
	}
	// apply preliminary tags to a local copy so board.update reads them correctly
	local := m
	local.tags = tags
	// determine if in check after move (makes move invalid)
	// Simulate the move on a temporary board copy so we can test
	// check status without mutating the actual position.
	tempBoard := *pos.board
	tempBoard.update(local)
	if !ownKingSafe && tempBoard.kingSquare(pos.turn) != NoSquare {
		if isSquareAttackedBy(&tempBoard, tempBoard.kingSquare(pos.turn), pos.turn.Other()) {
			tags |= inCheck
		}
	}
	if mode == generateLegalOnly || mode == generateUnsafeOnly {
		return tags
	}
	// determine if opponent in check after move
	if tempBoard.kingSquare(pos.turn.Other()) != NoSquare {
		if isSquareAttackedBy(&tempBoard, tempBoard.kingSquare(pos.turn.Other()), pos.turn) {
			tags |= Check
		}
	}
	return tags
}

func requiresOwnKingCheckSimulation(m Move, pos *Position, p Piece, tags MoveTag) bool {
	if pos.inCheck || p.Type() == King || tags&EnPassant != 0 {
		return true
	}
	return moveFromAlignedWithOwnKing(m, pos)
}

func moveFromAlignedWithOwnKing(m Move, pos *Position) bool {
	kingSq := pos.board.kingSquare(pos.turn)
	if kingSq == NoSquare {
		return false
	}
	return alignedMasks[kingSq]&bbForSquare(m.s1) != 0
}

func exposesOwnKingToSlider(m Move, pos *Position) bool {
	kingSq := pos.board.kingSquare(pos.turn)
	if kingSq == NoSquare {
		return false
	}
	occ := (^pos.board.emptySqs &^ bbForSquare(m.s1)) | bbForSquare(m.s2)
	attacker := pos.turn.Other()
	captured := bbForSquare(m.s2)
	queenBB, rookBB, bishopBB := sliderBitboards(pos.board, attacker)
	queenBB &^= captured
	orthogonal := kingSq.File() == m.s1.File() || kingSq.Rank() == m.s1.Rank()
	if orthogonal {
		rookBB &^= captured
		if hvAttack(occ, kingSq)&(queenBB|rookBB) != 0 {
			return true
		}
		return false
	}
	bishopBB &^= captured
	return diaAttack(occ, kingSq)&(queenBB|bishopBB) != 0
}
