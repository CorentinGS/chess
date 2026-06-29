package chess

func pawnCheckers(board *Board, kingSq Square, attacker Color) bitboard {
	pawns := board.bbForPiece(NewPiece(Pawn, attacker))
	var checkers bitboard
	for pawnBits := pawns; pawnBits != 0; pawnBits &= pawnBits - 1 {
		pawnSq := squareFromBit(pawnBits & -pawnBits)
		if pawnAttacks(attacker, pawnSq)&bbForSquare(kingSq) != 0 {
			checkers |= bbForSquare(pawnSq)
		}
	}
	return checkers
}

func pawnAttacks(c Color, sq Square) bitboard {
	bb := bbForSquare(sq)
	if c == White {
		return ((bb & ^bbFileH & ^bbRank8) >> 9) | ((bb & ^bbFileA & ^bbRank8) >> 7)
	}
	return ((bb & ^bbFileH & ^bbRank1) << 7) | ((bb & ^bbFileA & ^bbRank1) << 9)
}

func pinnedRayForPiece(pos *Position, s1 Square) bitboard {
	kingSq := pos.board.kingSquare(pos.turn)
	if kingSq == NoSquare || alignedMasks[kingSq]&bbForSquare(s1) == 0 {
		return 0
	}
	fileStep := int(rayFileSteps[kingSq][s1])
	rankStep := int(rayRankSteps[kingSq][s1])
	if fileStep == 0 && rankStep == 0 {
		return 0
	}
	if betweenMasks[kingSq][s1]&^pos.board.emptySqs != 0 {
		return 0
	}
	diagonal := rayDiagonals[kingSq][s1]
	file := int(s1.File()) + fileStep
	rank := int(s1.Rank()) + rankStep
	for file >= 0 && file < numOfSquaresInRow && rank >= 0 && rank < numOfSquaresInRow {
		sq := NewSquare(File(file), Rank(rank))
		p := pos.board.Piece(sq)
		if p != NoPiece {
			if p.Color() != pos.turn.Other() || !piecePinsAlong(p.Type(), diagonal) {
				return 0
			}
			return betweenMasks[kingSq][sq] | bbForSquare(sq)
		}
		file += fileStep
		rank += rankStep
	}
	return 0
}

func sliderBitboards(board *Board, c Color) (bitboard, bitboard, bitboard) {
	if c == White {
		return board.bbWhiteQueen, board.bbWhiteRook, board.bbWhiteBishop
	}
	return board.bbBlackQueen, board.bbBlackRook, board.bbBlackBishop
}

func squaresAligned(a Square, b Square) bool {
	fileDelta := int(a.File()) - int(b.File())
	if fileDelta < 0 {
		fileDelta = -fileDelta
	}
	rankDelta := int(a.Rank()) - int(b.Rank())
	if rankDelta < 0 {
		rankDelta = -rankDelta
	}
	return a.File() == b.File() || a.Rank() == b.Rank() || fileDelta == rankDelta
}

// isInCheck returns true if the side to move is in check in the given position.
func isInCheck(pos *Position) bool {
	kingSq := pos.board.kingSquare(pos.Turn())
	// king should only be missing in tests / examples
	if kingSq == NoSquare {
		return false
	}
	return squaresAreAttacked(pos, kingSq)
}

// isSquareAttackedBy returns true if the given square is attacked by the specified color.
// This is a board-level operation that does not require a full Position.
//
//nolint:mnd // this is a formula to determine if a square is attacked
func isSquareAttackedBy(board *Board, sq Square, attacker Color) bool {
	occ := ^board.emptySqs

	// hot path check to see if attack vector is possible
	s2BB := board.blackSqs
	if attacker == White {
		s2BB = board.whiteSqs
	}
	diagAttacks := diaAttack(occ, sq)
	orthogonalAttacks := hvAttack(occ, sq)
	if ((diagAttacks|orthogonalAttacks)&s2BB)|(bbKnightMoves[sq]&s2BB) == 0 {
		return false
	}

	// check queen attack vector
	queenBB := board.bbForPiece(NewPiece(Queen, attacker))
	bb := (diagAttacks | orthogonalAttacks) & queenBB
	if bb != 0 {
		return true
	}
	// check rook attack vector
	rookBB := board.bbForPiece(NewPiece(Rook, attacker))
	bb = orthogonalAttacks & rookBB
	if bb != 0 {
		return true
	}
	// check bishop attack vector
	bishopBB := board.bbForPiece(NewPiece(Bishop, attacker))
	bb = diagAttacks & bishopBB
	if bb != 0 {
		return true
	}
	// check knight attack vector
	knightBB := board.bbForPiece(NewPiece(Knight, attacker))
	bb = bbKnightMoves[sq] & knightBB
	if bb != 0 {
		return true
	}
	// check pawn attack vector
	if attacker == Black {
		capRight := (board.bbBlackPawn & ^bbFileH & ^bbRank1) << 7
		capLeft := (board.bbBlackPawn & ^bbFileA & ^bbRank1) << 9
		bb = (capRight | capLeft) & bbForSquare(sq)
		if bb != 0 {
			return true
		}
	} else {
		capRight := (board.bbWhitePawn & ^bbFileH & ^bbRank8) >> 9
		capLeft := (board.bbWhitePawn & ^bbFileA & ^bbRank8) >> 7
		bb = (capRight | capLeft) & bbForSquare(sq)
		if bb != 0 {
			return true
		}
	}
	// check king attack vector
	kingBB := board.bbForPiece(NewPiece(King, attacker))
	bb = bbKingMoves[sq] & kingBB
	return bb != 0
}

// squaresAreAttacked returns true if the opponent attacks any of the given squares
//
//	in the given position.
//
// The function checks attacks from:
//   - Sliding pieces (queen, rook, bishop)
//   - Knights
//   - Pawns
//   - King
func squaresAreAttacked(pos *Position, sqs ...Square) bool {
	otherColor := pos.Turn().Other()
	for _, sq := range sqs {
		if isSquareAttackedBy(&pos.board, sq, otherColor) {
			return true
		}
	}
	return false
}
