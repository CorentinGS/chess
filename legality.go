package chess

import (
	"math/bits"
)

// legality owns the king-safety policy for one position. Construct one per
// position with newLegality and call legal on every candidate move.
//
// legality never mutates its pos. The slow path (king moves, en passant,
// in-check, opponent-Check annotation) uses a stack copy of pos.board; the
// prefilter lives in legality.filter for movegen's hot loop.
//
// legal returns (tag, ok). ok is true iff the move leaves the moving side's
// own king safe. tag always carries the cheap bits (Capture/EnPassant/castle);
// in mode == generateLegalAnnotated it also carries Check when applicable.
// Callers must gate on ok before trusting tag for annotation.
type legality struct {
	pos        *Position
	mode       moveGenerationMode
	enabled    bool
	checkCount int
	checkMask  bitboard
}

func newLegality(pos *Position, mode moveGenerationMode) legality {
	lg := legality{pos: pos, mode: mode}
	if mode == generateUnsafeOnly {
		return lg
	}
	kingSq := pos.board.kingSquare(pos.turn)
	if kingSq == NoSquare {
		return lg
	}
	queenBB, rookBB, bishopBB := sliderBitboards(&pos.board, pos.turn.Other())
	if !pos.inCheck && alignedMasks[kingSq]&(queenBB|rookBB|bishopBB) == 0 {
		return lg
	}
	lg.enabled = true
	lg.checkMask = ^bitboard(0)
	if pos.inCheck {
		setCheckContext(&lg, kingSq)
	}
	return lg
}

func setCheckContext(lg *legality, kingSq Square) {
	board := lg.pos.board
	attacker := lg.pos.turn.Other()
	occ := ^board.emptySqs
	queenBB, rookBB, bishopBB := sliderBitboards(&board, attacker)

	checkers := (hvAttack(occ, kingSq) & (queenBB | rookBB)) |
		(diaAttack(occ, kingSq) & (queenBB | bishopBB)) |
		(bbKnightMoves[kingSq] & board.bbForPiece(NewPiece(Knight, attacker))) |
		(bbKingMoves[kingSq] & board.bbForPiece(NewPiece(King, attacker))) |
		pawnCheckers(&board, kingSq, attacker)

	lg.checkCount = bits.OnesCount64(uint64(checkers))
	if lg.checkCount == 1 {
		checkerSq := squareFromBit(checkers)
		lg.checkMask = bbForSquare(checkerSq)
		if squaresAligned(kingSq, checkerSq) {
			lg.checkMask |= squaresBetween(kingSq, checkerSq)
		}
	}
}

// filter restricts the pseudo-legal destinations for one piece using the
// precomputed king-safety context. King moves and pawns-with-en-passant
// always pass through (they need a per-move legality check); in double check,
// only King moves survive (movegen's non-King loops see s2BB == 0).
func (lg legality) filter(p Piece, s1 Square, moves bitboard) bitboard {
	if !lg.enabled {
		return moves
	}
	if p.Type() == King {
		return moves
	}
	if lg.pos.enPassantSquare != NoSquare && p.Type() == Pawn {
		return moves
	}
	if lg.checkCount > 1 {
		return 0
	}
	if lg.checkCount == 1 {
		moves &= lg.checkMask
	}
	if pinRay := pinnedRayForPiece(lg.pos, s1); pinRay != 0 {
		moves &= pinRay
	}
	return moves
}

func (lg legality) legal(m Move) (MoveTag, bool) {
	var tag MoveTag
	p := lg.pos.board.Piece(m.s1)

	if lg.pos.board.isOccupied(m.s2) {
		tag |= Capture
	} else if m.s2 == lg.pos.enPassantSquare && p.Type() == Pawn {
		tag |= EnPassant
	}
	if (p == WhiteKing && m.s1 == E1) || (p == BlackKing && m.s1 == E8) {
		switch m.s2 {
		case C1, C8:
			tag |= QueenSideCastle
		case G1, G8:
			tag |= KingSideCastle
		}
	}

	return lg.kingSafety(m, p, tag)
}

func (lg legality) kingSafety(m Move, p Piece, tag MoveTag) (MoveTag, bool) {
	// Fast path: not in check, not King, not en passant. Positions without
	// aligned enemy slider pressure skip the slider check entirely.
	if !lg.pos.inCheck && p.Type() != King && tag&EnPassant == 0 {
		if moveFromAlignedWithOwnKing(m, lg.pos) && exposesOwnKingToSlider(m, lg.pos) {
			return tag, false
		}
		if lg.mode == generateLegalAnnotated {
			return lg.annotateCheck(m, tag), true
		}
		return tag, true
	}
	return lg.simulate(m, tag)
}

func (lg legality) annotateCheck(m Move, tag MoveTag) MoveTag {
	b := lg.pos.board
	applied := m
	applied.tags = tag
	b.update(applied, computeMoveEffect(&lg.pos.board, applied))
	if b.kingSquare(lg.pos.turn.Other()) != NoSquare &&
		isSquareAttackedBy(&b, b.kingSquare(lg.pos.turn.Other()), lg.pos.turn) {
		tag |= Check
	}
	return tag
}

func (lg legality) simulate(m Move, tag MoveTag) (MoveTag, bool) {
	// Direct b.update on a stack copy: simulate only needs piece placement
	// for the attack test; applyMove's scalar state (moveCount, hash,
	// castleRights, EP) must not change on the live position.
	b := lg.pos.board
	applied := m
	applied.tags = tag
	b.update(applied, computeMoveEffect(&lg.pos.board, applied))
	if b.kingSquare(lg.pos.turn) != NoSquare &&
		isSquareAttackedBy(&b, b.kingSquare(lg.pos.turn), lg.pos.turn.Other()) {
		return tag, false
	}
	if lg.mode == generateLegalAnnotated {
		if b.kingSquare(lg.pos.turn.Other()) != NoSquare &&
			isSquareAttackedBy(&b, b.kingSquare(lg.pos.turn.Other()), lg.pos.turn) {
			tag |= Check
		}
	}
	return tag, true
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
	queenBB, rookBB, bishopBB := sliderBitboards(&pos.board, attacker)
	queenBB &^= captured
	rookBB &^= captured
	bishopBB &^= captured
	return hvAttack(occ, kingSq)&(queenBB|rookBB) != 0 ||
		diaAttack(occ, kingSq)&(queenBB|bishopBB) != 0
}
