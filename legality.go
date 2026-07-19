package chess

import (
	"math/bits"
)

// legality owns the king-safety policy for one position. Construct one per
// position with newLegality and call legal on every candidate move.
//
// legality never mutates its pos. The slow path (king moves, en passant,
// in-check, opponent-Check annotation) uses a stack copy of pos.board; the
// prefilter lives in legality.filter for movegen's hot loop, and king-safety
// reasoning in legality.legal.
//
// legal returns (tag, ok). ok is true iff the move leaves the moving side's
// own king safe. tag always carries the cheap bits (Capture/EnPassant/castle);
// in mode == generateLegalAnnotated it also carries Check when applicable.
// Callers must gate on ok before trusting tag for annotation.
type legality struct {
	pos        *Position
	mode       moveGenerationMode
	enabled    bool
	kingSq     Square
	enPassant  Square
	checkCount int
	checkMask  bitboard
}

func newLegality(pos *Position, mode moveGenerationMode) legality {
	lg := legality{
		pos:       pos,
		mode:      mode,
		kingSq:    pos.board.kingSquare(pos.turn),
		enPassant: pos.enPassantSquare,
	}
	if mode == generateUnsafeOnly || lg.kingSq == NoSquare {
		return lg
	}
	queenBB, rookBB, bishopBB := sliderBitboards(&pos.board, pos.turn.Other())
	if !pos.inCheck && alignedMasks[lg.kingSq]&(queenBB|rookBB|bishopBB) == 0 {
		return lg
	}
	lg.enabled = true
	lg.checkMask = ^bitboard(0)
	if pos.inCheck {
		setCheckContext(&lg)
	}
	return lg
}

func setCheckContext(lg *legality) {
	board := lg.pos.board
	attacker := lg.pos.turn.Other()
	occ := ^board.emptySqs
	queenBB, rookBB, bishopBB := sliderBitboards(&board, attacker)

	checkers := (hvAttack(occ, lg.kingSq) & (queenBB | rookBB)) |
		(diaAttack(occ, lg.kingSq) & (queenBB | bishopBB)) |
		(bbKnightMoves[lg.kingSq] & board.bbForPiece(NewPiece(Knight, attacker))) |
		(bbKingMoves[lg.kingSq] & board.bbForPiece(NewPiece(King, attacker))) |
		pawnCheckers(&board, lg.kingSq, attacker)

	lg.checkCount = bits.OnesCount64(uint64(checkers))
	if lg.checkCount == 1 {
		checkerSq := squareFromBit(checkers)
		lg.checkMask = bbForSquare(checkerSq)
		if squaresAligned(lg.kingSq, checkerSq) {
			lg.checkMask |= squaresBetween(lg.kingSq, checkerSq)
		}
	}
}

// filter restricts the pseudo-legal destinations for one piece using the
// precomputed king-safety context. Called by movegen's hot loop before any
// move is constructed. King moves and pawns-with-en-passant always pass
// through (they need a per-move legality check); in double check, only King
// moves survive (handled by movegen itself — non-King loops see s2BB == 0).
func (lg legality) filter(p Piece, s1 Square, moves bitboard) bitboard {
	if !lg.enabled {
		return moves
	}
	if p.Type() == King {
		return moves
	}
	if lg.enPassant != NoSquare && p.Type() == Pawn {
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
	} else if m.s2 == lg.enPassant && p.Type() == Pawn {
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
	// Fast path: not in check, not King, not en passant. The alignment + slider
	// check below catches pinned pieces; positions without aligned enemy slider
	// pressure skip the slider check entirely (moveFromAlignedWithOwnKing false).
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
	b := applyOnStackCopy(lg.pos.board, m, tag)
	if kingAttackedOnBoard(&b, lg.pos.turn.Other(), lg.pos.turn) {
		tag |= Check
	}
	return tag
}

func (lg legality) simulate(m Move, tag MoveTag) (MoveTag, bool) {
	// applyOnStackCopy operates on a stack copy of pos.board rather than
	// routing through Position.applyMove. That's intentional: the simulated
	// board only needs piece placement for the attack test, and avoiding
	// applyMove keeps the scalar state (moveCount, hash, castleRights, EP)
	// untouched on the live position.
	b := applyOnStackCopy(lg.pos.board, m, tag)
	if kingAttackedOnBoard(&b, lg.pos.turn, lg.pos.turn.Other()) {
		return tag, false
	}
	if lg.mode == generateLegalAnnotated &&
		kingAttackedOnBoard(&b, lg.pos.turn.Other(), lg.pos.turn) {
		tag |= Check
	}
	return tag, true
}

// applyOnStackCopy returns pos.board with m applied. tag is stashed on m so
// board.update can read it (en passant, castle handling consult m.tags).
func applyOnStackCopy(b Board, m Move, tag MoveTag) Board {
	applied := m
	applied.tags = tag
	b.update(applied, computeMoveEffect(&b, applied))
	return b
}

// kingAttackedOnBoard reports whether side's king on b is attacked by attacker.
func kingAttackedOnBoard(b *Board, side, attacker Color) bool {
	kingSq := b.kingSquare(side)
	if kingSq == NoSquare {
		return false
	}
	return isSquareAttackedBy(b, kingSq, attacker)
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

// moveTags computes the full set of public tags for a move. Kept as a
// delegator for callers in game_move.go, notation.go, and move_text_codec.go
// that previously imported the same name from movetags.go.
func moveTags(m Move, pos *Position) MoveTag {
	tag, _ := newLegality(pos, generateLegalAnnotated).legal(m)
	return tag
}

// moveTagsForMode is the mode-aware variant. Kept for castling.go.
func moveTagsForMode(m Move, pos *Position, mode moveGenerationMode) MoveTag {
	tag, _ := newLegality(pos, mode).legal(m)
	return tag
}
