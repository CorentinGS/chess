/*
Package chess implements a chess game engine that manages move generation,
position analysis, and game state validation.
The engine uses bitboard operations and lookup tables for efficient move
generation and position analysis. Move generation includes standard piece
moves, captures, castling, en passant, and pawn promotions.
Example usage:

	// Create a position
	pos := NewPosition()

	// Calculate legal moves for current position
	eng := engine{}
	moves := eng.CalcMoves(pos, false)

	// Check game status
	status := eng.Status(pos)
	if status == Checkmate {
		fmt.Println("Game Over - Checkmate")
	}
*/
package chess

import (
	"math/bits"
	"sync"
)

// engine implements chess move generation and position analysis.
type engine struct{}

type moveGenerationMode uint8

const (
	generateLegalAnnotated moveGenerationMode = iota
	generateLegalOnly
	generateUnsafeOnly
)

// CalcMoves returns all legal moves for the given position. If first is true,
// returns after finding the first legal move. This is useful for quick position
// validation.
//
// The moves are generated in the following order:
//  1. Standard piece moves and captures
//  2. Castling moves (if available)
//
// Each move is validated to ensure it doesn't leave the king in check
func (engine) CalcMoves(pos *Position, first bool) []Move {
	if first {
		if hasLegalMove(pos) {
			return []Move{{}}
		}
		return nil
	}

	// generate possible moves
	moves := legalMovesForMode(pos, generateLegalAnnotated)
	return moves
}

func legalMovesForMode(pos *Position, mode moveGenerationMode) []Move {
	moves := standardMoves(pos, false, mode)
	// return moves including castles
	var castles [2]Move
	count := castleMovesInto(pos, &castles, mode)
	moves = append(moves, castles[:count]...)
	return moves
}

// UnsafeMoves returns all pseudo-legal moves that are illegal because they
// leave the moving side's king in check.
func (engine) UnsafeMoves(pos *Position) []Move {
	return standardMoves(pos, false, generateUnsafeOnly)
}

// Status returns the current position's Method (Checkmate, Stalemate, or
// NoMethod).
//
// The Method is determined by:
//   - Whether the side to move is in check
//   - Whether any legal moves exist
//
// If the position has cached valid moves in pos.validMoves, those will be
// used. Otherwise, moves will be calculated to determine the Method.
func (e engine) Status(pos *Position) Method {
	var hasMove bool
	if pos.validMoves != nil {
		hasMove = len(pos.validMoves) > 0
	} else {
		hasMove = hasLegalMove(pos)
	}
	if !pos.inCheck && !hasMove {
		return Stalemate
	} else if pos.inCheck && !hasMove {
		return Checkmate
	}
	return NoMethod
}

func hasLegalMove(pos *Position) bool {
	if hasStandardMove(pos) {
		return true
	}
	return hasCastleMove(pos)
}

func hasStandardMove(pos *Position) bool {
	return visitStandardMoves(pos, generateLegalOnly, func(Move) bool { return true })
}

func visitLegalMoves(pos *Position, mode moveGenerationMode, visit func(Move) bool) bool {
	if visitStandardMoves(pos, mode, visit) {
		return true
	}
	if mode == generateUnsafeOnly {
		return false
	}
	var castles [2]Move
	count := castleMovesInto(pos, &castles, mode)
	for i := range count {
		if visit(castles[i]) {
			return true
		}
	}
	return false
}

func visitStandardMoves(pos *Position, mode moveGenerationMode, visit func(Move) bool) bool {
	var m Move
	ctx := legalMoveContextFor(pos, mode)

	bbAllowed := ^pos.board.whiteSqs
	if pos.Turn() == Black {
		bbAllowed = ^pos.board.blackSqs
	}

	for _, p := range allPieces {
		if pos.Turn() != p.Color() {
			continue
		}
		s1BB := pos.board.bbForPiece(p)
		if s1BB == 0 {
			continue
		}
		for s1Bits := s1BB; s1Bits != 0; s1Bits &= s1Bits - 1 {
			s1 := squareFromBit(s1Bits & -s1Bits)
			s2BB := bbForPossibleMoves(pos, p.Type(), s1) & bbAllowed
			if ctx.enabled {
				s2BB = ctx.filter(pos, p, s1, s2BB)
			}
			if s2BB == 0 {
				continue
			}
			for s2Bits := s2BB; s2Bits != 0; s2Bits &= s2Bits - 1 {
				s2 := squareFromBit(s2Bits & -s2Bits)

				m.s1 = s1
				m.s2 = s2

				if (p == WhitePawn && s2.Rank() == Rank8) || (p == BlackPawn && s2.Rank() == Rank1) {
					for _, pt := range promoPieceTypes {
						m.promo = pt
						m.tags = moveTagsForPiece(m, pos, mode, p, mode == generateLegalOnly && ctx.provesOwnKingSafe(p, s2))
						if moveMatchesMode(m, mode) {
							if visit(m) {
								return true
							}
						}
					}
				} else {
					m.promo = 0
					m.tags = moveTagsForPiece(m, pos, mode, p, mode == generateLegalOnly && ctx.provesOwnKingSafe(p, s2))
					if moveMatchesMode(m, mode) {
						if visit(m) {
							return true
						}
					}
				}
			}
		}
	}

	return false
}

func moveMatchesMode(m Move, mode moveGenerationMode) bool {
	if mode == generateUnsafeOnly {
		return m.HasTag(inCheck)
	}
	return !m.HasTag(inCheck)
}

type legalMoveContext struct {
	enabled    bool
	enPassant  Square
	checkCount int
	checkMask  bitboard
}

func legalMoveContextFor(pos *Position, mode moveGenerationMode) legalMoveContext {
	if mode == generateUnsafeOnly {
		return legalMoveContext{}
	}
	kingSq := pos.board.kingSquare(pos.turn)
	if kingSq == NoSquare {
		return legalMoveContext{}
	}
	queenBB, rookBB, bishopBB := sliderBitboards(pos.board, pos.turn.Other())
	if !pos.inCheck && alignedMasks[kingSq]&(queenBB|rookBB|bishopBB) == 0 {
		return legalMoveContext{}
	}
	ctx := legalMoveContext{
		enabled:   true,
		enPassant: pos.enPassantSquare,
		checkMask: ^bitboard(0),
	}
	if pos.inCheck {
		ctx.setChecks(pos, kingSq)
	}
	return ctx
}

func (ctx legalMoveContext) filter(pos *Position, p Piece, s1 Square, moves bitboard) bitboard {
	if p.Type() == King {
		return moves
	}
	if ctx.enPassant != NoSquare && p.Type() == Pawn {
		return moves
	}
	if ctx.checkCount > 1 {
		return 0
	}
	if ctx.checkCount == 1 {
		moves &= ctx.checkMask
	}
	if pinRay := pinnedRayForPiece(pos, s1); pinRay != 0 {
		moves &= pinRay
	}
	return moves
}

func (ctx legalMoveContext) provesOwnKingSafe(p Piece, s2 Square) bool {
	if p.Type() == King {
		return false
	}
	if !ctx.enabled {
		return false
	}
	if p.Type() == Pawn && ctx.enPassant != NoSquare {
		return false
	}
	return true
}

func (ctx *legalMoveContext) setChecks(pos *Position, kingSq Square) {
	board := pos.board
	attacker := pos.turn.Other()
	occ := ^board.emptySqs
	queenBB, rookBB, bishopBB := sliderBitboards(board, attacker)

	checkers := (hvAttack(occ, kingSq) & (queenBB | rookBB)) |
		(diaAttack(occ, kingSq) & (queenBB | bishopBB)) |
		(bbKnightMoves[kingSq] & board.bbForPiece(NewPiece(Knight, attacker))) |
		(bbKingMoves[kingSq] & board.bbForPiece(NewPiece(King, attacker))) |
		pawnCheckers(board, kingSq, attacker)

	ctx.checkCount = bits.OnesCount64(uint64(checkers))
	if ctx.checkCount == 1 {
		checkerSq := squareFromBit(checkers)
		ctx.checkMask = bbForSquare(checkerSq)
		if squaresAligned(kingSq, checkerSq) {
			ctx.checkMask |= squaresBetween(kingSq, checkerSq)
		}
	}
}

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

func rayStep(from Square, to Square) (fileStep int, rankStep int, diagonal bool) {
	fileDelta := int(to.File()) - int(from.File())
	rankDelta := int(to.Rank()) - int(from.Rank())
	switch {
	case fileDelta == 0:
		return 0, compareStep(rankDelta, 0), false
	case rankDelta == 0:
		return compareStep(fileDelta, 0), 0, false
	case abs(fileDelta) == abs(rankDelta):
		return compareStep(fileDelta, 0), compareStep(rankDelta, 0), true
	default:
		return 0, 0, false
	}
}

func piecePinsAlong(pt PieceType, diagonal bool) bool {
	if pt == Queen {
		return true
	}
	if diagonal {
		return pt == Bishop
	}
	return pt == Rook
}

func squaresBetween(a Square, b Square) bitboard {
	fileStep := compareStep(int(b.File()), int(a.File()))
	rankStep := compareStep(int(b.Rank()), int(a.Rank()))
	if fileStep == 0 && rankStep == 0 {
		return 0
	}
	if fileStep != 0 && rankStep != 0 && !sameDiagonal(a, b) {
		return 0
	}
	var out bitboard
	file := int(a.File()) + fileStep
	rank := int(a.Rank()) + rankStep
	for file != int(b.File()) || rank != int(b.Rank()) {
		out |= bbForSquare(NewSquare(File(file), Rank(rank)))
		file += fileStep
		rank += rankStep
	}
	return out
}

func compareStep(a, b int) int {
	switch {
	case a > b:
		return 1
	case a < b:
		return -1
	default:
		return 0
	}
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func sameDiagonal(a Square, b Square) bool {
	fileDelta := int(a.File()) - int(b.File())
	if fileDelta < 0 {
		fileDelta = -fileDelta
	}
	rankDelta := int(a.Rank()) - int(b.Rank())
	if rankDelta < 0 {
		rankDelta = -rankDelta
	}
	return fileDelta == rankDelta
}

func squareFromBit(bb bitboard) Square {
	return Square(63 - bits.TrailingZeros64(uint64(bb)))
}

// promoPieceTypes is an immutable array of promotion piece types.
// Treat as read-only; do not modify elements.
//
//nolint:gochecknoglobals // Immutable lookup table.
var promoPieceTypes = [4]PieceType{Queen, Rook, Bishop, Knight}

const maxPossibleMoves = 218 // Maximum possible moves in any chess position

// movePool is a pool of Move arrays to reduce allocations
// in the standardMoves function.
//
//nolint:gochecknoglobals // this is a sync pool
var movePool = &sync.Pool{
	New: func() interface{} {
		return &[maxPossibleMoves]Move{}
	},
}

// standardMoves generates all standard (non-castling) legal moves for the
// current position. If first is true, returns after finding the first
// legal move.
//
// The function uses a sync.Pool of move arrays to reduce allocations. Each
// move is validated to ensure it doesn't leave the king in check.
func standardMoves(pos *Position, first bool, mode moveGenerationMode) []Move {
	moves, ok := movePool.Get().(*[maxPossibleMoves]Move)
	if !ok {
		// Pool returned an unexpected type; allocate a fresh array rather
		// than dereferencing a nil pointer below.
		moves = &[maxPossibleMoves]Move{}
	}
	defer movePool.Put(moves)
	count := 0

	if first {
		var result [1]Move
		if visitStandardMoves(pos, mode, func(m Move) bool {
			result[0] = m
			return true
		}) {
			return result[:]
		}
		return nil
	}

	visitStandardMoves(pos, mode, func(m Move) bool {
		moves[count] = m
		count++
		return false
	})

	// Need to copy since we're returning array to pool
	result := make([]Move, count)
	copy(result, moves[:count])
	return result
}

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

func sliderBitboards(board *Board, c Color) (queen bitboard, rook bitboard, bishop bitboard) {
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
		if isSquareAttackedBy(pos.board, sq, otherColor) {
			return true
		}
	}
	return false
}

// bbForPossibleMoves returns a bitboard with 1s in positions where the piece
// of the given type at the given square can potentially move, without considering
// whether the moves would be legal (e.g., leave the king in check).
//
// The function handles movement patterns for:
//   - King: One square in any direction
//   - Queen: Sliding moves in all directions
//   - Rook: Sliding moves horizontally and vertically
//   - Bishop: Sliding moves diagonally
//   - Knight: L-shaped jumps
//   - Pawn: Forward moves and captures, including en passant
func bbForPossibleMoves(pos *Position, pt PieceType, sq Square) bitboard {
	switch pt {
	case King:
		return bbKingMoves[sq]
	case Queen:
		return diaAttack(^pos.board.emptySqs, sq) | hvAttack(^pos.board.emptySqs, sq)
	case Rook:
		return hvAttack(^pos.board.emptySqs, sq)
	case Bishop:
		return diaAttack(^pos.board.emptySqs, sq)
	case Knight:
		return bbKnightMoves[sq]
	case Pawn:
		return pawnMoves(pos, sq)
	}
	return bitboard(0)
}

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

// pawnMoves returns a bitboard with 1s in positions where the pawn at the
// given square can potentially move.
//
// The function considers:
//   - Single and double forward moves
//   - Diagonal captures
//   - En passant captures
//
//nolint:mnd // this is a formula to determine the color of a square
func pawnMoves(pos *Position, sq Square) bitboard {
	bb := bbForSquare(sq)
	var bbEnPassant bitboard
	if pos.enPassantSquare != NoSquare {
		bbEnPassant = bbForSquare(pos.enPassantSquare)
	}
	if pos.Turn() == White {
		capRight := ((bb & ^bbFileH & ^bbRank8) >> 9) & (pos.board.blackSqs | bbEnPassant)
		capLeft := ((bb & ^bbFileA & ^bbRank8) >> 7) & (pos.board.blackSqs | bbEnPassant)
		upOne := ((bb & ^bbRank8) >> 8) & pos.board.emptySqs
		upTwo := ((upOne & bbRank3) >> 8) & pos.board.emptySqs
		return capRight | capLeft | upOne | upTwo
	}
	capRight := ((bb & ^bbFileH & ^bbRank1) << 7) & (pos.board.whiteSqs | bbEnPassant)
	capLeft := ((bb & ^bbFileA & ^bbRank1) << 9) & (pos.board.whiteSqs | bbEnPassant)
	upOne := ((bb & ^bbRank1) << 8) & pos.board.emptySqs
	upTwo := ((upOne & bbRank6) << 8) & pos.board.emptySqs
	return capRight | capLeft | upOne | upTwo
}

// diaAttack returns a bitboard representing possible diagonal moves for a
// sliding piece, considering occupied squares as blocking further movement.
//
// Implementation: index a deterministic magic-bitboard table generated at
// package init from checked-in magic constants.
func diaAttack(occupied bitboard, sq Square) bitboard {
	return bishopMagicAttacks[sq][((occupied&bishopMagicMasks[sq])*bishopMagics[sq])>>bishopMagicShifts[sq]]
}

func slowDiaAttack(occupied bitboard, sq Square) bitboard {
	f := int(sq) & 7
	r := int(sq) >> 3
	occ := uint64(occupied)
	var attacks uint64
	// NE: rank+1, file+1 (NE-SW diagonal = bbDiagonals[sq]).
	for d := 1; f+d < 8 && r+d < 8; d++ {
		bit := uint64(1) << (63 - ((r + d) << 3) - (f + d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// SW: rank-1, file-1 (NE-SW diagonal, opposite side).
	for d := 1; f-d >= 0 && r-d >= 0; d++ {
		bit := uint64(1) << (63 - ((r - d) << 3) - (f - d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// NW: rank+1, file-1 (NW-SE diagonal = bbAntiDiagonals[sq]).
	for d := 1; f-d >= 0 && r+d < 8; d++ {
		bit := uint64(1) << (63 - ((r + d) << 3) - (f - d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// SE: rank-1, file+1 (NW-SE diagonal, opposite side).
	for d := 1; f+d < 8 && r-d >= 0; d++ {
		bit := uint64(1) << (63 - ((r - d) << 3) - (f + d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	return bitboard(attacks)
}

// hvAttack returns a bitboard representing possible horizontal and vertical
// moves for a sliding piece, considering occupied squares as blocking
// further movement.
//
// Implementation: index a deterministic magic-bitboard table generated at
// package init from checked-in magic constants.
func hvAttack(occupied bitboard, sq Square) bitboard {
	return rookMagicAttacks[sq][((occupied&rookMagicMasks[sq])*rookMagics[sq])>>rookMagicShifts[sq]]
}

func slowHVAttack(occupied bitboard, sq Square) bitboard {
	f := int(sq) & 7
	r := int(sq) >> 3
	occ := uint64(occupied)
	var attacks uint64
	// E: file+1.
	for df := 1; f+df < 8; df++ {
		bit := uint64(1) << (63 - (r << 3) - (f + df))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// W: file-1.
	for df := 1; f-df >= 0; df++ {
		bit := uint64(1) << (63 - (r << 3) - (f - df))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// N: rank+1.
	for dr := 1; r+dr < 8; dr++ {
		bit := uint64(1) << (63 - ((r + dr) << 3) - f)
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// S: rank-1.
	for dr := 1; r-dr >= 0; dr++ {
		bit := uint64(1) << (63 - ((r - dr) << 3) - f)
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	return bitboard(attacks)
}

const (
	bbFileA bitboard = 9259542123273814144
	bbFileB bitboard = 4629771061636907072
	bbFileC bitboard = 2314885530818453536
	bbFileD bitboard = 1157442765409226768
	bbFileE bitboard = 578721382704613384
	bbFileF bitboard = 289360691352306692
	bbFileG bitboard = 144680345676153346
	bbFileH bitboard = 72340172838076673

	bbRank1 bitboard = 18374686479671623680
	bbRank2 bitboard = 71776119061217280
	bbRank3 bitboard = 280375465082880
	bbRank4 bitboard = 1095216660480
	bbRank5 bitboard = 4278190080
	bbRank6 bitboard = 16711680
	bbRank7 bitboard = 65280
	bbRank8 bitboard = 255
)

// bbForSquare returns the bitboard mask for the given square.
// This is a package-level function rather than a Square method because it
// accesses the package-level lookup table bbSquares.
func bbForSquare(sq Square) bitboard {
	return bbSquares[sq]
}

// Lookup tables for piece movement patterns and board masks.
//
//nolint:gochecknoglobals // this is a lookup table
var (
	bbFiles = [8]bitboard{bbFileA, bbFileB, bbFileC, bbFileD, bbFileE, bbFileF, bbFileG, bbFileH} // bbFiles contains masks for each file (A-H)
	bbRanks = [8]bitboard{bbRank1, bbRank2, bbRank3, bbRank4, bbRank5, bbRank6, bbRank7, bbRank8} // bbRanks contains masks for each rank (1-8)

	bbDiagonals = [64]bitboard{9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 144396663052566528, 72057594037927936, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 144396663052566528, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 32832, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 128, 32832, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745}

	bbAntiDiagonals = [64]bitboard{9223372036854775808, 4647714815446351872, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 4647714815446351872, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 258, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 258, 1}

	bbKnightMoves = [64]bitboard{9077567998918656, 4679521487814656, 38368557762871296, 19184278881435648, 9592139440717824, 4796069720358912, 2257297371824128, 1128098930098176, 2305878468463689728, 1152939783987658752, 9799982666336960512, 4899991333168480256, 2449995666584240128, 1224997833292120064, 576469569871282176, 288234782788157440, 4620693356194824192, 11533718717099671552, 5802888705324613632, 2901444352662306816, 1450722176331153408, 725361088165576704, 362539804446949376, 145241105196122112, 18049583422636032, 45053588738670592, 22667534005174272, 11333767002587136, 5666883501293568, 2833441750646784, 1416171111120896, 567348067172352, 70506185244672, 175990581010432, 88545054707712, 44272527353856, 22136263676928, 11068131838464, 5531918402816, 2216203387392, 275414786112, 687463207072, 345879119952, 172939559976, 86469779988, 43234889994, 21609056261, 8657044482, 1075839008, 2685403152, 1351090312, 675545156, 337772578, 168886289, 84410376, 33816580, 4202496, 10489856, 5277696, 2638848, 1319424, 659712, 329728, 132096}

	bbBishopMoves = [64]bitboard{18049651735527937, 45053622886727936, 22667548931719168, 11334324221640704, 5667164249915392, 2833579985862656, 1416240237150208, 567382630219904, 4611756524879479810, 11529391036782871041, 5764696068147249408, 2882348036221108224, 1441174018118909952, 720587009051099136, 360293502378066048, 144117404414255168, 2323857683139004420, 1197958188344280066, 9822351133174399489, 4911175566595588352, 2455587783297826816, 1227793891648880768, 577868148797087808, 288793334762704928, 1161999073681608712, 581140276476643332, 326598935265674242, 9386671504487645697, 4693335752243822976, 2310639079102947392, 1155178802063085600, 577588851267340304, 580999811184992272, 290500455356698632, 145390965166737412, 108724279602332802, 9241705379636978241, 4620711952330133792, 2310355426409252880, 1155177711057110024, 290499906664153120, 145249955479592976, 72625527495610504, 424704217196612, 36100411639206946, 9241421692918565393, 4620710844311799048, 2310355422147510788, 145249953336262720, 72624976676520096, 283693466779728, 1659000848424, 141017232965652, 36099303487963146, 9241421688590368773, 4620710844295151618, 72624976668147712, 283691315142656, 1108177604608, 6480472064, 550848566272, 141012904249856, 36099303471056128, 9241421688590303744}

	bbRookMoves = [64]bitboard{9187484529235886208, 13781085504453754944, 16077885992062689312, 17226286235867156496, 17800486357769390088, 18087586418720506884, 18231136449196065282, 18302911464433844481, 9259260648297103488, 4665518383679160384, 2368647251370188832, 1220211685215703056, 645993902138460168, 358885010599838724, 215330564830528002, 143553341945872641, 9259541023762186368, 4629910699613634624, 2315095537539358752, 1157687956502220816, 578984165983651848, 289632270724367364, 144956323094725122, 72618349279904001, 9259542118978846848, 4629771607097753664, 2314886351157207072, 1157443723186933776, 578722409201797128, 289361752209228804, 144681423712944642, 72341259464802561, 9259542123257036928, 4629771063767613504, 2314885534022901792, 1157442769150545936, 578721386714368008, 289360695496279044, 144680349887234562, 72340177082712321, 9259542123273748608, 4629771061645230144, 2314885530830970912, 1157442765423841296, 578721382720276488, 289360691368494084, 144680345692602882, 72340172854657281, 9259542123273813888, 4629771061636939584, 2314885530818502432, 1157442765409283856, 578721382704674568, 289360691352369924, 144680345676217602, 72340172838141441, 9259542123273814143, 4629771061636907199, 2314885530818453727, 1157442765409226991, 578721382704613623, 289360691352306939, 144680345676153597, 72340172838076926}

	bbQueenMoves = [64]bitboard{9205534180971414145, 13826139127340482880, 16100553540994408480, 17237620560088797200, 17806153522019305480, 18090419998706369540, 18232552689433215490, 18303478847064064385, 13871017173176583298, 16194909420462031425, 8133343319517438240, 4102559721436811280, 2087167920257370120, 1079472019650937860, 575624067208594050, 287670746360127809, 11583398706901190788, 5827868887957914690, 12137446670713758241, 6068863523097809168, 3034571949281478664, 1517426162373248132, 722824471891812930, 361411684042608929, 10421541192660455560, 5210911883574396996, 2641485286422881314, 10544115227674579473, 5272058161445620104, 2600000831312176196, 1299860225776030242, 649930110732142865, 9840541934442029200, 4920271519124312136, 2460276499189639204, 1266167048752878738, 9820426766351346249, 4910072647826412836, 2455035776296487442, 1227517888139822345, 9550042029937901728, 4775021017124823120, 2387511058326581416, 1157867469641037908, 614821794359483434, 9530782384287059477, 4765391190004401930, 2382695595002168069, 9404792076610076608, 4702396038313459680, 2315169224285282160, 1157444424410132280, 578862399937640220, 325459994840333070, 9386102034266586375, 4693051017133293059, 9332167099941961855, 4630054752952049855, 2314886638996058335, 1157442771889699055, 578721933553179895, 289501704256556795, 180779649147209725, 9313761861428380670}

	bbKingMoves = [64]bitboard{4665729213955833856, 11592265440851656704, 5796132720425828352, 2898066360212914176, 1449033180106457088, 724516590053228544, 362258295026614272, 144959613005987840, 13853283560024178688, 16186183351374184448, 8093091675687092224, 4046545837843546112, 2023272918921773056, 1011636459460886528, 505818229730443264, 216739030602088448, 54114388906344448, 63227278716305408, 31613639358152704, 15806819679076352, 7903409839538176, 3951704919769088, 1975852459884544, 846636838289408, 211384331665408, 246981557485568, 123490778742784, 61745389371392, 30872694685696, 15436347342848, 7718173671424, 3307175149568, 825720045568, 964771708928, 482385854464, 241192927232, 120596463616, 60298231808, 30149115904, 12918652928, 3225468928, 3768639488, 1884319744, 942159872, 471079936, 235539968, 117769984, 50463488, 12599488, 14721248, 7360624, 3680312, 1840156, 920078, 460039, 197123, 49216, 57504, 28752, 14376, 7188, 3594, 1797, 770}

	bbSquares          = [64]bitboard{}
	lineSquares        = [4][64][8]Square{}
	lineBitboards      = [4][64][8]bitboard{}
	lineMasks          = [4][64]bitboard{}
	lineBitIndexes     = [4][64][64]uint8{}
	lineWordIndexes    = [4][64][4][65536]uint8{}
	fileWordIndexes    = [8][4][65536]uint8{}
	lineLens           = [4][64]int{}
	alignedMasks       = [64]bitboard{}
	betweenMasks       = [64][64]bitboard{}
	rayFileSteps       = [64][64]int8{}
	rayRankSteps       = [64][64]int8{}
	rayDiagonals       = [64][64]bool{}
	slidingAttacks     = [4][64][256]bitboard{}
	rookMagicMasks     = [64]bitboard{}
	bishopMagicMasks   = [64]bitboard{}
	rookMagicShifts    = [64]uint{}
	bishopMagicShifts  = [64]uint{}
	rookMagicAttacks   = [64][4096]bitboard{}
	bishopMagicAttacks = [64][512]bitboard{}
)

// Deterministically generated for this package's A1=MSB square numbering.
var rookMagics = [64]bitboard{ //nolint:gochecknoglobals // lookup constants
	0x000009284d008402, 0x0602004108040082, 0x5262005024880102, 0x071200141020086a,
	0x4003008620500009, 0x0000220010088042, 0x4001004000208015, 0x3620120880410022,
	0x02804d0280542200, 0x80c0081001020400, 0x2058044010200801, 0x4108004200040040,
	0x00a0080050008180, 0x0818220080401200, 0x8000220045028600, 0x40800020014000c0,
	0x4080009408420005, 0x0000080210040001, 0x88c1002400090002, 0x0809001008010004,
	0x00100a0010420020, 0x102c100020008080, 0x003000200040c008, 0x4080004020014000,
	0x010010984200011c, 0x1800010204001008, 0x101200540a000830, 0x0020800800800400,
	0x0100100023000900, 0x0022002042001084, 0x0060200042401008, 0x4000400020800080,
	0x0028012200004084, 0x3900611400d01802, 0x0202000404001020, 0x0002002200100408,
	0x3080080080100080, 0x8000200080801000, 0x0000200280400084, 0x0450288080004000,
	0x000202000c205181, 0x00222c000e100108, 0x8031080110400420, 0x0048008008800400,
	0x0002020020081041, 0x1020018061801000, 0x0010024000200040, 0x2080004000402000,
	0x9182000102046484, 0x0000800200010080, 0x0083000300080400, 0x082a002030048a00,
	0x2181001000090020, 0x0401801000200180, 0x3200404000201000, 0x0000802080004006,
	0x0200048114002042, 0x0200430406002088, 0x2200211028020024, 0x4100080010020500,
	0x0a80080004100082, 0x2700200031000840, 0x0040001000200041, 0x0080006015804008,
}

var bishopMagics = [64]bitboard{ //nolint:gochecknoglobals // lookup constants
	0x0002100142040242, 0x0000400841140080, 0x2030e04064084220, 0x000120813092020a,
	0x0510c0040020a800, 0x0000810104010400, 0x0400078084100260, 0x0480808808020200,
	0x1284040842042000, 0x0221a00400808021, 0x4000068850010010, 0x0a10001020222022,
	0x6802124842020062, 0x8050002084100106, 0x6222092108020020, 0x4a2c012188600001,
	0x4408880100400020, 0x10421e0204004212, 0x006210b000804100, 0x1410401011010210,
	0x0800044200908800, 0x0000104828007000, 0x1004210108041001, 0x60009004200210c0,
	0x10a408a482082400, 0x0008081120009082, 0x0802208104020040, 0x0084200200082080,
	0x0100020080080080, 0x4142045101900100, 0x0022022023900100, 0x4002034080200812,
	0x04a2048008540081, 0x4004088530421004, 0x8268080800808400, 0x0000848014002000,
	0x800c080100220040, 0x0000208224080080, 0x0008208004091244, 0x0428400420024a81,
	0x0081020a00420204, 0x5002011080b0880a, 0x10f2001040422080, 0x0044004480a02000,
	0x0008000420401100, 0x0784026088001240, 0x0202040810210200, 0x2021120420420220,
	0xe000422508088400, 0x000001008820880a, 0x4006011048040004, 0x0008020210008000,
	0x8000040408800821, 0x8016048104090400, 0x002e080829042120, 0x1040a00210011104,
	0x0020240948141002, 0xe025880d48200108, 0x0041010840020620, 0x0002121040400450,
	0x0148248900004010, 0x2008180460811001, 0x0010308080808600, 0x0804101208011014,
}

const (
	slideRank = iota
	slideFile
	slideDiag
	slideAntiDiag
	slideLineCount
)

func lineIndex(occupied bitboard, line int, sq Square) uint8 {
	u := uint64(occupied)
	t := &lineWordIndexes[line][sq]
	return t[0][uint16(u>>48)] |
		t[1][uint16(u>>32)] |
		t[2][uint16(u>>16)] |
		t[3][uint16(u)]
}

func rankLineIndex(occupied bitboard, sq Square) uint8 {
	shift := uint((7 - sq.Rank()) * 8)
	return bits.Reverse8(byte(uint64(occupied) >> shift))
}

func fileLineIndex(occupied bitboard, sq Square) uint8 {
	u := uint64(occupied)
	t := &fileWordIndexes[sq.File()]
	return t[0][uint16(u>>48)] |
		t[1][uint16(u>>32)] |
		t[2][uint16(u>>16)] |
		t[3][uint16(u)]
}

// init populates the bbSquares lookup table. This is done at package
// initialization because the values are constants derived from square indices.
//
//nolint:gochecknoinits // Required for lookup table initialization.
func init() {
	const numOfSquaresInBoard = 64
	for sq := range numOfSquaresInBoard {
		bbSquares[sq] = bitboard(uint64(1) << (uint8(63) - uint8(sq)))
	}
	initAlignedMasks()
	initRayMasks()
	initMagicAttackTables()
	initSlidingAttackTables()
}

func initMagicAttackTables() {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		rookMagicMasks[sq] = rookMagicMask(square)
		rookMagicShifts[sq] = uint(numOfSquaresInBoard - bits.OnesCount64(uint64(rookMagicMasks[sq])))
		initMagicAttack(rookMagicAttacks[sq][:], rookMagicMasks[sq], rookMagics[sq], rookMagicShifts[sq], square, slowHVAttack)

		bishopMagicMasks[sq] = bishopMagicMask(square)
		bishopMagicShifts[sq] = uint(numOfSquaresInBoard - bits.OnesCount64(uint64(bishopMagicMasks[sq])))
		initMagicAttack(
			bishopMagicAttacks[sq][:],
			bishopMagicMasks[sq],
			bishopMagics[sq],
			bishopMagicShifts[sq],
			square,
			slowDiaAttack,
		)
	}
}

func initMagicAttack(
	attacks []bitboard,
	mask bitboard,
	magic bitboard,
	shift uint,
	sq Square,
	attack func(bitboard, Square) bitboard,
) {
	for _, occupied := range magicOccupancies(mask) {
		idx := ((occupied & mask) * magic) >> shift
		attacks[idx] = attack(occupied, sq)
	}
}

func magicOccupancies(mask bitboard) []bitboard {
	bitCount := bits.OnesCount64(uint64(mask))
	occupancies := make([]bitboard, 1<<bitCount)
	maskBits := make([]uint, 0, bitCount)
	for rest := mask; rest != 0; rest &= rest - 1 {
		maskBits = append(maskBits, uint(bits.TrailingZeros64(uint64(rest))))
	}
	for subset := range occupancies {
		var occupied bitboard
		for i, bit := range maskBits {
			if subset&(1<<i) != 0 {
				occupied |= bitboard(uint64(1) << bit)
			}
		}
		occupancies[subset] = occupied
	}
	return occupancies
}

func rookMagicMask(sq Square) bitboard {
	file := int(sq.File())
	rank := int(sq.Rank())
	var mask bitboard
	for r := rank + 1; r <= 6; r++ {
		mask |= bbForSquare(NewSquare(File(file), Rank(r)))
	}
	for r := rank - 1; r >= 1; r-- {
		mask |= bbForSquare(NewSquare(File(file), Rank(r)))
	}
	for f := file + 1; f <= 6; f++ {
		mask |= bbForSquare(NewSquare(File(f), Rank(rank)))
	}
	for f := file - 1; f >= 1; f-- {
		mask |= bbForSquare(NewSquare(File(f), Rank(rank)))
	}
	return mask
}

func bishopMagicMask(sq Square) bitboard {
	file := int(sq.File())
	rank := int(sq.Rank())
	var mask bitboard
	for f, r := file+1, rank+1; f <= 6 && r <= 6; f, r = f+1, r+1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file-1, rank-1; f >= 1 && r >= 1; f, r = f-1, r-1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file-1, rank+1; f >= 1 && r <= 6; f, r = f-1, r+1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file+1, rank-1; f <= 6 && r >= 1; f, r = f+1, r-1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	return mask
}

func initAlignedMasks() {
	for a := range numOfSquaresInBoard {
		for b := range numOfSquaresInBoard {
			if squaresAligned(Square(a), Square(b)) {
				alignedMasks[a] |= bbForSquare(Square(b))
			}
		}
	}
}

func initRayMasks() {
	for a := range numOfSquaresInBoard {
		for b := range numOfSquaresInBoard {
			fileStep, rankStep, diagonal := rayStep(Square(a), Square(b))
			rayFileSteps[a][b] = int8(fileStep)
			rayRankSteps[a][b] = int8(rankStep)
			rayDiagonals[a][b] = diagonal
			betweenMasks[a][b] = squaresBetween(Square(a), Square(b))
		}
	}
}

func initSlidingAttackTables() {
	for sq := range numOfSquaresInBoard {
		initSlidingLines(Square(sq))
	}
	initLineWordIndexes()
	for line := range slideLineCount {
		for sq := range numOfSquaresInBoard {
			for idx := range 256 {
				slidingAttacks[line][sq][idx] = attackOnLine(line, Square(sq), uint8(idx))
			}
		}
	}
}

func initLineWordIndexes() {
	for line := range slideLineCount {
		if line == slideRank || line == slideFile {
			continue
		}
		for sq := range numOfSquaresInBoard {
			for wordIndex := range 4 {
				shift := uint((3 - wordIndex) * 16)
				for value := range 65536 {
					chunk := bitboard(uint64(value) << shift)
					lineWordIndexes[line][sq][wordIndex][value] = lineIndexFromMaskedOccupancy(chunk, line, Square(sq))
				}
			}
		}
	}
	for file := range numOfSquaresInRow {
		sq := NewSquare(File(file), Rank1)
		for wordIndex := range 4 {
			shift := uint((3 - wordIndex) * 16)
			for value := range 65536 {
				chunk := bitboard(uint64(value) << shift)
				fileWordIndexes[file][wordIndex][value] = lineIndexFromMaskedOccupancy(chunk, slideFile, sq)
			}
		}
	}
}

func lineIndexFromMaskedOccupancy(occupied bitboard, line int, sq Square) uint8 {
	lineOccupied := occupied & lineMasks[line][sq]
	var idx uint8
	for lineOccupied != 0 {
		bit := lineOccupied & -lineOccupied
		idx |= lineBitIndexes[line][sq][bits.TrailingZeros64(uint64(bit))]
		lineOccupied &= lineOccupied - 1
	}
	return idx
}

func initSlidingLines(sq Square) {
	f := int(sq.File())
	r := int(sq.Rank())

	for file := range numOfSquaresInRow {
		appendLineSquare(slideRank, sq, NewSquare(File(file), Rank(r)))
	}
	for rank := range numOfSquaresInRow {
		appendLineSquare(slideFile, sq, NewSquare(File(f), Rank(rank)))
	}

	startF, startR := f, r
	for startF > 0 && startR > 0 {
		startF--
		startR--
	}
	for startF < 8 && startR < 8 {
		appendLineSquare(slideDiag, sq, NewSquare(File(startF), Rank(startR)))
		startF++
		startR++
	}

	startF, startR = f, r
	for startF > 0 && startR < 7 {
		startF--
		startR++
	}
	for startF < 8 && startR >= 0 {
		appendLineSquare(slideAntiDiag, sq, NewSquare(File(startF), Rank(startR)))
		startF++
		startR--
	}
}

func appendLineSquare(line int, src Square, sq Square) {
	idx := lineLens[line][src]
	lineSquares[line][src][idx] = sq
	bb := bbForSquare(sq)
	lineBitboards[line][src][idx] = bb
	lineMasks[line][src] |= bb
	lineBitIndexes[line][src][bits.TrailingZeros64(uint64(bb))] = 1 << idx
	lineLens[line][src]++
}

func attackOnLine(line int, src Square, idx uint8) bitboard {
	var srcIdx int
	for i := range lineLens[line][src] {
		if lineSquares[line][src][i] == src {
			srcIdx = i
			break
		}
	}

	var attacks bitboard
	for i := srcIdx + 1; i < lineLens[line][src]; i++ {
		sq := lineSquares[line][src][i]
		attacks |= bbForSquare(sq)
		if idx&(1<<i) != 0 {
			break
		}
	}
	for i := srcIdx - 1; i >= 0; i-- {
		sq := lineSquares[line][src][i]
		attacks |= bbForSquare(sq)
		if idx&(1<<i) != 0 {
			break
		}
	}
	return attacks
}
