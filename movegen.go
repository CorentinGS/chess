package chess

import (
	"sync"
)

type moveGenerationMode uint8

const (
	generateLegalAnnotated moveGenerationMode = iota
	generateLegalOnly
	generateUnsafeOnly
)

// calcMoves returns all legal moves for the given position. If first is true,
// returns after finding the first legal move. This is useful for quick position
// validation.
//
// The moves are generated in the following order:
//  1. Standard piece moves and captures
//  2. Castling moves (if available)
//
// Each move is validated to ensure it doesn't leave the king in check.
func calcMoves(pos *Position, first bool) []Move {
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

// unsafeMoves returns all pseudo-legal moves that are illegal because they
// leave the moving side's king in check.
func unsafeMoves(pos *Position) []Move {
	return standardMoves(pos, false, generateUnsafeOnly)
}

// status returns the current position's Method (Checkmate, Stalemate, or
// NoMethod).
//
// The Method is determined by:
//   - Whether the side to move is in check
//   - Whether any legal moves exist
//
// If the position has cached valid moves in pos.validMoves, those will be
// used. Otherwise, moves will be calculated to determine the Method.
func status(pos *Position) Method {
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

func visitLegalMoves(pos *Position, mode moveGenerationMode, visit func(Move) bool) {
	if visitStandardMoves(pos, mode, visit) {
		return
	}
	if mode == generateUnsafeOnly {
		return
	}
	var castles [2]Move
	count := castleMovesInto(pos, &castles, mode)
	for _, m := range castles[:count] {
		if visit(m) {
			return
		}
	}
}

func visitStandardMoves(pos *Position, mode moveGenerationMode, visit func(Move) bool) bool {
	var m Move
	lg := newLegality(pos, mode)

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
			s2BB = lg.filter(p, s1, s2BB)
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
						tag, ok := lg.legal(m)
						m.tags = tag
						if ok == (mode != generateUnsafeOnly) && visit(m) {
							return true
						}
					}
				} else {
					m.promo = 0
					tag, ok := lg.legal(m)
					m.tags = tag
					if ok == (mode != generateUnsafeOnly) && visit(m) {
						return true
					}
				}
			}
		}
	}

	return false
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
	New: func() any {
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
