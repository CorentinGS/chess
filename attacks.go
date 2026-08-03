package chess

import "math/bits"

// AttacksFrom returns the squares attacked by the piece on sq.
//
// Attacks are based only on piece movement and board occupancy. They are
// independent of the side to move, check, pins and other legal-move
// restrictions.
//
// For sliding pieces, the first occupied square in each direction is included,
// regardless of the occupying piece's color, and squares beyond it are excluded.
//
// An empty or invalid source square returns an empty slice.
func (b *Board) AttacksFrom(sq Square) []Square {
	if b == nil || sq < A1 || sq > H8 {
		return nil
	}

	piece := b.Piece(sq)
	if piece == NoPiece {
		return nil
	}

	attacks := bbForPieceAttacks(b, piece, sq)

	result := make([]Square, 0, bits.OnesCount64(uint64(attacks)))
	for index := range numOfSquaresInBoard {
		target := Square(index)
		if attacks.Occupied(target) {
			result = append(result, target)
		}
	}

	return result
}

func bbForPieceAttacks(board *Board, piece Piece, sq Square) bitboard {
	occupied := ^board.emptySqs

	switch piece.Type() {
	case Pawn:
		return pawnAttacksFrom(piece.Color(), sq)

	case Knight:
		return bbKnightMoves[sq]

	case Bishop:
		return diaAttack(occupied, sq)

	case Rook:
		return hvAttack(occupied, sq)

	case Queen:
		return diaAttack(occupied, sq) |
			hvAttack(occupied, sq)

	case King:
		return bbKingMoves[sq]

	default:
		return 0
	}
}

func pawnAttacksFrom(color Color, sq Square) bitboard {
	source := bbForSquare(sq)

	if color == White {
		captureRight := (source & ^bbFileH & ^bbRank8) >> 9
		captureLeft := (source & ^bbFileA & ^bbRank8) >> 7

		return captureRight | captureLeft
	}

	captureRight := (source & ^bbFileH & ^bbRank1) << 7
	captureLeft := (source & ^bbFileA & ^bbRank1) << 9

	return captureRight | captureLeft
}
