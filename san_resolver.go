package chess

import (
	"errors"
	"fmt"
	"strings"
)

type sanMoveData struct {
	castle     string
	piece      PieceType
	originFile string
	originRank string
	dest       Square
	capture    bool
	promotion  PieceType
	canonical  bool
}

func resolveSANMove(pos *Position, data sanMoveData) (Move, error) {
	if data.castle != "" {
		return resolveSANCastle(pos, data.castle)
	}
	if data.dest == NoSquare {
		return Move{}, errorsInvalidSANMove()
	}

	if matched, ok := resolveSANMoveDirect(pos, data); ok {
		return matched, nil
	}

	var mismatchReasons []string
	originFile := byte(0)
	if data.originFile != "" {
		originFile = data.originFile[0]
	}
	originRank := int8(-1)
	if data.originRank != "" {
		originRank = int8(data.originRank[0] - '1')
	}

	var matched Move
	matchedMove := false
	visitLegalMoves(pos, generateLegalAnnotated, func(m Move) bool {
		if m.S2() != data.dest {
			return false
		}

		piece := pos.Board().Piece(m.S1())
		if piece.Type() != data.piece {
			mismatchReasons = append(mismatchReasons, "piece type mismatch")
			return false
		}
		if originFile != 0 && m.S1().File().Byte() != originFile {
			mismatchReasons = append(mismatchReasons, "origin file mismatch")
			return false
		}
		if originRank >= 0 && int8(m.S1().Rank()) != originRank {
			mismatchReasons = append(mismatchReasons, fmt.Sprintf("origin rank mismatch: %d", m.S1()/8+1))
			return false
		}
		if data.canonical && data.piece != Pawn && data.originFile+data.originRank != formS1(pos, m) {
			mismatchReasons = append(mismatchReasons, "disambiguation mismatch")
			return false
		}

		moveCaptures := m.HasTag(Capture) || m.HasTag(EnPassant)
		if data.capture != moveCaptures {
			mismatchReasons = append(mismatchReasons, "capture mismatch")
			return false
		}
		if data.promotion != NoPieceType && m.promo != data.promotion {
			mismatchReasons = append(mismatchReasons, "promotion mismatch")
			return false
		}

		matched = m
		matchedMove = true
		return true
	})

	if matchedMove {
		return matched, nil
	}
	if len(mismatchReasons) > 0 {
		return Move{}, fmt.Errorf("chess: no legal move found for position: %s", strings.Join(mismatchReasons, "; "))
	}
	return Move{}, errorsInvalidSANMove()
}

func resolveSANMoveDirect(pos *Position, data sanMoveData) (Move, bool) {
	var origins [16]Square
	count := sanCandidateOrigins(pos, data, &origins)
	if count == 0 {
		return Move{}, false
	}

	originFile := byte(0)
	if data.originFile != "" {
		originFile = data.originFile[0]
	}
	originRank := int8(-1)
	if data.originRank != "" {
		originRank = int8(data.originRank[0] - '1')
	}

	for i := range count {
		s1 := origins[i]
		if originFile != 0 && s1.File().Byte() != originFile {
			continue
		}
		if originRank >= 0 && int8(s1.Rank()) != originRank {
			continue
		}

		p := pos.board.Piece(s1)
		if p.Type() != data.piece || p.Color() != pos.turn {
			continue
		}

		m := Move{s1: s1, s2: data.dest, promo: data.promotion}
		if data.piece == Pawn && data.promotion == NoPieceType && pawnPromotes(pos.turn, data.dest) {
			continue
		}
		if data.piece != Pawn && data.promotion != NoPieceType {
			continue
		}

		tags := moveTagsForPiece(m, pos, generateLegalAnnotated, p, false)
		if tags&inCheck != 0 {
			continue
		}
		m.tags = tags

		moveCaptures := m.HasTag(Capture) || m.HasTag(EnPassant)
		if data.capture != moveCaptures {
			continue
		}
		if data.canonical && data.piece != Pawn && data.originFile+data.originRank != formS1(pos, m) {
			continue
		}

		return m, true
	}
	return Move{}, false
}

func sanCandidateOrigins(pos *Position, data sanMoveData, origins *[16]Square) int {
	switch data.piece {
	case Pawn:
		return sanPawnCandidateOrigins(pos, data, origins)
	case Knight:
		return sanOriginsFromBitboard(pos.board.bbForPiece(NewPiece(Knight, pos.turn))&bbKnightMoves[data.dest], origins)
	case Bishop:
		occ := ^pos.board.emptySqs
		return sanOriginsFromBitboard(pos.board.bbForPiece(NewPiece(Bishop, pos.turn))&diaAttack(occ, data.dest), origins)
	case Rook:
		occ := ^pos.board.emptySqs
		return sanOriginsFromBitboard(pos.board.bbForPiece(NewPiece(Rook, pos.turn))&hvAttack(occ, data.dest), origins)
	case Queen:
		occ := ^pos.board.emptySqs
		return sanOriginsFromBitboard(pos.board.bbForPiece(NewPiece(Queen, pos.turn))&(diaAttack(occ, data.dest)|hvAttack(occ, data.dest)), origins)
	case King:
		return sanOriginsFromBitboard(pos.board.bbForPiece(NewPiece(King, pos.turn))&bbKingMoves[data.dest], origins)
	default:
		return 0
	}
}

func sanOriginsFromBitboard(bb bitboard, origins *[16]Square) int {
	count := 0
	for bits := bb; bits != 0; bits &= bits - 1 {
		origins[count] = squareFromBit(bits & -bits)
		count++
	}
	return count
}

func sanPawnCandidateOrigins(pos *Position, data sanMoveData, origins *[16]Square) int {
	count := 0
	dest := data.dest
	pawns := pos.board.bbForPiece(NewPiece(Pawn, pos.turn))
	add := func(sq Square) {
		if sq < A1 || sq > H8 {
			return
		}
		if pawns&bbForSquare(sq) == 0 {
			return
		}
		origins[count] = sq
		count++
	}

	if pos.turn == White {
		if data.capture {
			if dest.File() != FileA {
				add(dest - 9)
			}
			if dest.File() != FileH {
				add(dest - 7)
			}
			return count
		}
		if dest.Rank() > Rank1 {
			one := dest - 8
			if !pos.board.isOccupied(one) {
				add(one)
				if dest.Rank() == Rank4 {
					two := dest - 16
					if !pos.board.isOccupied(two) {
						add(two)
					}
				}
			}
		}
		return count
	}

	if data.capture {
		if dest.File() != FileA {
			add(dest + 7)
		}
		if dest.File() != FileH {
			add(dest + 9)
		}
		return count
	}
	if dest.Rank() < Rank8 {
		one := dest + 8
		if !pos.board.isOccupied(one) {
			add(one)
			if dest.Rank() == Rank5 {
				two := dest + 16
				if !pos.board.isOccupied(two) {
					add(two)
				}
			}
		}
	}
	return count
}

func pawnPromotes(turn Color, dest Square) bool {
	return (turn == White && dest.Rank() == Rank8) || (turn == Black && dest.Rank() == Rank1)
}

func resolveSANCastle(pos *Position, castle string) (Move, error) {
	var castles [2]Move
	count := castleMovesInto(pos, &castles, generateLegalAnnotated)
	for _, m := range castles[:count] {
		if castle == castleKS && m.HasTag(KingSideCastle) {
			return m, nil
		}
		if castle == castleQS && m.HasTag(QueenSideCastle) {
			return m, nil
		}
	}
	return Move{}, errorsInvalidSANMove()
}

func errorsInvalidSANMove() error {
	return errors.New("chess: no legal move found for position")
}

func squareFromFileRank(file, rank byte) Square {
	if file < 'a' || file > 'h' || rank < '1' || rank > '8' {
		return NoSquare
	}
	return Square((file - 'a') + (rank-'1')*8)
}
