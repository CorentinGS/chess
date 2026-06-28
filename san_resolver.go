package chess

import (
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

func resolveSANCastle(pos *Position, castle string) (Move, error) {
	var castles [2]Move
	count := castleMovesInto(pos, &castles, generateLegalAnnotated)
	for i := range count {
		m := castles[i]
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
	return fmt.Errorf("chess: no legal move found for position")
}

func squareFromFileRank(file, rank byte) Square {
	if file < 'a' || file > 'h' || rank < '1' || rank > '8' {
		return NoSquare
	}
	return Square((file - 'a') + (rank-'1')*8)
}
