package chess

import (
	"errors"
	"fmt"
	"math/bits"
)

// Setup is an unvalidated description of a chess position. It separates
// "describing a position" from "playing a position" by letting callers build a
// complete position representation and then asking NewPosition to validate it.
type Setup struct {
	Board         Board
	Turn          Color
	CastleRights  CastleRights
	EnPassant     Square
	HalfMoveClock int
	FullMoveNo    int
}

// startingSetup is the setup that corresponds to the standard starting
// position. It is kept unexported because callers should normally use
// StartingPosition(); it exists primarily for tests of NewPosition.
var startingSetup = func() Setup {
	b, err := fenBoard("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
	if err != nil {
		panic(err)
	}
	return Setup{
		Board:         *b,
		Turn:          White,
		CastleRights:  NewCastleRights(true, true, true, true),
		EnPassant:     NoSquare,
		HalfMoveClock: 0,
		FullMoveNo:    1,
	}
}()

// NewPosition validates s and constructs a playable Position from it.
//
// Validation covers:
//   - exactly one king of each color
//   - no pawns on the first or eighth rank
//   - castling rights that match the king and rook placement on their starting
//     squares
//   - an en passant square that is consistent with a pawn that just moved two
//     squares
//   - non-negative halfmove clock and positive fullmove number
func NewPosition(s Setup) (*Position, error) {
	if err := validateSetup(&s); err != nil {
		return nil, err
	}

	pos := &Position{
		board:           s.Board,
		turn:            s.Turn,
		castleRights:    s.CastleRights,
		enPassantSquare: s.EnPassant,
		halfMoveClock:   s.HalfMoveClock,
		moveCount:       s.FullMoveNo,
	}
	pos.inCheck, pos.checkers = checkState(pos)
	pos.hash = pos.computeHash()
	return pos, nil
}

func validateSetup(s *Setup) error {
	if s.HalfMoveClock < 0 {
		return errors.New("chess: setup: halfmove clock cannot be negative")
	}
	if s.FullMoveNo < 1 {
		return errors.New("chess: setup: fullmove number must be at least 1")
	}

	whiteKings := bits.OnesCount64(uint64(s.Board.bbWhiteKing))
	blackKings := bits.OnesCount64(uint64(s.Board.bbBlackKing))
	if whiteKings != 1 {
		return fmt.Errorf("chess: setup: expected exactly one white king, got %d", whiteKings)
	}
	if blackKings != 1 {
		return fmt.Errorf("chess: setup: expected exactly one black king, got %d", blackKings)
	}

	for file := FileA; file <= FileH; file++ {
		if p := s.Board.Piece(NewSquare(file, Rank1)); p.Type() == Pawn {
			return fmt.Errorf("chess: setup: pawn on %s", NewSquare(file, Rank1))
		}
		if p := s.Board.Piece(NewSquare(file, Rank8)); p.Type() == Pawn {
			return fmt.Errorf("chess: setup: pawn on %s", NewSquare(file, Rank8))
		}
	}

	if s.CastleRights.CanCastle(White, KingSide) &&
		(s.Board.Piece(E1) != WhiteKing || s.Board.Piece(H1) != WhiteRook) {
		return errors.New("chess: setup: white kingside castling rights inconsistent with board")
	}
	if s.CastleRights.CanCastle(White, QueenSide) &&
		(s.Board.Piece(E1) != WhiteKing || s.Board.Piece(A1) != WhiteRook) {
		return errors.New("chess: setup: white queenside castling rights inconsistent with board")
	}
	if s.CastleRights.CanCastle(Black, KingSide) &&
		(s.Board.Piece(E8) != BlackKing || s.Board.Piece(H8) != BlackRook) {
		return errors.New("chess: setup: black kingside castling rights inconsistent with board")
	}
	if s.CastleRights.CanCastle(Black, QueenSide) &&
		(s.Board.Piece(E8) != BlackKing || s.Board.Piece(A8) != BlackRook) {
		return errors.New("chess: setup: black queenside castling rights inconsistent with board")
	}

	if s.EnPassant != NoSquare {
		switch s.Turn {
		case White:
			if s.EnPassant.Rank() != Rank6 {
				return fmt.Errorf("chess: setup: en passant square %s inconsistent with side to move White", s.EnPassant)
			}
			if s.Board.Piece(s.EnPassant-8) != BlackPawn {
				return fmt.Errorf("chess: setup: no black pawn on %s for en passant %s", s.EnPassant-8, s.EnPassant)
			}
		case Black:
			if s.EnPassant.Rank() != Rank3 {
				return fmt.Errorf("chess: setup: en passant square %s inconsistent with side to move Black", s.EnPassant)
			}
			if s.Board.Piece(s.EnPassant+8) != WhitePawn {
				return fmt.Errorf("chess: setup: no white pawn on %s for en passant %s", s.EnPassant+8, s.EnPassant)
			}
		}
	}

	return nil
}
