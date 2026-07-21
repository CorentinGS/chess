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

// EmptySetup returns a Setup representing an empty board: no pieces, White to
// move, no castling rights, no en passant square, halfmove clock 0, fullmove 1.
// It is the zero position used as a starting point for construction.
func EmptySetup() Setup {
	return Setup{Turn: White, EnPassant: NoSquare, FullMoveNo: 1}
}

// InitialSetup returns a Setup representing the standard chess starting
// position. The returned value is a copy and safe for callers to mutate.
func InitialSetup() Setup {
	return startingSetup
}

// SwapTurn returns a copy of s with the side to move flipped and the en
// passant square cleared. Halfmove clock and fullmove number are preserved.
// This is the FIDE "pass" semantic used for repetition detection: only the
// side to move and en passant rights change, not the move counters.
func (s Setup) SwapTurn() Setup {
	return Setup{
		Board:         s.Board,
		Turn:          s.Turn.Other(),
		CastleRights:  s.CastleRights,
		EnPassant:     NoSquare,
		HalfMoveClock: s.HalfMoveClock,
		FullMoveNo:    s.FullMoveNo,
	}
}

// Mirror returns a vertically mirrored copy of s: every piece swaps color and
// moves to the square on the same file but the opposite rank (rank 1 ↔ rank 8);
// the side to move flips; castling rights swap color; the en passant square
// moves to the opposite rank. Halfmove clock and fullmove number are preserved.
//
// Useful for opening-book authors who want to reuse one side's analysis for
// the other, and for symmetric position testing. Mirroring a legal standard
// position always produces a legal standard position.
func (s Setup) Mirror() Setup {
	m := make(map[Square]Piece, numOfSquaresInBoard)
	for sq := range numOfSquaresInBoard {
		p := s.Board.Piece(Square(sq))
		if p == NoPiece {
			continue
		}
		file := Square(sq).File()
		rank := 7 - Square(sq).Rank()
		m[NewSquare(file, rank)] = NewPiece(p.Type(), p.Color().Other())
	}
	b, err := NewBoard(m)
	if err != nil {
		// Mirror of a valid board cannot produce an invalid board: every
		// occupied square maps to a distinct occupied square.
		panic(fmt.Sprintf("chess: mirror produced invalid board: %v", err))
	}
	cr := NewCastleRights(
		s.CastleRights.CanCastle(Black, KingSide),
		s.CastleRights.CanCastle(Black, QueenSide),
		s.CastleRights.CanCastle(White, KingSide),
		s.CastleRights.CanCastle(White, QueenSide),
	)
	ep := NoSquare
	if s.EnPassant != NoSquare {
		ep = NewSquare(s.EnPassant.File(), 7-s.EnPassant.Rank())
	}
	return Setup{
		Board:         *b,
		Turn:          s.Turn.Other(),
		CastleRights:  cr,
		EnPassant:     ep,
		HalfMoveClock: s.HalfMoveClock,
		FullMoveNo:    s.FullMoveNo,
	}
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
