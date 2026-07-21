package chess

import (
	"errors"
	"fmt"
	"math/bits"
)

// Variant identifies the starting-position variant of a position. The zero
// value is Standard. Chess960 (Fischer Random Chess) selects one of 960
// indexed initial back-rank arrangements; its castling rights and rook origins
// differ from standard chess but its castling destinations do not.
type Variant int8

const (
	// Standard is the standard chess starting position.
	Standard Variant = iota
	// Chess960 is the Fischer Random Chess variant (960 indexed starts).
	Chess960
)

// String returns the variant's canonical name.
func (v Variant) String() string {
	switch v {
	case Standard:
		return "Standard"
	case Chess960:
		return "Chess960"
	}
	return "Standard"
}

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
	Variant       Variant
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
		Variant:       s.Variant,
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
		Variant:       s.Variant,
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
		variant:         s.Variant,
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

	if s.Variant == Chess960 {
		if err := validateChess960CastleRights(s); err != nil {
			return err
		}
	} else {
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

// Chess960Setup returns the Setup for the Chess960 starting position with the
// given Scharnagl index (0-959). The index selects one of 960 initial back-rank
// arrangements satisfying the Chess960 constraints: bishops on opposite-colored
// squares and the king placed between the two rooks. Index 518 is the standard
// starting position. The returned Setup has Variant Chess960 and full castling
// rights for both sides.
func Chess960Setup(index int) (Setup, error) {
	if index < 0 || index > 959 {
		return Setup{}, fmt.Errorf("chess: chess960 start index %d out of range [0, 959]", index)
	}

	// Scharnagl decomposition over the eight files of rank 1.
	const (
		lightFiles = "bdfh" // files 1, 3, 5, 7 (opposite color to "aceg")
		darkFiles  = "aceg" // files 0, 2, 4, 6
	)
	var back [8]Piece // white back rank, indexed by file 0..7
	used := [8]bool{}

	n := index

	// First bishop: one of the four light-square files.
	b1 := n % 4
	n /= 4
	f := int(lightFiles[b1] - 'a')
	back[f] = WhiteBishop
	used[f] = true

	// Second bishop: one of the four dark-square files.
	b2 := n % 4
	n /= 4
	f = int(darkFiles[b2] - 'a')
	back[f] = WhiteBishop
	used[f] = true

	// Queen: the n%6-th remaining empty file.
	q := n % 6
	n /= 6
	cnt := 0
	for i := range 8 {
		if !used[i] {
			if cnt == q {
				back[i] = WhiteQueen
				used[i] = true
				break
			}
			cnt++
		}
	}

	// Knights: the n-th pair (0..9) of the five remaining empty files.
	var rem [5]int
	ri := 0
	for i := range 8 {
		if !used[i] {
			rem[ri] = i
			ri++
		}
	}
	pair := 0
	for i := range 4 {
		for j := i + 1; j < 5; j++ {
			if pair == n {
				back[rem[i]] = WhiteKnight
				back[rem[j]] = WhiteKnight
				used[rem[i]] = true
				used[rem[j]] = true
			}
			pair++
		}
	}

	// Remaining three files receive Rook, King, Rook in increasing file order,
	// placing the king between the two rooks.
	var last [3]int
	li := 0
	for i := range 8 {
		if !used[i] {
			last[li] = i
			li++
		}
	}
	back[last[0]] = WhiteRook
	back[last[1]] = WhiteKing
	back[last[2]] = WhiteRook

	// Assemble the full board: white back rank on rank 1, black mirrored on
	// rank 8 (same file, opposite color), pawns on ranks 2 and 7.
	m := make(map[Square]Piece, 32)
	for file := range 8 {
		wp := back[file]
		m[NewSquare(File(file), Rank1)] = wp
		m[NewSquare(File(file), Rank8)] = NewPiece(wp.Type(), Black)
		m[NewSquare(File(file), Rank2)] = WhitePawn
		m[NewSquare(File(file), Rank7)] = BlackPawn
	}
	b, err := NewBoard(m)
	if err != nil {
		return Setup{}, fmt.Errorf("chess: chess960 start %d: %w", index, err)
	}

	return Setup{
		Board:         *b,
		Turn:          White,
		CastleRights:  NewCastleRights(true, true, true, true),
		EnPassant:     NoSquare,
		HalfMoveClock: 0,
		FullMoveNo:    1,
		Variant:       Chess960,
	}, nil
}

// validateChess960CastleRights checks that castling rights are consistent with
// the board for a Chess960 position. A held right requires the side's king on
// its back rank and, for the king side, a rook somewhere to the king's right
// (higher file); for the queen side, a rook to the king's left (lower file).
// Unlike standard chess the king and rooks need not occupy fixed squares.
func validateChess960CastleRights(s *Setup) error {
	for _, c := range []Color{White, Black} {
		var (
			rank      Rank
			kingPiece Piece
			rookPiece Piece
		)
		if c == White {
			rank, kingPiece, rookPiece = Rank1, WhiteKing, WhiteRook
		} else {
			rank, kingPiece, rookPiece = Rank8, BlackKing, BlackRook
		}

		kingSide := s.CastleRights.CanCastle(c, KingSide)
		queenSide := s.CastleRights.CanCastle(c, QueenSide)
		if !kingSide && !queenSide {
			continue
		}

		kingFile := -1
		for f := range 8 {
			if s.Board.Piece(NewSquare(File(f), rank)) == kingPiece {
				kingFile = f
				break
			}
		}
		if kingFile < 0 {
			return fmt.Errorf("chess: setup: %s castling rights require the %s king on its back rank", c.Name(), c.Name())
		}
		if kingSide {
			ok := false
			for f := kingFile + 1; f < 8; f++ {
				if s.Board.Piece(NewSquare(File(f), rank)) == rookPiece {
					ok = true
					break
				}
			}
			if !ok {
				return fmt.Errorf("chess: setup: %s kingside castling rights require a rook to the king's right", c.Name())
			}
		}
		if queenSide {
			ok := false
			for f := kingFile - 1; f >= 0; f-- {
				if s.Board.Piece(NewSquare(File(f), rank)) == rookPiece {
					ok = true
					break
				}
			}
			if !ok {
				return fmt.Errorf("chess: setup: %s queenside castling rights require a rook to the king's left", c.Name())
			}
		}
	}
	return nil
}
