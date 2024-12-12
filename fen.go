package chess

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"sync"
)

// Decodes FEN notation into a GameState.  An error is returned
// if there is a parsing error.  FEN notation format:
// rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1.
func decodeFEN(fen string) (*Position, error) {
	const minFENParts = 6
	fen = strings.TrimSpace(fen)
	parts := strings.Split(fen, " ")

	if len(parts) != minFENParts {
		return nil, errors.New("chess: fen invalid format")
	}
	b, err := fenBoard(parts[0])
	if err != nil {
		return nil, err
	}
	turn, ok := fenTurnMap[parts[1]]
	if !ok {
		return nil, errors.New("chess: fen invalid turn")
	}
	rights, err := formCastleRights(parts[2])
	if err != nil {
		return nil, err
	}
	sq, err := formEnPassant(parts[3])
	if err != nil {
		return nil, err
	}
	halfMoveClock, err := strconv.Atoi(parts[4])
	if err != nil || halfMoveClock < 0 {
		return nil, errors.New("chess: fen invalid half move clock")
	}
	moveCount, err := strconv.Atoi(parts[5])
	if err != nil || moveCount < 1 {
		return nil, errors.New("chess: fen invalid move count")
	}
	return &Position{
		board:           b,
		turn:            turn,
		castleRights:    rights,
		enPassantSquare: sq,
		halfMoveClock:   halfMoveClock,
		moveCount:       moveCount,
	}, nil
}

// preallocated array to avoid strings.Split allocation
//
//nolint:gochecknoglobals // this is a preallocated array.
var rankBuffer [8]string

const (
	fileMapSize  = 8
	pieceMapSize = 32
)

// pools for map reuse.
var (
	// pool for the main piece map (32 pieces max)
	//note: this is a sync.Pool
	//nolint:gochecknoglobals // this is a pool.
	pieceMapPool = sync.Pool{
		New: func() interface{} {
			return make(map[Square]Piece, pieceMapSize)
		},
	}

	// pool for the file map (8 pieces per rank max)
	//note: this is a sync.Pool
	//nolint:gochecknoglobals // this is a pool.
	fileMapPool = sync.Pool{
		New: func() interface{} {
			return make(map[File]Piece, fileMapSize)
		},
	}
)

// clearMap helper to clear a map without deallocating.
func clearMap[K comparable, V any](m map[K]V) {
	for k := range m {
		delete(m, k)
	}
}

// fenBoard generates board from FEN format while minimizing allocations.
func fenBoard(boardStr string) (*Board, error) {
	const maxRankLen = 8

	// Get maps from pools
	m, _ := pieceMapPool.Get().(map[Square]Piece)
	fileMap, _ := fileMapPool.Get().(map[File]Piece)

	// Clear maps (in case they were reused)
	clearMap(m)
	clearMap(fileMap)

	// Ensure maps are returned to pools on exit
	defer func() {
		pieceMapPool.Put(m)
		fileMapPool.Put(fileMap)
	}()

	// Split string into ranks without allocation
	rankCount := 0
	start := 0
	for i := range len(boardStr) {
		if boardStr[i] == '/' {
			if rankCount >= maxRankLen {
				return nil, errors.New("chess: fen invalid board")
			}
			rankBuffer[rankCount] = boardStr[start:i]
			rankCount++
			start = i + 1
		}
	}

	// Handle last rank
	if start < len(boardStr) {
		if rankCount >= maxRankLen {
			return nil, errors.New("chess: fen invalid board")
		}
		rankBuffer[rankCount] = boardStr[start:]
		rankCount++
	}

	if rankCount != maxRankLen {
		return nil, errors.New("chess: fen invalid board")
	}

	for i := range maxRankLen {
		rank := Rank(7 - i)

		// Clear fileMap for reuse
		clearMap(fileMap)

		// Parse rank into reused map
		if err := fenFormRank(rankBuffer[i], fileMap); err != nil {
			return nil, err
		}

		// Transfer pieces to main map
		for file, piece := range fileMap {
			m[NewSquare(file, rank)] = piece
		}
	}

	// Create new board with the pooled map
	// Note: NewBoard must copy the map since we're returning m to the pool
	return NewBoard(m), nil
}

// fenFormRank converts a FEN rank string to a map of pieces, reusing the provided map.
func fenFormRank(rankStr string, m map[File]Piece) error {
	const maxRankLen = 8
	var count int

	for i := range len(rankStr) {
		c := rankStr[i]

		// Handle empty squares (digits 1-8)
		if c >= '1' && c <= '8' {
			count += int(c - '0')
			continue
		}

		// Get piece from lookup table
		piece := fenCharToPiece[c]
		if piece == NoPiece {
			return errors.New("chess: fen invalid piece")
		}

		m[File(count)] = piece
		count++
	}

	if count != maxRankLen {
		return errors.New("chess: invalid rank length")
	}

	return nil
}

func formCastleRights(castleStr string) (CastleRights, error) {
	// check for duplicates aka. KKkq right now is valid
	for _, s := range []string{"K", "Q", "k", "q", "-"} {
		if strings.Count(castleStr, s) > 1 {
			return "-", fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
		}
	}
	for _, r := range castleStr {
		c := fmt.Sprintf("%c", r)
		switch c {
		case "K", "Q", "k", "q", "-":
		default:
			return "-", fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
		}
	}
	return CastleRights(castleStr), nil
}

func formEnPassant(enPassant string) (Square, error) {
	if enPassant == "-" {
		return NoSquare, nil
	}
	sq := strToSquareMap[enPassant]
	if sq == NoSquare || !(sq.Rank() == Rank3 || sq.Rank() == Rank6) {
		return NoSquare, fmt.Errorf("chess: fen invalid En Passant square %s", enPassant)
	}
	return sq, nil
}

var (
	// whitePiecesToFEN provides direct mapping for white pieces to FEN characters
	//nolint:gochecknoglobals // this is a lookup table.
	whitePiecesToFEN = [7]byte{
		0,   // NoType (index 0)
		'K', // King   (index 1)
		'Q', // Queen  (index 2)
		'R', // Rook   (index 3)
		'B', // Bishop (index 4)
		'N', // Knight (index 5)
		'P', // Pawn   (index 6)
	}

	// blackPiecesToFEN provides direct mapping for black pieces to FEN characters
	//nolint:gochecknoglobals // this is a lookup table.
	blackPiecesToFEN = [7]byte{
		0,   // NoType (index 0)
		'k', // King   (index 1)
		'q', // Queen  (index 2)
		'r', // Rook   (index 3)
		'b', // Bishop (index 4)
		'n', // Knight (index 5)
		'p', // Pawn   (index 6)
	}

	// fenTurnMap provides direct mapping for FEN characters to colors
	//nolint:gochecknoglobals // this is a lookup table.
	fenTurnMap = map[string]Color{
		"w": White,
		"b": Black,
	}

	// Direct lookup array for FEN characters to pieces
	// Note: NoPiece is used for invalid characters
	//nolint:gochecknoglobals // this is a lookup table.
	fenCharToPiece = [128]Piece{
		'K': WhiteKing,
		'Q': WhiteQueen,
		'R': WhiteRook,
		'B': WhiteBishop,
		'N': WhiteKnight,
		'P': WhitePawn,
		'k': BlackKing,
		'q': BlackQueen,
		'r': BlackRook,
		'b': BlackBishop,
		'n': BlackKnight,
		'p': BlackPawn,
	}
)
