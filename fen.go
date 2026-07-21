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
	setup, err := decodeFENSetup(fen)
	if err != nil {
		return nil, err
	}
	pos, err := NewPosition(setup)
	if err != nil {
		return nil, fmt.Errorf("%w: %w", ErrInvalidFEN, err)
	}
	return pos, nil
}

// decodeFENUnsafe parses a FEN string and constructs a Position without
// semantic validation. It exists for internal tests and legacy callers that
// need to create positions with arbitrary or incomplete board states.
func decodeFENUnsafe(fen string) (*Position, error) {
	setup, err := decodeFENSetup(fen)
	if err != nil {
		return nil, err
	}
	pos := &Position{
		board:           setup.Board,
		turn:            setup.Turn,
		castleRights:    setup.CastleRights,
		enPassantSquare: setup.EnPassant,
		halfMoveClock:   setup.HalfMoveClock,
		moveCount:       setup.FullMoveNo,
		variant:         setup.Variant,
	}
	pos.inCheck, pos.checkers = checkState(pos)
	pos.hash = pos.computeHash()
	return pos, nil
}

// decodeFENForHash parses a FEN string and returns a Position whose Zobrist
// hash is correct but whose inCheck/checkers fields are not populated. It is
// used by HashFromFEN to avoid the attack-set scan that full position
// construction requires.
func decodeFENForHash(fen string) (*Position, error) {
	setup, err := decodeFENSetup(fen)
	if err != nil {
		return nil, err
	}
	pos := &Position{
		board:           setup.Board,
		turn:            setup.Turn,
		castleRights:    setup.CastleRights,
		enPassantSquare: setup.EnPassant,
		halfMoveClock:   setup.HalfMoveClock,
		moveCount:       setup.FullMoveNo,
		variant:         setup.Variant,
	}
	pos.hash = pos.computeHash()
	return pos, nil
}

// decodeFENSetup parses a FEN string into a Setup. It performs only syntactic
// validation; semantic validation is left to NewPosition.
//
// The parser is relaxed, matching the de-facto behavior of widely deployed
// FEN producers (shakmaty, lichess, SCID, chess.com):
//
//   - Fields may be separated by spaces, tabs, or underscores ('_').
//   - Missing fields (except the board) are filled with the defaults from
//     `8/8/8/8/8/8/8/8 w - - 0 1`: White to move, no castling rights, no en
//     passant square, halfmove clock 0, fullmove number 1.
//   - A fullmove number of 0 is accepted and treated as 1.
//
// Every returned error wraps ErrInvalidFEN so callers can branch with
// errors.Is without inspecting message text.
func decodeFENSetup(fen string) (Setup, error) {
	const maxFENParts = 6
	fen = strings.TrimSpace(fen)
	parts := strings.FieldsFunc(fen, func(r rune) bool {
		return r == ' ' || r == '\t' || r == '_'
	})

	if len(parts) < 1 || len(parts) > maxFENParts {
		return Setup{}, fmt.Errorf("chess: fen expects 1-6 fields, got %d: %w", len(parts), ErrInvalidFEN)
	}
	b, err := fenBoard(parts[0])
	if err != nil {
		return Setup{}, fmt.Errorf("chess: fen: board: %w: %w", err, ErrInvalidFEN)
	}

	// Defaults for missing fields, matching shakmaty's `8/8/8/8/8/8/8/8 w - - 0 1`.
	turn := White
	rights := CastleRights{}
	sq := NoSquare
	halfMoveClock := 0
	moveCount := 1
	variant := Standard

	if len(parts) > 1 {
		var ok bool
		turn, ok = fenTurnMap[parts[1]]
		if !ok {
			return Setup{}, fmt.Errorf("chess: fen invalid turn %q: %w", parts[1], ErrInvalidFEN)
		}
	}
	if len(parts) > 2 {
		rights, variant, err = formCastleRightsWithVariant(parts[2], b)
		if err != nil {
			return Setup{}, fmt.Errorf("chess: fen: castle rights: %w: %w", err, ErrInvalidFEN)
		}
	}
	if len(parts) > 3 {
		sq, err = formEnPassant(parts[3])
		if err != nil {
			return Setup{}, fmt.Errorf("chess: fen: en passant: %w: %w", err, ErrInvalidFEN)
		}
	}
	if len(parts) > 4 {
		halfMoveClock, err = strconv.Atoi(parts[4])
		if err != nil || halfMoveClock < 0 {
			return Setup{}, fmt.Errorf("chess: fen invalid half move clock %q: %w", parts[4], ErrInvalidFEN)
		}
	}
	if len(parts) > 5 {
		moveCount, err = strconv.Atoi(parts[5])
		if err != nil || moveCount < 0 {
			return Setup{}, fmt.Errorf("chess: fen invalid move count %q: %w", parts[5], ErrInvalidFEN)
		}
		if moveCount == 0 {
			moveCount = 1
		}
	}

	return Setup{
		Board:         *b,
		Turn:          turn,
		CastleRights:  rights,
		EnPassant:     sq,
		HalfMoveClock: halfMoveClock,
		FullMoveNo:    moveCount,
		Variant:       variant,
	}, nil
}

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
		New: func() any {
			return make(map[Square]Piece, pieceMapSize)
		},
	}

	// pool for the file map (8 pieces per rank max)
	//note: this is a sync.Pool
	//nolint:gochecknoglobals // this is a pool.
	fileMapPool = sync.Pool{
		New: func() any {
			return make(map[File]Piece, fileMapSize)
		},
	}
)

func getPieceMap() map[Square]Piece {
	m, ok := pieceMapPool.Get().(map[Square]Piece)
	if !ok || m == nil {
		return make(map[Square]Piece, pieceMapSize)
	}
	return m
}

func getFileMap() map[File]Piece {
	m, ok := fileMapPool.Get().(map[File]Piece)
	if !ok || m == nil {
		return make(map[File]Piece, fileMapSize)
	}
	return m
}

// fenBoard generates board from FEN format while minimizing allocations.
func fenBoard(boardStr string) (*Board, error) {
	const maxRankLen = 8

	// Local preallocated array to avoid strings.Split allocations.
	// Kept local (instead of global) so fenBoard is concurrency-safe.
	var rankBuffer [maxRankLen]string

	// Get maps from pools
	m := getPieceMap()
	fileMap := getFileMap()

	// Clear maps (in case they were reused)
	clear(m)
	clear(fileMap)

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
		clear(fileMap)

		// Parse rank into reused map
		if err := fenFormRank(rankBuffer[i], fileMap); err != nil {
			return nil, fmt.Errorf("chess: fen: rank %d: %w", 8-i, err)
		}

		// Transfer pieces to main map
		for file, piece := range fileMap {
			m[NewSquare(file, rank)] = piece
		}
	}

	// Create new board with the pooled map
	// Note: NewBoard must copy the map since we're returning m to the pool
	board, err := NewBoard(m)
	if err != nil {
		return nil, fmt.Errorf("chess: fen: board construction: %w", err)
	}
	return board, nil
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
	if castleStr == "-" {
		return CastleRights{}, nil
	}
	var seen [256]bool
	var cr CastleRights
	for i := range castleStr {
		c := castleStr[i]
		switch c {
		case 'K':
			if seen[c] {
				return CastleRights{}, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
			}
			seen[c] = true
			cr.White.KingSide = true
		case 'Q':
			if seen[c] {
				return CastleRights{}, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
			}
			seen[c] = true
			cr.White.QueenSide = true
		case 'k':
			if seen[c] {
				return CastleRights{}, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
			}
			seen[c] = true
			cr.Black.KingSide = true
		case 'q':
			if seen[c] {
				return CastleRights{}, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
			}
			seen[c] = true
			cr.Black.QueenSide = true
		default:
			return CastleRights{}, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
		}
	}
	return cr, nil
}

// formCastleRightsWithVariant parses a FEN castling-rights field with
// Chess960 awareness. It accepts the standard KQkq letters, Shredder-FEN and
// X-FEN file letters (uppercase A-H = white rook files, lowercase a-h = black
// rook files), and any X-FEN mix of the two. File letters are mapped to king
// or queen side using each color's king file on its back rank. The presence of
// any file letter implies the Chess960 variant; a pure KQkq (or "-") field is
// Standard. The board is required to resolve file letters.
func formCastleRightsWithVariant(castleStr string, board *Board) (CastleRights, Variant, error) {
	if castleStr == "-" {
		return CastleRights{}, Standard, nil
	}
	if len(castleStr) > 8 {
		return CastleRights{}, Standard, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
	}
	var (
		cr            CastleRights
		variant       = Standard
		seen          = [256]bool{}
		whiteKingFile = backRankKingFile(board, White)
		blackKingFile = backRankKingFile(board, Black)
	)
	for i := range castleStr {
		c := castleStr[i]
		if seen[c] {
			return CastleRights{}, Standard, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
		}
		seen[c] = true
		switch {
		case c == 'K':
			cr.White.KingSide = true
		case c == 'Q':
			cr.White.QueenSide = true
		case c == 'k':
			cr.Black.KingSide = true
		case c == 'q':
			cr.Black.QueenSide = true
		case c >= 'A' && c <= 'H':
			variant = Chess960
			if err := setRookRight(&cr, board, White, int(c-'A'), whiteKingFile, castleStr); err != nil {
				return CastleRights{}, Standard, err
			}
		case c >= 'a' && c <= 'h':
			variant = Chess960
			if err := setRookRight(&cr, board, Black, int(c-'a'), blackKingFile, castleStr); err != nil {
				return CastleRights{}, Standard, err
			}
		default:
			return CastleRights{}, Standard, fmt.Errorf("chess: fen invalid castle rights %s", castleStr)
		}
	}
	return cr, variant, nil
}

// backRankKingFile returns the file (0-7) of the given color's king on its back
// rank, or -1 if it is not there. Chess960 castling rights name rooks by file,
// so the king's file is needed to decide king side versus queen side.
func backRankKingFile(board *Board, c Color) int {
	var rank Rank
	var king Piece
	if c == White {
		rank, king = Rank1, WhiteKing
	} else {
		rank, king = Rank8, BlackKing
	}
	for f := range 8 {
		if board.Piece(NewSquare(File(f), rank)) == king {
			return f
		}
	}
	return -1
}

// setRookRight maps a Chess960 rook file to the king or queen side based on its
// position relative to the king's file: a rook to the king's right (higher
// file) grants the king side; to the left (lower file) grants the queen side.
// The Shredder letter names a specific rook file, so a friendly rook must
// actually occupy that file on the back rank; a malformed FEN whose rights
// claim a file with no rook is rejected here (the alternative — silently
// dropping the right — would corrupt the position's round-trip).
func setRookRight(cr *CastleRights, board *Board, c Color, rookFile, kingFile int, src string) error {
	if kingFile < 0 {
		return fmt.Errorf("chess: fen castle rights %s: %s king not on back rank", src, c.Name())
	}
	if rookFile == kingFile {
		return fmt.Errorf("chess: fen castle rights %s: rook on king's file", src)
	}
	var (
		rank Rank
		rook Piece
	)
	if c == White {
		rank, rook = Rank1, WhiteRook
	} else {
		rank, rook = Rank8, BlackRook
	}
	if board.Piece(NewSquare(File(rookFile), rank)) != rook {
		return fmt.Errorf("chess: fen castle rights %s: no %s rook on file %d", src, c.Name(), rookFile)
	}
	if rookFile > kingFile {
		if c == White {
			cr.White.KingSide = true
		} else {
			cr.Black.KingSide = true
		}
	} else {
		if c == White {
			cr.White.QueenSide = true
		} else {
			cr.Black.QueenSide = true
		}
	}
	return nil
}

// appendCastleRights appends the FEN castling-rights field to buf. Standard
// positions emit the KQkq form; Chess960 positions emit Shredder-FEN file
// letters (uppercase for white rooks, lowercase for black), one per held
// right, in the canonical KQkq positional order, or "-" when no side can
// castle. The board is required to locate each Chess960 rook's file.
func appendCastleRights(buf []byte, cr CastleRights, board *Board, variant Variant) []byte {
	if variant == Standard {
		return append(buf, cr.String()...)
	}
	start := len(buf)
	whiteKing := backRankKingFile(board, White)
	blackKing := backRankKingFile(board, Black)
	if cr.CanCastle(White, KingSide) {
		if f := rookFileForSide(board, White, whiteKing, KingSide); f >= 0 {
			buf = append(buf, byte('A'+f))
		}
	}
	if cr.CanCastle(White, QueenSide) {
		if f := rookFileForSide(board, White, whiteKing, QueenSide); f >= 0 {
			buf = append(buf, byte('A'+f))
		}
	}
	if cr.CanCastle(Black, KingSide) {
		if f := rookFileForSide(board, Black, blackKing, KingSide); f >= 0 {
			buf = append(buf, byte('a'+f))
		}
	}
	if cr.CanCastle(Black, QueenSide) {
		if f := rookFileForSide(board, Black, blackKing, QueenSide); f >= 0 {
			buf = append(buf, byte('a'+f))
		}
	}
	if len(buf) == start {
		buf = append(buf, '-')
	}
	return buf
}

// rookFileForSide returns the file (0-7) of the rook that grants the given
// castling side for color c: the nearest friendly rook on the back rank to the
// king's right for KingSide, or to the king's left for QueenSide, or -1 if the
// king is not on its back rank or no such rook exists.
func rookFileForSide(board *Board, c Color, kingFile int, side Side) int {
	if kingFile < 0 {
		return -1
	}
	var rank Rank
	var rook Piece
	if c == White {
		rank, rook = Rank1, WhiteRook
	} else {
		rank, rook = Rank8, BlackRook
	}
	switch side {
	case KingSide:
		for f := kingFile + 1; f < 8; f++ {
			if board.Piece(NewSquare(File(f), rank)) == rook {
				return f
			}
		}
	case QueenSide:
		for f := kingFile - 1; f >= 0; f-- {
			if board.Piece(NewSquare(File(f), rank)) == rook {
				return f
			}
		}
	}
	return -1
}

func formEnPassant(enPassant string) (Square, error) {
	if enPassant == "-" {
		return NoSquare, nil
	}
	sq, err := ParseSquare(enPassant)
	if err != nil || (sq.Rank() != Rank3 && sq.Rank() != Rank6) {
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
