package chess

import (
	"fmt"
	"regexp"
	"strings"
	"sync"
)

// moveComponents represents the parsed components of an algebraic notation move.
type moveComponents struct {
	piece      string
	originFile string
	originRank string
	capture    string
	file       string
	rank       string
	promotes   string
	castles    string
}

// emptyComponents is an empty moveComponents struct
//
//nolint:gochecknoglobals // false positive.
var emptyComponents = moveComponents{}

// pgnRegex is a regular expression to parse PGN strings
//
//nolint:gochecknoglobals // false positive.
var pgnRegex = regexp.MustCompile(`^(?:([RNBQKP]?)([abcdefgh]?)(\d?)(x?)([abcdefgh])(\d)(=[QRBN])?|(O-O(?:-O)?))([+#!?]|e\.p\.)*$`)

const piecesPoolCapacity = 4

// Use string pools for common strings to reduce allocations.
var (
	//nolint:gochecknoglobals // false positive
	stringPool = sync.Pool{
		New: func() interface{} {
			return new(strings.Builder)
		},
	}

	// Pre-allocate slices for options to avoid allocations in hot path
	//nolint:gochecknoglobals // false positive
	pieceOptionsPool = sync.Pool{
		New: func() interface{} {
			s := make([]string, 0, piecesPoolCapacity)
			return &s // Return pointer to slice
		},
	}
)

// Constants for common strings to avoid allocations.
const (
	kingStr    = "K"
	queenStr   = "Q"
	rookStr    = "R"
	bishopStr  = "B"
	knightStr  = "N"
	castleKS   = "O-O"
	castleQS   = "O-O-O"
	equalStr   = "="
	checkStr   = "+"
	mateStr    = "#"
	captureStr = "x"
)

// Pre-allocate piece type maps for faster lookups.
var pieceTypeToChar = map[PieceType]string{
	King:   kingStr,
	Queen:  queenStr,
	Rook:   rookStr,
	Bishop: bishopStr,
	Knight: knightStr,
}

// Encoder is the interface implemented by objects that can
// encode a move into a string given the position.  It is not
// the encoders responsibility to validate the move.
type Encoder interface {
	Encode(pos *Position, m *Move) string
}

// Decoder is the interface implemented by objects that can
// decode a string into a move given the position. It is not
// the decoders responsibility to validate the move.  An error
// is returned if the string could not be decoded.
type Decoder interface {
	Decode(pos *Position, s string) (*Move, error)
}

// Notation is the interface implemented by objects that can
// encode and decode moves.
type Notation interface {
	Encoder
	Decoder
}

// UCINotation is a more computer friendly alternative to algebraic
// notation.  This notation uses the same format as the UCI (Universal Chess
// Interface).  Examples: e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion).
type UCINotation struct{}

// String implements the fmt.Stringer interface and returns
// the notation's name.
func (UCINotation) String() string {
	return "UCI Notation"
}

// Encode implements the Encoder interface.
func (UCINotation) Encode(_ *Position, m *Move) string {
	const maxLen = 5
	// Get a string builder from the pool
	sb, _ := stringPool.Get().(*strings.Builder)
	sb.Reset()
	defer stringPool.Put(sb)

	// Exact size needed: 4 chars for squares + up to 1 for promotion
	sb.Grow(maxLen)

	sb.Write(m.S1().Bytes())
	sb.Write(m.S2().Bytes())
	if m.Promo() != NoPieceType {
		sb.Write(m.Promo().Bytes())
	}

	return sb.String()
}

// Decode implements the Decoder interface.
func (UCINotation) Decode(pos *Position, s string) (*Move, error) {
	const promoLen = 5

	l := len(s)
	if l < 4 || l > 5 {
		return nil, fmt.Errorf("chess: invalid UCI notation length %d in %q", l, s)
	}

	// Convert directly instead of using map lookups
	s1 := Square((s[0] - 'a') + (s[1]-'1')*8)
	s2 := Square((s[2] - 'a') + (s[3]-'1')*8)

	if s1 < A1 || s1 > H8 || s2 < A1 || s2 > H8 {
		return nil, fmt.Errorf("chess: invalid squares in UCI notation %q", s)
	}

	m := Move{s1: s1, s2: s2}

	// Promotion (Use a precomputed lookup)
	if l == promoLen {
		promoMap := [256]PieceType{
			'n': Knight, 'b': Bishop, 'r': Rook, 'q': Queen,
		}
		promo := promoMap[s[4]]
		if promo == NoPieceType {
			return nil, fmt.Errorf("chess: invalid promotion piece in UCI notation %q", s)
		}
		m.promo = promo
	}

	if pos == nil {
		return &m, nil
	}

	// check for check
	addTags(&m, pos)

	return &m, nil
}

// AlgebraicNotation (or Standard Algebraic Notation) is the
// official chess notation used by FIDE. Examples: e4, e5,
// O-O (short castling), e8=Q (promotion).
type AlgebraicNotation struct{}

// String implements the fmt.Stringer interface and returns
// the notation's name.
func (AlgebraicNotation) String() string {
	return "Algebraic Notation"
}

// Encode implements the Encoder interface.
func (AlgebraicNotation) Encode(pos *Position, m *Move) string {
	// Handle castling without builder
	checkChar := getCheckChar(pos, m)
	if m.HasTag(KingSideCastle) {
		return castleKS + checkChar
	}
	if m.HasTag(QueenSideCastle) {
		return castleQS + checkChar
	}

	// Get a string builder from the pool
	sb, _ := stringPool.Get().(*strings.Builder)
	sb.Reset()
	defer stringPool.Put(sb)

	p := pos.Board().Piece(m.S1())
	if pChar := pieceTypeToChar[p.Type()]; pChar != "" {
		sb.WriteString(pChar)
	}

	if s1Str := formS1(pos, m); s1Str != "" {
		sb.WriteString(s1Str)
	}

	if m.HasTag(Capture) || m.HasTag(EnPassant) {
		if p.Type() == Pawn && sb.Len() == 0 {
			sb.WriteString(m.s1.File().String())
		}
		sb.WriteString(captureStr)
	}

	sb.WriteString(m.s2.String())

	if m.promo != NoPieceType {
		sb.WriteString(equalStr)
		sb.WriteString(pieceTypeToChar[m.promo])
	}

	sb.WriteString(getCheckChar(pos, m))
	return sb.String()
}

// algebraicNotationParts parses a move string into its components.
func algebraicNotationParts(s string) (moveComponents, error) {
	submatches := pgnRegex.FindStringSubmatch(s)
	if len(submatches) == 0 {
		return emptyComponents, fmt.Errorf("chess: invalid algebraic notation %s", s)
	}

	// Return struct instead of multiple returns
	return moveComponents{
		piece:      submatches[1],
		originFile: submatches[2],
		originRank: submatches[3],
		capture:    submatches[4],
		file:       submatches[5],
		rank:       submatches[6],
		promotes:   submatches[7],
		castles:    submatches[8],
	}, nil
}

// cleanMove creates a standardized string from move components.
func (mc moveComponents) clean() string {
	// Get a string builder from pool
	sb, _ := stringPool.Get().(*strings.Builder)
	sb.Reset()
	defer stringPool.Put(sb)

	sb.WriteString(mc.piece)
	sb.WriteString(mc.originFile)
	sb.WriteString(mc.originRank)
	sb.WriteString(mc.capture)
	sb.WriteString(mc.file)
	sb.WriteString(mc.rank)
	sb.WriteString(mc.promotes)
	sb.WriteString(mc.castles)

	return sb.String()
}

// generateMoveOptions creates possible alternative notations for a move.
func (mc moveComponents) generateOptions() []string {
	// Get pre-allocated slice from pool
	options := pieceOptionsPool.Get().(*[]string)
	*options = (*options)[:0]           // Clear but keep capacity
	defer pieceOptionsPool.Put(options) // Now passing pointer

	// Build move options using string builder for efficiency
	sb, _ := stringPool.Get().(*strings.Builder)
	defer stringPool.Put(sb)

	if mc.piece != "" {
		// Option 1: no origin coordinates
		sb.Reset()
		sb.WriteString(mc.piece)
		sb.WriteString(mc.capture)
		sb.WriteString(mc.file)
		sb.WriteString(mc.rank)
		sb.WriteString(mc.promotes)
		sb.WriteString(mc.castles)
		*options = append(*options, sb.String())

		// Option 2: with rank, no file
		sb.Reset()
		sb.WriteString(mc.piece)
		sb.WriteString(mc.originRank)
		sb.WriteString(mc.capture)
		sb.WriteString(mc.file)
		sb.WriteString(mc.rank)
		sb.WriteString(mc.promotes)
		sb.WriteString(mc.castles)
		*options = append(*options, sb.String())

		// Option 3: with file, no rank
		sb.Reset()
		sb.WriteString(mc.piece)
		sb.WriteString(mc.originFile)
		sb.WriteString(mc.capture)
		sb.WriteString(mc.file)
		sb.WriteString(mc.rank)
		sb.WriteString(mc.promotes)
		sb.WriteString(mc.castles)
		*options = append(*options, sb.String())
	} else {
		if mc.capture != "" {
			// Pawn capture without rank
			sb.Reset()
			sb.WriteString(mc.originFile)
			sb.WriteString(mc.capture)
			sb.WriteString(mc.file)
			sb.WriteString(mc.rank)
			sb.WriteString(mc.promotes)
			*options = append(*options, sb.String())
		}
		if mc.originFile != "" && mc.originRank != "" {
			// Full coordinates version
			sb.Reset()
			sb.WriteString(mc.capture)
			sb.WriteString(mc.file)
			sb.WriteString(mc.rank)
			sb.WriteString(mc.promotes)
			*options = append(*options, sb.String())
		}
	}

	return *options
}

// Decode implements the Decoder interface.
func (AlgebraicNotation) Decode(pos *Position, s string) (*Move, error) {
	// Parse move components
	components, err := algebraicNotationParts(s)
	if err != nil {
		return nil, err
	}

	// Get cleaned input move
	cleanedInput := components.clean()

	// Try matching against valid moves
	for _, m := range pos.ValidMoves() {
		// Encode current move
		moveStr := AlgebraicNotation{}.Encode(pos, &m)

		// Parse and clean encoded move
		notationParts, algebraicNotationError := algebraicNotationParts(moveStr)
		if algebraicNotationError != nil {
			continue // Skip invalid moves
		}

		// Compare cleaned versions
		if cleanedInput == notationParts.clean() {
			return &m, nil
		}

		// Try alternative notations
		for _, opt := range components.generateOptions() {
			if opt == notationParts.clean() {
				return &m, nil
			}
		}
	}

	return nil, fmt.Errorf("chess: move %s is not valid", s)
}

// LongAlgebraicNotation is a fully expanded version of
// algebraic notation in which the starting and ending
// squares are specified.
// Examples: e2e4, Rd3xd7, O-O (short castling), e7e8=Q (promotion).
type LongAlgebraicNotation struct{}

// String implements the fmt.Stringer interface and returns
// the notation's name.
func (LongAlgebraicNotation) String() string {
	return "Long Algebraic Notation"
}

// Encode implements the Encoder interface.
func (LongAlgebraicNotation) Encode(pos *Position, m *Move) string {
	checkChar := getCheckChar(pos, m)
	if m.HasTag(KingSideCastle) {
		return "O-O" + checkChar
	} else if m.HasTag(QueenSideCastle) {
		return "O-O-O" + checkChar
	}
	p := pos.Board().Piece(m.S1())
	pChar := charFromPieceType(p.Type())
	s1Str := m.s1.String()
	capChar := ""
	if m.HasTag(Capture) || m.HasTag(EnPassant) {
		capChar = "x"
		if p.Type() == Pawn && s1Str == "" {
			capChar = m.s1.File().String() + "x"
		}
	}
	promoText := charForPromo(m.promo)
	return pChar + s1Str + capChar + m.s2.String() + promoText + checkChar
}

// Decode implements the Decoder interface.
func (LongAlgebraicNotation) Decode(pos *Position, s string) (*Move, error) {
	return AlgebraicNotation{}.Decode(pos, s)
}

func getCheckChar(pos *Position, move *Move) string {
	if !move.HasTag(Check) {
		return ""
	}
	nextPos := pos.Update(move)
	if nextPos.Status() == Checkmate {
		return "#"
	}
	return "+"
}

// getCheckBytes returns the check or mate bytes for a move
//
//nolint:unused // I don't care about this
func getCheckBytes(pos *Position, move *Move) []byte {
	if !move.HasTag(Check) {
		return []byte{}
	}
	if pos.Update(move).Status() == Checkmate {
		return []byte(mateStr)
	}
	return []byte(checkStr)
}

func formS1(pos *Position, m *Move) string {
	p := pos.board.Piece(m.s1)
	if p.Type() == Pawn {
		return ""
	}

	var req, fileReq, rankReq bool

	// Use a string builder from the pool
	sb, _ := stringPool.Get().(*strings.Builder)
	sb.Reset()
	defer stringPool.Put(sb)

	for _, mv := range pos.ValidMoves() {
		if mv.s1 != m.s1 && mv.s2 == m.s2 && p == pos.board.Piece(mv.s1) {
			req = true

			if mv.s1.File() == m.s1.File() {
				rankReq = true
			}

			if mv.s1.Rank() == m.s1.Rank() {
				fileReq = true
			}
		}
	}

	if fileReq || !rankReq && req {
		sb.WriteByte(m.s1.File().Byte())
	}

	if rankReq {
		sb.WriteByte(m.s1.Rank().Byte())
	}

	return sb.String()
}

func charForPromo(p PieceType) string {
	c := charFromPieceType(p)
	if c != "" {
		c = "=" + c
	}
	return c
}

func charFromPieceType(p PieceType) string {
	switch p {
	case King:
		return "K"
	case Queen:
		return "Q"
	case Rook:
		return "R"
	case Bishop:
		return "B"
	case Knight:
		return "N"
	}
	return ""
}

func pieceTypeFromChar(c string) PieceType {
	switch c {
	case "q":
		return Queen
	case "r":
		return Rook
	case "b":
		return Bishop
	case "n":
		return Knight
	}
	return NoPieceType
}
