package chess

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"iter"
	"strconv"
	"sync"
)

// Side represents a side of the board.
type Side int

const (
	// KingSide is the right side of the board from white's perspective.
	KingSide Side = iota + 1
	// QueenSide is the left side of the board from white's perspective.
	QueenSide
)

// castleSideRights holds the castling rights for one side.
type castleSideRights struct {
	KingSide  bool
	QueenSide bool
}

// CastleRights holds the state of both sides castling abilities.
type CastleRights struct {
	White castleSideRights
	Black castleSideRights
}

// NewCastleRights returns a CastleRights value with the four per-color,
// per-side flags set explicitly. It is the supported way for callers outside
// this package to construct a CastleRights value.
func NewCastleRights(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide bool) CastleRights {
	return CastleRights{
		White: castleSideRights{KingSide: whiteKingSide, QueenSide: whiteQueenSide},
		Black: castleSideRights{KingSide: blackKingSide, QueenSide: blackQueenSide},
	}
}

// CanCastle returns true if the given color and side combination can castle.
//
// Example:
//
//	if rights.CanCastle(White, KingSide) {
//	    // White can castle kingside
//	}
func (cr CastleRights) CanCastle(c Color, side Side) bool {
	switch c {
	case White:
		switch side {
		case KingSide:
			return cr.White.KingSide
		case QueenSide:
			return cr.White.QueenSide
		}
	case Black:
		switch side {
		case KingSide:
			return cr.Black.KingSide
		case QueenSide:
			return cr.Black.QueenSide
		}
	}
	return false
}

// String implements the fmt.Stringer interface and returns a FEN compatible
// string in canonical order (KQkq), or "-" when no side can castle.
func (cr CastleRights) String() string {
	var b [4]byte
	n := 0
	if cr.White.KingSide {
		b[n] = 'K'
		n++
	}
	if cr.White.QueenSide {
		b[n] = 'Q'
		n++
	}
	if cr.Black.KingSide {
		b[n] = 'k'
		n++
	}
	if cr.Black.QueenSide {
		b[n] = 'q'
		n++
	}
	if n == 0 {
		return "-"
	}
	return string(b[:n])
}

// MarshalText implements the encoding.TextMarshaler interface using the FEN
// representation.
func (cr CastleRights) MarshalText() ([]byte, error) {
	return []byte(cr.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface using the FEN
// representation.
func (cr *CastleRights) UnmarshalText(text []byte) error {
	rights, err := formCastleRights(string(text))
	if err != nil {
		return err
	}
	*cr = rights
	return nil
}

// Position represents a complete chess position state.
// It includes piece placement, castling rights, en passant squares,
// move counts, and side to move.
type Position struct {
	board           Board        // Current board state
	castleRights    CastleRights // Available castling options
	legalMoves      []Move       // Cache of legal moves
	halfMoveClock   int          // Half-move counter
	moveCount       int          // Full move counter
	turn            Color        // Side to move
	enPassantSquare Square       // En passant target square
	inCheck         bool         // Whether current side is in check
	checkers        bitboard     // Bitboard of pieces giving check (zero when not in check)
	hash            uint64       // Zobrist hash for O(1) position comparison
	status          Method       // Cached Status result
	statusCached    bool         // Whether status contains a valid cached value
	variant         Variant      // Starting-position variant (zero = Standard)
}

const (
	startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" // Starting position FEN
)

var (
	startingPositionOnce sync.Once
	startingPosition     Position
)

// StartingPosition returns the starting position
// rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1.
func StartingPosition() *Position {
	startingPositionOnce.Do(func() {
		pos, err := decodeFEN(startFEN)
		if err != nil {
			panic(err)
		}
		startingPosition = *pos
	})
	return startingPosition.copy()
}

// AnyLegalMove reports whether the position has at least one legal move.
func (pos *Position) AnyLegalMove() bool {
	return hasLegalMove(pos)
}

// IsLegal reports whether m is a legal move in the current position. It
// returns false for null moves and for moves whose origin square belongs to
// the side not to move. The check is a linear scan of the legal move list;
// callers who need the canonical Move with position-derived tags should use
// Game.Move or resolveCanonicalMove.
func (pos *Position) IsLegal(m Move) bool {
	if pos == nil || m.HasTag(Null) {
		return false
	}
	for _, valid := range pos.legalMovesCached() {
		if valid.s1 == m.s1 && valid.s2 == m.s2 && valid.promo == m.promo {
			return true
		}
	}
	return false
}

// Update returns a new position resulting from the given move.
// The move isn't validated - use Game.Move() for validation.
// This method is optimized for move generation where validation
// is handled separately.
//
// Example:
//
//	newPos := pos.Update(move)
func (pos *Position) Update(m Move) *Position {
	// Null moves flip the side to move without touching the board.
	if m.HasTag(Null) {
		return pos.nullUpdate()
	}

	// Seed a fresh position with the pre-move scalars (including the pre-move
	// hash) so applyMove can mutate it in one place. applyMove overwrites
	// every field it owns — board, hash, turn, castleRights, enPassantSquare,
	// halfMoveClock, moveCount — and reads the seed for the pre-move state
	// that updateCastleRights, updateEnPassantSquare, and updateHash need
	// before overwriting.
	newPos := &Position{
		board:           pos.board,
		turn:            pos.turn,
		castleRights:    pos.castleRights,
		enPassantSquare: pos.enPassantSquare,
		halfMoveClock:   pos.halfMoveClock,
		moveCount:       pos.moveCount,
		variant:         pos.variant,
		hash:            pos.hash,
	}
	newPos.applyMove(m)
	return newPos
}

// ApplyInPlace applies m to pos, mutating it in place instead of returning a
// copy. It is the in-place counterpart of Update for callers that replay a
// single linear line and never need the pre-move position again: the same
// move effects (board, hash, castle rights, en passant, clocks, check state)
// are applied without the allocation Update pays for its fresh Position.
// The receiver must not be shared; after the call the pre-move state is gone.
func (pos *Position) ApplyInPlace(m Move) {
	if m.HasTag(Null) {
		next := pos.nullUpdate()
		*pos = *next
		return
	}
	pos.applyMove(m)
}

// updateHash computes the new Zobrist hash incrementally from a move. It
// consumes the moveEffect that drove the board mutation so the hash delta and
// the board read one interpretation of the move's physical facts (en-passant
// square, castle rook squares, capture target) instead of each re-reading
// MoveTags and risking drift.
//
// newCR and newEP are the post-move castling rights and en-passant target
// square — these are bookkeeping rules that live in their own helpers
// (updateCastleRights, updateEnPassantSquare), not in moveEffect, so they are
// passed in alongside.
func (pos *Position) updateHash(m Move, newCR CastleRights, newEP Square, eff moveEffect) uint64 {
	hash := pos.hash

	// Toggle side to move
	hash ^= polyglotHashesUint64[780]

	// XOR out moving piece from origin square
	oldIdx := pieceZobristIndex(eff.moving, m.s1)
	if oldIdx >= 0 {
		hash ^= polyglotHashesUint64[oldIdx]
	}

	// XOR in landing piece at destination (a promotion piece when eff.landing != eff.moving)
	destIdx := pieceZobristIndex(eff.landing, m.s2)
	if destIdx >= 0 {
		hash ^= polyglotHashesUint64[destIdx]
	}

	// XOR out captured piece at its square. Normal capture and en passant
	// share one branch: moveEffect already resolved the en-passant square to
	// s2±8 when applicable.
	if eff.capPiece != NoPiece {
		capIdx := pieceZobristIndex(eff.capPiece, eff.capSq)
		if capIdx >= 0 {
			hash ^= polyglotHashesUint64[capIdx]
		}
	}

	// XOR the castle rook between its origin and destination. The rook piece
	// is always the moving side's rook; moveEffect carries the squares.
	if eff.rookFrom != NoSquare {
		rook := NewPiece(Rook, eff.moving.Color())
		hash ^= polyglotHashesUint64[pieceZobristIndex(rook, eff.rookFrom)]
		hash ^= polyglotHashesUint64[pieceZobristIndex(rook, eff.rookTo)]
	}

	// Update castling rights: XOR out removed rights
	if pos.castleRights.CanCastle(White, KingSide) && !newCR.CanCastle(White, KingSide) {
		hash ^= polyglotHashesUint64[768]
	}
	if pos.castleRights.CanCastle(White, QueenSide) && !newCR.CanCastle(White, QueenSide) {
		hash ^= polyglotHashesUint64[769]
	}
	if pos.castleRights.CanCastle(Black, KingSide) && !newCR.CanCastle(Black, KingSide) {
		hash ^= polyglotHashesUint64[770]
	}
	if pos.castleRights.CanCastle(Black, QueenSide) && !newCR.CanCastle(Black, QueenSide) {
		hash ^= polyglotHashesUint64[771]
	}

	// Update en passant: XOR out old if present
	if oldEPFile := enPassantFileForHash(&pos.board, pos.enPassantSquare); oldEPFile >= 0 {
		hash ^= polyglotHashesUint64[772+oldEPFile]
	}
	// XOR in new if present
	if newEPFile := enPassantFileForHash(&pos.board, newEP); newEPFile >= 0 {
		hash ^= polyglotHashesUint64[772+newEPFile]
	}

	return hash
}

// LegalMoves returns all legal moves in the current position.
// The moves are cached for performance. The returned slice is a
// defensive copy safe for modification by the caller.
func (pos *Position) LegalMoves() []Move {
	if pos.legalMoves != nil {
		return append([]Move(nil), pos.legalMoves...)
	}
	pos.legalMoves = calcMoves(pos, false)
	return append([]Move(nil), pos.legalMoves...)
}

// legalMovesCached returns all legal moves in the current position
// without copying. The caller must not modify the returned slice.
// This is a zero-allocation alternative to LegalMoves() for hot paths.
func (pos *Position) legalMovesCached() []Move {
	if pos.legalMoves != nil {
		return pos.legalMoves
	}
	pos.legalMoves = calcMoves(pos, false)
	return pos.legalMoves
}

// LegalMovesFast returns the legal moves in the same order as LegalMoves but
// skips computing the Check tag, which only matters for SAN rendering. The
// returned moves remain valid inputs to Position.Update. This is intended for
// replay/index codecs that need move identity and legality but not SAN tags.
func (pos *Position) LegalMovesFast() []Move {
	return legalMovesForMode(pos, generateLegalOnly)
}

// LegalMovesSeq returns an iterator over the legal moves in the current
// position, in the same order and with the same tags as LegalMoves. Iteration
// does not allocate once the position's move cache is warm; the first call
// may compute and cache the moves.
//
//	for move := range pos.LegalMovesSeq() {
//	    // process move
//	}
func (pos *Position) LegalMovesSeq() iter.Seq[Move] {
	return func(yield func(Move) bool) {
		for _, m := range pos.legalMovesCached() {
			if !yield(m) {
				return
			}
		}
	}
}

// PseudoLegalMoves returns every move that follows the pieces' movement rules
// for the side to move, including those that leave its own king in check.
// Castling is included only when it is fully legal. The moves carry no Check
// tag. Filter with IsLegal to separate legal from illegal moves; only legal
// moves may be passed to Update or ApplyInPlace.
func (pos *Position) PseudoLegalMoves() []Move {
	return append(legalMovesForMode(pos, generateLegalOnly), unsafeMoves(pos)...)
}

// Status returns the position's outcome Method (e.g. Checkmate, Stalemate, or
// NoMethod).
func (pos *Position) Status() Method {
	if pos.statusCached {
		return pos.status
	}
	pos.status = status(pos)
	pos.statusCached = true
	return pos.status
}

// Outcome returns the decisive or draw outcome implied by the position, or
// NoOutcome if play continues. It considers board-state terminal conditions
// only: checkmate, stalemate, insufficient material, and the seventy-five
// move rule. Game-level outcomes such as resignation, draw offer, and the
// claimable fifty-move rule are not included and must be queried on Game.
func (pos *Position) Outcome() Outcome {
	if pos == nil {
		return NoOutcome
	}
	outcome, _ := classifyOutcome(pos, 0, outcomeRules{includeAutoDraws: true})
	return outcome
}

// HasInsufficientMaterial reports whether color c cannot force checkmate with
// its remaining material against any opposing material.
func (pos *Position) HasInsufficientMaterial(c Color) bool {
	if pos == nil {
		return true
	}
	return pos.board.HasInsufficientMaterial(c)
}

// Board returns the position's board.
func (pos *Position) Board() *Board {
	if pos == nil {
		return nil
	}
	return pos.board.copy()
}

// Turn returns the color to move next.
func (pos *Position) Turn() Color {
	return pos.turn
}

// IsCheck reports whether the side to move is in check.
func (pos *Position) IsCheck() bool {
	if pos == nil {
		return false
	}
	return pos.inCheck
}

// Checkers returns the squares of the pieces giving check to the side to
// move. It is empty when the position is not in check.
func (pos *Position) Checkers() Bitboard {
	if pos == nil {
		return 0
	}
	return Bitboard(pos.checkers)
}

// nullUpdateHash computes the Zobrist hash delta for a null move: the only
// state that changed is the side to move (always flipped) and the en-passant
// square (always cleared), so the only XOR is the side-to-move key plus any
// previously-active en-passant file key.
func (pos *Position) nullUpdateHash(_ Square) uint64 {
	hash := pos.hash ^ polyglotHashesUint64[780]
	if oldEPFile := enPassantFileForHash(&pos.board, pos.enPassantSquare); oldEPFile >= 0 {
		hash ^= polyglotHashesUint64[772+oldEPFile]
	}
	return hash
}

// HalfMoveClock returns the half-move clock (50-rule).
func (pos *Position) HalfMoveClock() int {
	return pos.halfMoveClock
}

// EnPassantSquare returns the raw en-passant target square set after any
// double pawn push, even when no enemy pawn can capture it. This is the FEN
// en-passant field.
func (pos *Position) EnPassantSquare() Square {
	return pos.enPassantSquare
}

// LegalEnPassantSquare returns the en-passant target square only if an enemy
// pawn can actually capture en passant; otherwise it returns NoSquare. This
// is the value that feeds the Zobrist hash.
func (pos *Position) LegalEnPassantSquare() Square {
	if pos == nil {
		return NoSquare
	}
	return pos.relevantEnPassantSquare()
}

// CastleRights returns the castling rights of the position.
func (pos *Position) CastleRights() CastleRights {
	return pos.castleRights
}

// Variant returns the starting-position variant of the position. The zero
// value is Standard.
func (pos *Position) Variant() Variant {
	return pos.variant
}

// Ply returns the number of half-moves played since the start of the game,
// derived from the full-move number and side to move: 0 in the starting
// position, 1 after 1.e4. It matches MoveNode.Ply for positions reached
// from the standard start.
func (pos *Position) Ply() int {
	if pos == nil {
		return 0
	}
	ply := (pos.moveCount - 1) * 2
	if pos.turn == Black {
		ply++
	}
	return max(ply, 0)
}

// FullMoveNumber returns the FEN full-move number: 1 at the start, incremented
// after each Black move.
func (pos *Position) FullMoveNumber() int {
	if pos == nil {
		return 0
	}
	return pos.moveCount
}

// FEN returns the position in Forsyth-Edwards Notation, for example
// "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1". The en passant
// field is kept as set, even when no pawn can capture, so a parsed FEN
// round-trips exactly. Chess960 castling rights use Shredder-FEN file letters.
// CanonicalFEN gives the normalised form and FENKey the identity fields.
func (pos *Position) FEN() string {
	buf := pos.appendPositionKey(make([]byte, 0, 90), pos.enPassantSquare)
	buf = append(buf, ' ')
	buf = strconv.AppendInt(buf, int64(pos.halfMoveClock), 10)
	buf = append(buf, ' ')
	buf = strconv.AppendInt(buf, int64(pos.moveCount), 10)
	return string(buf)
}

// FENKey returns the first four FEN fields (placement, side to move, castling
// rights, en passant square as set) without the two move clocks.
func (pos *Position) FENKey() string {
	return string(pos.appendPositionKey(make([]byte, 0, 86), pos.enPassantSquare))
}

func (pos *Position) appendPositionKey(buf []byte, enPassantSquare Square) []byte {
	buf = pos.board.appendFEN(buf)
	buf = append(buf, ' ', pos.turn.String()[0], ' ')
	buf = appendCastleRights(buf, pos.castleRights, &pos.board, pos.variant)
	buf = append(buf, ' ')
	if enPassantSquare == NoSquare {
		return append(buf, '-')
	}
	return append(buf, enPassantSquare.String()...)
}

// CanonicalFEN is FEN with the en passant square written only when a pawn can
// actually capture en passant, the form most tools (Lichess, Stockfish) print.
// Positions that are the same by FIDE rules and share clocks have the same
// CanonicalFEN.
func (pos *Position) CanonicalFEN() string {
	buf := pos.appendPositionKey(make([]byte, 0, 90), pos.relevantEnPassantSquare())
	buf = append(buf, ' ')
	buf = strconv.AppendInt(buf, int64(pos.halfMoveClock), 10)
	buf = append(buf, ' ')
	buf = strconv.AppendInt(buf, int64(pos.moveCount), 10)
	return string(buf)
}

// String returns FEN, so a Position prints as its FEN.
func (pos *Position) String() string {
	return pos.FEN()
}

// ZobristHash returns the Zobrist hash of the position.
// This is a fast, collision-resistant hash suitable for transposition tables
// and position comparison. Two positions that are identical by FIDE rules
// will have the same hash value.
func (pos *Position) ZobristHash() uint64 {
	return pos.hash
}

// MarshalText implements the encoding.TextMarshaler interface and
// encodes the position's FEN.
func (pos *Position) MarshalText() ([]byte, error) {
	return []byte(pos.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface and
// assumes the data is in the FEN format.
func (pos *Position) UnmarshalText(text []byte) error {
	cp, err := decodeFEN(string(text))
	if err != nil {
		return fmt.Errorf("chess: unmarshal position FEN: %w", err)
	}
	pos.board = cp.board
	pos.castleRights = cp.castleRights
	pos.turn = cp.turn
	pos.enPassantSquare = cp.enPassantSquare
	pos.halfMoveClock = cp.halfMoveClock
	pos.moveCount = cp.moveCount
	pos.inCheck, pos.checkers = checkState(cp)
	pos.hash = cp.hash
	return nil
}

const (
	bitsCastleWhiteKing uint8 = 1 << iota
	bitsCastleWhiteQueen
	bitsCastleBlackKing
	bitsCastleBlackQueen
	bitsTurn
	bitsHasEnPassant
	bitsChess960
)

// MarshalBinary implements the encoding.BinaryMarshaler interface.
func (pos *Position) MarshalBinary() ([]byte, error) {
	boardBytes, err := pos.board.MarshalBinary()
	if err != nil {
		return nil, err
	}
	buf := bytes.NewBuffer(boardBytes)
	if err = binary.Write(buf, binary.BigEndian, uint8(pos.halfMoveClock)); err != nil {
		return nil, err
	}
	if err = binary.Write(buf, binary.BigEndian, uint16(pos.moveCount)); err != nil {
		return nil, err
	}
	if err = binary.Write(buf, binary.BigEndian, pos.enPassantSquare); err != nil {
		return nil, err
	}
	var b uint8
	if pos.castleRights.CanCastle(White, KingSide) {
		b |= bitsCastleWhiteKing
	}
	if pos.castleRights.CanCastle(White, QueenSide) {
		b |= bitsCastleWhiteQueen
	}
	if pos.castleRights.CanCastle(Black, KingSide) {
		b |= bitsCastleBlackKing
	}
	if pos.castleRights.CanCastle(Black, QueenSide) {
		b |= bitsCastleBlackQueen
	}
	if pos.turn == Black {
		b |= bitsTurn
	}
	if pos.enPassantSquare != NoSquare {
		b |= bitsHasEnPassant
	}
	if pos.variant == Chess960 {
		b |= bitsChess960
	}
	if err = binary.Write(buf, binary.BigEndian, b); err != nil {
		return nil, err
	}
	return buf.Bytes(), err
}

// UnmarshalBinary implements the encoding.BinaryMarshaler interface.
func (pos *Position) UnmarshalBinary(data []byte) error {
	const size = 101
	if len(data) != size {
		return errors.New("chess: position binary data should consist of 101 bytes")
	}
	if err := pos.board.UnmarshalBinary(data[:96]); err != nil {
		return err
	}
	buf := bytes.NewBuffer(data[96:])
	halfMove := uint8(pos.halfMoveClock)
	if err := binary.Read(buf, binary.BigEndian, &halfMove); err != nil {
		return err
	}
	pos.halfMoveClock = int(halfMove)
	moveCount := uint16(pos.moveCount)
	if err := binary.Read(buf, binary.BigEndian, &moveCount); err != nil {
		return err
	}
	pos.moveCount = int(moveCount)
	if err := binary.Read(buf, binary.BigEndian, &pos.enPassantSquare); err != nil {
		return err
	}
	var b uint8
	if err := binary.Read(buf, binary.BigEndian, &b); err != nil {
		return err
	}
	pos.castleRights = CastleRights{}
	pos.turn = White
	if b&bitsCastleWhiteKing != 0 {
		pos.castleRights.White.KingSide = true
	}
	if b&bitsCastleWhiteQueen != 0 {
		pos.castleRights.White.QueenSide = true
	}
	if b&bitsCastleBlackKing != 0 {
		pos.castleRights.Black.KingSide = true
	}
	if b&bitsCastleBlackQueen != 0 {
		pos.castleRights.Black.QueenSide = true
	}
	if b&bitsTurn != 0 {
		pos.turn = Black
	}
	if b&bitsHasEnPassant == 0 {
		pos.enPassantSquare = NoSquare
	}
	if b&bitsChess960 != 0 {
		pos.variant = Chess960
	}
	pos.inCheck, pos.checkers = checkState(pos)
	pos.hash = pos.computeHash()
	return nil
}

func (pos *Position) copy() *Position {
	return &Position{
		board:           pos.board,
		turn:            pos.turn,
		castleRights:    pos.castleRights,
		enPassantSquare: pos.enPassantSquare,
		halfMoveClock:   pos.halfMoveClock,
		moveCount:       pos.moveCount,
		variant:         pos.variant,
		inCheck:         pos.inCheck,
		checkers:        pos.checkers,
		hash:            pos.hash,
	}
}

// pieceZobristIndex returns the polyglot hash index for a piece on a square.
// Index mapping: piece type * 64 + square, with black pieces first in each pair.
func pieceZobristIndex(p Piece, sq Square) int {
	var pieceOffset int
	switch p.Type() {
	case Pawn:
		pieceOffset = 0
	case Knight:
		pieceOffset = 2
	case Bishop:
		pieceOffset = 4
	case Rook:
		pieceOffset = 6
	case Queen:
		pieceOffset = 8
	case King:
		pieceOffset = 10
	default:
		return -1
	}
	colorOffset := 0
	if p.Color() == White {
		colorOffset = 1
	}
	return (pieceOffset+colorOffset)*64 + int(sq)
}

// computeHash computes the full Zobrist hash from the current position state.
func (pos *Position) computeHash() uint64 {
	var hash uint64
	// XOR in all pieces
	for sq := range 64 {
		p := pos.board.Piece(Square(sq))
		if p != NoPiece {
			idx := pieceZobristIndex(p, Square(sq))
			if idx >= 0 {
				hash ^= polyglotHashesUint64[idx]
			}
		}
	}
	// XOR in castling rights
	if pos.castleRights.CanCastle(White, KingSide) {
		hash ^= polyglotHashesUint64[768]
	}
	if pos.castleRights.CanCastle(White, QueenSide) {
		hash ^= polyglotHashesUint64[769]
	}
	if pos.castleRights.CanCastle(Black, KingSide) {
		hash ^= polyglotHashesUint64[770]
	}
	if pos.castleRights.CanCastle(Black, QueenSide) {
		hash ^= polyglotHashesUint64[771]
	}
	// XOR in en passant if a pawn can capture
	if epFile := enPassantFileForHash(&pos.board, pos.enPassantSquare); epFile >= 0 {
		hash ^= polyglotHashesUint64[772+epFile]
	}
	// XOR in side to move (white)
	if pos.turn == White {
		hash ^= polyglotHashesUint64[780]
	}
	return hash
}

// SamePosition returns true if the two positions are the same
// according to FIDE Article 9.2.3. Uses Zobrist hash as a fast-path,
// falling back to full field comparison on hash collision.
func (pos *Position) SamePosition(pos2 *Position) bool {
	if pos.hash != pos2.hash {
		return false
	}
	return pos.board == pos2.board &&
		pos.turn == pos2.turn &&
		pos.castleRights == pos2.castleRights &&
		pos.relevantEnPassantSquare() == pos2.relevantEnPassantSquare()
}

// enPassantFileForHash returns the file index (0-7) of the given en passant
// square if a pawn can actually capture en passant, or -1 otherwise.
// This is the single source of truth for en passant relevance in Zobrist hashing.
func enPassantFileForHash(board *Board, epSquare Square) int {
	if epSquare == NoSquare {
		return -1
	}
	epFile := epSquare.File()
	epRank := epSquare.Rank()

	var captureRank Rank
	var capturingPawn Piece
	if epRank == Rank3 {
		captureRank = Rank4
		capturingPawn = BlackPawn
	} else {
		captureRank = Rank5
		capturingPawn = WhitePawn
	}

	if epFile > FileA {
		sq := NewSquare(epFile-1, captureRank)
		if board.Piece(sq) == capturingPawn {
			return int(epFile)
		}
	}
	if epFile < FileH {
		sq := NewSquare(epFile+1, captureRank)
		if board.Piece(sq) == capturingPawn {
			return int(epFile)
		}
	}
	return -1
}

// relevantEnPassantSquare returns the en passant square only if
// an en passant capture is actually possible. Per FIDE rules,
// the en passant square is only relevant if there is an opponent
// pawn that can make the capture.
func (pos *Position) relevantEnPassantSquare() Square {
	if enPassantFileForHash(&pos.board, pos.enPassantSquare) >= 0 {
		return pos.enPassantSquare
	}
	return NoSquare
}
