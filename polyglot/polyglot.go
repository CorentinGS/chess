// Package polyglot reads, edits and writes Polyglot opening books (.bin), the
// binary book format used by most chess engines.
//
// A book is keyed by the Polyglot Zobrist hash of a position, which is what
// chess.Position.ZobristHash returns:
//
//	book, err := polyglot.Load(f)
//	if err != nil {
//	    return err
//	}
//	moves, err := book.Moves(pos.ZobristHash())
package polyglot

import (
	"cmp"
	"encoding/binary"
	"errors"
	"io"
	"math/rand/v2"
	"slices"
	"sort"

	"github.com/corentings/chess/v3"
)

// Entry represents a single entry in a polyglot opening book.
// Each entry is exactly 16 bytes and contains information about a chess position
// and a recommended move.
type Entry struct {
	Key    uint64 // Zobrist hash of the chess position
	Move   uint16 // Encoded move (see DecodeMove for format)
	Weight uint16 // Relative weight for move selection
	Learn  uint32 // Learning data (usually 0)
}

// RawMove is a Polyglot move code decoded into board coordinates. Use
// ChessMove to get a chess.Move.
// The coordinates use 0-based indices where:
// - Files go from 0 (a-file) to 7 (h-file)
// - Ranks go from 0 (1st rank) to 7 (8th rank).
type RawMove struct {
	FromFile     int  // Source file (0-7)
	FromRank     int  // Source rank (0-7)
	ToFile       int  // Target file (0-7)
	ToRank       int  // Target rank (0-7)
	Promotion    int  // Promotion piece type (0=none, 1=knight, 2=bishop, 3=rook, 4=queen)
	CastlingMove bool // True if this is a castling move
}

// Book represents a polyglot opening book with optimized lookup capabilities.
// A polyglot book is a binary file format widely used in chess engines to store opening moves.
// Each entry in the book contains a position hash, a move, and additional metadata.
//
// Example usage:
//
//	// Load from file
//	book, err := Load(fileReader)
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	// Find moves for a position
//	hash := uint64(0x463b96181691fc9c) // Starting position hash
//	moves := book.Entries(hash)
//
//	// Get a random move weighted by the stored weights
//	randomMove := book.RandomMove(hash)
type Book struct {
	entries []Entry
}

// WeightedMove is a helper struct that couples a chess.Move with a weight.
type WeightedMove struct {
	Move   chess.Move
	Weight uint16
}

// EncodeMove returns the Polyglot move code for m.
func EncodeMove(m chess.Move) uint16 {
	var encoded uint16
	encoded |= uint16(int(m.S2().File()) & 0x7)                // bits 0-2
	encoded |= uint16((int(m.S2().Rank()) & 0x7) << 3)         // bits 3-5
	encoded |= uint16((int(m.S1().File()) & 0x7) << 6)         // bits 6-8
	encoded |= uint16((int(m.S1().Rank()) & 0x7) << 9)         // bits 9-11
	encoded |= uint16((promotionValue(m.Promo()) & 0x7) << 12) // bits 12-14
	return encoded
}

// Encode returns the Polyglot move code for pm.
func (pm RawMove) Encode() uint16 {
	var encoded uint16
	encoded |= uint16(pm.ToFile & 0x7)        // bits 0-2
	encoded |= uint16(pm.ToRank&0x7) << 3     // bits 3-5
	encoded |= uint16(pm.FromFile&0x7) << 6   // bits 6-8
	encoded |= uint16(pm.FromRank&0x7) << 9   // bits 9-11
	encoded |= uint16(pm.Promotion&0x7) << 12 // bits 12-14
	return encoded
}

func convertPolyglotCastleToUCI(fromFile, toFile, rank byte) (byte, byte, byte, byte) {
	if fromFile == 'e' {
		switch toFile {
		case 'h':
			return 'e', rank, 'g', rank // King-side
		case 'a':
			return 'e', rank, 'c', rank // chess.Queen-side
		}
	}
	return fromFile, rank, toFile, rank
}

// ChessMove converts pm to a chess.Move. Polyglot encodes castling as the king
// capturing its own rook (e1h1); ChessMove returns the king's destination
// (e1g1) with the castling tag set.
func (pm RawMove) ChessMove() chess.Move {
	var moveBuf [5]byte
	moveBuf[0] = 'a' + byte(pm.FromFile)
	moveBuf[1] = '1' + byte(pm.FromRank)
	moveBuf[2] = 'a' + byte(pm.ToFile)
	moveBuf[3] = '1' + byte(pm.ToRank)

	if pm.CastlingMove {
		moveBuf[0], moveBuf[1], moveBuf[2], moveBuf[3] = convertPolyglotCastleToUCI(moveBuf[0], moveBuf[2], moveBuf[1])
	}

	var moveStr string
	if pm.Promotion > 0 && pm.Promotion <= 4 {
		moveBuf[4] = " nbrq"[pm.Promotion] // Promotion lookup
		moveStr = string(moveBuf[:5])
	} else {
		moveStr = string(moveBuf[:4])
	}

	raw, err := chess.UCI().DecodeRaw(moveStr)
	if err != nil {
		return chess.Move{}
	}
	decode := raw.Move()

	if pm.CastlingMove {
		if pm.FromFile == 4 && (pm.ToFile == 0 || pm.ToFile == 2) {
			decode = decode.WithTag(chess.QueenSideCastle)
		} else {
			decode = decode.WithTag(chess.KingSideCastle)
		}
	}

	return decode
}

// Load loads a polyglot book from an io.Reader.
// Note that this will read the entire input into memory.
//
// Example:
//
//	file, err := os.Open("openings.bin")
//	if err != nil {
//	    log.Fatal(err)
//	}
//	defer file.Close()
//
//	book, err := Load(file)
//	if err != nil {
//	    log.Fatal(err)
//	}
func Load(reader io.Reader) (*Book, error) {
	data, err := io.ReadAll(reader)
	if err != nil {
		return nil, err
	}
	return parseBookData(data)
}

// LoadBytes loads a polyglot book from a byte slice.
// This is useful when the book data is already in memory.
//
// Example:
//
//	data := // ... your book data ...
//	book, err := LoadBytes(data)
//	if err != nil {
//	    log.Fatal(err)
//	}
func LoadBytes(data []byte) (*Book, error) {
	return parseBookData(data)
}

// ponytail: parseBookData is the single entry for both readers and bytes —
// one shape per loader, no BookSource adapter layer. Add a streaming loader
// here if a future caller needs to feed a polyglot book without buffering.
func parseBookData(data []byte) (*Book, error) {
	if len(data)%16 != 0 {
		return nil, errors.New("polyglot: invalid polyglot book data size")
	}

	entries := make([]Entry, 0, len(data)/16)
	for i := 0; i < len(data); i += 16 {
		entries = append(entries, Entry{
			Key:    binary.BigEndian.Uint64(data[i : i+8]),
			Move:   binary.BigEndian.Uint16(data[i+8 : i+10]),
			Weight: binary.BigEndian.Uint16(data[i+10 : i+12]),
			Learn:  binary.BigEndian.Uint32(data[i+12 : i+16]),
		})
	}

	slices.SortFunc(entries, func(a, b Entry) int {
		return cmp.Compare(a.Key, b.Key)
	})

	return &Book{entries: entries}, nil
}

// Entries looks up all moves for a given position hash.
// Returns moves sorted by weight (highest weight first).
// Returns nil if no moves are found.
//
// Example:
//
//	hash := uint64(0x463b96181691fc9c) // Starting position
//	moves := book.Entries(hash)
//	if moves != nil {
//	    for _, move := range moves {
//	        decodedMove := DecodeMove(move.Move)
//	        fmt.Printf("Move: %v, Weight: %d\n", decodedMove, move.Weight)
//	    }
//	}
func (book *Book) Entries(positionHash uint64) []Entry {
	idx := sort.Search(len(book.entries), func(i int) bool {
		return book.entries[i].Key >= positionHash
	})

	if idx >= len(book.entries) || book.entries[idx].Key != positionHash {
		return nil
	}

	var moves []Entry
	for i := idx; i < len(book.entries) && book.entries[i].Key == positionHash; i++ {
		moves = append(moves, book.entries[i])
	}

	slices.SortFunc(moves, func(a, b Entry) int {
		return cmp.Compare(b.Weight, a.Weight)
	})

	return moves
}

// DecodeMove converts a polyglot move encoding into a more usable format.
// The move encoding uses bit fields as follows:
//   - bits 0-2: to file
//   - bits 3-5: to rank
//   - bits 6-8: from file
//   - bits 9-11: from rank
//   - bits 12-14: promotion piece
//
// Promotion pieces are encoded as:
//   - 0: none
//   - 1: knight
//   - 2: bishop
//   - 3: rook
//   - 4: queen
//
// Example:
//
//	move := uint16(0x1234) // Some move from the book
//	decoded := DecodeMove(move)
//	fmt.Printf("From: %c%d, To: %c%d\n",
//	    'a'+decoded.FromFile, decoded.FromRank+1,
//	    'a'+decoded.ToFile, decoded.ToRank+1)
func DecodeMove(move uint16) RawMove {
	return RawMove{
		FromFile:     int((move >> 6) & 0x7),
		FromRank:     int((move >> 9) & 0x7),
		ToFile:       int(move & 0x7),
		ToRank:       int((move >> 3) & 0x7),
		Promotion:    int((move >> 12) & 0x7),
		CastlingMove: isCastlingMove(int((move>>6)&0x7), int((move>>9)&0x7), int(move&0x7), int((move>>3)&0x7)),
	}
}

// promotionValue returns the Polyglot promotion code of pt (0 when pt is not
// a promotion piece).
func promotionValue(pt chess.PieceType) int {
	switch pt {
	case chess.Knight:
		return 1
	case chess.Bishop:
		return 2
	case chess.Rook:
		return 3
	case chess.Queen:
		return 4
	default:
		return 0
	}
}

// Helper function to identify castling moves.
func isCastlingMove(fromFile, fromRank, toFile, toRank int) bool {
	return fromFile == 4 && (fromRank == 0 || fromRank == 7) &&
		(toFile == 0 || toFile == 7) && toRank == fromRank
}

// RandomMove returns a weighted random move from the available moves for a position.
// The probability of selecting a move is proportional to its weight.
// Returns nil if no moves are available.
//
// Example:
//
//	hash := uint64(0x463b96181691fc9c) // Starting position
//	move, err := book.RandomMove(hash)
//	if err != nil {
//	    log.Fatal(err)
//	}
//	if move != nil {
//	    decodedMove := DecodeMove(move.Move)
//	    fmt.Printf("Selected move: %v\n", decodedMove)
//	}
func (book *Book) RandomMove(positionHash uint64) (*Entry, error) {
	moves := book.Entries(positionHash)
	if len(moves) == 0 {
		return nil, nil //nolint:nilnil // nil,nil is the documented "no move for this position" signal
	}

	totalWeight := 0
	for _, move := range moves {
		totalWeight += int(move.Weight)
	}
	if totalWeight == 0 {
		return nil, nil //nolint:nilnil // nil,nil is the documented "no weighted move available" signal
	}

	rn := int(rand.Uint32()) % totalWeight
	currentWeight := 0
	for _, move := range moves {
		currentWeight += int(move.Weight)
		if rn < currentWeight {
			return &move, nil
		}
	}

	return &moves[0], nil
}

// NewBook creates a Book from a map where
// the key is the zobrist hash (uint64) and the value is a slice of WeightedMove.
func NewBook(m map[uint64][]WeightedMove) *Book {
	var entries []Entry
	for key, moves := range m {
		for _, mw := range moves {
			entry := Entry{
				Key:    key,
				Move:   EncodeMove(mw.Move),
				Weight: mw.Weight,
				Learn:  0, // default or as needed
			}
			entries = append(entries, entry)
		}
	}
	slices.SortFunc(entries, func(a, b Entry) int {
		return cmp.Compare(a.Key, b.Key)
	})
	return &Book{entries: entries}
}

// AddMove adds a new move (with its weight) to a given position hash in the book.
func (book *Book) AddMove(positionHash uint64, move chess.Move, weight uint16) {
	entry := Entry{
		Key:    positionHash,
		Move:   EncodeMove(move),
		Weight: weight,
		Learn:  0,
	}
	book.entries = append(book.entries, entry)
	// Re-sort after adding
	slices.SortFunc(book.entries, func(a, b Entry) int {
		return cmp.Compare(a.Key, b.Key)
	})
}

// UpdateMove searches for an existing move at the given position and updates its weight.
func (book *Book) UpdateMove(positionHash uint64, move chess.Move, newWeight uint16) error {
	target := EncodeMove(move)
	updated := false
	for i, entry := range book.entries {
		if entry.Key == positionHash && entry.Move == target {
			book.entries[i].Weight = newWeight
			updated = true
		}
	}
	if !updated {
		return errors.New("polyglot: move not found for update")
	}
	return nil
}

// DeleteMoves removes all moves for a given position hash from the book.
func (book *Book) DeleteMoves(positionHash uint64) {
	var newEntries []Entry
	for _, entry := range book.entries {
		if entry.Key != positionHash {
			newEntries = append(newEntries, entry)
		}
	}
	book.entries = newEntries
}

// Moves returns the book moves for the position with the given Polyglot key
// (Position.ZobristHash), highest weight first.
func (book *Book) Moves(positionHash uint64) ([]chess.Move, error) {
	entries := book.Entries(positionHash)
	if entries == nil {
		return nil, errors.New("polyglot: no moves found for the given position")
	}
	var moves []chess.Move
	for _, entry := range entries {
		pm := DecodeMove(entry.Move)
		move := pm.ChessMove()
		moves = append(moves, move)
	}
	return moves, nil
}

// WeightedMoves returns every book move grouped by position key.
func (book *Book) WeightedMoves() map[uint64][]WeightedMove {
	result := make(map[uint64][]WeightedMove, len(book.entries))
	for _, entry := range book.entries {
		pm := DecodeMove(entry.Move)
		move := pm.ChessMove()
		mw := WeightedMove{
			Move:   move,
			Weight: entry.Weight,
		}
		result[entry.Key] = append(result[entry.Key], mw)
	}
	return result
}
