package chess

import (
	"bytes"
	"crypto/rand"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"os"
	"sort"
)

// PolyglotEntry represents a single entry in a polyglot opening book.
// Each entry is exactly 16 bytes and contains information about a chess position
// and a recommended move.
type PolyglotEntry struct {
	Key    uint64 // Zobrist hash of the chess position
	Move   uint16 // Encoded move (see DecodeMove for format)
	Weight uint16 // Relative weight for move selection
	Learn  uint32 // Learning data (usually 0)
}

// PolyglotMove represents a decoded chess move from a polyglot entry.
// The coordinates use 0-based indices where:
// - Files go from 0 (a-file) to 7 (h-file)
// - Ranks go from 0 (1st rank) to 7 (8th rank)
type PolyglotMove struct {
	FromFile     int  // Source file (0-7)
	FromRank     int  // Source rank (0-7)
	ToFile       int  // Target file (0-7)
	ToRank       int  // Target rank (0-7)
	Promotion    int  // Promotion piece type (0=none, 1=knight, 2=bishop, 3=rook, 4=queen)
	CastlingMove bool // True if this is a castling move
}

// PolyglotBook represents a polyglot opening book with optimized lookup capabilities.
// A polyglot book is a binary file format widely used in chess engines to store opening moves.
// Each entry in the book contains a position hash, a move, and additional metadata.
//
// Example usage:
//
//	// Load from file
//	book, err := LoadBookFromFile("openings.bin")
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	// Find moves for a position
//	hash := uint64(0x463b96181691fc9c) // Starting position hash
//	moves := book.FindMoves(hash)
//
//	// Get a random move weighted by the stored weights
//	randomMove := book.GetRandomMove(hash)
type PolyglotBook struct {
	entries []PolyglotEntry
}

// BookSource defines the interface for reading polyglot book data.
// This interface allows for different source implementations (file, memory, etc.)
// while maintaining consistent access patterns.
type BookSource interface {
	// Read reads exactly len(p) bytes into p or returns an error
	Read(p []byte) (n int, err error)
	// Size returns the total size of the book data
	Size() (int64, error)
}

// ReaderBookSource implements BookSource for io.Reader
type ReaderBookSource struct {
	reader    io.Reader
	data      []byte // Buffered data for Size() implementation
	readIndex int64
}

// NewReaderBookSource creates a new reader-based book source
// Note: This will read the entire input into memory to support Size() and multiple reads
func NewReaderBookSource(reader io.Reader) (*ReaderBookSource, error) {
	// Read all data into memory
	data, err := io.ReadAll(reader)
	if err != nil {
		return nil, err
	}

	return &ReaderBookSource{
		reader:    bytes.NewReader(data),
		data:      data,
		readIndex: 0,
	}, nil
}

// Read implements BookSource for ReaderBookSource
func (r *ReaderBookSource) Read(p []byte) (n int, err error) {
	if r.readIndex >= int64(len(r.data)) {
		return 0, io.EOF
	}

	n = copy(p, r.data[r.readIndex:])
	r.readIndex += int64(n)

	if n < len(p) {
		return n, io.EOF
	}
	return n, nil
}

// Size implements BookSource for ReaderBookSource
func (r *ReaderBookSource) Size() (int64, error) {
	return int64(len(r.data)), nil
}

// FileBookSource implements BookSource for files
type FileBookSource struct {
	path string
}

// BytesBookSource implements BookSource for byte slices
type BytesBookSource struct {
	data  []byte
	index int64
}

// NewFileBookSource creates a new file-based book source
func NewFileBookSource(path string) *FileBookSource {
	return &FileBookSource{path: path}
}

// Read implements BookSource for FileBookSource
func (f *FileBookSource) Read(p []byte) (n int, err error) {
	file, err := os.Open(f.path)
	if err != nil {
		return 0, err
	}
	defer file.Close()
	return io.ReadFull(file, p)
}

// Size implements BookSource for FileBookSource
func (f *FileBookSource) Size() (int64, error) {
	file, err := os.Open(f.path)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	stat, err := file.Stat()
	if err != nil {
		return 0, err
	}
	return stat.Size(), nil
}

// NewBytesBookSource creates a new memory-based book source
func NewBytesBookSource(data []byte) *BytesBookSource {
	return &BytesBookSource{
		data:  data,
		index: 0,
	}
}

// Read implements BookSource for BytesBookSource
func (b *BytesBookSource) Read(p []byte) (n int, err error) {
	if b.index >= int64(len(b.data)) {
		return 0, io.EOF
	}

	n = copy(p, b.data[b.index:])
	b.index += int64(n)

	if n < len(p) {
		return n, io.EOF
	}
	return n, nil
}

// Size implements BookSource for BytesBookSource
func (b *BytesBookSource) Size() (int64, error) {
	return int64(len(b.data)), nil
}

// LoadFromSource loads a polyglot book from any BookSource
func LoadFromSource(source BookSource) (*PolyglotBook, error) {
	size, err := source.Size()
	if err != nil {
		return nil, err
	}

	if size%16 != 0 {
		return nil, errors.New("invalid polyglot book data size")
	}

	numEntries := size / 16
	entries := make([]PolyglotEntry, 0, numEntries)

	buf := make([]byte, 16)
	for {
		_, readErr := source.Read(buf)
		if readErr == io.EOF {
			break
		}
		if readErr != nil {
			return nil, readErr
		}

		entry := PolyglotEntry{
			Key:    binary.BigEndian.Uint64(buf[0:8]),
			Move:   binary.BigEndian.Uint16(buf[8:10]),
			Weight: binary.BigEndian.Uint16(buf[10:12]),
			Learn:  binary.BigEndian.Uint32(buf[12:16]),
		}
		entries = append(entries, entry)
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Key < entries[j].Key
	})

	return &PolyglotBook{entries: entries}, nil
}

// LoadFromReader loads a polyglot book from an io.Reader.
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
//	book, err := LoadFromReader(file)
//	if err != nil {
//	    log.Fatal(err)
//	}
func LoadFromReader(reader io.Reader) (*PolyglotBook, error) {
	source, err := NewReaderBookSource(reader)
	if err != nil {
		return nil, err
	}
	return LoadFromSource(source)
}

// LoadBookFromFile loads a polyglot book from a file path.
// This is a convenience function for the common case of loading from a file.
//
// Example:
//
//	book, err := LoadBookFromFile("openings.bin")
//	if err != nil {
//	    log.Fatal(err)
//	}
func LoadBookFromFile(filename string) (*PolyglotBook, error) {
	source := NewFileBookSource(filename)
	return LoadFromSource(source)
}

// LoadFromBytes loads a polyglot book from a byte slice.
// This is useful when the book data is already in memory.
//
// Example:
//
//	data := // ... your book data ...
//	book, err := LoadFromBytes(data)
//	if err != nil {
//	    log.Fatal(err)
//	}
func LoadFromBytes(data []byte) (*PolyglotBook, error) {
	source := NewBytesBookSource(data)
	return LoadFromSource(source)
}

// FindMoves looks up all moves for a given position hash.
// Returns moves sorted by weight (highest weight first).
// Returns nil if no moves are found.
//
// Example:
//
//	hash := uint64(0x463b96181691fc9c) // Starting position
//	moves := book.FindMoves(hash)
//	if moves != nil {
//	    for _, move := range moves {
//	        decodedMove := DecodeMove(move.Move)
//	        fmt.Printf("Move: %v, Weight: %d\n", decodedMove, move.Weight)
//	    }
//	}
func (book *PolyglotBook) FindMoves(positionHash uint64) []PolyglotEntry {
	idx := sort.Search(len(book.entries), func(i int) bool {
		return book.entries[i].Key >= positionHash
	})

	if idx >= len(book.entries) || book.entries[idx].Key != positionHash {
		return nil
	}

	var moves []PolyglotEntry
	for i := idx; i < len(book.entries) && book.entries[i].Key == positionHash; i++ {
		moves = append(moves, book.entries[i])
	}

	sort.Slice(moves, func(i, j int) bool {
		return moves[i].Weight > moves[j].Weight
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
func DecodeMove(move uint16) PolyglotMove {
	return PolyglotMove{
		FromFile:     int((move >> 6) & 0x7),
		FromRank:     int((move >> 9) & 0x7),
		ToFile:       int(move & 0x7),
		ToRank:       int((move >> 3) & 0x7),
		Promotion:    int((move >> 12) & 0x7),
		CastlingMove: isCastlingMove(int((move>>6)&0x7), int((move>>9)&0x7), int(move&0x7), int((move>>3)&0x7)),
	}
}

// Helper function to identify castling moves
func isCastlingMove(fromFile, fromRank, toFile, toRank int) bool {
	return fromFile == 4 && (fromRank == 0 || fromRank == 7) &&
		(toFile == 0 || toFile == 7) && toRank == fromRank
}

// GetRandomMove returns a weighted random move from the available moves for a position.
// The probability of selecting a move is proportional to its weight.
// Returns nil if no moves are available.
//
// Example:
//
//	hash := uint64(0x463b96181691fc9c) // Starting position
//	move := book.GetRandomMove(hash)
//	if move != nil {
//	    decodedMove := DecodeMove(move.Move)
//	    fmt.Printf("Selected move: %v\n", decodedMove)
//	}
func (book *PolyglotBook) GetRandomMove(positionHash uint64) *PolyglotEntry {
	moves := book.FindMoves(positionHash)
	if len(moves) == 0 {
		return nil
	}

	totalWeight := 0
	for _, move := range moves {
		totalWeight += int(move.Weight)
	}

	r := int(fastRand()) % totalWeight
	currentWeight := 0
	for _, move := range moves {
		currentWeight += int(move.Weight)
		if r < currentWeight {
			return &move
		}
	}

	return &moves[0]
}

// fastRand returns a cryptographically secure random uint32.
// This implementation uses crypto/rand instead of math/rand to ensure
// that move selection cannot be predicted or manipulated.
func fastRand() uint32 {
	b := make([]byte, 4)
	_, err := rand.Read(b)
	if err != nil {
		panic(fmt.Sprintf("failed to generate random number: %v", err))
	}
	return binary.BigEndian.Uint32(b)
}
