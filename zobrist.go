package chess

import (
	"encoding/hex"
	"errors"
	"strconv"
	"strings"
)

// ZobristHasher provides methods to generate Zobrist hashes for chess positions
type ZobristHasher struct {
	enPassantRank int
	enPassantFile int
	pawnNearby    bool
	hasError      bool
}

// Hash represents a Zobrist hash as a byte slice
type Hash []byte

// emptyHash is the initial hash value
var emptyHash = parseHexString("0000000000000000")

// NewChessHasher creates a new instance of ChessHasher
// Deprecated: Use NewZobristHasher instead
func NewChessHasher() *ZobristHasher {
	return &ZobristHasher{
		enPassantRank: -1,
		enPassantFile: -1,
		pawnNearby:    false,
		hasError:      false,
	}
}

// NewZobristHasher creates a new instance of ZobristHasher
func NewZobristHasher() *ZobristHasher {
	return &ZobristHasher{
		enPassantRank: -1,
		enPassantFile: -1,
		pawnNearby:    false,
		hasError:      false,
	}
}

// parseHexString converts a hex string to a byte slice efficiently
func parseHexString(s string) Hash {
	// Ensure the input has an even length
	if len(s)%2 != 0 {
		return nil // Handle invalid input
	}

	// Use a preallocated buffer for zero allocations
	result := make([]byte, len(s)/2)
	_, err := hex.Decode(result, []byte(s))
	if err != nil {
		return nil // Handle invalid hex string
	}

	return result
}

// createHexString converts a byte slice to a hex string efficiently
func createHexString(h Hash) string {
	return hex.EncodeToString(h)
}

func xorArrays(a, b Hash) {
	length := len(a)
	if len(b) < length {
		length = len(b)
	}
	for i := 0; i < length; i++ {
		a[i] ^= b[i] // XOR in place, avoiding new slice allocation
	}
}

func xorArraysInto(a, b, out Hash) {
	length := len(a)
	if len(b) < length {
		length = len(b)
	}
	if len(out) < length {
		panic("output buffer too small")
	}
	for i := 0; i < length; i++ {
		out[i] = a[i] ^ b[i]
	}
}

// xorHash performs an in-place XOR operation on a hash
func (ch *ZobristHasher) xorHash(arr Hash, num int) {
	// Get the precomputed Polyglot hash as a byte slice
	polyglotHash := GetPolyglotHashBytes(num)

	// Perform in-place XOR
	xorArrays(arr, polyglotHash)
}

// parseEnPassant processes the en passant square
func (ch *ZobristHasher) parseEnPassant(s string) {
	if s == "-" {
		return
	}

	if len(s) != 2 {
		ch.hasError = true
		return
	}

	file := int(s[0] - 'a')
	rank := int(s[1] - '1')

	if file < 0 || file > 7 || rank < 0 || rank > 7 {
		ch.hasError = true
		return
	}

	ch.enPassantFile = file
	ch.enPassantRank = rank
}

// hashSide computes the hash for the side to move
func (ch *ZobristHasher) hashSide(arr Hash, color Color) Hash {
	if color == White {
		ch.xorHash(arr, 780)
	}
	return arr
}

// hashCastling updates hash based on castling rights
func (ch *ZobristHasher) hashCastling(arr Hash, s string) Hash {
	if s == "-" {
		return arr
	}

	if strings.Contains(s, "K") {
		ch.xorHash(arr, 768)
	}
	if strings.Contains(s, "Q") {
		ch.xorHash(arr, 769)
	}
	if strings.Contains(s, "k") {
		ch.xorHash(arr, 770)
	}
	if strings.Contains(s, "q") {
		ch.xorHash(arr, 771)
	}

	return arr
}

// hashPieces computes hash for the piece positions
func (ch *ZobristHasher) hashPieces(arr Hash, s string) Hash {
	ranks := strings.Split(s, "/")
	if len(ranks) != 8 {
		ch.hasError = true
		return arr
	}

	for i := 0; i < 8; i++ {
		file := 0
		rank := 7 - i
		for j := 0; j < len(ranks[i]); j++ {
			piece := ranks[i][j]
			switch piece {
			case 'p':
				ch.xorHash(arr, 8*rank+file)
				if ch.enPassantRank == 2 && rank == 3 && ch.enPassantFile > 0 && file == ch.enPassantFile-1 {
					ch.pawnNearby = true
				}
				if ch.enPassantRank == 2 && rank == 3 && ch.enPassantFile < 7 && file == ch.enPassantFile+1 {
					ch.pawnNearby = true
				}
				file++
			case 'P':
				ch.xorHash(arr, 64*1+8*rank+file)
				if ch.enPassantRank == 5 && rank == 4 && ch.enPassantFile > 0 && file == ch.enPassantFile-1 {
					ch.pawnNearby = true
				}
				if ch.enPassantRank == 5 && rank == 4 && ch.enPassantFile < 7 && file == ch.enPassantFile+1 {
					ch.pawnNearby = true
				}
				file++
			case 'n':
				ch.xorHash(arr, 64*2+8*rank+file)
				file++
			case 'N':
				ch.xorHash(arr, 64*3+8*rank+file)
				file++
			case 'b':
				ch.xorHash(arr, 64*4+8*rank+file)
				file++
			case 'B':
				ch.xorHash(arr, 64*5+8*rank+file)
				file++
			case 'r':
				ch.xorHash(arr, 64*6+8*rank+file)
				file++
			case 'R':
				ch.xorHash(arr, 64*7+8*rank+file)
				file++
			case 'q':
				ch.xorHash(arr, 64*8+8*rank+file)
				file++
			case 'Q':
				ch.xorHash(arr, 64*9+8*rank+file)
				file++
			case 'k':
				ch.xorHash(arr, 64*10+8*rank+file)
				file++
			case 'K':
				ch.xorHash(arr, 64*11+8*rank+file)
				file++
			case '1', '2', '3', '4', '5', '6', '7', '8':
				file += int(piece - '0')
			default:
				ch.hasError = true
				return arr
			}
		}
		if file != 8 {
			ch.hasError = true
		}
	}
	return arr
}

// HashPosition computes a Zobrist hash for a chess position in FEN notation
func (ch *ZobristHasher) HashPosition(fen string) (string, error) {
	ch.hasError = false
	ch.enPassantRank = -1
	ch.enPassantFile = -1
	ch.pawnNearby = false

	// FEN should have at least 4 parts
	parts := strings.SplitN(fen, " ", 5)
	if len(parts) < 4 {
		return "", errors.New("invalid FEN format")
	}

	pieces, color, castling, enPassant := parts[0], parts[1], parts[2], parts[3]

	// Quick validation without regex
	if len(color) != 1 || (color[0] != 'w' && color[0] != 'b') {
		return "", errors.New("invalid side to move")
	}

	if len(castling) > 4 {
		return "", errors.New("invalid castling rights")
	}

	hash := make(Hash, len(emptyHash))
	copy(hash, emptyHash)

	ch.parseEnPassant(enPassant)
	hash = ch.hashPieces(hash, pieces)

	if ch.pawnNearby {
		ch.xorHash(hash, 772+ch.enPassantFile)
	}

	hash = ch.hashSide(hash, ColorFromString(color))
	hash = ch.hashCastling(hash, castling)

	if ch.hasError {
		return "", errors.New("invalid piece placement")
	}

	return createHexString(hash), nil
}

func ZobristHashToUint64(hash string) uint64 {
	// Ensure the input is exactly 16 hex digits
	if len(hash) != 16 {
		return 0
	}

	// Convert directly using `strconv.ParseUint`
	result, err := strconv.ParseUint(hash, 16, 64)
	if err != nil {
		return 0

	}

	return result
}
