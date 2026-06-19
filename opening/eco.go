package opening

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"io"
	"strings"
	"sync"

	"github.com/corentings/chess/v3"
)

var (
	defaultBook     *BookECO
	defaultBookErr  error
	defaultBookOnce sync.Once
)

// DefaultBook returns the standard ECO opening book.
// The book is parsed lazily on first call and cached for subsequent calls.
// It is safe for concurrent use.
func DefaultBook() (*BookECO, error) {
	defaultBookOnce.Do(func() {
		defaultBook, defaultBookErr = NewBook(bytes.NewReader(ecoData))
	})
	return defaultBook, defaultBookErr
}

// BookECO represents the Encyclopedia of Chess Openings https://en.wikipedia.org/wiki/Encyclopaedia_of_Chess_Openings
// BookECO is safe for concurrent use.
type BookECO struct {
	root             *node
	startingPosition *chess.Position
}

// NewBook creates a new opening book from an ECO TSV reader.
// Use this for custom opening data or when you need isolation from the default book.
// NewBook validates the input during construction so malformed books fail
// before use. Opening.Game replays validated move paths on demand instead of
// storing games for every opening.
func NewBook(r io.Reader) (*BookECO, error) {
	b := &BookECO{
		root: &node{
			children: map[uint32]*node{},
			pos:      chess.NewGame().Position(),
		},
		startingPosition: chess.NewGame().Position(),
	}
	csvReader := csv.NewReader(r)
	csvReader.Comma = '\t'
	csvReader.FieldsPerRecord = -1
	records, err := csvReader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("opening: failed to parse ECO data: %w", err)
	}
	for i, row := range records {
		rowNum := i + 1
		if i == 0 {
			continue // skip header
		}
		if len(row) < 4 {
			return nil, fmt.Errorf("opening: ECO row %d: expected at least 4 columns, got %d", rowNum, len(row))
		}
		moveList := parseMoveList(row[3])
		o := newOpening(row[0], row[1], row[3], moveList)
		if err := b.insertOpening(o); err != nil {
			return nil, fmt.Errorf("opening: ECO row %d (%s %s): %w", rowNum, row[0], row[1], err)
		}
	}
	return b, nil
}

// Find implements the Book interface.
// Use Find for performance-sensitive opening detection paths.
func (b *BookECO) Find(moves []chess.Move) *Opening {
	for n := b.followPath(b.root, moves); n != nil; n = n.parent {
		if n.opening != nil {
			return n.opening
		}
	}
	return nil
}

// Possible implements the Book interface.
// Use Possible for performance-sensitive opening exploration paths.
func (b *BookECO) Possible(moves []chess.Move) []*Opening {
	root := b.followPath(b.root, moves)
	var openings []*Opening
	b.collectOpenings(root, &openings)
	return openings
}

// collectOpenings walks the subtree rooted at n and appends each node's
// opening (if any) directly into the result slice, avoiding the intermediate
// []*node allocation that nodeList/collectNodes used to require.
func (b *BookECO) collectOpenings(n *node, result *[]*Opening) {
	if n.opening != nil {
		*result = append(*result, n.opening)
	}
	for _, c := range n.children {
		b.collectOpenings(c, result)
	}
}

func (b *BookECO) followPath(n *node, moves []chess.Move) *node {
	if len(moves) == 0 {
		return n
	}
	c, ok := n.children[moveKey(moves[0])]
	if !ok {
		return n
	}
	return b.followPath(c, moves[1:])
}

func (b *BookECO) insertOpening(o *Opening) error {
	if len(o.moveList) == 0 {
		return fmt.Errorf("opening has no moves")
	}

	n := b.root
	for _, moveStr := range o.moveList {
		m, err := chess.UCINotation{}.Decode(n.pos, moveStr)
		if err != nil {
			return fmt.Errorf("decode move %s: %w", moveStr, err)
		}
		key := moveKey(m)
		if child, ok := n.children[key]; ok {
			n = child
			continue
		}

		if !isLegalMove(n.pos, m) {
			return fmt.Errorf("apply move %s: move is not valid for the current position", moveStr)
		}

		child := &node{
			parent:   n,
			children: map[uint32]*node{},
			pos:      n.pos.Update(m),
		}
		n.children[key] = child
		n = child
	}
	n.opening = o
	return nil
}

func isLegalMove(pos *chess.Position, move chess.Move) bool {
	for _, validMove := range pos.ValidMovesUnsafe() {
		if validMove.S1() == move.S1() && validMove.S2() == move.S2() && validMove.Promo() == move.Promo() {
			return true
		}
	}
	return false
}

// Bit layout for moveKey: a Square fits in 6 bits (0..63), a PieceType fits
// in the low 3 bits of the second 6-bit field. The packed key is therefore
// S1 | S2<<6 | Promo<<12 (18 bits total, fits in a uint32).
const (
	moveKeySquareBits = 6
	moveKeyPromoShift = moveKeySquareBits * 2
)

func moveKey(move chess.Move) uint32 {
	return uint32(move.S1()) |
		uint32(move.S2())<<moveKeySquareBits |
		uint32(move.Promo())<<moveKeyPromoShift
}

type node struct {
	parent   *node
	children map[uint32]*node
	opening  *Opening
	pos      *chess.Position
}

// 1.b2b4 e7e5 2.c1b2 f7f6 3.e2e4 f8b4 4.f1c4 b8c6 5.f2f4 d8e7 6.f4f5 g7g6.
func parseMoveList(pgn string) []string {
	strs := strings.Fields(pgn)
	var cp []string
	for _, s := range strs {
		i := strings.Index(s, ".")
		if i == -1 {
			cp = append(cp, s)
		} else {
			cp = append(cp, s[i+1:])
		}
	}
	return cp
}
