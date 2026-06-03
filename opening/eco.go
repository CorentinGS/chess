package opening

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"io"
	"strings"
	"sync"

	"github.com/corentings/chess/v2"
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
func NewBook(r io.Reader) (*BookECO, error) {
	b := &BookECO{
		root: &node{
			children: map[string]*node{},
			pos:      chess.NewGame().Position(),
		},
		startingPosition: chess.NewGame().Position(),
	}
	csvReader := csv.NewReader(r)
	csvReader.Comma = '\t'
	records, err := csvReader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("opening: failed to parse ECO data: %w", err)
	}
	for i, row := range records {
		if i == 0 {
			continue // skip header
		}
		if len(row) < 4 {
			continue // skip malformed rows
		}
		o := newOpening(row[0], row[1], row[3])
		if err := b.insert(o); err != nil {
			return nil, fmt.Errorf("opening: failed to insert opening %s: %w", o.code, err)
		}
	}
	return b, nil
}

// NewBookECO returns a new BookECO using the default embedded ECO data.
// Deprecated: Use DefaultBook() for the standard book or NewBook() for custom data.
func NewBookECO() *BookECO {
	b, err := DefaultBook()
	if err != nil {
		panic(err)
	}
	return b
}

// Find implements the Book interface.
func (b *BookECO) Find(moves []*chess.Move) *Opening {
	for n := b.followPath(b.root, moves); n != nil; n = n.parent {
		if n.opening != nil {
			return n.opening
		}
	}
	return nil
}

// Possible implements the Book interface.
func (b *BookECO) Possible(moves []*chess.Move) []*Opening {
	n := b.followPath(b.root, moves)
	var openings []*Opening
	for _, n := range b.nodeList(n) {
		if n.opening != nil {
			openings = append(openings, n.opening)
		}
	}
	return openings
}

func (b *BookECO) followPath(n *node, moves []*chess.Move) *node {
	if len(moves) == 0 {
		return n
	}
	c, ok := n.children[moves[0].String()]
	if !ok {
		return n
	}
	return b.followPath(c, moves[1:])
}

func (b *BookECO) insert(o *Opening) error {
	posList := []*chess.Position{b.startingPosition}
	var moves []*chess.Move
	for _, s := range parseMoveList(o.pgn) {
		pos := posList[len(posList)-1]
		m, err := chess.UCINotation{}.Decode(pos, s)
		if err != nil {
			return fmt.Errorf("error decoding move %s: %w", s, err)
		}
		moves = append(moves, m)
		posList = append(posList, pos.Update(m))
	}
	n := b.root
	b.ins(n, o, posList[1:], moves)
	return nil
}

func (b *BookECO) ins(n *node, o *Opening, posList []*chess.Position, moves []*chess.Move) {
	pos := posList[0]
	move := moves[0]
	moveStr := move.String()
	var child *node
	for mv, c := range n.children {
		if mv == moveStr {
			child = c
			break
		}
	}
	if child == nil {
		child = &node{
			parent:   n,
			children: map[string]*node{},
			pos:      pos,
		}
		n.children[moveStr] = child
	}
	if len(posList) == 1 {
		child.opening = o
		return
	}
	b.ins(child, o, posList[1:], moves[1:])
}

type node struct {
	parent   *node
	children map[string]*node
	opening  *Opening
	pos      *chess.Position
}

func (b *BookECO) nodeList(root *node) []*node {
	var result []*node
	b.collectNodes(root, &result)
	return result
}

func (b *BookECO) collectNodes(n *node, result *[]*node) {
	*result = append(*result, n)
	for _, c := range n.children {
		b.collectNodes(c, result)
	}
}

// 1.b2b4 e7e5 2.c1b2 f7f6 3.e2e4 f8b4 4.f1c4 b8c6 5.f2f4 d8e7 6.f4f5 g7g6.
func parseMoveList(pgn string) []string {
	strs := strings.Split(pgn, " ")
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
