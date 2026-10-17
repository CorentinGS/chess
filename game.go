package chess

import (
	"bytes"
	"fmt"
	"io"
	"maps"
)

// tagMap represents a collection of PGN tag pairs.
type tagMap map[string]string

// A Game represents a single chess game.
type Game struct {
	outcome                        Outcome   // Game result
	tagPairs                       tagMap    // PGN tag pairs
	tree                           *MoveTree // Move tree and active cursor
	method                         Method    // How the game ended
	ignoreFivefoldRepetitionDraw   bool      // Flag for automatic FivefoldRepetition draw handling
	ignoreSeventyFiveMoveRuleDraw  bool      // Flag for automatic SeventyFiveMoveRule draw handling
	ignoreInsufficientMaterialDraw bool      // Flag for automatic InsufficientMaterial draw handling
}

// GameOption configures a Game created by NewGame.
type GameOption func(*Game)

// FEN returns a GameOption that starts the game from the position described
// by fen instead of the standard starting position. The game has no moves.
// It returns an error wrapping ErrInvalidFEN when fen does not parse.
func FEN(fen string) (GameOption, error) {
	pos, err := ParseFEN(fen)
	if err != nil {
		return nil, fmt.Errorf("chess: game FEN option: %w", err)
	}
	return func(g *Game) {
		g.tree.setRootPosition(pos)
		g.evaluatePositionStatus()
	}, nil
}

// NewGame returns a new game in the standard starting position, configured by
// options.
//
//	game := NewGame()
//
//	opt, err := FEN("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1")
//	if err != nil {
//	    return err
//	}
//	game = NewGame(opt, IgnoreFivefoldRepetitionDraw())
func NewGame(options ...GameOption) *Game {
	pos := StartingPosition()

	game := &Game{
		tree:     newMoveTree(pos),
		tagPairs: make(map[string]string),
		outcome:  NoOutcome,
		method:   NoMethod,
	}
	for _, f := range options {
		if f != nil {
			f(game)
		}
	}
	return game
}

// MoveTree returns the game's move tree.
func (g *Game) MoveTree() *MoveTree { return g.tree }

// IsAtStart returns true if the game is at the start.
func (g *Game) IsAtStart() bool {
	return g.tree == nil || g.tree.Current() == nil || g.tree.Current() == g.tree.Root()
}

// IsAtEnd returns true if the game is at the end.
func (g *Game) IsAtEnd() bool {
	return g.tree != nil && g.tree.Current() != nil && len(g.tree.Current().children) == 0
}

// LegalMoves returns all legal moves in the current position.
func (g *Game) LegalMoves() []Move {
	return g.currentPosition().LegalMoves()
}

// Moves returns the main-line move values of the game.
func (g *Game) Moves() []Move {
	nodes := g.tree.MainLine()
	moves := make([]Move, len(nodes))
	for i, node := range nodes {
		moves[i] = node.move
	}
	return moves
}

// Position returns the game's current position.
func (g *Game) Position() *Position {
	pos := g.currentPosition()
	if pos == nil {
		return nil
	}
	return pos.copy()
}

// Variant returns the starting-position variant of the game (Standard or
// Chess960), derived from the game's root position.
func (g *Game) Variant() Variant {
	if g == nil || g.tree == nil || g.tree.rootPos == nil {
		return Standard
	}
	return g.tree.rootPos.variant
}

func (g *Game) currentPosition() *Position {
	if g == nil || g.tree == nil {
		return nil
	}
	return g.tree.position()
}

// Outcome returns the game outcome.
func (g *Game) Outcome() Outcome {
	return g.outcome
}

// Method returns the method in which the outcome occurred.
func (g *Game) Method() Method {
	return g.method
}

// String implements the fmt.Stringer interface and returns
// the game's PGN.
func (g *Game) String() string {
	return defaultPGNRenderer.render(g)
}

// WritePGN writes the game's PGN to w
// and returns any write error.
func (g *Game) WritePGN(w io.Writer) error {
	return defaultPGNRenderer.renderTo(g, w)
}

// FEN returns the FEN notation of the current position.
// Returns "" if the game has no current position.
func (g *Game) FEN() string {
	pos := g.currentPosition()
	if pos == nil {
		return ""
	}
	return pos.FEN()
}

// MarshalText implements the encoding.TextMarshaler interface and
// encodes the game's PGN.
func (g *Game) MarshalText() ([]byte, error) {
	return []byte(g.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface and
// assumes the data is in the PGN format.
func (g *Game) UnmarshalText(text []byte) error {
	r := bytes.NewReader(text)

	game, err := ParsePGN(r)
	if err != nil {
		return fmt.Errorf("chess: unmarshal game PGN: %w", err)
	}
	g.copy(game)
	g.tree = game.tree // parsed game is local; hand over the tree, do not share it

	return nil
}

// SetTag sets the PGN tag k to v, replacing any existing value.
func (g *Game) SetTag(k, v string) {
	if g.tagPairs == nil {
		g.tagPairs = make(tagMap)
	}
	g.tagPairs[k] = v
}

// Tag returns the value of the PGN tag k, or "" if the game has no such tag.
func (g *Game) Tag(k string) string {
	return g.tagPairs[k]
}

// Tags returns a copy of the game's PGN tags. Modifying the returned map does
// not affect the game.
func (g *Game) Tags() map[string]string {
	return maps.Clone(map[string]string(g.tagPairs))
}

// RemoveTag removes the PGN tag k and reports whether it was present.
func (g *Game) RemoveTag(k string) bool {
	if _, existing := g.tagPairs[k]; existing {
		delete(g.tagPairs, k)
		return true
	}
	return false
}

// copy copies game state except the move tree from the given game. The caller
// must set g.tree explicitly; copy never aliases or clones it.
func (g *Game) copy(game *Game) {
	g.tagPairs = maps.Clone(game.tagPairs)
	g.outcome = game.outcome
	g.method = game.method
	g.ignoreFivefoldRepetitionDraw = game.ignoreFivefoldRepetitionDraw
	g.ignoreSeventyFiveMoveRuleDraw = game.ignoreSeventyFiveMoveRuleDraw
	g.ignoreInsufficientMaterialDraw = game.ignoreInsufficientMaterialDraw
}

// Clone returns a deep copy of the game, including its move tree.
func (g *Game) Clone() *Game {
	ret := &Game{}
	ret.copy(g)
	ret.tree = g.tree.Clone() // copy() skips tree; Clone deep-copies it

	return ret
}

// Positions returns all positions in the game in the main line.
// This includes the starting position and all positions after each move.
// The cursor is saved and restored so the caller's active position is unchanged.
func (g *Game) Positions() []*Position {
	root := g.tree.Root()
	if root == nil || len(root.children) == 0 {
		return []*Position{}
	}
	defer g.tree.setCurrent(g.tree.Current())
	g.tree.GoToStart()
	positions := make([]*Position, 0, len(root.children)+1)
	if pos := g.tree.Peek(); pos != nil {
		positions = append(positions, pos.copy())
	}
	for g.tree.GoForward() {
		if pos := g.tree.Peek(); pos != nil {
			positions = append(positions, pos.copy())
		}
	}
	return positions
}

func (g *Game) numOfRepetitions() int {
	count := 0
	target := g.currentPosition()
	// numOfRepetitions walks the cursor through the main line; save and
	// restore so callers see an unchanged active position.
	defer g.tree.setCurrent(g.tree.Current())
	g.tree.GoToStart()
	for {
		if g.tree.pos != nil && target.SamePosition(g.tree.pos) {
			count++
		}
		if !g.tree.GoForward() {
			break
		}
	}
	return count
}
