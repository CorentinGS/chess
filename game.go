/*
Package chess provides a complete chess game implementation with support for move
validation, game tree management, and standard chess formats (PGN, FEN).
The package manages complete chess games including move history, variations,
and game outcomes. It supports standard chess rules including all special moves
(castling, en passant, promotion) and automatic draw detection.
Example usage:

	// Create new game
	game := NewGame()

	// Make moves
	game.PushMove("e4", nil)
	game.PushMove("e5", nil)

	// Check game status

	if game.Outcome() != NoOutcome {
		fmt.Printf("Game ended: %s by %s\n", game.Outcome(), game.Method())
	}
*/
package chess

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"maps"
)

// TagPairs represents a collection of PGN tag pairs.
type TagPairs map[string]string

// A Game represents a single chess game.
type Game struct {
	outcome                        Outcome    // Game result
	tagPairs                       TagPairs   // PGN tag pairs
	tree                           *MoveTree  // Move tree and active cursor
	comments                       [][]string // Game comments
	method                         Method     // How the game ended
	ignoreFivefoldRepetitionDraw   bool       // Flag for automatic FivefoldRepetition draw handling
	ignoreSeventyFiveMoveRuleDraw  bool       // Flag for automatic SeventyFiveMoveRule draw handling
	ignoreInsufficientMaterialDraw bool       // Flag for automatic InsufficientMaterial draw handling
}

// FEN takes a string and returns a function that updates
// the game to reflect the FEN data.  Since FEN doesn't encode
// prior moves, the move list will be empty.  The returned
// function is designed to be used in the NewGame constructor.
// An error is returned if there is a problem parsing the FEN data.
func FEN(fen string) (func(*Game), error) {
	pos, err := decodeFEN(fen)
	if err != nil {
		return nil, fmt.Errorf("chess: game FEN option: %w", err)
	}
	if pos == nil {
		return nil, errors.New("chess: invalid FEN")
	}
	return func(g *Game) {
		pos.inCheck = isInCheck(pos)
		g.tree.setRootPosition(pos)
		g.evaluatePositionStatus()
	}, nil
}

// NewGame returns a new game in the standard starting position.
// Optional functions can be provided to configure the initial game state.
//
// Example:
//
//	// Standard game
//	game := NewGame()
//
//	// Game from FEN
//	game := NewGame(FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
func NewGame(options ...func(*Game)) *Game {
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

// ValidMoves returns all legal moves in the current position.
func (g *Game) ValidMoves() []Move {
	return g.currentPosition().ValidMoves()
}

// UnsafeMoves returns all pseudo-legal moves that leave the moving side's king in check.
// These moves are valid piece movements but illegal because they expose the king.
func (g *Game) UnsafeMoves() []Move {
	return g.currentPosition().UnsafeMoves()
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

// MoveHistory is a move's result from Game's MoveHistory method.
// It contains the move itself, any comments, and the pre and post positions.
type MoveHistory struct {
	PrePosition  *Position
	PostPosition *Position
	Move         Move
	Comments     []string
}

// MoveHistory returns the main-line moves in order along with the pre and post
// positions and any comments. Variations are not included.
// Returns an empty slice for games with no moves.
func (g *Game) MoveHistory() []*MoveHistory {
	root := g.tree.Root()
	if root == nil || len(root.children) == 0 {
		return []*MoveHistory{}
	}

	history := make([]*MoveHistory, 0)
	current := root

	for current != nil && len(current.children) > 0 {
		move := current.children[0]
		if move == nil {
			break
		}
		comments := []string(nil)
		if move.Comments() != "" {
			comments = []string{move.Comments()}
		}

		history = append(history, &MoveHistory{
			PrePosition:  current.position,
			PostPosition: move.position,
			Move:         move.move,
			Comments:     comments,
		})
		current = move
	}

	return history
}

// Comments returns the comments for the game indexed by moves.
func (g *Game) Comments() [][]string {
	if g.comments == nil {
		return [][]string{}
	}
	return copyComments(g.comments)
}

// copyComments returns a deep copy of comments. Internal use; the exported
// Comments method goes through it as well.
func copyComments(src [][]string) [][]string {
	if src == nil {
		return [][]string{}
	}
	out := make([][]string, len(src))
	for i, c := range src {
		if c == nil {
			out[i] = nil
			continue
		}
		cc := make([]string, len(c))
		copy(cc, c)
		out[i] = cc
	}
	return out
}

// Position returns the game's current position.
func (g *Game) Position() *Position {
	pos := g.currentPosition()
	if pos == nil {
		return nil
	}
	return pos.copy()
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

// FEN returns the FEN notation of the current position.
func (g *Game) FEN() string {
	return g.currentPosition().String()
}

// String implements the fmt.Stringer interface and returns
// the game's PGN. It delegates to DefaultPGNRenderer.
func (g *Game) String() string {
	return DefaultPGNRenderer.Render(g)
}

// WritePGN writes the game's PGN to w. It delegates to DefaultPGNRenderer
// and returns any write error.
func (g *Game) WritePGN(w io.Writer) error {
	return DefaultPGNRenderer.RenderGameTo(g, w)
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

	return nil
}

// AddTagPair adds or updates a tag pair with the given key and
// value and returns true if the value is overwritten.
func (g *Game) AddTagPair(k, v string) bool {
	if g.tagPairs == nil {
		g.tagPairs = make(map[string]string)
	}
	if _, existing := g.tagPairs[k]; existing {
		g.tagPairs[k] = v
		return true
	}
	g.tagPairs[k] = v
	return false
}

// GetTagPair returns the tag pair for the given key or nil
// if it is not present.
func (g *Game) GetTagPair(k string) string {
	return g.tagPairs[k]
}

// RemoveTagPair removes the tag pair for the given key and
// returns true if a tag pair was removed.
func (g *Game) RemoveTagPair(k string) bool {
	if _, existing := g.tagPairs[k]; existing {
		delete(g.tagPairs, k)
		return true
	}

	return false
}

// copy copies the game state from the given game.
func (g *Game) copy(game *Game) {
	g.tagPairs = maps.Clone(game.tagPairs)
	g.tree = game.tree
	g.outcome = game.outcome
	g.method = game.method
	g.comments = copyComments(game.comments)
	g.ignoreFivefoldRepetitionDraw = game.ignoreFivefoldRepetitionDraw
	g.ignoreSeventyFiveMoveRuleDraw = game.ignoreSeventyFiveMoveRuleDraw
	g.ignoreInsufficientMaterialDraw = game.ignoreInsufficientMaterialDraw
}

// Clone returns a deep copy of the game.
func (g *Game) Clone() *Game {
	ret := &Game{}
	ret.copy(g)
	ret.tree = g.tree.Clone()

	return ret
}

// Positions returns all positions in the game in the main line.
// This includes the starting position and all positions after each move.
func (g *Game) Positions() []*Position {
	positions := make([]*Position, 0)
	current := g.tree.Root()

	for current != nil {
		if current.position != nil {
			positions = append(positions, current.position)
		}
		if len(current.children) == 0 {
			break
		}
		current = current.children[0]
	}

	return positions
}

func (g *Game) numOfRepetitions() int {
	count := 0
	pos := g.currentPosition()
	for current := g.tree.Root(); current != nil; {
		if current.position != nil && pos.SamePosition(current.position) {
			count++
		}
		if len(current.children) == 0 {
			break
		}
		current = current.children[0]
	}
	return count
}
