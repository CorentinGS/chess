package chess

import (
	"errors"
	"fmt"
	"io"
)

// A Outcome is the result of a game.
type Outcome string

const (
	// NoOutcome indicates that a game is in progress or ended without a result.
	NoOutcome Outcome = "*"
	// WhiteWon indicates that white won the game.
	WhiteWon Outcome = "1-0"
	// BlackWon indicates that black won the game.
	BlackWon Outcome = "0-1"
	// Draw indicates that game was a draw.
	Draw Outcome = "1/2-1/2"
)

// String implements the fmt.Stringer interface.
func (o Outcome) String() string {
	return string(o)
}

// A Method is the method that generated the outcome.
type Method uint8

const (
	// NoMethod indicates that an outcome hasn't occurred or that the method can't be determined.
	NoMethod Method = iota
	// Checkmate indicates that the game was won checkmate.
	Checkmate
	// Resignation indicates that the game was won by resignation.
	Resignation
	// DrawOffer indicates that the game was drawn by a draw offer.
	DrawOffer
	// Stalemate indicates that the game was drawn by stalemate.
	Stalemate
	// ThreefoldRepetition indicates that the game was drawn when the game
	// state was repeated three times and a player requested a draw.
	ThreefoldRepetition
	// FivefoldRepetition indicates that the game was automatically drawn
	// by the game state being repeated five times.
	FivefoldRepetition
	// FiftyMoveRule indicates that the game was drawn by the half
	// move clock being one hundred or greater when a player requested a draw.
	FiftyMoveRule
	// SeventyFiveMoveRule indicates that the game was automatically drawn
	// when the half move clock was one hundred and fifty or greater.
	SeventyFiveMoveRule
	// InsufficientMaterial indicates that the game was automatically drawn
	// because there was insufficient material for checkmate.
	InsufficientMaterial
)

type TagPairs map[string]string

// A Game represents a single chess game.
type Game struct {
	pos                  *Position
	outcome              Outcome
	tagPairs             TagPairs
	rootMove             *Move // Root of the move tree
	currentMove          *Move // Current position in the tree
	comments             [][]string
	method               Method
	ignoreAutomaticDraws bool
}

// PGN takes a reader and returns a function that updates
// the game to reflect the PGN data.  The PGN can use any
// move notation supported by this package.  The returned
// function is designed to be used in the NewGame constructor.
// An error is returned if there is a problem parsing the PGN data.
func PGN(r io.Reader) (func(*Game), error) {
	scanner := NewScanner(r)

	for scanner.HasNext() {
		gameScanned, err := scanner.ScanGame()

		if err != nil {
			return nil, err
		}

		tokens, err := TokenizeGame(gameScanned)

		if err != nil {
			return nil, err
		}

		parser := NewParser(tokens)

		game, err := parser.Parse()
		if err != nil {
			return nil, err
		}

		return func(g *Game) {
			g.copy(game)
		}, nil
	}
	return nil, nil
}

// FEN takes a string and returns a function that updates
// the game to reflect the FEN data.  Since FEN doesn't encode
// prior moves, the move list will be empty.  The returned
// function is designed to be used in the NewGame constructor.
// An error is returned if there is a problem parsing the FEN data.
func FEN(fen string) (func(*Game), error) {
	pos, err := decodeFEN(fen)
	if err != nil {
		return nil, err
	}
	if pos == nil {
		return nil, errors.New("chess: invalid FEN")
	}
	return func(g *Game) {
		pos.inCheck = isInCheck(pos)
		g.pos = pos
		g.updatePosition()
	}, nil
}

// NewGame defaults to returning a game in the standard
// opening position.  Options can be given to configure
// the game's initial state.
func NewGame(options ...func(*Game)) *Game {
	pos := StartingPosition()
	rootMove := &Move{
		position: pos,
	}

	game := &Game{
		rootMove:    rootMove,
		currentMove: rootMove,
		pos:         pos,
		outcome:     NoOutcome,
		method:      NoMethod,
	}
	for _, f := range options {
		if f != nil {
			f(game)
		}
	}
	return game
}

func (g *Game) AddVariation(parent *Move, newMove *Move) {
	parent.children = append(parent.children, newMove)
	newMove.parent = parent
}

func (g *Game) NavigateToMainLine() {
	current := g.currentMove
	for current.parent != nil {
		if len(current.parent.children) > 0 {
			current = current.parent.children[0]
		}
		current = current.parent
	}
	g.currentMove = current
}

// Move updates the game with the given move.  An error is returned
// if the move is invalid or the game has already been completed.
func (g *Game) Move(m *Move) error {
	valid := moveSlice(g.ValidMoves()).find(m)
	if valid == nil {
		return fmt.Errorf("chess: invalid move %s", m)
	}
	g.MakeMove(m)
	g.updatePosition()
	return nil
}

// MakeMove adds the given move to the game.  The move is not validated and is assumed to be valid.
func (g *Game) MakeMove(move *Move) {
	// For the first move in the game
	if g.currentMove == g.rootMove {
		move.parent = g.rootMove
		g.rootMove.children = append(g.rootMove.children, move)
	} else {
		// Normal move in the main line
		move.parent = g.currentMove
		g.currentMove.children = append(g.currentMove.children, move)
	}

	// Update position
	if newPos := g.pos.Update(move); newPos != nil {
		g.pos = newPos
	}

	move.position = g.pos.copy()

	g.currentMove = move
}

func (g *Game) GoBack() bool {
	if g.currentMove != nil && g.currentMove.parent != nil {
		g.currentMove = g.currentMove.parent
		return true
	}
	return false
}

func (g *Game) GoForward() bool {
	if g.currentMove != nil && len(g.currentMove.children) > 0 {
		g.currentMove = g.currentMove.children[0] // Follow main line
		return true
	}
	return false
}

func (g *Game) IsAtStart() bool {
	return g.currentMove == nil || g.currentMove == g.rootMove
}

func (g *Game) IsAtEnd() bool {
	return g.currentMove != nil && len(g.currentMove.children) == 0
}

// ValidMoves returns a list of valid moves in the
// current position.
func (g *Game) ValidMoves() []Move {
	return g.pos.ValidMoves()
}

// Moves returns the move history of the game following the main line.
func (g *Game) Moves() []*Move {
	if g.rootMove == nil {
		return nil
	}

	moves := make([]*Move, 0)
	current := g.rootMove

	// Traverse the main line (first child of each move)
	for current != nil {
		moves = append(moves, current)
		if len(current.children) == 0 {
			break
		}
		// Follow main line (first variation)
		current = current.children[0]
	}

	return moves[1:] // Skip the root move
}

// Variations returns all alternative moves at the given position.
func (g *Game) Variations(move *Move) []*Move {
	if move == nil || len(move.children) <= 1 {
		return nil
	}
	// Return all moves except the main line (first child)
	return move.children[1:]
}

// Comments returns the comments for the game indexed by moves.
func (g *Game) Comments() [][]string {
	return append([][]string(nil), g.comments...)
}

// Position returns the game's current position.
func (g *Game) Position() *Position {
	return g.pos
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
	return g.pos.String()
}

// String implements the fmt.Stringer interface and returns
// the game's PGN.
func (g *Game) String() string {
	return "TODO"
}

// MarshalText implements the encoding.TextMarshaler interface and
// encodes the game's PGN.
func (g *Game) MarshalText() ([]byte, error) {
	return []byte(g.String()), nil
}

// UnmarshalText implements the encoding.TextUnarshaler interface and
// assumes the data is in the PGN format.
func (g *Game) UnmarshalText(_ []byte) error {
	return errors.New("chess: unmarshal text not implemented")
}

// Draw attempts to draw the game by the given method.  If the
// method is valid, then the game is updated to a draw by that
// method.  If the method isn't valid then an error is returned.
func (g *Game) Draw(method Method) error {
	const halfMoveClockForFiftyMoveRule = 100
	const numOfRepetitionsForThreefoldRepetition = 3

	switch method {
	case ThreefoldRepetition:
		if g.numOfRepetitions() < numOfRepetitionsForThreefoldRepetition {
			return errors.New("chess: draw by ThreefoldRepetition requires at least three repetitions of the current board state")
		}
	case FiftyMoveRule:
		if g.pos.halfMoveClock < halfMoveClockForFiftyMoveRule {
			return errors.New("chess: draw by FiftyMoveRule requires a half move clock of 100 or greater")
		}
	case DrawOffer:
	default:
		return errors.New("chess: invalid draw method")
	}
	g.outcome = Draw
	g.method = method
	return nil
}

// Resign resigns the game for the given color.  If the game has
// already been completed then the game is not updated.
func (g *Game) Resign(color Color) {
	if g.outcome != NoOutcome || color == NoColor {
		return
	}
	if color == White {
		g.outcome = BlackWon
	} else {
		g.outcome = WhiteWon
	}
	g.method = Resignation
}

// EligibleDraws returns valid inputs for the Draw() method.
func (g *Game) EligibleDraws() []Method {
	const halfMoveClockForFiftyMoveRule = 100
	const numOfRepetitionsForThreefoldRepetition = 3

	draws := []Method{DrawOffer}
	if g.numOfRepetitions() >= numOfRepetitionsForThreefoldRepetition {
		draws = append(draws, ThreefoldRepetition)
	}
	if g.pos.halfMoveClock >= halfMoveClockForFiftyMoveRule {
		draws = append(draws, FiftyMoveRule)
	}
	return draws
}

// AddTagPair adds or updates a tag pair with the given key and
// value and returns true if the value is overwritten.
func (g *Game) AddTagPair(k, v string) bool {
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

func (g *Game) updatePosition() {
	method := g.pos.Status()
	if method == Stalemate {
		g.method = Stalemate
		g.outcome = Draw
	} else if method == Checkmate {
		g.method = Checkmate
		g.outcome = WhiteWon
		if g.pos.Turn() == White {
			g.outcome = BlackWon
		}
	}
	if g.outcome != NoOutcome {
		return
	}

	// five fold rep creates automatic draw
	if !g.ignoreAutomaticDraws && g.numOfRepetitions() >= 5 {
		g.outcome = Draw
		g.method = FivefoldRepetition
	}

	// 75 move rule creates automatic draw
	if !g.ignoreAutomaticDraws && g.pos.halfMoveClock >= 150 && g.method != Checkmate {
		g.outcome = Draw
		g.method = SeventyFiveMoveRule
	}

	// insufficient material creates automatic draw
	if !g.ignoreAutomaticDraws && !g.pos.board.hasSufficientMaterial() {
		g.outcome = Draw
		g.method = InsufficientMaterial
	}
}

func (g *Game) copy(game *Game) {
	g.tagPairs = game.tagPairs
	g.rootMove = game.rootMove
	g.currentMove = game.currentMove
	g.pos = game.pos
	g.outcome = game.outcome
	g.method = game.method
	g.comments = game.Comments()
}

func (g *Game) Clone() *Game {
	return &Game{
		tagPairs:    g.tagPairs,
		rootMove:    g.rootMove,
		currentMove: g.currentMove,
		pos:         g.pos,
		outcome:     g.outcome,
		method:      g.method,
	}
}

func (g *Game) Positions() []*Position {
	positions := make([]*Position, 0)
	current := g.rootMove

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
	for _, pos := range g.Positions() {
		if pos == nil {
			continue
		}
		if g.pos.samePosition(pos) {
			count++
		}
	}
	return count
}

func (g *Game) MoveStr(move string) error {
	m, err := AlgebraicNotation{}.Decode(g.pos, move)
	if err != nil {
		return err
	}
	return g.Move(m)
}
