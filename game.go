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
	"slices"
	"strings"
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

// TagPairs represents a collection of PGN tag pairs.
type TagPairs map[string]string

// A Game represents a single chess game.
type Game struct {
	pos                  *Position  // Current position
	outcome              Outcome    // Game result
	tagPairs             TagPairs   // PGN tag pairs
	rootMove             *Move      // Root of move tree
	currentMove          *Move      // Current position in tree
	comments             [][]string // Game comments
	method               Method     // How the game ended
	ignoreAutomaticDraws bool       // Flag for automatic draw handling
}

// PGN takes a reader and returns a function that updates
// the game to reflect the PGN data.  The PGN can use any
// move notation supported by this package.  The returned
// function is designed to be used in the NewGame constructor.
// An error is returned if there is a problem parsing the PGN data.
func PGN(r io.Reader) (func(*Game), error) {
	scanner := NewScanner(r)

	if !scanner.HasNext() {
		return nil, ErrNoGameFound
	}

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

	// Return a function that updates the game with the parsed game state
	return func(g *Game) {
		g.copy(game)
	}, nil
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
		g.rootMove.position = pos
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
	rootMove := &Move{
		position: pos,
	}

	game := &Game{
		rootMove:    rootMove,
		tagPairs:    make(map[string]string),
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

// AddVariation adds a new variation to the game.
// The parent move must be a move in the game or nil to add a variation to the root.
func (g *Game) AddVariation(parent *Move, newMove *Move) {
	parent.children = append(parent.children, newMove)
	newMove.parent = parent
}

// NavigateToMainLine navigates to the main line of the game.
// The main line is the first child of each move.
func (g *Game) NavigateToMainLine() {
	current := g.currentMove

	// First, navigate up to find a move that's part of the main line
	for current.parent != nil && !isMainLine(current) {
		current = current.parent
	}

	// If there are no moves in the game, stay at root
	if len(g.rootMove.children) == 0 {
		g.currentMove = g.rootMove
		return
	}

	// Otherwise, navigate to the first move of the main line
	g.currentMove = g.rootMove.children[0]
}

func isMainLine(move *Move) bool {
	if move.parent == nil {
		return true
	}
	return move == move.parent.children[0] && isMainLine(move.parent)
}

// GoBack navigates to the previous move in the game.
// Returns true if the move was successful. Returns false if there are no moves to go back to.
// If the game is at the start, it will return false.
func (g *Game) GoBack() bool {
	if g.currentMove != nil && g.currentMove.parent != nil {
		g.currentMove = g.currentMove.parent
		g.pos = g.currentMove.position.copy()
		return true
	}
	return false
}

// GoForward navigates to the next move in the game.
// Returns true if the move was successful. Returns false if there are no moves to go forward to.
// If the game is at the end, it will return false.
func (g *Game) GoForward() bool {
	// Check if current move exists and has children
	if g.currentMove != nil && len(g.currentMove.children) > 0 {
		g.currentMove = g.currentMove.children[0] // Follow main line
		g.pos = g.currentMove.position
		return true
	}
	return false
}

// IsAtStart returns true if the game is at the start.
func (g *Game) IsAtStart() bool {
	return g.currentMove == nil || g.currentMove == g.rootMove
}

// IsAtEnd returns true if the game is at the end.
func (g *Game) IsAtEnd() bool {
	return g.currentMove != nil && len(g.currentMove.children) == 0
}

// ValidMoves returns all legal moves in the current position.
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

// GetRootMove returns the root move of the game.
func (g *Game) GetRootMove() *Move {
	return g.rootMove
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
// Comments returns the comments for the game indexed by moves.
func (g *Game) Comments() [][]string {
	if g.comments == nil {
		return [][]string{}
	}
	return append([][]string(nil), g.comments...)
}

// Position returns the game's current position.
func (g *Game) Position() *Position {
	return g.pos
}

// CurrentPosition returns the game's current move position.
// This is the position at the current pointer in the move tree.
// This should be used to get the current position of the game instead of Position().
func (g *Game) CurrentPosition() *Position {
	if g.currentMove == nil {
		return g.pos
	}

	return g.currentMove.position
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
	var sb strings.Builder

	var tagPairList = make([]sortableTagPair, len(g.tagPairs))

	var idx uint = 0
	for tag, value := range g.tagPairs {
		tagPairList[idx] = sortableTagPair{
			Key:   tag,
			Value: value,
		}
		idx++
	}

	slices.SortFunc(tagPairList, cmpTags)

	// Write tag pairs.
	for _, tagPair := range tagPairList {
		sb.WriteString(fmt.Sprintf("[%s \"%s\"]\n", tagPair.Key, tagPair.Value))
	}

	// Append empty line after tag pairs as per definition
	if len(g.tagPairs) > 0 {
		sb.WriteString("\n")
	}

	// Assume g.rootMove is a dummy root (holding the initial position)
	// and that its first child is the first actual move.
	if g.rootMove != nil && len(g.rootMove.children) > 0 {
		writeMoves(g.rootMove, 1, true, &sb, false)
	}

	// Append the game result.
	sb.WriteString(g.Outcome().String()) // outcomeString() returns the result as a string (e.g. "1-0")
	return sb.String()
}

// sortableTagPair is its own
type sortableTagPair struct {
	Key   string
	Value string
}

// Compares two tags to determine in which order they should be brought up
func cmpTags(a, b sortableTagPair) int {
	// Don't re-order duplicate keys
	if a.Key == b.Key {
		return 0
	}

	// PGN defined tags take priority
	for _, req := range []string{
		"Event",
		"Site",
		"Date",
		"Round",
		"White",
		"Black",
		"Result",
	} {
		if a.Key == req {
			return -1
		}
		if b.Key == req {
			return +1
		}
	}

	// Finally compare the keys directly and sort by ascending
	if a.Key < b.Key {
		return -1
	} else if b.Key < a.Key {
		return +1
	}
	return 0
}

// writeMoves recursively writes the PGN-formatted move sequence starting from the given move node into the provided strings.Builder.
// It handles move numbering for white and black moves, encodes moves using algebraic notation based on the appropriate position,
// and appends comments and command annotations if present. The function distinguishes between main line moves and sub-variations;
// when processing a sub-variation, moves are enclosed in parentheses.
//
// Parameters:
//
//	node - pointer to the current move node from which to write moves.
//	moveNum - the current move number corresponding to white’s moves.
//	isWhite - true if it is white’s move, false if it is black’s move.
//	sb - pointer to a strings.Builder where the formatted move notation is appended.
//	subVariation - true if the current call is within a sub-variation, affecting formatting details.
//
// The function recurses through the move tree, writing the main line first and then processing any additional variations,
// ensuring that the output adheres to standard PGN conventions. Future enhancements may include support for all NAG values.
func writeMoves(node *Move, moveNum int, isWhite bool, sb *strings.Builder, subVariation bool) {
	// If no moves remain, stop.
	if node == nil {
		return
	}

	var currentMove *Move

	// The main line is the first child.
	if subVariation {
		currentMove = node
	} else {
		if len(node.children) == 0 {
			return // nothing to print if no child exists (should not happen for a proper game)
		}
		currentMove = node.children[0]
	}

	writeMoveNumber(moveNum, isWhite, subVariation, sb)

	// Encode the move using your AlgebraicNotation.
	writeMoveEncoding(node, currentMove, sb)

	// Append a comment if present.
	writeComments(currentMove, sb)

	writeCommands(currentMove, sb)

	//TODO: Add support for all nags values in the future

	// if subvariation is over don't add space
	if !subVariation {
		sb.WriteString(" ")
	} else if len(currentMove.children) > 0 {
		sb.WriteString(" ")
	}

	// Process any variations (children beyond the first).
	// In PGN, variations are enclosed in parentheses.
	writeVariations(node, moveNum, isWhite, sb)

	if len(currentMove.children) > 0 {
		var nextMoveNum int
		var nextIsWhite bool
		if isWhite {
			// After white’s move, black plays using the same move number.
			nextMoveNum = moveNum
			nextIsWhite = false
		} else {
			// After black’s move, increment move number.
			nextMoveNum = moveNum + 1
			nextIsWhite = true
		}
		writeMoves(currentMove, nextMoveNum, nextIsWhite, sb, false)
	}
}

func writeMoveNumber(moveNum int, isWhite bool, subVariation bool, sb *strings.Builder) {
	if isWhite {
		sb.WriteString(fmt.Sprintf("%d. ", moveNum))
	} else if subVariation {
		sb.WriteString(fmt.Sprintf("%d... ", moveNum))
	}
}

func writeMoveEncoding(node *Move, currentMove *Move, sb *strings.Builder) {
	if node.Parent() == nil {
		sb.WriteString(AlgebraicNotation{}.Encode(node.Position(), currentMove))
	} else {
		moveStr := AlgebraicNotation{}.Encode(node.Parent().Position(), currentMove)
		sb.WriteString(moveStr)
	}
}

func writeComments(move *Move, sb *strings.Builder) {
	if move.comments != "" {
		sb.WriteString(" {" + move.comments + "}")
	}
}

func writeCommands(move *Move, sb *strings.Builder) {
	if len(move.command) > 0 {
		sb.WriteString(" {")
		for key, value := range move.command {
			sb.WriteString(" [%" + key + " " + value + "]")
		}
		sb.WriteString(" }")
	}
}

func writeVariations(node *Move, moveNum int, isWhite bool, sb *strings.Builder) {
	if len(node.children) > 1 {
		for i := 1; i < len(node.children); i++ {
			variation := node.children[i]
			sb.WriteString("(")
			writeMoves(variation, moveNum, isWhite, sb, true)
			sb.WriteString(") ")
		}
	}
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

	toGame, err := PGN(r)
	if err != nil {
		return err
	}
	toGame(g)

	return nil
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

// evaluatePositionStatus updates the game's outcome and method based on the current position.
func (g *Game) evaluatePositionStatus() {
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

// copy copies the game state from the given game.
func (g *Game) copy(game *Game) {
	g.tagPairs = make(map[string]string)
	for k, v := range game.tagPairs {
		g.tagPairs[k] = v
	}
	g.rootMove = game.rootMove
	g.currentMove = game.currentMove
	g.pos = game.pos
	g.outcome = game.outcome
	g.method = game.method
	g.comments = game.Comments()
	g.ignoreAutomaticDraws = game.ignoreAutomaticDraws
}

// Clone returns a deep copy of the game.
func (g *Game) Clone() *Game {
	return &Game{
		tagPairs:             g.tagPairs,
		rootMove:             g.rootMove,
		currentMove:          g.currentMove,
		pos:                  g.pos,
		outcome:              g.outcome,
		method:               g.method,
		comments:             g.Comments(),
		ignoreAutomaticDraws: g.ignoreAutomaticDraws,
	}
}

// Positions returns all positions in the game.
// This includes the starting position and all positions after each move.
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

// PushMoveOptions contains options for pushing a move to the game
type PushMoveOptions struct {
	// ForceMainline makes this move the main line if variations exist
	ForceMainline bool
}

// PushMove adds a move in algebraic notation to the game.
// Returns an error if the move is invalid.
//
// Example:
//
//	err := game.PushMove("e4", &PushMoveOptions{ForceMainline: true})
func (g *Game) PushMove(algebraicMove string, options *PushMoveOptions) error {
	if options == nil {
		options = &PushMoveOptions{}
	}

	move, err := g.parseAndValidateMove(algebraicMove)
	if err != nil {
		return err
	}

	existingMove := g.findExistingMove(move)
	g.addOrReorderMove(move, existingMove, options.ForceMainline)

	g.updatePosition(move)
	g.currentMove = move

	// Add this line to evaluate the position after the move
	g.evaluatePositionStatus()

	return nil
}

func (g *Game) parseAndValidateMove(algebraicMove string) (*Move, error) {
	tokens, err := TokenizeGame(&GameScanned{Raw: algebraicMove})
	if err != nil {
		return nil, errors.New("failed to tokenize move")
	}

	parser := NewParser(tokens)
	parser.game = g
	parser.currentMove = g.currentMove

	move, err := parser.parseMove()
	if err != nil {
		return nil, err
	}

	if g.pos == nil {
		return nil, errors.New("no current position")
	}

	return move, nil
}

func (g *Game) findExistingMove(move *Move) *Move {
	if g.currentMove == nil {
		return nil
	}
	for _, child := range g.currentMove.children {
		if child.s1 == move.s1 && child.s2 == move.s2 && child.promo == move.promo {
			return child
		}
	}
	return nil
}

func (g *Game) addOrReorderMove(move, existingMove *Move, forceMainline bool) {
	move.parent = g.currentMove

	if existingMove != nil {
		if forceMainline && existingMove != g.currentMove.children[0] {
			g.reorderMoveToFront(existingMove)
		}
	} else {
		g.addNewMove(move, forceMainline)
	}
}

func (g *Game) reorderMoveToFront(move *Move) {
	children := g.currentMove.children
	for i, child := range children {
		if child == move {
			copy(children[1:i+1], children[:i])
			children[0] = move
			break
		}
	}
}

func (g *Game) addNewMove(move *Move, forceMainline bool) {
	if forceMainline {
		g.currentMove.children = append([]*Move{move}, g.currentMove.children...)
	} else {
		g.currentMove.children = append(g.currentMove.children, move)
	}
}

func (g *Game) updatePosition(move *Move) {
	if newPos := g.pos.Update(move); newPos != nil {
		g.pos = newPos
		move.position = newPos
	}
}
