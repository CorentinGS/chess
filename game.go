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
)

// A Outcome is the result of a game.
type Outcome string

const (
	UnknownOutcome Outcome = ""
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
	pos                            *Position  // Current position
	outcome                        Outcome    // Game result
	tagPairs                       TagPairs   // PGN tag pairs
	rootMove                       *MoveNode  // Root of move tree
	currentMove                    *MoveNode  // Current position in tree
	comments                       [][]string // Game comments
	method                         Method     // How the game ended
	ignoreFivefoldRepetitionDraw   bool       // Flag for automatic FivefoldRepetition draw handling
	ignoreSeventyFiveMoveRuleDraw  bool       // Flag for automatic SeventyFiveMoveRule draw handling
	ignoreInsufficientMaterialDraw bool       // Flag for automatic InsufficientMaterial draw handling
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
	rootMove := &MoveNode{
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

// AddVariation appends a new variation as a sibling of parent's main-line
// child. Pass nil for parent to add a variation at the root.
//
// AddVariation edits the move tree directly and bypasses the terminal outcome
// guard enforced by Move / UnsafeMove / Resign. A caller reviewing or analysing
// a finished game can still shape variations without first calling ClearOutcome.
func (g *Game) AddVariation(parent *MoveNode, newMove Move) {
	if parent == nil {
		parent = g.rootMove
	}
	node := &MoveNode{move: newMove, parent: parent}
	if parent.position != nil {
		node.position = parent.position.Update(newMove)
	}
	parent.children = append(parent.children, node)
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

func isMainLine(move *MoveNode) bool {
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
		g.pos = g.currentMove.position.copy()
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

// UnsafeMoves returns all pseudo-legal moves that leave the moving side's king in check.
// These moves are valid piece movements but illegal because they expose the king.
func (g *Game) UnsafeMoves() []Move {
	return g.pos.UnsafeMoves()
}

// Moves returns the main-line move values of the game.
func (g *Game) Moves() []Move {
	nodes := g.MoveNodes()
	moves := make([]Move, len(nodes))
	for i, node := range nodes {
		moves[i] = node.move
	}
	return moves
}

// MoveNodes returns the move nodes of the game following the main line.
func (g *Game) MoveNodes() []*MoveNode {
	if g.rootMove == nil {
		return nil
	}

	moves := make([]*MoveNode, 0)
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
	if g.rootMove == nil || len(g.rootMove.children) == 0 {
		return []*MoveHistory{}
	}

	history := make([]*MoveHistory, 0)
	current := g.rootMove

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

// GetRootMove returns the root move of the game.
func (g *Game) GetRootMove() *MoveNode {
	return g.rootMove
}

// Variations returns all alternative moves at the given position.
func (g *Game) Variations(move *MoveNode) []*MoveNode {
	if move == nil || len(move.children) <= 1 {
		return nil
	}
	// Return all moves except the main line (first child)
	return move.children[1:]
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

// OutcomeMethodPair pairs an Outcome with the Method that produced it.
type OutcomeMethodPair struct {
	Outcome Outcome
	Method  Method
}

// SetOutcomeMethod sets the game's outcome and method together after validating
// that the pair is internally consistent. It returns an error for invalid pairs
// such as WhiteWon with Stalemate or Draw with Checkmate. It does not modify
// any Result tag in tagPairs.
func (g *Game) SetOutcomeMethod(pair OutcomeMethodPair) error {
	if !validOutcomeMethodPair(pair) {
		return fmt.Errorf("chess: invalid outcome/method pair: outcome=%s method=%s", pair.Outcome, pair.Method)
	}
	g.outcome = pair.Outcome
	g.method = pair.Method
	return nil
}

func validOutcomeMethodPair(pair OutcomeMethodPair) bool {
	if pair.Outcome == UnknownOutcome {
		return false
	}
	switch pair.Outcome {
	case NoOutcome:
		return pair.Method == NoMethod
	case WhiteWon, BlackWon:
		switch pair.Method {
		case NoMethod, Checkmate, Resignation:
			return true
		}
	case Draw:
		switch pair.Method {
		case NoMethod, DrawOffer, Stalemate, ThreefoldRepetition,
			FivefoldRepetition, FiftyMoveRule, SeventyFiveMoveRule, InsufficientMaterial:
			return true
		}
	}
	return false
}

// ClearOutcome resets the game to NoOutcome/NoMethod. It does not modify any
// tag in tagPairs (including the Result tag): PGN tag metadata is treated as
// caller-owned data, separate from the live outcome. Callers that want PGN
// tag parity must update tagPairs["Result"] themselves, or rebuild from the
// game state via Split which does sync the tag.
func (g *Game) ClearOutcome() {
	g.outcome = NoOutcome
	g.method = NoMethod
}

// FEN returns the FEN notation of the current position.
func (g *Game) FEN() string {
	return g.pos.String()
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
// Returns ErrGameAlreadyEnded if the game is already in a terminal state.
func (g *Game) Draw(method Method) error {
	const halfMoveClockForFiftyMoveRule = 100
	const numOfRepetitionsForThreefoldRepetition = 3

	if g.outcome != NoOutcome {
		return ErrGameAlreadyEnded
	}
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
// already been completed or the color is invalid, Resign returns an error
// and does not update the game.
func (g *Game) Resign(color Color) error {
	if color == NoColor {
		return errors.New("chess: cannot resign with NoColor")
	}
	if g.outcome != NoOutcome {
		return ErrGameAlreadyEnded
	}
	if color == White {
		g.outcome = BlackWon
	} else {
		g.outcome = WhiteWon
	}
	g.method = Resignation
	return nil
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
	switch method {
	case Stalemate:
		g.method = Stalemate
		g.outcome = Draw
	case Checkmate:
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
	if !g.ignoreFivefoldRepetitionDraw && g.numOfRepetitions() >= 5 {
		g.outcome = Draw
		g.method = FivefoldRepetition
	}

	// 75 move rule creates automatic draw
	if !g.ignoreSeventyFiveMoveRuleDraw && g.pos.halfMoveClock >= 150 && g.method != Checkmate {
		g.outcome = Draw
		g.method = SeventyFiveMoveRule
	}

	// insufficient material creates automatic draw
	if !g.ignoreInsufficientMaterialDraw && !g.pos.board.hasSufficientMaterial() {
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
	g.comments = copyComments(game.comments)
	g.ignoreFivefoldRepetitionDraw = game.ignoreFivefoldRepetitionDraw
	g.ignoreSeventyFiveMoveRuleDraw = game.ignoreSeventyFiveMoveRuleDraw
	g.ignoreInsufficientMaterialDraw = game.ignoreInsufficientMaterialDraw
}

// Clone returns a deep copy of the game.
func (g *Game) Clone() *Game {
	ret := &Game{}
	ret.copy(g)

	// we have to also deep copy the moves so that modifications to the
	// clone do not impact the parent
	ret.rootMove = g.rootMove.clone()
	if g.currentMove == nil {
		ret.currentMove = ret.rootMove
	} else {
		ret.currentMove = findClonedMove(g.rootMove, ret.rootMove, g.currentMove)
		if ret.currentMove == nil {
			ret.currentMove = ret.rootMove
		}
	}
	ret.pos = ret.currentMove.position

	return ret
}

func findClonedMove(original, clone, target *MoveNode) *MoveNode {
	if original == nil || clone == nil || target == nil {
		return nil
	}
	if original == target {
		return clone
	}
	for i, child := range original.children {
		if i >= len(clone.children) {
			return nil
		}
		if found := findClonedMove(child, clone.children[i], target); found != nil {
			return found
		}
	}
	return nil
}

// Positions returns all positions in the game in the main line.
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
		if g.pos.SamePosition(pos) {
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

// Deprecated: use PushNotationMove instead.
//
// PushMove adds a move in algebraic notation to the game.
// Returns an error if the move is invalid.
// This method now validates moves for consistency with other move methods.
//
// Example:
//
//	err := game.PushMove("e4", &PushMoveOptions{ForceMainline: true})
func (g *Game) PushMove(algebraicMove string, options *PushMoveOptions) error {
	return g.PushNotationMove(algebraicMove, AlgebraicNotation{}, options)
}

// PushNotationMove adds a move to the game using any supported notation.
// It validates the move before adding it to ensure game correctness.
// For high-performance scenarios where moves are pre-validated, use UnsafePushNotationMove.
//
// Example:
//
//	err := game.PushNotationMove("e4", chess.AlgebraicNotation{}, &PushMoveOptions{ForceMainline: true})
//	if err != nil {
//	  panic(err)
//	}
//
//	game.PushNotationMove("c7c5", chess.UCINotation{}, nil)
//	game.PushNotationMove("Nc1f3", chess.LongAlgebraicNotation{}, nil)
func (g *Game) PushNotationMove(moveStr string, notation Notation, options *PushMoveOptions) error {
	move, err := notation.Decode(g.pos, moveStr)
	if err != nil {
		return err
	}

	return g.Move(move, options)
}

// UnsafePushNotationMove adds a move to the game using any supported notation without validation.
// This method is intended for high-performance scenarios where moves are known to be valid.
// Use this method only when you have already validated the move or are certain it's legal.
// For general use, prefer PushNotationMove which includes validation.
//
// Example:
//
//	// Only use when you're certain the move is valid
//	err := game.UnsafePushNotationMove("e4", chess.AlgebraicNotation{}, nil)
//	if err != nil {
//	    panic(err) // Should not happen with valid notation/moves
//	}
func (g *Game) UnsafePushNotationMove(moveStr string, notation Notation, options *PushMoveOptions) error {
	move, err := notation.Decode(g.pos, moveStr)
	if err != nil {
		return err
	}

	return g.UnsafeMove(move, options)
}

// Move method adds a move to the game using a Move struct.
// It returns an error if the move is invalid.
// This method validates the move before adding it to ensure game correctness.
// For high-performance scenarios where moves are pre-validated, use UnsafeMove.
//
// Example:
//
//	possibleMove := game.ValidMoves()[0]
//
//	err := game.Move(possibleMove, nil)
//	if err != nil {
//	    panic(err)
//	}
func (g *Game) Move(move Move, options *PushMoveOptions) error {
	if options == nil {
		options = &PushMoveOptions{}
	}

	// Validate the move before adding it
	if err := g.validateMove(move); err != nil {
		return err
	}

	return g.moveUnchecked(move, options)
}

// UnsafeMove adds a move to the game without validation.
// This method is intended for high-performance scenarios where moves are known to be valid.
// Use this method only when you have already validated the move or are certain it's legal.
// For general use, prefer the Move method which includes validation.
//
// Example:
//
//	// Only use when you're certain the move is valid
//	validMoves := game.ValidMoves()
//	move := validMoves[0] // We know this is valid
//	err := game.UnsafeMove(move, nil)
//	if err != nil {
//	    panic(err) // Should not happen with valid moves
//	}
func (g *Game) UnsafeMove(move Move, options *PushMoveOptions) error {
	if options == nil {
		options = &PushMoveOptions{}
	}

	return g.moveUnchecked(move, options)
}

// moveUnchecked is the internal implementation that performs the move without validation.
// This is shared by both Move (after validation) and MoveUnchecked.
func (g *Game) moveUnchecked(move Move, options *PushMoveOptions) error {
	if g.outcome != NoOutcome {
		return ErrGameAlreadyEnded
	}

	existingMove := g.findExistingMove(move)
	node := g.addOrReorderMove(move, existingMove, options.ForceMainline)

	g.updatePosition(node)
	g.currentMove = node

	g.evaluatePositionStatus()

	return nil
}

// validateMove checks if the given move is valid for the current position.
// It returns an error if the move is invalid.
func (g *Game) validateMove(move Move) error {
	if g.pos == nil {
		return errors.New("no current position")
	}

	// Check if the move exists in the list of valid moves for the current position
	validMoves := g.pos.ValidMovesUnsafe()
	for _, validMove := range validMoves {
		if validMove.s1 == move.s1 && validMove.s2 == move.s2 && validMove.promo == move.promo {
			return nil // Move is valid
		}
	}

	return fmt.Errorf("move %s is not valid for the current position", move.String())
}

func (g *Game) findExistingMove(move Move) *MoveNode {
	if g.currentMove == nil {
		return nil
	}
	for _, child := range g.currentMove.children {
		if child.move.s1 == move.s1 && child.move.s2 == move.s2 && child.move.promo == move.promo {
			return child
		}
	}
	return nil
}

func (g *Game) addOrReorderMove(move Move, existingMove *MoveNode, forceMainline bool) *MoveNode {
	if existingMove != nil {
		if forceMainline && existingMove != g.currentMove.children[0] {
			g.reorderMoveToFront(existingMove)
		}
		return existingMove
	}
	return g.addNewMove(move, forceMainline)
}

func (g *Game) reorderMoveToFront(move *MoveNode) {
	children := g.currentMove.children
	for i, child := range children {
		if child == move {
			copy(children[1:i+1], children[:i])
			children[0] = move
			break
		}
	}
}

func (g *Game) addNewMove(move Move, forceMainline bool) *MoveNode {
	node := &MoveNode{move: move, parent: g.currentMove}
	if forceMainline {
		g.currentMove.children = append([]*MoveNode{node}, g.currentMove.children...)
	} else {
		g.currentMove.children = append(g.currentMove.children, node)
	}
	return node
}

func (g *Game) updatePosition(node *MoveNode) {
	if newPos := g.pos.Update(node.move); newPos != nil {
		g.pos = newPos
		node.position = newPos
	}
}

// Split takes a Game with a main line and 0 or more variations and returns a
// slice of Games (one for each variation), each containing exactly only a main
// line and 0 variations
func (g *Game) Split() []*Game {
	// Collect all move paths starting from the root's children
	var paths [][]*MoveNode
	for _, m := range g.rootMove.children {
		paths = append(paths, collectPaths(m)...)
	}

	// Build a Game for each path
	var games []*Game
	for _, path := range paths {
		newG := g.buildOneGameFromPath(path)
		games = append(games, newG)
	}

	return games
}

// collectPaths returns all paths from the given move to each leaf node.
// Each path is represented as a slice of *MoveNode, starting with the given node
// and ending with a leaf (a move with no children).
func collectPaths(node *MoveNode) [][]*MoveNode {
	if node == nil {
		return nil
	}
	// If leaf, return a single path containing this node
	if len(node.children) == 0 {
		return [][]*MoveNode{{node}}
	}
	// Otherwise, collect paths from each child and prepend this node
	var paths [][]*MoveNode
	for _, c := range node.children {
		childPaths := collectPaths(c)
		for _, p := range childPaths {
			path := append([]*MoveNode{node}, p...)
			paths = append(paths, path)
		}
	}
	return paths
}

func (g *Game) buildOneGameFromPath(path []*MoveNode) *Game {
	rootMove := &MoveNode{position: g.rootMove.position.copy()}
	cur := rootMove

	for _, m := range path {
		child := &MoveNode{
			move:          m.move,
			position:      m.position.copy(),
			number:        m.number,
			nag:           m.nag,
			commentBlocks: copyCommentBlocks(m.commentBlocks),
		}
		child.parent = cur

		cur.children = []*MoveNode{child}
		cur = child
	}

	newG := &Game{}
	newG.copy(g)
	newG.rootMove = rootMove
	newG.currentMove = cur
	newG.pos = cur.position

	newG.recomputeOutcomeFromLeaf()

	return newG
}

func (g *Game) recomputeOutcomeFromLeaf() {
	leaf := g.pos
	if leaf == nil {
		g.outcome = NoOutcome
		g.method = NoMethod
		g.syncResultTag()
		return
	}

	switch leaf.Status() {
	case Checkmate:
		g.method = Checkmate
		if leaf.Turn() == White {
			g.outcome = BlackWon
		} else {
			g.outcome = WhiteWon
		}
	case Stalemate:
		g.method = Stalemate
		g.outcome = Draw
	default:
		g.outcome = NoOutcome
		g.method = NoMethod
		if leaf.board != nil && !leaf.board.hasSufficientMaterial() {
			g.outcome = Draw
			g.method = InsufficientMaterial
		}
	}

	g.syncResultTag()
}

func (g *Game) syncResultTag() {
	switch g.outcome {
	case NoOutcome:
		delete(g.tagPairs, "Result")
	case WhiteWon:
		g.tagPairs["Result"] = "1-0"
	case BlackWon:
		g.tagPairs["Result"] = "0-1"
	case Draw:
		g.tagPairs["Result"] = "1/2-1/2"
	}
}

// ValidateSAN checks if a string is valid Standard Algebraic Notation (SAN) syntax.
// This function only validates the syntax, not whether the move is legal in any position.
// Examples of valid SAN: "e4", "Nf3", "O-O", "Qxd2+", "e8=Q#"
func ValidateSAN(s string) error {
	_, err := algebraicNotationParts(s)
	return err
}

// IgnoreFivefoldRepetitionDraw returns a Game option that disables automatic draws
// caused by the fivefold repetition rule. When applied, the game will not
// automatically end in a draw if the same position occurs five times.
func IgnoreFivefoldRepetitionDraw() func(*Game) {
	return func(g *Game) {
		g.ignoreFivefoldRepetitionDraw = true
	}
}

// IgnoreSeventyFiveMoveRuleDraw returns a Game option that disables automatic draws
// triggered by the seventy-five move rule. When applied, the game will not
// automatically end in a draw if one hundred fifty half-moves pass without a pawn move or capture.
func IgnoreSeventyFiveMoveRuleDraw() func(*Game) {
	return func(g *Game) {
		g.ignoreSeventyFiveMoveRuleDraw = true
	}
}

// IgnoreInsufficientMaterialDraw returns a Game option that disables automatic draws
// caused by insufficient material. When applied, the game will not automatically
// end in a draw even if checkmate is impossible with the remaining pieces.
func IgnoreInsufficientMaterialDraw() func(*Game) {
	return func(g *Game) {
		g.ignoreInsufficientMaterialDraw = true
	}
}
