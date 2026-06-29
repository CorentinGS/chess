package chess

import (
	"errors"
	"fmt"
)

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
		if g.currentPosition().halfMoveClock < halfMoveClockForFiftyMoveRule {
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
	if g.currentPosition().halfMoveClock >= halfMoveClockForFiftyMoveRule {
		draws = append(draws, FiftyMoveRule)
	}
	return draws
}

// evaluatePositionStatus updates the game's outcome and method based on the
// current position. It runs the Full outcome policy (all automatic draws,
// honouring the Game's ignore flags) and is called after every Move and FEN.
func (g *Game) evaluatePositionStatus() {
	g.outcome, g.method = classifyOutcome(g.currentPosition(), g.numOfRepetitions(), fullOutcomeRules(g))
}

// evaluateTerminalPositionStatus updates only board-derived terminal outcomes.
// PGN parsing calls this once on the final main-line position with the
// Terminal-only policy (mate/stalemate only, no automatic draws) so that the
// Result tag and movetext token remain authoritative; resolveOutcome then
// arbitrates between board, tag and token.
func (g *Game) evaluateTerminalPositionStatus() {
	g.outcome, g.method = classifyOutcome(g.currentPosition(), 0, outcomeRules{})
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
