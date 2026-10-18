package chess

import (
	"errors"
	"fmt"
)

// SetOutcome sets the game's outcome and the method that produced it, after
// checking that they agree: a win takes Checkmate, Resignation, TimeForfeit,
// Abandoned, Adjudication, RulesInfraction or NoMethod; a draw takes a draw
// rule, DrawOffer, TimeForfeit, Adjudication or NoMethod; NoOutcome takes
// NoMethod. It does not change the Result tag.
func (g *Game) SetOutcome(outcome Outcome, method Method) error {
	if !validOutcomeMethod(outcome, method) {
		return fmt.Errorf("chess: invalid outcome/method pair: outcome=%s method=%s", outcome, method)
	}
	g.outcome = outcome
	g.method = method
	return nil
}

func validOutcomeMethod(outcome Outcome, method Method) bool {
	switch outcome {
	case NoOutcome:
		return method == NoMethod
	case WhiteWon, BlackWon:
		switch method {
		case NoMethod, Checkmate, Resignation, TimeForfeit, Abandoned, Adjudication, RulesInfraction:
			return true
		}
	case Draw:
		switch method {
		case NoMethod, DrawOffer, Stalemate, ThreefoldRepetition,
			FivefoldRepetition, FiftyMoveRule, SeventyFiveMoveRule, InsufficientMaterial,
			TimeForfeit, Adjudication:
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
	if g.outcome != NoOutcome {
		return ErrGameAlreadyEnded
	}
	switch method {
	case ThreefoldRepetition:
		if !g.canClaimThreefoldRepetition() {
			return errors.New("chess: draw by ThreefoldRepetition requires at least three repetitions of the current board state")
		}
	case FiftyMoveRule:
		if !g.canClaimFiftyMoveRule() {
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
	draws := []Method{DrawOffer}
	if g.canClaimThreefoldRepetition() {
		draws = append(draws, ThreefoldRepetition)
	}
	if g.canClaimFiftyMoveRule() {
		draws = append(draws, FiftyMoveRule)
	}
	return draws
}

// canClaimThreefoldRepetition reports whether the current position has
// occurred at least three times, so a player may claim a draw.
func (g *Game) canClaimThreefoldRepetition() bool {
	const numOfRepetitionsForThreefoldRepetition = 3
	return g.numOfRepetitions() >= numOfRepetitionsForThreefoldRepetition
}

// canClaimFiftyMoveRule reports whether fifty moves by each side have passed
// without a capture or pawn move, so a player may claim a draw.
func (g *Game) canClaimFiftyMoveRule() bool {
	const halfMoveClockForFiftyMoveRule = 100
	return g.currentPosition().halfMoveClock >= halfMoveClockForFiftyMoveRule
}

// evaluateOutcome updates the game's outcome and method based on the
// current position. It runs the Full outcome policy (all automatic draws,
// honouring the Game's ignore flags) and is called after every Move and FEN.
func (g *Game) evaluateOutcome() {
	g.outcome, g.method = classifyOutcome(g.currentPosition(), g.numOfRepetitions(), fullOutcomeRules(g))
}

// IgnoreFivefoldRepetitionDraw returns a Game option that disables automatic draws
// caused by the fivefold repetition rule. When applied, the game will not
// automatically end in a draw if the same position occurs five times.
func IgnoreFivefoldRepetitionDraw() GameOption {
	return func(g *Game) {
		g.ignoreFivefoldRepetitionDraw = true
	}
}

// IgnoreSeventyFiveMoveRuleDraw returns a Game option that disables automatic draws
// triggered by the seventy-five move rule. When applied, the game will not
// automatically end in a draw if one hundred fifty half-moves pass without a pawn move or capture.
func IgnoreSeventyFiveMoveRuleDraw() GameOption {
	return func(g *Game) {
		g.ignoreSeventyFiveMoveRuleDraw = true
	}
}

// IgnoreInsufficientMaterialDraw returns a Game option that disables automatic draws
// caused by insufficient material. When applied, the game will not automatically
// end in a draw even if checkmate is impossible with the remaining pieces.
func IgnoreInsufficientMaterialDraw() GameOption {
	return func(g *Game) {
		g.ignoreInsufficientMaterialDraw = true
	}
}
