package chess

import "errors"

// outcomeRules configures classifyOutcome. The zero value is Terminal-only:
// only checkmate and stalemate are detected and the auto-draw fields are
// ignored. Populate includeAutoDraws to enable the automatic draw rules,
// gated individually by their ignore flags.
type outcomeRules struct {
	includeAutoDraws      bool
	ignoreFivefold        bool
	ignoreSeventyFiveMove bool
	ignoreInsufficient    bool
}

// classifyOutcome derives the automatic outcome for a position. It is a pure
// function of pos, repetitions and rules: it never reads or writes Game state,
// the move tree, or tag pairs.
//
// A terminal mate/stalemate short-circuits and returns immediately. Otherwise,
// when includeAutoDraws is set, the automatic draw rules apply in the order
// fivefold repetition, seventy-five move rule, insufficient material, each
// later match overwriting the earlier one. This precedence matches the
// historical Move/FEN evaluation so that InsufficientMaterial wins when several
// rules apply to the same position.
func classifyOutcome(pos *Position, repetitions int, rules outcomeRules) (Outcome, Method) {
	switch pos.Status() {
	case Stalemate:
		return Draw, Stalemate
	case Checkmate:
		if pos.Turn() == White {
			return BlackWon, Checkmate
		}
		return WhiteWon, Checkmate
	}

	if !rules.includeAutoDraws {
		return NoOutcome, NoMethod
	}

	var outcome = NoOutcome
	var method = NoMethod

	if !rules.ignoreFivefold && repetitions >= 5 {
		outcome = Draw
		method = FivefoldRepetition
	}

	if !rules.ignoreSeventyFiveMove && pos.halfMoveClock >= 150 {
		outcome = Draw
		method = SeventyFiveMoveRule
	}

	if !rules.ignoreInsufficient && !pos.board.hasSufficientMaterial() {
		outcome = Draw
		method = InsufficientMaterial
	}

	return outcome, method
}

// fullOutcomeRules builds the Full ruleset from a Game's ignore flags. It is
// the translation layer between the public functional-option API on Game and
// the pure classifyOutcome policy.
func fullOutcomeRules(g *Game) outcomeRules {
	return outcomeRules{
		includeAutoDraws:      true,
		ignoreFivefold:        g.ignoreFivefoldRepetitionDraw,
		ignoreSeventyFiveMove: g.ignoreSeventyFiveMoveRuleDraw,
		ignoreInsufficient:    g.ignoreInsufficientMaterialDraw,
	}
}

// arbitratePGNOutcome reconciles the three outcome sources visible during PGN
// parsing: the board-derived terminal outcome (checkmate/stalemate only, since
// PGN parsing uses the Terminal-only classifyOutcome policy), the Result tag,
// and the movetext result token (1-0 / 0-1 / 1/2-1/2 / *).
//
// Precedence: a board-terminal outcome wins and conflicts with tag/token are
// errors; otherwise the movetext token wins; otherwise the Result tag wins;
// otherwise NoOutcome. UnknownOutcome (the empty Result spelling) is treated
// as NoOutcome. The returned error's message is preserved verbatim by the
// parser when it wraps the error into *ParserError.
//
// Pure: no Game state, no allocation on the success path. The parser owns the
// side effect of writing the result back onto the Game.
func arbitratePGNOutcome(boardOutcome Outcome, boardMethod Method, tagOutcome, tokenOutcome Outcome) (Outcome, Method, error) {
	normalize := func(o Outcome) Outcome {
		if o == UnknownOutcome {
			return NoOutcome
		}
		return o
	}
	tagOutcome = normalize(tagOutcome)
	tokenOutcome = normalize(tokenOutcome)

	boardTerminal := boardMethod == Checkmate || boardMethod == Stalemate

	if boardTerminal {
		if tokenOutcome != NoOutcome && tokenOutcome != boardOutcome {
			return NoOutcome, NoMethod, errors.New("movetext result token conflicts with board-derivable outcome")
		}
		if tagOutcome != NoOutcome && tagOutcome != boardOutcome {
			return NoOutcome, NoMethod, errors.New("Result tag conflicts with board-derivable outcome")
		}
		return boardOutcome, boardMethod, nil
	}

	if tokenOutcome != NoOutcome {
		if tagOutcome != NoOutcome && tagOutcome != tokenOutcome {
			return NoOutcome, NoMethod, errors.New("movetext result token conflicts with Result tag")
		}
		return tokenOutcome, NoMethod, nil
	}

	if tagOutcome != NoOutcome {
		return tagOutcome, NoMethod, nil
	}

	return NoOutcome, NoMethod, nil
}
