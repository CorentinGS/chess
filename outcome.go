package chess

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

	var outcome Outcome = NoOutcome
	var method Method = NoMethod

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
