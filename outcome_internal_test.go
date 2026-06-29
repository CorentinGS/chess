package chess

import "testing"

func TestClassifyOutcome(t *testing.T) {
	mateBlackMated := "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4" // Qxf7#, black mated -> WhiteWon
	mateWhiteMated := "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 3"      // fool's mate, white mated -> BlackWon
	stalemate := "5k2/5P2/5K2/8/8/8/8/8 b - - 0 1"
	kvk0 := "8/8/8/3k4/3K4/8/8/8 w - - 0 60"      // insufficient material, isolated (clock 0)
	kvk150 := "8/8/8/3k4/3K4/8/8/8 w - - 150 60"  // insufficient + clock 150 (precedence)
	rook := "4k3/8/8/8/8/8/8/R3K3 w - - 10 60"    // sufficient material, no mate
	rook75 := "4k3/8/8/8/8/8/8/R3K3 w - - 150 60" // sufficient material, clock 150

	full := func(overrides ...func(*outcomeRules)) outcomeRules {
		r := outcomeRules{includeAutoDraws: true}
		for _, o := range overrides {
			o(&r)
		}
		return r
	}
	withIgnoreFivefold := func(r *outcomeRules) { r.ignoreFivefold = true }
	withIgnore75 := func(r *outcomeRules) { r.ignoreSeventyFiveMove = true }
	withIgnoreInsufficient := func(r *outcomeRules) { r.ignoreInsufficient = true }

	tests := []struct {
		name        string
		fen         string
		repetitions int
		rules       outcomeRules
		wantOutcome Outcome
		wantMethod  Method
	}{
		// Terminal outcomes (present in both Full and Terminal-only).
		{"checkmate white wins", mateBlackMated, 0, outcomeRules{}, WhiteWon, Checkmate},
		{"checkmate black wins", mateWhiteMated, 0, outcomeRules{}, BlackWon, Checkmate},
		{"checkmate white wins full", mateBlackMated, 5, full(), WhiteWon, Checkmate}, // mate short-circuits, draws ignored
		{"stalemate", stalemate, 0, outcomeRules{}, Draw, Stalemate},

		// Automatic draws (Full only).
		{"fivefold", rook, 5, full(), Draw, FivefoldRepetition},
		{"fivefold ignored", rook, 5, full(withIgnoreFivefold), NoOutcome, NoMethod},
		{"fivefold below threshold", rook, 4, full(), NoOutcome, NoMethod},
		{"seventyfive move", rook75, 0, full(), Draw, SeventyFiveMoveRule},
		{"seventyfive move ignored", rook75, 0, full(withIgnore75), NoOutcome, NoMethod},
		{"insufficient material", kvk0, 0, full(), Draw, InsufficientMaterial},
		{"insufficient material ignored", kvk0, 0, full(withIgnoreInsufficient), NoOutcome, NoMethod},

		// Terminal-only policy never emits automatic draws.
		{"terminal only skips all draws", kvk150, 5, outcomeRules{}, NoOutcome, NoMethod},

		// Precedence: when fivefold, 75-move and insufficient all apply,
		// InsufficientMaterial wins (it is evaluated last).
		{"precedence insufficient wins", kvk150, 5, full(), Draw, InsufficientMaterial},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos, err := decodeFEN(tt.fen)
			if err != nil {
				t.Fatalf("decodeFEN(%q): %v", tt.fen, err)
			}
			if pos == nil {
				t.Fatalf("decodeFEN(%q) returned nil position", tt.fen)
			}
			// decodeFEN does not populate inCheck; production callers always
			// have it set (FEN option / move application), so mirror that here
			// so Status() can detect checkmate.
			pos.inCheck = isInCheck(pos)
			gotOutcome, gotMethod := classifyOutcome(pos, tt.repetitions, tt.rules)
			if gotOutcome != tt.wantOutcome || gotMethod != tt.wantMethod {
				t.Errorf("classifyOutcome = (%s, %s), want (%s, %s)", gotOutcome, gotMethod, tt.wantOutcome, tt.wantMethod)
			}
		})
	}
}
