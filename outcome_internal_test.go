package chess

import (
	"strings"
	"testing"
)

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

func TestArbitratePGNOutcome(t *testing.T) {
	tests := []struct {
		name               string
		boardOutcome       Outcome
		boardMethod        Method
		tagOutcome         Outcome
		tokenOutcome       Outcome
		wantOutcome        Outcome
		wantMethod         Method
		wantConflict       bool
		wantConflictSubstr string
	}{
		// No sources: NoOutcome / NoMethod.
		{"all empty", NoOutcome, NoMethod, NoOutcome, NoOutcome, NoOutcome, NoMethod, false, ""},

		// Empty / unknown result spellings are treated as NoOutcome (no conflict).
		{"unknown tag normalised", NoOutcome, NoMethod, NoOutcome, NoOutcome, NoOutcome, NoMethod, false, ""},
		{"unknown token normalised", NoOutcome, NoMethod, NoOutcome, NoOutcome, NoOutcome, NoMethod, false, ""},

		// Tag only.
		{"tag white wins", NoOutcome, NoMethod, WhiteWon, NoOutcome, WhiteWon, NoMethod, false, ""},
		{"tag draw", NoOutcome, NoMethod, Draw, NoOutcome, Draw, NoMethod, false, ""},
		{"tag black wins", NoOutcome, NoMethod, BlackWon, NoOutcome, BlackWon, NoMethod, false, ""},

		// Token only.
		{"token white wins", NoOutcome, NoMethod, NoOutcome, WhiteWon, WhiteWon, NoMethod, false, ""},
		{"token draw", NoOutcome, NoMethod, NoOutcome, Draw, Draw, NoMethod, false, ""},

		// Token beats tag when they agree.
		{"tag and token agree", NoOutcome, NoMethod, WhiteWon, WhiteWon, WhiteWon, NoMethod, false, ""},

		// Token and tag conflict (no board).
		{"token conflicts with tag", NoOutcome, NoMethod, BlackWon, WhiteWon, NoOutcome, NoMethod, true, "movetext result token conflicts with Result tag"},

		// Board-terminal wins over tag/token.
		{"board checkmate beats agreeing tag and token", WhiteWon, Checkmate, WhiteWon, WhiteWon, WhiteWon, Checkmate, false, ""},
		{"board stalemate beats agreeing tag and token", Draw, Stalemate, Draw, Draw, Draw, Stalemate, false, ""},
		{"board terminal tag omitted token omitted", WhiteWon, Checkmate, NoOutcome, NoOutcome, WhiteWon, Checkmate, false, ""},

		// Board-terminal conflict with token.
		{"board checkmate conflicts with token", WhiteWon, Checkmate, NoOutcome, BlackWon, NoOutcome, NoMethod, true, "movetext result token conflicts with board-derivable outcome"},
		// Board-terminal conflict with tag (token agrees).
		{"board checkmate conflicts with tag", WhiteWon, Checkmate, BlackWon, WhiteWon, NoOutcome, NoMethod, true, "Result tag conflicts with board-derivable outcome"},

		// Non-terminal board method (e.g. Resignation carried on a FEN-rooted
		// game — shouldn't normally happen at PGN parse since the policy is
		// Terminal-only, but the function must not treat it as terminal).
		{"non-terminal board method ignored", NoOutcome, Resignation, NoOutcome, WhiteWon, WhiteWon, NoMethod, false, ""},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotOutcome, gotMethod, err := arbitratePGNOutcome(tt.boardOutcome, tt.boardMethod, tt.tagOutcome, tt.tokenOutcome)
			if tt.wantConflict {
				if err == nil {
					t.Fatalf("expected conflict error, got nil")
				}
				if !strings.Contains(err.Error(), tt.wantConflictSubstr) {
					t.Errorf("error = %q, want substring %q", err.Error(), tt.wantConflictSubstr)
				}
				if gotOutcome != NoOutcome || gotMethod != NoMethod {
					t.Errorf("conflict returned (%s, %s), want (NoOutcome, NoMethod)", gotOutcome, gotMethod)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if gotOutcome != tt.wantOutcome || gotMethod != tt.wantMethod {
				t.Errorf("arbitratePGNOutcome = (%s, %s), want (%s, %s)", gotOutcome, gotMethod, tt.wantOutcome, tt.wantMethod)
			}
		})
	}
}
