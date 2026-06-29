package chess_test

import (
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func assertNoVariations(t *testing.T, g *chess.Game) {
	t.Helper()
	var walk func(node *chess.MoveNode)
	walk = func(node *chess.MoveNode) {
		if children := node.Children(); len(children) > 1 {
			t.Errorf("node has %d children, split games must have 0 variations", len(children))
		}
		for _, c := range node.Children() {
			walk(c)
		}
	}
	walk(g.MoveTree().Root())
}

func TestSplit_DropsManualResignationOnDivergingLine(t *testing.T) {
	g := chess.NewGame()
	for _, m := range []string{"e4", "e5", "Nf3", "Nc6"} {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatalf("push %s: %v", m, err)
		}
	}
	if !g.MoveTree().GoBack() {
		t.Fatal("GoBack failed")
	}
	if _, err := g.PushMove("d6", nil); err != nil {
		t.Fatalf("push d6: %v", err)
	}
	if _, err := g.PushMove("d4", nil); err != nil {
		t.Fatalf("push d4: %v", err)
	}
	if err := g.Resign(chess.White); err != nil {
		t.Fatalf("Resign: %v", err)
	}

	if g.Outcome() != chess.BlackWon || g.Method() != chess.Resignation {
		t.Fatalf("parent outcome/method = %s/%s, want BlackWon/Resignation", g.Outcome(), g.Method())
	}

	splitGames := g.Split()
	if len(splitGames) != 2 {
		t.Fatalf("split game count = %d, want 2", len(splitGames))
	}

	for i, sg := range splitGames {
		if sg.Outcome() != chess.NoOutcome {
			t.Errorf("split[%d] outcome = %s, want NoOutcome (resignation is not recomputed)", i, sg.Outcome())
		}
		if sg.Method() != chess.NoMethod {
			t.Errorf("split[%d] method = %s, want NoMethod", i, sg.Method())
		}
		assertNoVariations(t, sg)
	}
}

func TestSplit_RecomputesTerminalOutcomesFromLeaf(t *testing.T) {
	tests := []struct {
		name        string
		pgn         string
		wantOutcome chess.Outcome
		wantMethod  chess.Method
		wantResult  string
	}{
		{
			name:        "checkmate",
			pgn:         "1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6?? 4. Qxf7# 1-0",
			wantOutcome: chess.WhiteWon,
			wantMethod:  chess.Checkmate,
			wantResult:  "1-0",
		},
		{
			name: "stalemate",
			pgn: `[Event "E"]
[Site "S"]
[Date "2026.06.19"]
[Round "1"]
[White "A"]
[Black "B"]
[Result "1/2-1/2"]

1. e3 a5 2. Qh5 Ra6 3. Qxa5 h5 4. h4 Rah6 5. Qxc7 f6 6. Qxd7+ Kf7 7. Qxb7 Qd3 8. Qxb8 Qh7 9. Qxc8 Kg6 10. Qe6 1/2-1/2`,
			wantOutcome: chess.Draw,
			wantMethod:  chess.Stalemate,
			wantResult:  "1/2-1/2",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g, err := chess.ParsePGN(strings.NewReader(tt.pgn))
			if err != nil {
				t.Fatal(err)
			}

			splitGames := g.Split()
			if len(splitGames) == 0 {
				t.Fatal("expected at least one split game")
			}

			found := false
			for _, sg := range splitGames {
				if sg.Outcome() == tt.wantOutcome && sg.Method() == tt.wantMethod {
					found = true
					if got := sg.GetTagPair("Result"); got != tt.wantResult {
						t.Errorf("Result tag = %q, want %q (Split must sync the Result tag)", got, tt.wantResult)
					}
				}
				assertNoVariations(t, sg)
			}
			if !found {
				t.Errorf("no split line recomputed to %s/%s", tt.wantOutcome, tt.wantMethod)
			}
		})
	}
}

// TestSplit_HonoursIgnoreFlags pins the recomputeOutcomeFromLeaf bug fix:
// Split previously checked insufficient material without consulting the
// Game's ignore flag, so a parent game kept in progress by
// IgnoreInsufficientMaterialDraw was wrongly drawn after Split.
func TestSplit_HonoursIgnoreFlags(t *testing.T) {
	fenOpt, err := chess.FEN("4k3/8/8/8/8/8/8/3K4 w - - 0 60") // KvK: insufficient material
	if err != nil {
		t.Fatal(err)
	}
	// The ignore option must precede FEN: FEN evaluates status while applied,
	// so the flag has to be set first for the parent to stay in progress.
	g := chess.NewGame(chess.IgnoreInsufficientMaterialDraw(), fenOpt)
	if g.Outcome() != chess.NoOutcome {
		t.Fatalf("parent outcome = %s, want NoOutcome (ignore flag honoured at FEN)", g.Outcome())
	}
	// Split yields a game per line; push a quiet king move (position stays KvK
	// insufficient) so there is a line to split.
	if _, err := g.PushMove("Kd2", nil); err != nil {
		t.Fatalf("push Kd2: %v", err)
	}
	if g.Outcome() != chess.NoOutcome {
		t.Fatalf("parent outcome after move = %s, want NoOutcome", g.Outcome())
	}

	splitGames := g.Split()
	if len(splitGames) != 1 {
		t.Fatalf("split game count = %d, want 1", len(splitGames))
	}
	sg := splitGames[0]
	if sg.Outcome() != chess.NoOutcome || sg.Method() != chess.NoMethod {
		t.Errorf("split outcome/method = %s/%s, want NoOutcome/NoMethod (ignore flag must carry through Split)",
			sg.Outcome(), sg.Method())
	}
	assertNoVariations(t, sg)
}

// TestSplit_EmitsAutoDraws pins the observable change: Split now derives
// automatic draws (fivefold, seventy-five move rule) from the rebuilt main
// line. The deleted recomputeOutcomeFromLeaf only handled mate/stalemate and
// insufficient material, so these positions previously stayed NoOutcome.
func TestSplit_EmitsAutoDraws(t *testing.T) {
	t.Run("seventy-five move rule", func(t *testing.T) {
		// Start one half-move below the threshold with sufficient material
		// (rook), then push a quiet rook move to reach clock 150.
		fenOpt, err := chess.FEN("4k3/8/8/8/8/8/8/R3K3 w - - 149 60")
		if err != nil {
			t.Fatal(err)
		}
		g := chess.NewGame(fenOpt)
		if g.Outcome() != chess.NoOutcome {
			t.Fatalf("parent outcome at clock 149 = %s, want NoOutcome", g.Outcome())
		}
		if _, err := g.PushMove("Ra2", nil); err != nil {
			t.Fatalf("push Ra2: %v", err)
		}
		if g.Outcome() != chess.Draw || g.Method() != chess.SeventyFiveMoveRule {
			t.Fatalf("parent outcome/method = %s/%s, want Draw/SeventyFiveMoveRule", g.Outcome(), g.Method())
		}

		splitGames := g.Split()
		if len(splitGames) != 1 {
			t.Fatalf("split game count = %d, want 1", len(splitGames))
		}
		sg := splitGames[0]
		if sg.Outcome() != chess.Draw || sg.Method() != chess.SeventyFiveMoveRule {
			t.Errorf("split outcome/method = %s/%s, want Draw/SeventyFiveMoveRule", sg.Outcome(), sg.Method())
		}
		if got := sg.GetTagPair("Result"); got != "1/2-1/2" {
			t.Errorf("Result tag = %q, want \"1/2-1/2\" (Split must sync the tag)", got)
		}
		assertNoVariations(t, sg)
	})

	t.Run("fivefold repetition", func(t *testing.T) {
		// Shuffle both knights out and back four times: the position returns
		// to the start after each 4-ply cycle, giving five occurrences and an
		// automatic fivefold-repetition draw.
		g := chess.NewGame()
		cycle := []string{"Nf3", "Nf6", "Ng1", "Ng8"}
		for range 4 {
			for _, m := range cycle {
				if _, err := g.PushMove(m, nil); err != nil {
					t.Fatalf("push %s: %v", m, err)
				}
			}
		}
		if g.Outcome() != chess.Draw || g.Method() != chess.FivefoldRepetition {
			t.Fatalf("parent outcome/method = %s/%s, want Draw/FivefoldRepetition", g.Outcome(), g.Method())
		}

		splitGames := g.Split()
		if len(splitGames) != 1 {
			t.Fatalf("split game count = %d, want 1", len(splitGames))
		}
		sg := splitGames[0]
		if sg.Outcome() != chess.Draw || sg.Method() != chess.FivefoldRepetition {
			t.Errorf("split outcome/method = %s/%s, want Draw/FivefoldRepetition", sg.Outcome(), sg.Method())
		}
		if got := sg.GetTagPair("Result"); got != "1/2-1/2" {
			t.Errorf("Result tag = %q, want \"1/2-1/2\" (Split must sync the tag)", got)
		}
		assertNoVariations(t, sg)
	})
}
