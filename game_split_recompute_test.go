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
	walk(g.GetRootMove())
}

func TestSplit_DropsManualResignationOnDivergingLine(t *testing.T) {
	g := chess.NewGame()
	for _, m := range []string{"e4", "e5", "Nf3", "Nc6"} {
		if err := g.PushMove(m, nil); err != nil {
			t.Fatalf("push %s: %v", m, err)
		}
	}
	if !g.GoBack() {
		t.Fatal("GoBack failed")
	}
	if err := g.PushMove("d6", nil); err != nil {
		t.Fatalf("push d6: %v", err)
	}
	if err := g.PushMove("d4", nil); err != nil {
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
			opt, err := chess.PGN(strings.NewReader(tt.pgn))
			if err != nil {
				t.Fatal(err)
			}
			g := chess.NewGame(opt)

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
