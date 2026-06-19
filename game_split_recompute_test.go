package chess_test

import (
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestSplitRecomputesResignationOnDivergingLine(t *testing.T) {
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
	g.Resign(chess.White)

	if g.Outcome() != chess.BlackWon || g.Method() != chess.Resignation {
		t.Fatalf("parent outcome/method = %s/%s, want BlackWon/Resignation", g.Outcome(), g.Method())
	}

	splitGames := g.Split()
	if len(splitGames) != 2 {
		t.Fatalf("split game count = %d, want 2", len(splitGames))
	}

	for i, sg := range splitGames {
		if sg.Outcome() != chess.NoOutcome {
			t.Errorf("split[%d] outcome = %s, want NoOutcome", i, sg.Outcome())
		}
		if sg.Method() != chess.NoMethod {
			t.Errorf("split[%d] method = %s, want NoMethod", i, sg.Method())
		}
	}
}

func TestSplitRecomputesCheckmateFromLeaf(t *testing.T) {
	opt, err := chess.PGN(strings.NewReader("1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6?? 4. Qxf7# 1-0"))
	if err != nil {
		t.Fatal(err)
	}
	g := chess.NewGame(opt)

	if g.Outcome() != chess.WhiteWon || g.Method() != chess.Checkmate {
		t.Fatalf("parent outcome/method = %s/%s, want WhiteWon/Checkmate", g.Outcome(), g.Method())
	}

	splitGames := g.Split()
	if len(splitGames) == 0 {
		t.Fatal("expected at least one split game")
	}

	foundMate := false
	for _, sg := range splitGames {
		if sg.Outcome() == chess.WhiteWon && sg.Method() == chess.Checkmate {
			foundMate = true
		}
	}
	if !foundMate {
		t.Errorf("no split line recomputed to WhiteWon/Checkmate")
	}
}
