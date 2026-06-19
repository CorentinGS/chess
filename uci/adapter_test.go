package uci_test

import (
	"testing"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/uci"
)

func Test_FakeAdapter_CmdUCI(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"uci": {"id name TestEngine", "id author test", "option name Hash type spin default 16 min 1 max 33554432", "uciok"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	err := eng.Run(uci.CmdUCI{})
	if err != nil {
		t.Fatal(err)
	}

	id := eng.ID()
	if id["name"] != "TestEngine" {
		t.Errorf("expected name TestEngine, got %s", id["name"])
	}
	if id["author"] != "test" {
		t.Errorf("expected author test, got %s", id["author"])
	}

	opts := eng.Options()
	if _, ok := opts["Hash"]; !ok {
		t.Error("expected Hash option")
	}
}

func Test_FakeAdapter_CmdGo(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"go": {"info depth 10 score cp 50 nodes 1000 nps 500000 tbhits 0 time 2 pv e2e4", "bestmove e2e4"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	pos := chess.StartingPosition()
	cmdPos := uci.CmdPosition{Position: pos}
	cmdGo := uci.CmdGo{MoveTime: 100}
	if err := eng.Run(cmdPos, cmdGo); err != nil {
		t.Fatal(err)
	}

	results := eng.SearchResults()
	if results.BestMove == (chess.Move{}) {
		t.Fatal("expected best move")
	}
	if results.Info.Depth != 10 {
		t.Errorf("expected depth 10, got %d", results.Info.Depth)
	}
	if results.Info.Score.CP != 50 {
		t.Errorf("expected score cp 50, got %d", results.Info.Score.CP)
	}
}

func Test_FakeAdapter_CmdEval(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"eval": {"Final evaluation 12.5"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	pos := chess.StartingPosition()
	cmdPos := uci.CmdPosition{Position: pos}
	if err := eng.Run(cmdPos, uci.CmdEval{}); err != nil {
		t.Fatal(err)
	}

	eval := eng.Eval()
	if eval != 1250 {
		t.Errorf("expected eval 1250, got %d", eval)
	}
}

func Test_FakeAdapter_MultiPV(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"go": {
				"info depth 10 multipv 1 score cp 50 nodes 1000 pv e2e4",
				"info depth 10 multipv 2 score cp 30 nodes 1000 pv d2d4",
				"bestmove e2e4",
			},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	pos := chess.StartingPosition()
	cmdPos := uci.CmdPosition{Position: pos}
	cmdGo := uci.CmdGo{MoveTime: 100}
	if err := eng.Run(cmdPos, cmdGo); err != nil {
		t.Fatal(err)
	}

	results := eng.SearchResults()
	if len(results.MultiPVInfo) != 2 {
		t.Fatalf("expected 2 MultiPV lines, got %d", len(results.MultiPVInfo))
	}
	if results.MultiPVInfo[0].Score.CP != 50 {
		t.Errorf("expected cp 50 for pv 1, got %d", results.MultiPVInfo[0].Score.CP)
	}
	if results.MultiPVInfo[1].Score.CP != 30 {
		t.Errorf("expected cp 30 for pv 2, got %d", results.MultiPVInfo[1].Score.CP)
	}
}

func Test_FakeAdapter_CmdIsReady(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"isready": {"readyok"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdIsReady{}); err != nil {
		t.Fatal(err)
	}
}

func Test_FakeAdapter_FireAndForgetPassthrough(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdUCINewGame{}); err != nil {
		t.Fatal(err)
	}
	if err := eng.Run(uci.CmdStop{}); err != nil {
		t.Fatal(err)
	}
	if err := eng.Run(uci.CmdPonderHit{}); err != nil {
		t.Fatal(err)
	}
}

func Test_FakeAdapter_FullGame(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"uci":     {"id name FakeFish", "uciok"},
			"isready": {"readyok"},
			"go":      {"info depth 1 score cp 10 nodes 1 pv e2e4", "bestmove e2e4"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdUCI{}, uci.CmdIsReady{}, uci.CmdUCINewGame{}); err != nil {
		t.Fatal(err)
	}

	id := eng.ID()
	if id["name"] != "FakeFish" {
		t.Errorf("expected name FakeFish, got %s", id["name"])
	}

	pos := chess.StartingPosition()
	cmdPos := uci.CmdPosition{Position: pos}
	cmdGo := uci.CmdGo{MoveTime: 100}
	if err := eng.Run(cmdPos, cmdGo); err != nil {
		t.Fatal(err)
	}

	bestMove := eng.SearchResults().BestMove
	if bestMove == (chess.Move{}) {
		t.Fatal("expected best move")
	}
	san := chess.AlgebraicNotation{}.Encode(pos, bestMove)
	if san != "e4" {
		t.Errorf("expected e4, got %s", san)
	}
}
