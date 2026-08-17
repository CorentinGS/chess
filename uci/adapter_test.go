package uci_test

import (
	"bytes"
	"log"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
	"github.com/corentings/chess/v3/uci"
)

func Test_FakeAdapter_ExchangeEmptyCommand(t *testing.T) {
	emptyCmd := fakeEmptyCmd{}
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"":   {"ignored"},
			"go": {"bestmove e2e4"},
		},
	}
	t.Run("explicit empty key configured", func(t *testing.T) {
		lines, err := fake.Exchange(emptyCmd)
		if err != nil {
			t.Fatalf("Exchange returned error: %v", err)
		}
		if len(lines) != 1 || lines[0] != "ignored" {
			t.Errorf("expected canned response for empty key, got %v", lines)
		}
	})
	t.Run("unconfigured empty key returns nil", func(t *testing.T) {
		fake2 := &uci.FakeAdapter{Responses: map[string][]string{}}
		lines, err := fake2.Exchange(emptyCmd)
		if err != nil {
			t.Fatalf("Exchange returned error: %v", err)
		}
		if lines != nil {
			t.Errorf("expected nil for unconfigured empty key, got %v", lines)
		}
	})
}

type fakeEmptyCmd struct{}

func (fakeEmptyCmd) String() string                         { return "" }
func (fakeEmptyCmd) IsDone(_ string) bool                   { return false }
func (fakeEmptyCmd) LockRequired() bool                     { return false }
func (fakeEmptyCmd) Handle(_ []string, _ *uci.Engine) error { return nil }

func Test_InfoUnmarshalText_WDLBounds(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		wantErr    bool
		wantErrMsg string
	}{
		{
			name:    "valid wdl with three values",
			input:   "info depth 24 seldepth 32 multipv 1 score cp 29 wdl 791 209 0 nodes 5130101 nps 819897",
			wantErr: false,
		},
		{
			name:    "truncated wdl missing draw and loss",
			input:   "info depth 24 score cp 29 wdl 791",
			wantErr: true,
		},
		{
			name:    "truncated wdl missing loss",
			input:   "info depth 24 score cp 29 wdl 791 209",
			wantErr: true,
		},
		{
			name:       "wdl non-integer value",
			input:      "info depth 24 score cp 29 wdl 791 x 0",
			wantErr:    true,
			wantErrMsg: "uci: invalid info wdl draw",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var info uci.Info
			err := info.UnmarshalText([]byte(tt.input))
			if (err != nil) != tt.wantErr {
				t.Fatalf("UnmarshalText(%q) err = %v, wantErr %v", tt.input, err, tt.wantErr)
			}
			if tt.wantErrMsg != "" && !strings.Contains(err.Error(), tt.wantErrMsg) {
				t.Fatalf("UnmarshalText(%q) err = %v, want substring %q", tt.input, err, tt.wantErrMsg)
			}
		})
	}
}

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

func Test_EngineSearchResultsReturnsDefensiveCopy(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"go": {
				"info depth 10 multipv 1 score cp 50 nodes 1000 pv e2e4 e7e5",
				"info depth 10 multipv 2 score cp 30 nodes 1000 pv d2d4 d7d5",
				"bestmove e2e4",
			},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	pos := chess.StartingPosition()
	if err := eng.Run(uci.CmdPosition{Position: pos}, uci.CmdGo{MoveTime: 100}); err != nil {
		t.Fatal(err)
	}

	results := eng.SearchResults()
	results.Info.PV[0] = chess.Move{}
	results.MultiPVInfo[0] = uci.Info{}
	results.MultiPVInfo[1].PV[0] = chess.Move{}

	fresh := eng.SearchResults()
	if fresh.Info.PV[0] == (chess.Move{}) {
		t.Fatal("SearchResults returned mutable Info.PV backing array")
	}
	if fresh.MultiPVInfo[0].Score.CP != 50 {
		t.Fatalf("SearchResults returned mutable MultiPVInfo backing array, cp = %d", fresh.MultiPVInfo[0].Score.CP)
	}
	if fresh.MultiPVInfo[1].PV[0] == (chess.Move{}) {
		t.Fatal("SearchResults returned mutable MultiPVInfo PV backing array")
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

func Test_FakeAdapter_CmdPositionStoredOnEngine(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"position": {"readyok"},
			"go":       {"bestmove e7e5"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	fenStr := "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
	pos := &chess.Position{}
	if err := pos.UnmarshalText([]byte(fenStr)); err != nil {
		t.Fatal(err)
	}
	cmdPos := uci.CmdPosition{Position: pos}
	cmdGo := uci.CmdGo{MoveTime: 100}
	if err := eng.Run(cmdPos, cmdGo); err != nil {
		t.Fatal(err)
	}

	bestMove := eng.SearchResults().BestMove
	if bestMove == (chess.Move{}) {
		t.Fatal("expected best move decoded against non-starting position")
	}
	if bestMove.S1().String() != "e7" || bestMove.S2().String() != "e5" {
		t.Errorf("expected e7e5, got %s%s", bestMove.S1(), bestMove.S2())
	}
}

func Test_EngineIDReturnsDefensiveCopy(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"uci": {"id name TestEngine", "uciok"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdUCI{}); err != nil {
		t.Fatal(err)
	}

	id := eng.ID()
	id["name"] = "Mutated"
	id["injected"] = "value"

	id2 := eng.ID()
	if id2["name"] != "TestEngine" {
		t.Errorf("ID() returned shared map; external mutation leaked back, got name=%q", id2["name"])
	}
	if _, ok := id2["injected"]; ok {
		t.Errorf("ID() returned shared map; injected key persisted across calls")
	}
}

func Test_EngineOptionsReturnsDefensiveCopy(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"uci": {"option name Hash type spin default 16 min 1 max 1024", "uciok"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdUCI{}); err != nil {
		t.Fatal(err)
	}

	opts := eng.Options()
	opts["Hash"] = uci.Option{Name: "Hash", Type: uci.OptionSpin, Default: "9999"}
	opts["Injected"] = uci.Option{Name: "Injected"}

	opts2 := eng.Options()
	if opts2["Hash"].Default != "16" {
		t.Errorf("Options() returned shared map; external mutation leaked back, got Hash.Default=%q", opts2["Hash"].Default)
	}
	if _, ok := opts2["Injected"]; ok {
		t.Errorf("Options() returned shared map; injected key persisted across calls")
	}
}

func Test_FakeAdapter_FireAndForgetNoResponseShortCircuit(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	if err := eng.Run(uci.CmdUCINewGame{}); err != nil {
		t.Fatalf("CmdUCINewGame should short-circuit on empty response: %v", err)
	}
	if err := eng.Run(uci.CmdStop{}); err != nil {
		t.Fatalf("CmdStop should short-circuit on empty response: %v", err)
	}
	if err := eng.Run(uci.CmdPonderHit{}); err != nil {
		t.Fatalf("CmdPonderHit should short-circuit on empty response: %v", err)
	}
}

func Test_FakeAdapter_CmdSetOptionRendersCorrectly(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"setoption": {},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	cmd := uci.CmdSetOption{Name: "UCI_Elo", Value: "1500"}
	got := cmd.String()
	want := "setoption name UCI_Elo value 1500"
	if got != want {
		t.Errorf("CmdSetOption.String() = %q, want %q", got, want)
	}
	if err := eng.Run(cmd); err != nil {
		t.Fatal(err)
	}
}

func Test_InfoUnmarshalTextWDL(t *testing.T) {
	info := &uci.Info{}
	line := "info depth 24 seldepth 32 multipv 1 score cp 29 wdl 791 209 0 nodes 5130101 nps 819897 hashfull 967 tbhits 0 time 6257 pv d2d4"
	if err := info.UnmarshalText([]byte(line)); err != nil {
		t.Fatal(err)
	}
	if info.Score.Win != 791 || info.Score.Draw != 209 || info.Score.Loss != 0 {
		t.Errorf("wdl parse: got Win=%d Draw=%d Loss=%d, want 791/209/0", info.Score.Win, info.Score.Draw, info.Score.Loss)
	}
	if info.Score.CP != 29 {
		t.Errorf("cp parse: got %d, want 29", info.Score.CP)
	}
	if info.Depth != 24 || info.Nodes != 5130101 || info.Hashfull != 967 {
		t.Errorf("field parse: got depth=%d nodes=%d hashfull=%d, want 24/5130101/967", info.Depth, info.Nodes, info.Hashfull)
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
	san, err := chess.SAN().Encode(pos, bestMove)
	if err != nil {
		t.Fatalf("SAN encode failed: %v", err)
	}
	if san != "e4" {
		t.Errorf("expected e4, got %s", san)
	}
}

func Test_CmdEval_MalformedReturnsError(t *testing.T) {
	pos := chess.StartingPosition()

	t.Run("non-numeric value", func(t *testing.T) {
		fake := &uci.FakeAdapter{
			Responses: map[string][]string{
				"eval": {"Final evaluation notanumber"},
			},
		}
		eng := uci.NewWithAdapter(fake)
		defer eng.Close()

		err := eng.Run(uci.CmdPosition{Position: pos}, uci.CmdEval{})
		if err == nil {
			t.Fatal("expected error for malformed eval value, got nil")
		}
		if !strings.Contains(err.Error(), "notanumber") {
			t.Errorf("expected error to mention the bad value, got %v", err)
		}
	})

	t.Run("missing value", func(t *testing.T) {
		fake := &uci.FakeAdapter{
			Responses: map[string][]string{
				"eval": {"Final evaluation"},
			},
		}
		eng := uci.NewWithAdapter(fake)
		defer eng.Close()

		err := eng.Run(uci.CmdPosition{Position: pos}, uci.CmdEval{})
		if err == nil {
			t.Fatal("expected error for eval line with no value, got nil")
		}
	})
}

func Test_CmdUCI_MalformedOptionLoggedUnderDebug(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"uci": {
				"id name TestEngine",
				"option name Hash type spin default 16 min 1 max 1024",
				"option name Bad type notatype default 1",
				"uciok",
			},
		},
	}

	t.Run("logged and non-fatal when debug on", func(t *testing.T) {
		var buf bytes.Buffer
		eng := uci.NewWithAdapter(fake, uci.Logger(log.New(&buf, "", 0)), uci.Debug)
		defer eng.Close()

		if err := eng.Run(uci.CmdUCI{}); err != nil {
			t.Fatalf("malformed option should not be fatal, got %v", err)
		}

		opts := eng.Options()
		if _, ok := opts["Hash"]; !ok {
			t.Error("expected the valid Hash option to still be parsed")
		}
		if _, ok := opts["Bad"]; ok {
			t.Error("malformed option should not appear in Options()")
		}

		if !strings.Contains(buf.String(), "dropping malformed option line") {
			t.Errorf("expected malformed option to be logged, got %q", buf.String())
		}
	})

	t.Run("silent when debug off", func(t *testing.T) {
		var buf bytes.Buffer
		eng := uci.NewWithAdapter(fake, uci.Logger(log.New(&buf, "", 0)))
		defer eng.Close()

		if err := eng.Run(uci.CmdUCI{}); err != nil {
			t.Fatalf("malformed option should not be fatal, got %v", err)
		}
		if buf.String() != "" {
			t.Errorf("expected no logging without debug, got %q", buf.String())
		}
	})
}

func Test_CmdGo_MalformedInfoLineLoggedUnderDebug(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"go": {
				"info depth abc",
				"info depth 10 score cp 50 nodes 1000 pv e2e4",
				"bestmove e2e4",
			},
		},
	}

	t.Run("logged and non-fatal when debug on", func(t *testing.T) {
		var buf bytes.Buffer
		eng := uci.NewWithAdapter(fake, uci.Logger(log.New(&buf, "", 0)), uci.Debug)
		defer eng.Close()

		pos := chess.StartingPosition()
		if err := eng.Run(uci.CmdPosition{Position: pos}, uci.CmdGo{MoveTime: 100}); err != nil {
			t.Fatalf("malformed info line should not be fatal, got %v", err)
		}

		results := eng.SearchResults()
		if results.BestMove == (chess.Move{}) {
			t.Fatal("expected best move to still be parsed")
		}
		if results.Info.Depth != 10 {
			t.Errorf("expected valid info depth 10 to survive, got %d", results.Info.Depth)
		}

		if !strings.Contains(buf.String(), "dropping unparseable info line") {
			t.Errorf("expected malformed info line to be logged, got %q", buf.String())
		}
	})

	t.Run("silent when debug off", func(t *testing.T) {
		var buf bytes.Buffer
		eng := uci.NewWithAdapter(fake, uci.Logger(log.New(&buf, "", 0)))
		defer eng.Close()

		pos := chess.StartingPosition()
		if err := eng.Run(uci.CmdPosition{Position: pos}, uci.CmdGo{MoveTime: 100}); err != nil {
			t.Fatalf("malformed info line should not be fatal, got %v", err)
		}
		if buf.String() != "" {
			t.Errorf("expected no logging without debug, got %q", buf.String())
		}
	})
}
