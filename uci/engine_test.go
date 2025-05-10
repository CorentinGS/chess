//go:build engine

package uci_test

import (
	"fmt"
	"os/exec"
	"testing"
	"time"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/uci"
)

var engines = []string{"stockfish", "lc0"}

func isEngineAvailable(engine string) bool {
	_, err := exec.LookPath(engine)
	return err == nil
}

func Test_EngineInfo(t *testing.T) {
	for _, name := range engines {
		fenStr := "r1bq1rk1/ppp2ppp/2n2n2/3pp3/3P4/2P1PN2/PP1N1PPP/R1BQ1RK1 w - - 0 8"

		t.Run("EngineInfo_"+name, func(t *testing.T) {
			if !isEngineAvailable(name) {
				t.Skipf("engine %s not available", name)
			}

			pos := &chess.Position{}
			if err := pos.UnmarshalText([]byte(fenStr)); err != nil {
				t.Fatal("failed to parse FEN", err)
			}

			eng, err := uci.New(name, uci.Debug)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()

			cmdMultiPV := uci.CmdSetOption{Name: "multipv", Value: "2"}
			cmdPos := uci.CmdPosition{Position: pos}
			cmdGo := uci.CmdGo{MoveTime: time.Second / 10}
			if err := eng.Run(uci.CmdUCI, uci.CmdIsReady, uci.CmdUCINewGame, cmdMultiPV, cmdPos, cmdGo); err != nil {
				t.Fatal("failed to run command", err)
			}

			move := eng.SearchResults().Info.PV[0]
			moveStr := chess.AlgebraicNotation{}.Encode(pos, move)

			if moveStr != "Ne5" {
				t.Errorf("expected Ne5, got %s", moveStr)
			}
		})
	}
}

func Test_EngineMultiPVInfo(t *testing.T) {
	for _, name := range engines {
		fenStr := "r1bq1rk1/ppp2ppp/2n2n2/3pp3/3P4/2P1PN2/PP1N1PPP/R1BQ1RK1 w - - 0 8"

		t.Run("EngineMultiPVInfo_"+name, func(t *testing.T) {
			if !isEngineAvailable(name) {
				t.Skipf("engine %s not available", name)
			}

			pos := &chess.Position{}
			if err := pos.UnmarshalText([]byte(fenStr)); err != nil {
				t.Fatal("failed to parse FEN", err)
			}

			eng, err := uci.New(name, uci.Debug)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()

			cmdMultiPV := uci.CmdSetOption{Name: "multipv", Value: "2"}
			cmdPos := uci.CmdPosition{Position: pos}
			cmdGo := uci.CmdGo{MoveTime: time.Second / 10}
			if err := eng.Run(uci.CmdUCI, uci.CmdIsReady, uci.CmdUCINewGame, cmdMultiPV, cmdPos, cmdGo); err != nil {
				t.Fatal("failed to run command", err)
			}

			multiPVInfo := eng.SearchResults().MultiPVInfo

			if len(multiPVInfo) != 2 {
				t.Errorf("expected 2 MultiPV lines, got %d", len(multiPVInfo))
			}

			move := multiPVInfo[0].PV[0]
			moveStr := chess.AlgebraicNotation{}.Encode(pos, move)
			if moveStr != "Ne5" {
				t.Errorf("expected Ne5, got %s", moveStr)
			}

			move = multiPVInfo[1].PV[0]
			moveStr = chess.AlgebraicNotation{}.Encode(pos, move)
			if moveStr != "e5" {
				t.Errorf("expected e5, got %s", moveStr)
			}
		})
	}
}

func Test_UCIMovesTags(t *testing.T) {
	for _, name := range engines {
		t.Run("UCIMovesTags_"+name, func(t *testing.T) {
			if !isEngineAvailable(name) {
				t.Skipf("engine %s not available", name)
			}

			eng, err := uci.New(name, uci.Debug)
			if err != nil {
				t.Fatal(err)
			}
			defer eng.Close()
			setOpt := uci.CmdSetOption{Name: "UCI_Elo", Value: "1500"}
			setPos := uci.CmdPosition{Position: chess.StartingPosition()}
			setGo := uci.CmdGo{MoveTime: time.Second / 10}
			if err := eng.Run(uci.CmdUCI, uci.CmdIsReady, setOpt, uci.CmdUCINewGame, setPos, setGo); err != nil {
				t.Fatal("failed to run command", err)
			}

			game := chess.NewGame()
			notation := chess.AlgebraicNotation{}

			for game.Outcome() == chess.NoOutcome {
				cmdPos := uci.CmdPosition{Position: game.Position()}
				cmdGo := uci.CmdGo{MoveTime: time.Second / 100}
				if err2 := eng.Run(cmdPos, cmdGo); err2 != nil {
					t.Fatal("failed to run command", err2)
				}

				move := eng.SearchResults().BestMove
				pos := game.Position()
				san := notation.Encode(pos, move)

				err = game.PushMove(san, nil)
				if err != nil {
					t.Fatal(fmt.Sprintf("failed to push move %s - %s - %v. Pos: %s", san, move.String(), move.HasTag(chess.Capture), pos.String()), err)
				}
			}
		})
	}
}
