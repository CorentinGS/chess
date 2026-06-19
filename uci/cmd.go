package uci

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
	"time"

	"github.com/corentings/chess/v3"
)

// Cmd represents a command sent to a UCI engine. Each implementation defines
// how the command is serialized (String), when the engine's response is
// complete (IsDone), how to process the response (Handle), and whether the
// command requires exclusive access to the engine (LockRequired).
type Cmd interface {
	fmt.Stringer
	IsDone(line string) bool
	Handle(lines []string, e *Engine) error
	LockRequired() bool
}

// CmdUCI sends the "uci" command and waits for the "uciok" response. The engine
// reports its identity and available options.
type CmdUCI struct{}

func (CmdUCI) String() string { return "uci" }

func (CmdUCI) IsDone(line string) bool { return line == "uciok" }

func (CmdUCI) LockRequired() bool { return true }

func (CmdUCI) Handle(lines []string, e *Engine) error {
	e.id = map[string]string{}
	e.options = map[string]Option{}
	for _, text := range lines {
		k, v, err := parseIDLine(text)
		if err == nil {
			e.id[k] = v
			continue
		}
		o := &Option{}
		if err = o.UnmarshalText([]byte(text)); err == nil {
			e.options[o.Name] = *o
		}
	}
	return nil
}

// CmdIsReady sends the "isready" command and waits for the "readyok" response,
// confirming the engine is ready to accept further commands.
type CmdIsReady struct{}

func (CmdIsReady) String() string { return "isready" }

func (CmdIsReady) IsDone(line string) bool { return line == "readyok" }

func (CmdIsReady) LockRequired() bool { return true }

func (CmdIsReady) Handle(_ []string, _ *Engine) error { return nil }

// CmdUCINewGame sends the "ucinewgame" command, signaling the engine to clear
// any state from previous games (e.g. hash tables).
type CmdUCINewGame struct{}

func (CmdUCINewGame) String() string { return "ucinewgame" }

func (CmdUCINewGame) IsDone(_ string) bool { return true }

func (CmdUCINewGame) LockRequired() bool { return true }

func (CmdUCINewGame) Handle(_ []string, _ *Engine) error { return nil }

// CmdPonderHit sends the "ponderhit" command, informing the engine that the
// opponent has played the move it was pondering.
type CmdPonderHit struct{}

func (CmdPonderHit) String() string { return "ponderhit" }

func (CmdPonderHit) IsDone(_ string) bool { return true }

func (CmdPonderHit) LockRequired() bool { return false }

func (CmdPonderHit) Handle(_ []string, _ *Engine) error { return nil }

// CmdStop sends the "stop" command, instructing the engine to stop calculating
// and return the best move found so far.
type CmdStop struct{}

func (CmdStop) String() string { return "stop" }

func (CmdStop) IsDone(_ string) bool { return true }

func (CmdStop) LockRequired() bool { return false }

func (CmdStop) Handle(_ []string, _ *Engine) error { return nil }

// CmdQuit sends the "quit" command, instructing the engine to exit.
type CmdQuit struct{}

func (CmdQuit) String() string { return "quit" }

func (CmdQuit) IsDone(_ string) bool { return true }

func (CmdQuit) LockRequired() bool { return true }

func (CmdQuit) Handle(_ []string, _ *Engine) error { return nil }

// CmdEval sends the "eval" command and parses the engine's static evaluation
// output. Not all engines support this command.
type CmdEval struct{}

func (CmdEval) String() string { return "eval" }

func (CmdEval) IsDone(line string) bool {
	lower := strings.ToLower(line)
	return strings.HasPrefix(line, "Final evaluation") ||
		strings.Contains(lower, "error") ||
		strings.Contains(lower, "unknown command")
}

func (CmdEval) LockRequired() bool { return true }

func (CmdEval) Handle(lines []string, e *Engine) error {
	for _, text := range lines {
		lower := strings.ToLower(text)
		if strings.Contains(lower, "error") || strings.Contains(lower, "unknown command") {
			return errors.New("eval command not supported")
		}
		if strings.HasPrefix(text, "Final evaluation") {
			parts := strings.Fields(text)
			if len(parts) >= 3 {
				evalStr := parts[2]
				eval, err := strconv.ParseFloat(evalStr, 64)
				if err == nil {
					e.eval = int(math.Round(eval * 100))
				}
				break
			}
		}
	}
	return nil
}

// CmdSetOption sends a "setoption" command to change an engine option at runtime.
type CmdSetOption struct {
	Name  string
	Value string
}

func (cmd CmdSetOption) String() string {
	return fmt.Sprintf("setoption name %s value %s", cmd.Name, cmd.Value)
}

func (CmdSetOption) IsDone(_ string) bool { return true }

func (CmdSetOption) LockRequired() bool { return false }

func (CmdSetOption) Handle(_ []string, _ *Engine) error { return nil }

// CmdPosition sends a "position" command to set the engine's current position,
// optionally applying a sequence of moves from the starting position or a FEN.
type CmdPosition struct {
	Position *chess.Position
	Moves    []chess.Move
}

func (cmd CmdPosition) String() string {
	if cmd.Position == nil {
		cmd.Position = chess.StartingPosition()
	}
	if len(cmd.Moves) == 0 {
		return "position fen " + cmd.Position.String()
	}
	moveStrs := []string{}
	for _, m := range cmd.Moves {
		mStr := chess.UCINotation{}.Encode(nil, m)
		moveStrs = append(moveStrs, mStr)
	}
	return fmt.Sprintf("position fen %s moves %s", cmd.Position, strings.Join(moveStrs, " "))
}

func (CmdPosition) IsDone(_ string) bool { return true }

func (CmdPosition) LockRequired() bool { return true }

func (CmdPosition) Handle(_ []string, _ *Engine) error { return nil }

// CmdGo sends a "go" command to start the engine's search. The fields control
// search parameters such as time limits, depth, and node counts.
type CmdGo struct {
	SearchMoves    []chess.Move
	WhiteTime      time.Duration
	BlackTime      time.Duration
	WhiteIncrement time.Duration
	BlackIncrement time.Duration
	MovesToGo      int
	Depth          int
	Nodes          int
	Mate           int
	MoveTime       time.Duration
	Ponder         bool
	Infinite       bool
}

func (cmd CmdGo) String() string {
	a := []string{"go"}
	if cmd.Ponder {
		a = append(a, "ponder")
	}
	if cmd.WhiteTime > 0 {
		a = append(a, "wtime", msecStr(cmd.WhiteTime))
	}
	if cmd.BlackTime > 0 {
		a = append(a, "btime", msecStr(cmd.BlackTime))
	}
	if cmd.WhiteIncrement > 0 {
		a = append(a, "winc", msecStr(cmd.WhiteIncrement))
	}
	if cmd.BlackIncrement > 0 {
		a = append(a, "binc", msecStr(cmd.BlackIncrement))
	}
	if cmd.MovesToGo > 0 {
		a = append(a, "movestogo", strconv.Itoa(cmd.MovesToGo))
	}
	if cmd.Depth > 0 {
		a = append(a, "depth", strconv.Itoa(cmd.Depth))
	}
	if cmd.Nodes > 0 {
		a = append(a, "nodes", strconv.Itoa(cmd.Nodes))
	}
	if cmd.Mate > 0 {
		a = append(a, "mate", strconv.Itoa(cmd.Mate))
	}
	if cmd.MoveTime > 0 {
		a = append(a, "movetime", msecStr(cmd.MoveTime))
	}
	if cmd.Infinite {
		a = append(a, "infinite")
	}
	if len(cmd.SearchMoves) > 0 {
		a = append(a, "searchmoves")
		for _, m := range cmd.SearchMoves {
			mStr := chess.UCINotation{}.Encode(nil, m)
			a = append(a, mStr)
		}
	}
	return strings.Join(a, " ")
}

func (CmdGo) IsDone(line string) bool {
	return strings.HasPrefix(line, "bestmove")
}

func (CmdGo) LockRequired() bool { return true }

func (CmdGo) Handle(lines []string, e *Engine) error {
	const maxParts = 4

	results := SearchResults{MultiPVInfo: make([]Info, 1)}
	for _, text := range lines {
		if strings.HasPrefix(text, "bestmove") {
			parts := strings.Split(text, " ")
			if len(parts) <= 1 {
				return errors.New("best move not found " + text)
			}
			var position *chess.Position
			if e.hasPos {
				position = e.position.Position
			}
			bestMove, err := chess.UCINotation{}.Decode(position, parts[1])
			if err != nil {
				return err
			}
			results.BestMove = bestMove
			if len(parts) >= maxParts {
				ponderMove, decodeErr := chess.UCINotation{}.Decode(position, parts[3])
				if decodeErr != nil {
					return decodeErr
				}
				results.Ponder = ponderMove
			}
			continue
		}

		info := &Info{}
		err := info.UnmarshalText([]byte(text))
		if err != nil {
			continue
		}

		if info.Multipv == 0 || info.Multipv == 1 {
			results.Info = *info
		}

		if info.Multipv > 1 && info.Multipv < 300 {
			currentPVCount := len(results.MultiPVInfo)
			if info.Multipv > currentPVCount {
				for i := currentPVCount; i < info.Multipv; i++ {
					results.MultiPVInfo = append(results.MultiPVInfo, Info{})
				}
			}
			results.MultiPVInfo[info.Multipv-1] = *info
		}
	}
	results.MultiPVInfo[0] = results.Info
	e.results = results
	return nil
}

func parseIDLine(s string) (string, string, error) {
	const numParts = 3
	if !strings.HasPrefix(s, "id") {
		return "", "", errors.New("uci: invalid id line")
	}
	parts := strings.Split(s, " ")

	if len(parts) < numParts {
		return "", "", errors.New("uci: invalid id line")
	}
	return parts[1], strings.Join(parts[2:], " "), nil
}

func msecStr(dur time.Duration) string {
	return strconv.Itoa(int(dur / time.Millisecond))
}
