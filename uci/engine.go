package uci

import (
	"log"
	"maps"
	"os"
	"sync"

	"github.com/corentings/chess/v3"
)

// Engine manages communication with a UCI-compatible chess engine such as
// Stockfish. It sends commands, parses responses, and exposes the engine's
// reported state (ID, options, search results).
type Engine struct {
	adapter         Adapter
	logger          *log.Logger
	id              map[string]string
	options         map[string]Option
	mu              *sync.RWMutex
	position        CmdPosition
	hasPos          bool
	results         SearchResults
	eval            int
	debug           bool
	chess960Enabled bool
}

// EngineOption configures an Engine created by New or NewWithAdapter.
type EngineOption func(*Engine)

// Debug returns an EngineOption that logs every command sent to the engine and
// every response line received from it.
func Debug() EngineOption {
	return func(e *Engine) {
		e.debug = true
	}
}

// Logger returns an EngineOption that sets the logger used for debug output.
func Logger(logger *log.Logger) EngineOption {
	return func(e *Engine) {
		e.logger = logger
	}
}

// New creates an Engine backed by a subprocess started from the executable at
// path, configured by opts (for example Debug or Logger).
func New(path string, opts ...EngineOption) (*Engine, error) {
	adapter, err := NewSubprocessAdapter(path)
	if err != nil {
		return nil, err
	}
	return NewWithAdapter(adapter, opts...), nil
}

// NewWithAdapter creates an Engine that communicates through the provided
// Adapter instead of spawning a subprocess, for example an in-memory fake in
// tests or a remote transport.
func NewWithAdapter(adapter Adapter, opts ...EngineOption) *Engine {
	e := &Engine{
		adapter:  adapter,
		logger:   log.New(os.Stdout, "uci", log.LstdFlags),
		mu:       &sync.RWMutex{},
		position: CmdPosition{},
		results:  SearchResults{MultiPVInfo: []Info{}},
	}
	for _, opt := range opts {
		opt(e)
	}
	return e
}

// ID returns a copy of the engine's identification key-value pairs as reported
// in response to the CmdUCI command (e.g. name and author).
func (e *Engine) ID() map[string]string {
	e.mu.RLock()
	defer e.mu.RUnlock()

	cp := maps.Clone(e.id)
	if cp == nil {
		return map[string]string{}
	}
	return cp
}

// Options returns a copy of the engine's available options as reported in
// response to the CmdUCI command.
func (e *Engine) Options() map[string]Option {
	e.mu.RLock()
	defer e.mu.RUnlock()

	cp := maps.Clone(e.options)
	if cp == nil {
		return map[string]Option{}
	}
	return cp
}

// SearchResults returns the results from the most recent CmdGo command.
func (e *Engine) SearchResults() SearchResults {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.results.copy()
}

// Eval returns the static evaluation from the most recent CmdEval command, in
// centipawns. Not all engines support the eval command.
func (e *Engine) Eval() int {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.eval
}

// Run executes the given commands in order, returning the first error
// encountered. Commands that require exclusive access (LockRequired) are
// serialized via the engine's mutex.
func (e *Engine) Run(cmds ...Cmd) error {
	for _, cmd := range cmds {
		if !cmd.LockRequired() {
			if err := e.processCommand(cmd); err != nil {
				return err
			}
		} else {
			if err := e.processCommandLocked(cmd); err != nil {
				return err
			}
		}
	}
	return nil
}

// Pid returns the operating system process ID of the engine subprocess.
// Returns 0 when the engine is not backed by a subprocess (e.g. when created
// with NewWithAdapter using a non-subprocess Adapter).
func (e *Engine) Pid() int {
	if sa, ok := e.adapter.(*SubprocessAdapter); ok {
		return sa.Pid()
	}
	return 0
}

// Close sends a quit command to the engine and closes the underlying adapter.
func (e *Engine) Close() error {
	quitErr := e.Run(CmdQuit{})
	closeErr := e.adapter.Close()
	if quitErr != nil {
		return quitErr
	}
	return closeErr
}

func (e *Engine) processCommandLocked(cmd Cmd) error {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.processCommand(cmd)
}

func (e *Engine) processCommand(cmd Cmd) error {
	if posCmd, ok := cmd.(CmdPosition); ok {
		if err := e.ensureChess960(posCmd.Position); err != nil {
			return err
		}
	}
	if e.debug {
		e.logger.Println(cmd.String())
	}
	lines, err := e.adapter.Exchange(cmd)
	if err != nil {
		return err
	}
	if e.debug {
		for _, line := range lines {
			e.logger.Println(line)
		}
	}
	if posCmd, ok := cmd.(CmdPosition); ok {
		e.position = posCmd
		e.hasPos = true
	}
	return cmd.Handle(lines, e)
}

// ensureChess960 keeps the engine's UCI_Chess960 option aligned with the
// variant of the position about to be sent. A Chess960 position enables the
// option so the engine accepts the king-to-rook castling encoding; a later
// standard position disables it again. The option is toggled only when the
// engine advertises it, and at most once per state change.
func (e *Engine) ensureChess960(pos *chess.Position) error {
	want := pos != nil && pos.Variant() == chess.Chess960
	if want == e.chess960Enabled {
		return nil
	}
	if _, ok := e.options["UCI_Chess960"]; !ok {
		return nil
	}
	value := "false"
	if want {
		value = "true"
	}
	opt := CmdSetOption{Name: "UCI_Chess960", Value: value}
	if e.debug {
		e.logger.Println(opt.String())
	}
	if _, err := e.adapter.Exchange(opt); err != nil {
		return err
	}
	e.chess960Enabled = want
	return nil
}
