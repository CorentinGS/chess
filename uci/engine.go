package uci

import (
	"log"
	"os"
	"sync"
)

// Engine manages communication with a UCI-compatible chess engine such as
// Stockfish. It sends commands, parses responses, and exposes the engine's
// reported state (ID, options, search results).
type Engine struct {
	adapter  Adapter
	logger   *log.Logger
	id       map[string]string
	options  map[string]Option
	mu       *sync.RWMutex
	position CmdPosition
	hasPos   bool
	results  SearchResults
	eval     int
	debug    bool
}

// Debug enables debug logging of commands sent to and responses received from
// the engine. It is intended to be passed as an option to New.
func Debug(e *Engine) {
	e.debug = true
}

// Logger sets the logger the engine uses for debug output. It is intended to be
// passed as an option to New.
func Logger(logger *log.Logger) func(e *Engine) {
	return func(e *Engine) {
		e.logger = logger
	}
}

// New creates an Engine backed by a subprocess started from the executable at
// the given path. Optional configuration functions (e.g. Debug, Logger) may be
// passed to customize the engine.
func New(path string, opts ...func(e *Engine)) (*Engine, error) {
	adapter, err := NewSubprocessAdapter(path)
	if err != nil {
		return nil, err
	}
	return NewWithAdapter(adapter, opts...), nil
}

// NewWithAdapter creates an Engine that communicates through the provided
// Adapter instead of spawning a subprocess. This is primarily useful for
// testing with a FakeAdapter.
func NewWithAdapter(adapter Adapter, opts ...func(e *Engine)) *Engine {
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

	cp := map[string]string{}
	for k, v := range e.id {
		cp[k] = v
	}
	return cp
}

// Options returns a copy of the engine's available options as reported in
// response to the CmdUCI command.
func (e *Engine) Options() map[string]Option {
	e.mu.RLock()
	defer e.mu.RUnlock()

	cp := map[string]Option{}
	for k, v := range e.options {
		cp[k] = v
	}
	return cp
}

// SearchResults returns the results from the most recent CmdGo command.
func (e *Engine) SearchResults() SearchResults {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.results
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

// Getpid returns the operating system process ID of the engine subprocess.
// Returns 0 when the engine is not backed by a subprocess (e.g. when created
// with NewWithAdapter using a non-subprocess Adapter).
func (e *Engine) Getpid() int {
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
