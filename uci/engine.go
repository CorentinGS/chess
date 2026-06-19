package uci

import (
	"log"
	"os"
	"sync"
)

type Engine struct {
	adapter  Adapter
	logger   *log.Logger
	id       map[string]string
	options  map[string]Option
	mu       *sync.RWMutex
	position *CmdPosition
	results  SearchResults
	eval     int
	debug    bool
}

func Debug(e *Engine) {
	e.debug = true
}

func Logger(logger *log.Logger) func(e *Engine) {
	return func(e *Engine) {
		e.logger = logger
	}
}

func New(path string, opts ...func(e *Engine)) (*Engine, error) {
	adapter, err := NewSubprocessAdapter(path)
	if err != nil {
		return nil, err
	}
	return NewWithAdapter(adapter, opts...), nil
}

func NewWithAdapter(adapter Adapter, opts ...func(e *Engine)) *Engine {
	e := &Engine{
		adapter:  adapter,
		logger:   log.New(os.Stdout, "uci", log.LstdFlags),
		mu:       &sync.RWMutex{},
		position: &CmdPosition{},
		results:  SearchResults{MultiPVInfo: []Info{}},
	}
	for _, opt := range opts {
		opt(e)
	}
	return e
}

func (e *Engine) ID() map[string]string {
	e.mu.RLock()
	defer e.mu.RUnlock()

	cp := map[string]string{}
	for k, v := range e.id {
		cp[k] = v
	}
	return cp
}

func (e *Engine) Options() map[string]Option {
	e.mu.RLock()
	defer e.mu.RUnlock()

	cp := map[string]Option{}
	for k, v := range e.options {
		cp[k] = v
	}
	return cp
}

func (e *Engine) SearchResults() SearchResults {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.results
}

func (e *Engine) Eval() int {
	e.mu.RLock()
	defer e.mu.RUnlock()
	return e.eval
}

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
		e.position = &posCmd
	}
	return cmd.Handle(lines, e)
}
