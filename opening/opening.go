// Package opening implements chess opening determination and exploration.
package opening

import (
	"sync"

	"github.com/corentings/chess/v2"
)

// A Opening represents a specific sequence of moves from the staring position.
type Opening struct {
	game *chess.Game
	mu   sync.Mutex
	code string
	title string
	pgn   string
}

func newOpening(code, title, pgn string) *Opening {
	return &Opening{
		code:  code,
		title: title,
		pgn:   pgn,
	}
}

// Code returns the Encyclopaedia of Chess Openings (ECO) code.
func (o *Opening) Code() string {
	return o.code
}

// Title returns the Encyclopaedia of Chess Openings (ECO) title of the opening.
func (o *Opening) Title() string {
	return o.title
}

// PGN returns the opening in PGN format.
func (o *Opening) PGN() string {
	return o.pgn
}

// Game returns the opening as a game.
// It lazily constructs the game on first call and caches it.
// It is safe for concurrent use.
func (o *Opening) Game() *chess.Game {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.game != nil {
		return o.game
	}
	game := chess.NewGame()
	for _, moveStr := range parseMoveList(o.pgn) {
		pos := game.Position()
		m, err := chess.UCINotation{}.Decode(pos, moveStr)
		if err != nil {
			return nil
		}
		if err := game.Move(m, nil); err != nil {
			return nil
		}
	}
	o.game = game
	return o.game
}

// Book is an opening book that returns openings for move sequences.
type Book interface {
	// Find returns the most specific opening for the list of moves.  If no opening is found, Find returns nil.
	Find(moves []*chess.Move) *Opening
	// Possible returns the possible openings after the moves given.  If moves is empty or nil all openings are returned.
	Possible(moves []*chess.Move) []*Opening
}
