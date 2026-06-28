// Package opening implements chess opening determination and exploration.
package opening

import (
	"github.com/corentings/chess/v3"
)

// A Opening represents a specific sequence of moves from the staring position.
type Opening struct {
	moveList []string
	game     *chess.Game
	code     string
	title    string
	pgn      string
}

func newOpening(code, title, pgn string, moveList []string) *Opening {
	return &Opening{
		moveList: moveList,
		code:     code,
		title:    title,
		pgn:      pgn,
		game:     buildGame(moveList),
	}
}

func buildGame(moveList []string) *chess.Game {
	game := chess.NewGame()
	for _, moveStr := range moveList {
		m, err := chess.UCINotation{}.Decode(game.Position(), moveStr)
		if err != nil {
			return nil
		}
		if _, err := game.Move(m, nil); err != nil {
			return nil
		}
	}
	return game
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

// Game returns the opening as a caller-owned game.
//
// The game is pre-computed when the opening is constructed (per ADR-005), so
// Game does not replay UCI moves. Each call returns an independent clone so
// callers may mutate the returned game without affecting the opening book.
func (o *Opening) Game() *chess.Game {
	if o.game == nil {
		return nil
	}
	return o.game.Clone()
}

// Book is an opening book that returns openings for move sequences.
type Book interface {
	// Find returns the most specific opening for the list of moves. If no opening is found, Find returns nil.
	// Use Find for performance-sensitive opening detection paths.
	Find(moves []chess.Move) *Opening
	// Possible returns the possible openings after the moves given. If moves is empty or nil all openings are returned.
	// Use Possible for performance-sensitive opening exploration paths.
	Possible(moves []chess.Move) []*Opening
}
