package chess

import "fmt"

// A Outcome is the result of a game.
type Outcome int8

const (
	// NoOutcome indicates that a game is in progress or ended without a result.
	NoOutcome Outcome = iota
	// WhiteWon indicates that white won the game.
	WhiteWon
	// BlackWon indicates that black won the game.
	BlackWon
	// Draw indicates that game was a draw.
	Draw
)

// String implements the fmt.Stringer interface and returns the PGN result token.
func (o Outcome) String() string {
	switch o {
	case NoOutcome:
		return "*"
	case WhiteWon:
		return "1-0"
	case BlackWon:
		return "0-1"
	case Draw:
		return "1/2-1/2"
	}
	return "*"
}

// MarshalText implements the encoding.TextMarshaler interface using the PGN
// result token.
func (o Outcome) MarshalText() ([]byte, error) {
	return []byte(o.String()), nil
}

// UnmarshalText implements the encoding.TextUnmarshaler interface from a PGN
// result token.
func (o *Outcome) UnmarshalText(text []byte) error {
	parsed, err := ParseOutcome(string(text))
	if err != nil {
		return err
	}
	*o = parsed
	return nil
}

// ParseOutcome converts a PGN result token into a typed Outcome. Unknown
// tokens return an error.
func ParseOutcome(s string) (Outcome, error) {
	switch s {
	case "*", "", "?":
		return NoOutcome, nil
	case "1-0":
		return WhiteWon, nil
	case "0-1":
		return BlackWon, nil
	case "1/2-1/2":
		return Draw, nil
	}
	return NoOutcome, fmt.Errorf("chess: invalid outcome %q", s)
}

// A Method is the method that generated the outcome.
type Method uint8

const (
	// NoMethod indicates that an outcome hasn't occurred or that the method can't be determined.
	NoMethod Method = iota
	// Checkmate indicates that the game was won checkmate.
	Checkmate
	// Resignation indicates that the game was won by resignation.
	Resignation
	// DrawOffer indicates that the game was drawn by a draw offer.
	DrawOffer
	// Stalemate indicates that the game was drawn by stalemate.
	Stalemate
	// ThreefoldRepetition indicates that the game was drawn when the game
	// state was repeated three times and a player requested a draw.
	ThreefoldRepetition
	// FivefoldRepetition indicates that the game was automatically drawn
	// by the game state being repeated five times.
	FivefoldRepetition
	// FiftyMoveRule indicates that the game was drawn by the half
	// move clock being one hundred or greater when a player requested a draw.
	FiftyMoveRule
	// SeventyFiveMoveRule indicates that the game was automatically drawn
	// when the half move clock was one hundred and fifty or greater.
	SeventyFiveMoveRule
	// InsufficientMaterial indicates that the game was automatically drawn
	// because there was insufficient material for checkmate.
	InsufficientMaterial
)
