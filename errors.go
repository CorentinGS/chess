package chess

import (
	"errors"
	"fmt"
)

// pgnLexError is a lexer diagnostic attached to a PGN token.
type pgnLexError struct {
	msg string
	pos int // position where error occurred
}

func (e *pgnLexError) Error() string {
	return e.msg
}

func (e *pgnLexError) Is(target error) bool {
	var t *pgnLexError
	ok := errors.As(target, &t)
	if !ok {
		return false
	}

	return e.msg == t.msg
}

// Lexer diagnostic constructors, internal to the PGN lexer.
var (
	errUnterminatedComment = func(pos int) error { return &pgnLexError{"unterminated comment", pos} }
	errUnterminatedQuote   = func(pos int) error { return &pgnLexError{"unterminated quote", pos} }
	errInvalidCommand      = func(pos int) error { return &pgnLexError{"invalid command in comment", pos} }
	errInvalidPiece        = func(pos int) error { return &pgnLexError{"invalid piece", pos} }
	errInvalidSquare       = func(pos int) error { return &pgnLexError{"invalid square", pos} }
	errInvalidRank         = func(pos int) error { return &pgnLexError{"invalid rank", pos} }
)

// Error sentinels. Errors returned by this package wrap them, so callers can
// branch with errors.Is.
var (
	// ErrInvalidFEN is the sentinel for any FEN parse failure. Specific
	// causes are wrapped with %w so callers can errors.Is(err, ErrInvalidFEN)
	// without inspecting message text.
	ErrInvalidFEN = errors.New("chess: invalid FEN")

	// ErrInvalidPGN is the sentinel for any PGN parse failure, including
	// empty input. Branch on this for "the PGN did not yield a Game".
	ErrInvalidPGN = errors.New("chess: invalid PGN")

	// ErrNoGameFound is returned when the PGN input contains no game. It
	// wraps ErrInvalidPGN so errors.Is(err, ErrInvalidPGN) also matches.
	ErrNoGameFound = fmt.Errorf("%w: no game found in PGN data", ErrInvalidPGN)

	// ErrGameAlreadyEnded is returned when a move is applied to a Game that
	// already has a terminal Outcome. Callers must ClearOutcome first.
	ErrGameAlreadyEnded = errors.New("chess: game already ended")
)

// ParserError describes where PGN parsing failed. Errors from ParsePGN and
// the other PGN decoders wrap it together with ErrInvalidPGN; use errors.As to
// read it. When the failure has a cause, such as an illegal move (ErrIllegalMove)
// or a bad FEN tag (ErrInvalidFEN), errors.Is reaches it through Unwrap.
type ParserError struct {
	// Message describes the failure.
	Message string
	// TokenValue is the text of the token where parsing stopped, if any.
	TokenValue string
	// Position is the index of that token within the game record.
	Position int

	tokenType pgnTokenType
	err       error
}

func (e *ParserError) Error() string {
	return fmt.Sprintf("parser error at token %d: %s (Token: %v, Value: %s)",
		e.Position, e.Message, e.tokenType, e.TokenValue)
}

// Unwrap returns the underlying cause, or nil.
func (e *ParserError) Unwrap() error {
	return e.err
}
