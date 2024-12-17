package chess

import (
	"errors"
	"fmt"
)

// PGNError custom error types for different PGN errors.
type PGNError struct {
	msg string
	pos int // position where error occurred
}

func (e *PGNError) Error() string {
	return e.msg
}

func (e *PGNError) Is(target error) bool {
	var t *PGNError
	ok := errors.As(target, &t)
	if !ok {
		return false
	}

	return e.msg == t.msg
}

// Custom error types for different PGN errors
//
//nolint:gochecknoglobals // this is a custom error type.
var (
	ErrUnterminatedComment = func(pos int) error { return &PGNError{"unterminated comment", pos} }
	ErrUnterminatedQuote   = func(pos int) error { return &PGNError{"unterminated quote", pos} }
	ErrInvalidCommand      = func(pos int) error { return &PGNError{"invalid command in comment", pos} }
	ErrInvalidPiece        = func(pos int) error { return &PGNError{"invalid piece", pos} }
	ErrInvalidSquare       = func(pos int) error { return &PGNError{"invalid square", pos} }
	ErrInvalidRank         = func(pos int) error { return &PGNError{"invalid rank", pos} }

	ErrNoGameFound = errors.New("no game found in PGN data")
)

type ParserError struct {
	Message    string
	TokenValue string
	TokenType  TokenType
	Position   int
}

func (e *ParserError) Error() string {
	return fmt.Sprintf("Parser error at position %d: %s (Token: %v, Value: %s)",
		e.Position, e.Message, e.TokenType, e.TokenValue)
}
