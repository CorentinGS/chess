package chess

// PGNError custom error types for different PGN errors.
type PGNError struct {
	msg string
	pos int // position where error occurred
}

func (e *PGNError) Error() string {
	return e.msg
}

func (e *PGNError) Is(target error) bool {
	t, ok := target.(*PGNError)
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
	ErrUnterminatedTag     = func(pos int) error { return &PGNError{"unterminated tag", pos} }
	ErrUnterminatedQuote   = func(pos int) error { return &PGNError{"unterminated quote", pos} }
	ErrUnterminatedRAV     = func(pos int) error { return &PGNError{"unterminated variation", pos} }
	ErrInvalidCommand      = func(pos int) error { return &PGNError{"invalid command in comment", pos} }
	ErrInvalidPiece        = func(pos int) error { return &PGNError{"invalid piece", pos} }
	ErrInvalidSquare       = func(pos int) error { return &PGNError{"invalid square", pos} }
	ErrInvalidRank         = func(pos int) error { return &PGNError{"invalid rank", pos} }
)
