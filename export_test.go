package chess

import (
	"io"
	"testing"

	"go.uber.org/goleak"
)

func TestMain(m *testing.M) {
	goleak.VerifyTestMain(m)
}

func PGN(r io.Reader) (func(*Game), error) {
	game, err := ParsePGN(r)
	if err == io.EOF {
		return nil, ErrNoGameFound
	}
	if err != nil {
		return nil, err
	}
	return func(g *Game) { g.copy(game) }, nil
}

func NewParser(tokens []Token) *Parser {
	return newParser(tokens)
}
