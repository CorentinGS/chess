package chess

import "io"

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

type Scanner struct{ scanner *scanner }

type ScannerOption func(*Scanner)

func WithExpandVariations() ScannerOption {
	return func(s *Scanner) {
		s.scanner.opts.ExpandVariations = true
	}
}

func NewScanner(r io.Reader, opts ...ScannerOption) *Scanner {
	s := &Scanner{scanner: newScanner(r)}
	for _, opt := range opts {
		opt(s)
	}
	return s
}

func (s *Scanner) ScanGame() (*GameScanned, error) {
	return s.scanner.scanGame()
}

func (s *Scanner) HasNext() bool {
	return s.scanner.hasNext()
}

func (s *Scanner) ParseNext() (*Game, error) {
	return s.scanner.parseNext()
}
