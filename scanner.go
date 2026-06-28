/*
Package chess provides functionality for reading and parsing chess games in PGN
(Portable Game Notation) format. It includes a scanner for reading multiple games
from a single source and a tokenizer for converting PGN text into processable tokens.
The scanner handles PGN-specific syntax including game metadata, moves, comments,
and variations. It supports streaming processing of large PGN files and provides
proper handling of game boundaries and special notation.
Example usage:
	// Create scanner for PGN input
	scanner := newScanner(reader)

	// Read all games
	for scanner.hasNext() {
		game, err := scanner.parseNext()
		if err != nil {
			log.Fatalf("Failed to parse game: %v", err)
		}
		// Process game
	}
*/

package chess

import (
	"bufio"
	"bytes"
	"io"
	"sync"
)

// GameScanned represents a complete chess game in PGN format.
type GameScanned struct {
	// Raw contains the complete PGN text of the game
	Raw string
}

// TokenizeGame converts a PGN game into a sequence of tokens.
// Returns nil if the game is nil. Returns an error if tokenization fails.
//
// The function handles all PGN elements including moves, comments,
// annotations, and metadata tags.
//
// Example:
//
//	tokens, err := TokenizeGame(game)
//	if err != nil {
//	    // Handle error
//	}
func TokenizeGame(game *GameScanned) ([]Token, error) {
	if game == nil {
		return nil, nil
	}
	// Preallocate the token slice. Empirically a PGN byte produces ~3 tokens
	// (move pairs, NAGs, comments, tags), so size to avoid reallocation
	// during growth. Slice growth from a nil starting point throws away
	// every prior backing array, which previously dominated allocations.
	return tokenizeInto(game, make([]Token, 0, len(game.Raw)/3+16))
}

func tokenizeInto(game *GameScanned, tokens []Token) ([]Token, error) {
	if game == nil {
		return nil, nil
	}

	lexer := NewLexer(game.Raw)
	tokens = tokens[:0]
	for {
		token := lexer.NextToken()
		if token.Type == EOF {
			break
		}
		tokens = append(tokens, token)
	}

	return tokens, nil
}

//nolint:gochecknoglobals // Pool amortizes token backing arrays across streamed games.
var tokenSlicePool = sync.Pool{
	New: func() any {
		tokens := make([]Token, 0, 256)
		return &tokens
	},
}

// scanner provides functionality to read chess games from a PGN source.
// It supports streaming processing of multiple games and proper handling
// of PGN syntax.
type scanner struct {
	lastError       error // Store last error
	scanner         *bufio.Scanner
	nextGame        *GameScanned // Buffer for peeked game
	nextParsedGames []*Game      // only valid when ExpandVariations==true
	opts            scannerOpts
}

type scannerOption func(*scanner)

// withExpandVariations() instructs the scanner to expand all variations in
// a single GameScanned into multiple Game instances (1 per variation) rather
// than a single Game instance.
func withExpandVariations() scannerOption {
	return func(s *scanner) {
		s.opts.ExpandVariations = true
	}
}

type scannerOpts struct {
	ExpandVariations bool // default false
}

// newScanner creates a new PGN scanner that reads from the provided reader.
// The scanner is configured to properly split PGN games and handle
// PGN-specific syntax.
//
// Example:
//
//	scanner := newScanner(strings.NewReader(pgnText))
func newScanner(r io.Reader, opts ...scannerOption) *scanner {
	s := bufio.NewScanner(r)
	s.Split(splitPGNGames)
	ret := &scanner{
		scanner:         s,
		nextParsedGames: make([]*Game, 0),
	}

	// apply all the options
	for _, opt := range opts {
		opt(ret)
	}

	return ret
}

// scanGame reads and returns the next game from the source.
// Returns nil and io.EOF when no more games are available.
// Returns nil and an error if reading fails.
//
// Example:
//
//	game, err := scanner.scanGame()
//	if err == io.EOF {
//	    // No more games
//	}
func (s *scanner) scanGame() (*GameScanned, error) {
	// If we have a buffered game from hasNext(), return it
	if s.nextGame != nil {
		game := s.nextGame
		s.nextGame = nil
		return game, nil
	}

	// Otherwise scan the next game
	if s.scanner.Scan() {
		return &GameScanned{Raw: s.scanner.Text()}, nil
	}

	// Check for errors
	if err := s.scanner.Err(); err != nil {
		return nil, err
	}
	return nil, io.EOF
}

// hasNext returns true if there are more games available to read.
// This method can be used to iterate over all games in the source.
//
// Example:
//
//	for scanner.hasNext() {
//	    scangame, err := scanner.scanGame()
//	    // Process scangame
//	}
func (s *scanner) hasNext() bool {
	// If we already have a buffered game, return true
	if s.nextGame != nil || len(s.nextParsedGames) > 0 {
		return true
	}

	// Try to scan the next game
	if s.scanner.Scan() {
		// Store the game in the buffer
		s.nextGame = &GameScanned{Raw: s.scanner.Text()}
		return true
	}

	// Store any error that occurred
	s.lastError = s.scanner.Err()
	return false
}

// parseNext is a convenience wrapper combining the functionality of
// scanGame(), TokenizeGame(), newParser(), and Parse() enabling
// callers to simplify iterating over each Game within a PGN file.
//
// Example:
//
//	for scanner.hasNext() {
//	    game, err := scanner.parseNext()
//	    // Process game
//	}
func (s *scanner) parseNext() (*Game, error) {
	if len(s.nextParsedGames) > 0 {
		ret := s.nextParsedGames[0]
		s.nextParsedGames = s.nextParsedGames[1:]
		return ret, nil
	}

	scannedGame, err := s.scanGame()
	if err != nil {
		return nil, err
	}
	pooledTokens, ok := tokenSlicePool.Get().(*[]Token)
	if !ok {
		tokens := make([]Token, 0, len(scannedGame.Raw)/3+16)
		pooledTokens = &tokens
	}
	tokens, err := tokenizeInto(scannedGame, *pooledTokens)
	if err != nil {
		*pooledTokens = tokens[:0]
		tokenSlicePool.Put(pooledTokens)
		return nil, err
	}
	defer func() {
		for i := range tokens {
			tokens[i] = Token{}
		}
		*pooledTokens = tokens[:0]
		tokenSlicePool.Put(pooledTokens)
	}()
	parser := newParser(tokens)
	game, err := parser.Parse()
	if err != nil {
		return nil, err
	}
	if !s.opts.ExpandVariations {
		return game, nil
	} // else

	parsedGames := game.Split()
	s.nextParsedGames = parsedGames[1:]
	return parsedGames[0], nil
}

// Split function for bufio.Scanner to split PGN games.
func splitPGNGames(data []byte, atEOF bool) (int, []byte, error) {
	// Skip leading whitespace
	start := skipLeadingWhitespace(data)
	if start == len(data) {
		return handleEOF(data, atEOF)
	}

	// Find the start of the game
	start = findGameStart(data, start, atEOF)
	if start == -1 {
		return 0, nil, nil
	}

	// Process the game content
	return processGameContent(data, start, atEOF)
}

// Helper to skip leading whitespace.
func skipLeadingWhitespace(data []byte) int {
	start := 0
	for ; start < len(data); start++ {
		if !isWhitespace(data[start]) {
			break
		}
	}
	return start
}

// Helper to handle EOF scenarios.
func handleEOF(data []byte, atEOF bool) (int, []byte, error) {
	if atEOF {
		return len(data), nil, nil
	}
	return 0, nil, nil
}

// Helper to find the start of a game (normally first '[' character).
func findGameStart(data []byte, start int, atEOF bool) int {
	// If the first character is not '[', find the next '[' character
	if start < len(data) && data[start] != '[' {
		idx := bytes.IndexByte(data[start:], '[')
		if idx == -1 {
			return findTaglessGameStart(data, start, atEOF)
		}
		start += idx
	}
	return start
}

// Helper to find the start of a game without tags
func findTaglessGameStart(data []byte, start int, atEOF bool) int {
	// If the first character is not '[', find the next '[' character
	if start < len(data) && data[start] != '1' {
		idx := bytes.IndexByte(data[start:], '1')
		if idx == -1 || data[start+idx+1] != '.' ||
			(idx != 0 && data[start+idx-1] != '\n') {
			if atEOF {
				return -1 // this could be removed as we return -1 in the next line anyway (just to be explicit and debuggable)
			}
			return -1
		}
		start += idx
	}

	return start
}

// Helper to process the content of a game and return the token or advance position.
func processGameContent(data []byte, start int, atEOF bool) (int, []byte, error) {
	var i int                                   // Loop variable
	var inBrackets, inComment, foundResult bool // State variables
	resultStart := -1                           // Start position of result token

	// Process the game content
	for i = start; i < len(data); i++ {
		// first check if we are in brackets or comments
		inBrackets = updateBracketState(data[i], inBrackets, inComment)
		inComment = updateCommentState(data[i], inComment)

		// when we are not in brackets or comments, we can check for the result token
		if foundResult && !inBrackets && !inComment && data[i] == '\n' {
			nextGame := findNextGameStart(data[i:])
			if nextGame != -1 {
				// return the next game start position and the current game content
				return i + nextGame, bytes.TrimSpace(data[start:i]), nil
			}
		}

		// check for result token if we are not in brackets or comments and haven't found it yet
		if !inBrackets && !inComment && !foundResult {
			foundResult, resultStart = checkForResult(data, i)
		}
	}

	// check for result token at EOF if we haven't found it yet
	if atEOF && foundResult && resultStart > 0 {
		return len(data), bytes.TrimSpace(data[start:]), nil
	}

	if !atEOF || i <= start {
		return 0, nil, nil
	}

	// return the current game content
	return len(data), bytes.TrimSpace(data[start:]), nil
}

// Helper to update bracket state based on current character.
func updateBracketState(ch byte, inBrackets bool, inComment bool) bool {
	if ch == '[' && !inComment {
		return true
	} else if ch == ']' && !inComment {
		return false
	}
	return inBrackets
}

// Helper to update comment state based on current character.
func updateCommentState(ch byte, inComment bool) bool {
	if ch == '{' {
		return true
	} else if ch == '}' && inComment {
		return false
	}
	return inComment
}

// Helper to find the next game start after a newline character.
func findNextGameStart(data []byte) int {
	nextGame := bytes.Index(data, []byte("[Event "))
	if nextGame != -1 {
		return nextGame
	}
	return -1
}

// Helper to check for game result tokens (e.g., "1-0", "0-1", "1/2-1/2", "*").
func checkForResult(data []byte, i int) (bool, int) {
	const minLength = 3        // Minimum length for results like "1-0"
	const fullResultLength = 7 // Length for "1/2-1/2"

	switch data[i] {
	case '1', '0', '*':
	default:
		return false, -1
	}

	if len(data)-i >= minLength {
		switch {
		case bytes.HasPrefix(data[i:], []byte("1-0")):
			return true, i
		case bytes.HasPrefix(data[i:], []byte("0-1")):
			return true, i
		case len(data)-i >= fullResultLength && bytes.HasPrefix(data[i:], []byte("1/2-1/2")):
			return true, i
		case data[i] == '*':
			return true, i
		default:
			break
		}
	}
	return false, -1
}
