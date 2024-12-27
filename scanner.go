/*
Package chess provides functionality for reading and parsing chess games in PGN
(Portable Game Notation) format. It includes a scanner for reading multiple games
from a single source and a tokenizer for converting PGN text into processable tokens.
The scanner handles PGN-specific syntax including game metadata, moves, comments,
and variations. It supports streaming processing of large PGN files and provides
proper handling of game boundaries and special notation.
Example usage:
	// Create scanner for PGN input
	scanner := NewScanner(reader)

	// Read all games
	for scanner.HasNext() {
		game, err := scanner.ScanGame()
		if err != nil {
			log.Fatal(err)
		}
		// Process game
	}

	// Tokenize a specific game
	tokens, err := TokenizeGame(game)
*/

package chess

import (
	"bufio"
	"bytes"
	"io"
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

	lexer := NewLexer(game.Raw)
	var tokens []Token

	for {
		token := lexer.NextToken()
		if token.Type == EOF {
			break
		}
		tokens = append(tokens, token)
	}

	return tokens, nil
}

// Scanner provides functionality to read chess games from a PGN source.
// It supports streaming processing of multiple games and proper handling
// of PGN syntax.
type Scanner struct {
	scanner   *bufio.Scanner
	nextGame  *GameScanned // Buffer for peeked game
	lastError error        // Store last error
}

// NewScanner creates a new PGN scanner that reads from the provided reader.
// The scanner is configured to properly split PGN games and handle
// PGN-specific syntax.
//
// Example:
//
//	scanner := NewScanner(strings.NewReader(pgnText))
func NewScanner(r io.Reader) *Scanner {
	s := bufio.NewScanner(r)
	s.Split(splitPGNGames)
	return &Scanner{scanner: s}
}

// ScanGame reads and returns the next game from the source.
// Returns nil and io.EOF when no more games are available.
// Returns nil and an error if reading fails.
//
// Example:
//
//	game, err := scanner.ScanGame()
//	if err == io.EOF {
//	    // No more games
//	}
func (s *Scanner) ScanGame() (*GameScanned, error) {
	// If we have a buffered game from HasNext(), return it
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

// HasNext returns true if there are more games available to read.
// This method can be used to iterate over all games in the source.
//
// Example:
//
//	for scanner.HasNext() {
//	    game, err := scanner.ScanGame()
//	    // Process game
//	}
func (s *Scanner) HasNext() bool {
	// If we already have a buffered game, return true
	if s.nextGame != nil {
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

// Helper to find the start of a game (first '[' character).
func findGameStart(data []byte, start int, atEOF bool) int {
	// If the first character is not '[', find the next '[' character
	if start < len(data) && data[start] != '[' {
		idx := bytes.IndexByte(data[start:], '[')
		if idx == -1 {
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
