// PGN game framing: splits a byte stream into individual PGN game records.
//
// The pgnFramer type reads from an io.Reader, buffers data, and uses
// splitPGNGames to emit complete PGN records as strings. The splitPGNGames
// function and its helpers handle PGN-specific syntax including game metadata,
// moves, comments, and variations. Token-level parsing is driven lazily from
// pgn.go via lexerTokenSource; this file does not own any token-shaped public
// API.

package chess

import (
	"bufio"
	"bytes"
	"io"
)

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

// Helper to skip leading whitespace and a UTF-8 byte order mark.
func skipLeadingWhitespace(data []byte) int {
	start := 0
	if bytes.HasPrefix(data, utf8BOM) {
		start = len(utf8BOM)
	}
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

// Helper to find the start of a game: its tag section ('['), or its
// movetext when the game has no tags. Anything else is preamble skipped up to
// the next '['.
func findGameStart(data []byte, start int, atEOF bool) int {
	if start < len(data) && startsTaglessGame(data[start]) {
		return start
	}
	if start < len(data) && data[start] != '[' {
		idx := bytes.IndexByte(data[start:], '[')
		if idx == -1 {
			return findTaglessGameStart(data, start, atEOF)
		}
		start += idx
	}
	return start
}

// startsTaglessGame reports whether b can open the movetext of a game without
// tags: a move number or a leading comment.
func startsTaglessGame(b byte) bool {
	return (b >= '1' && b <= '9') || b == '{'
}

// Helper to find the start of a game without tags.
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

// utf8BOM is the byte order mark some editors write at the start of a file.
var utf8BOM = []byte("\xEF\xBB\xBF")

// Helper to find the next game start after a newline that follows a result:
// the next tag section or tagless movetext, else the next "[Event " tag.
func findNextGameStart(data []byte) int {
	next := skipLeadingWhitespace(data)
	if next < len(data) && (data[next] == '[' || startsTaglessGame(data[next])) {
		return next
	}
	return bytes.Index(data, []byte("[Event "))
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

type pgnFramer struct {
	reader     *bufio.Reader
	buffer     []byte
	chunk      []byte
	baseOffset int64
	index      int64
	readErr    error
	eof        bool
}

func newPGNFramer(r io.Reader, opts ...PGNOption) *pgnFramer {
	options := applyPGNOptions(opts)
	bufferSize := options.bufferSize
	if bufferSize <= 0 {
		bufferSize = 256 * 1024
	}
	return &pgnFramer{reader: bufio.NewReaderSize(r, bufferSize), chunk: make([]byte, bufferSize)}
}

func (f *pgnFramer) next() (PGNRecord, error) {
	for {
		advance, token, err := splitPGNGames(f.buffer, f.eof)
		if err != nil {
			return PGNRecord{}, err
		}
		if advance == 0 && token == nil {
			if f.readErr != nil {
				// The reader failed and the buffered bytes do not form a
				// complete record: report the error instead of reading
				// again, which would spin forever on a failing reader.
				return PGNRecord{}, f.readErr
			}
			if f.eof {
				return PGNRecord{}, io.EOF
			}
			if readErr := f.readMore(); readErr != nil {
				// The reader failed. Keep the error: the buffered bytes may
				// still form a complete record, which is returned before the
				// error is reported.
				f.readErr = readErr
			}
			continue
		}
		start := f.baseOffset
		if len(token) > 0 {
			if rel := bytes.Index(f.buffer[:advance], token); rel >= 0 {
				start = f.baseOffset + int64(rel)
			}
		}
		f.advance(advance)
		f.baseOffset += int64(advance)
		if len(token) == 0 {
			continue
		}
		f.index++
		return PGNRecord{Index: f.index, Offset: start, Raw: string(token)}, nil
	}
}

func (f *pgnFramer) readMore() error {
	n, err := f.reader.Read(f.chunk)
	if n > 0 {
		f.buffer = append(f.buffer, f.chunk[:n]...)
	}
	if err == io.EOF {
		f.eof = true
		return nil
	}
	return err
}

func (f *pgnFramer) advance(n int) {
	if n >= len(f.buffer) {
		f.buffer = f.buffer[:0]
		return
	}
	retained := len(f.buffer) - n
	if retained*4 < cap(f.buffer) {
		f.buffer = append([]byte(nil), f.buffer[n:]...)
		return
	}
	f.buffer = f.buffer[n:]
}

func applyPGNOptions(opts []PGNOption) pgnOptions {
	options := defaultPGNOptions()
	for _, opt := range opts {
		opt(&options)
	}
	return options
}

func defaultPGNOptions() pgnOptions {
	return pgnOptions{moveTextCodec: PGNImportSAN()}
}
