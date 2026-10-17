// The PGN lexer turns PGN text into a stream of pgnTokens for the parser and
// the streaming accessors. It is internal: callers decode PGN through
// ParsePGN, PGNGames, PGNRecords, PGNEvents or PGNDecoder.

package chess

import (
	"strings"
	"unicode"
)

// pgnTokenType represents the type of token in PGN text.
type pgnTokenType int

const (
	tokEOF pgnTokenType = iota
	tokUndefined
	tokTagStart             // [
	tokTagEnd               // ]
	tokTagKey               // The key part of a tag (e.g., "Site")
	tokTagValue             // The value part of a tag (e.g., "Internet")
	tokMoveNumber           // 1, 2, 3, etc.
	tokDot                  // .
	tokEllipsis             // ...
	tokPiece                // N, B, R, Q, K
	tokSquare               // e4, e5, etc.
	tokCommentStart         // {
	tokCommentEnd           // }
	tokComment              // The comment text
	tokResult               // 1-0, 0-1, 1/2-1/2, *
	tokCapture              // 'x' in moves
	tokFile                 // a-h in moves when used as disambiguation
	tokRank                 // 1-8 in moves when used as disambiguation
	tokKingsideCastle       // 0-0
	tokQueensideCastle      // 0-0-0
	tokPromotion            // = in moves
	tokPromotionPiece       // The piece being promoted to (Q, R, B, N)
	tokCheck                // + in moves
	tokCheckmate            // # in moves
	tokNAG                  // Numeric Annotation Glyph (e.g., $1, $2, etc.)
	tokVariationStart       // ( for starting a variation
	tokVariationEnd         // ) for ending a variation
	tokCommandStart         // [%
	tokCommandName          // The command name (e.g., clk, eval)
	tokCommandParam         // Command parameter
	tokCommandEnd           // ]
	tokDisambiguationSquare // Full square disambiguation (e.g., e8 in Qe8f7)
	tokNullMove             // A null move: Z0, Z1, --, or @@
)

func (t pgnTokenType) String() string {
	types := []string{
		"EOF",
		"Undefined",
		"TagStart",
		"TagEnd",
		"TagKey",
		"TagValue",
		"MoveNumber",
		"DOT",
		"ELLIPSIS",
		"PIECE",
		"SQUARE",
		"CommentStart",
		"CommentEnd",
		"COMMENT",
		"RESULT",
		"CAPTURE",
		"FILE",
		"RANK",
		"KingsideCastle",
		"QueensideCastle",
		"PROMOTION",
		"PromotionPiece",
		"CHECK",
		"CHECKMATE",
		"NAG",
		"VariationStart",
		"VariationEnd",
		"CommandStart",
		"CommandName",
		"CommandParam",
		"CommandEnd",
		"DeambiguationSquare",
		"NullMove",
	}

	if t < 0 || int(t) >= len(types) {
		return "Unknown"
	}

	return types[t]
}

// pgnToken represents a lexical token from PGN text.
type pgnToken struct {
	Error error
	Value string
	Type  pgnTokenType
}

// pgnLexer provides lexical analysis of PGN text.
type pgnLexer struct {
	input          string
	position       int
	readPosition   int
	ch             byte
	inTag          bool
	inComment      bool
	inCommand      bool
	inCommandParam bool
}

// newPGNLexer creates a new Lexer for the provided input text.
// The lexer is initialized and ready to produce tokens through
// calls to NextToken().
//
// Example:
//
//	lexer := newPGNLexer("1. e4 e5")
func newPGNLexer(input string) *pgnLexer {
	l := &pgnLexer{input: input}
	l.readChar()
	return l
}

func (l *pgnLexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *pgnLexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *pgnLexer) readNumber() pgnToken {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return pgnToken{Type: tokMoveNumber, Value: l.input[position:l.position]}
}

func (l *pgnLexer) readCommandName() pgnToken {
	position := l.position
	// Read alphanumeric characters until space
	for isAlphaNumeric(l.ch) {
		l.readChar()
	}
	l.inCommandParam = true
	return pgnToken{Type: tokCommandName, Value: l.input[position:l.position]}
}

func (l *pgnLexer) readCommandParam() pgnToken {
	l.skipWhitespace()

	// check for EOF
	if l.ch == 0 {
		return pgnToken{Type: tokEOF, Value: ""}
	}

	position := l.position
	if l.ch == '"' {
		// Handle quoted parameter
		l.readChar() // skip opening quote
		position = l.position
		for l.ch != '"' && l.ch != 0 {
			l.readChar()
		}
		if l.ch == 0 {
			return pgnToken{Type: tokEOF, Error: ErrUnterminatedQuote(position)}
		}
		value := l.input[position:l.position]
		l.readChar() // skip closing quote
		return pgnToken{Type: tokCommandParam, Value: value}
	}

	// Read until comma or ] for non-quoted parameters
	// Allow colons and other characters within the parameter
	for l.ch != ',' && l.ch != ']' && l.ch != 0 && l.ch != '}' {
		l.readChar()
	}

	if l.ch == '}' && l.inCommand {
		return pgnToken{Type: tokEOF, Error: ErrInvalidCommand(l.position)}
	}

	l.inCommandParam = l.ch == ',' // set flag if we are still in a command parameter
	return pgnToken{Type: tokCommandParam, Value: strings.TrimSpace(l.input[position:l.position])}
}

func (l *pgnLexer) readNAG() pgnToken {
	// Handle cases where NAG starts with '!' or '?' (symbolic move-quality
	// spellings such as ! ? !! ?? !? ?!). This isn't part of the strict PGN
	// spec, but Lichess and other sources emit them, so they are accepted on
	// import. The lexer reads the maximal run of '!' and '?' characters as a
	// single token; normalisation to the canonical "$N" form happens at the
	// storage boundary (see ParseNAG).
	if l.ch == '!' || l.ch == '?' {
		position := l.position
		for l.ch == '!' || l.ch == '?' {
			l.readChar()
		}
		value := l.input[position:l.position]
		return pgnToken{Type: tokNAG, Value: value}
	}
	l.readChar() // skip the $ symbol
	position := l.position

	// Read all digits following the $
	for isDigit(l.ch) {
		l.readChar()
	}

	// Include the $ in the token value
	return pgnToken{
		Type:  tokNAG,
		Value: "$" + l.input[position:l.position],
	}
}

func (l *pgnLexer) readResult() pgnToken {
	position := l.position
	for !isWhitespace(l.ch) && l.ch != 0 {
		l.readChar()
	}
	result := l.input[position:l.position]
	if isResult(result) {
		return pgnToken{Type: tokResult, Value: result}
	}
	return pgnToken{Type: tokMoveNumber, Value: result}
}

func (l *pgnLexer) readRank() pgnToken {
	rank := l.input[l.position : l.position+1]
	if !isRank(l.ch) {
		l.readChar()
		return pgnToken{Type: tokRank, Error: ErrInvalidRank(l.position), Value: rank}
	}
	l.readChar()
	return pgnToken{Type: tokRank, Value: rank}
}

func (l *pgnLexer) readComment() pgnToken {
	position := l.position

	// Look for command start sequence
	for l.ch != '}' && l.ch != 0 {
		if l.ch == '\\' && l.peekChar() == '}' {
			l.readChar()
			l.readChar()
			continue
		}
		if l.ch == '[' && l.peekChar() == '%' {
			if position != l.position {
				// Return accumulated comment text before the command
				return pgnToken{Type: tokComment, Value: cleanCommentText(l.input[position:l.position])}
			}
			// Start command processing
			l.readChar() // skip [
			l.readChar() // skip %
			l.inCommand = true
			// check for EOF after command start
			if l.ch == 0 {
				return pgnToken{
					Type:  tokEOF,
					Error: ErrInvalidCommand(l.position),
				}
			}
			return pgnToken{Type: tokCommandStart, Value: "[%"}
		}
		l.readChar()
	}

	// Check for unterminated comment
	if l.ch == 0 {
		l.readChar()
		return pgnToken{
			Type:  tokEOF,
			Error: ErrUnterminatedComment(position),
		}
	}

	// Return remaining comment text if any
	if position != l.position {
		return pgnToken{Type: tokComment, Value: cleanCommentText(l.input[position:l.position])}
	}

	return pgnToken{Type: tokCommentEnd, Value: "}"}
}

func cleanCommentText(text string) string {
	text = strings.TrimSpace(text)
	return strings.ReplaceAll(text, `\}`, "}")
}

// Update readPieceMove to handle piece moves.
func (l *pgnLexer) readPieceMove() pgnToken {
	// Capture just the piece
	piece := l.input[l.position : l.position+1]
	if !isPiece(l.ch) {
		l.readChar()
		return pgnToken{Type: tokPiece, Error: ErrInvalidPiece(l.position), Value: piece}
	}
	l.readChar()

	// Return just the piece - the square or capture will be read in subsequent tokens
	return pgnToken{Type: tokPiece, Value: piece}
}

func (l *pgnLexer) readMove() pgnToken {
	const disambiguationLength = 3
	const disambiguationSquareLength = 4

	position := l.position

	// Check for EOF early
	if l.ch == 0 {
		return pgnToken{Type: tokEOF, Value: ""}
	}

	// For pawn captures
	if isFile(l.ch) {
		file := l.input[l.position : l.position+1]
		l.readChar()

		// Check for capture
		if l.ch == 'x' {
			return pgnToken{Type: tokFile, Value: file}
		}
	}

	for isFile(l.ch) || isDigit(l.ch) {
		l.readChar()

		// Check for EOF during the loop
		if l.ch == 0 {
			break
		}
	}

	// Get the total length of what we read
	length := l.position - position

	// Handle full square disambiguation (e.g., "e8f7" -> "e8" then "f7")
	if length == disambiguationSquareLength &&
		isFile(l.input[position]) && isDigit(l.input[position+1]) &&
		isFile(l.input[position+2]) && isDigit(l.input[position+3]) {
		// Reset to return just the first square
		l.position = position + 2
		l.readPosition = position + 3
		l.ch = l.input[l.position] // set current char to the first character of the second square
		return pgnToken{Type: tokDisambiguationSquare, Value: l.input[position : position+2]}
	}

	// Handle full square disambiguation before a capture
	// (e.g., "g8xg7" in "Qg8xg7" -> "g8" then "xg7").
	if length == 2 && position > 0 && isPiece(l.input[position-1]) &&
		isFile(l.input[position]) && isDigit(l.input[position+1]) &&
		l.ch == 'x' && l.readPosition+1 < len(l.input) &&
		isFile(l.input[l.readPosition]) && isDigit(l.input[l.readPosition+1]) {
		return pgnToken{Type: tokDisambiguationSquare, Value: l.input[position:l.position]}
	}

	// If we read 3 characters, first one is disambiguation
	if length == disambiguationLength {
		l.readPosition = position + 1
		l.readChar()
		// Return just the first character as disambiguation
		return pgnToken{Type: tokFile, Value: l.input[position : position+1]}
	}

	// Validate the square (e.g., "e4")
	if length < 2 || !isFile(l.input[position]) || position+1 >= len(l.input) || !isDigit(l.input[position+1]) {
		l.readChar()
		return pgnToken{Type: tokSquare, Value: "", Error: ErrInvalidSquare(position)}
	}

	return pgnToken{Type: tokSquare, Value: l.input[position:l.position]}
}

func (l *pgnLexer) readPromotionPiece() pgnToken {
	piece := l.input[l.position : l.position+1]
	if !isPiece(l.ch) {
		l.readChar()
		return pgnToken{Type: tokPromotionPiece, Error: ErrInvalidPiece(l.position), Value: piece}
	}
	l.readChar()
	return pgnToken{Type: tokPromotionPiece, Value: piece}
}

func (l *pgnLexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *pgnLexer) readTagValue() pgnToken {
	l.readChar() // skip opening quote
	position := l.position
	for l.ch != '"' && l.ch != '\\' && l.ch != 0 {
		l.readChar()
	}
	if l.ch != '\\' {
		value := l.input[position:l.position]
		l.readChar() // skip closing quote
		return pgnToken{Type: tokTagValue, Value: value}
	}

	var value strings.Builder
	value.WriteString(l.input[position:l.position])
	for l.ch != '"' && l.ch != 0 {
		if l.ch == '\\' {
			next := l.peekChar()
			if next == '"' || next == '\\' {
				l.readChar() // skip backslash
				value.WriteByte(l.ch)
				l.readChar()
				continue
			}
		}
		value.WriteByte(l.ch)
		l.readChar()
	}
	l.readChar() // skip closing quote
	return pgnToken{Type: tokTagValue, Value: value.String()}
}

func (l *pgnLexer) readTagKey() pgnToken {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return pgnToken{Type: tokTagKey, Value: l.input[position:l.position]}
}

func (l *pgnLexer) readCastling() (pgnToken, bool) {
	position := l.position

	// First character should be uppercase 'O'
	if l.ch != 'O' {
		return pgnToken{}, false
	}

	// Check if we have enough characters for at least kingside castling (O-O)
	if l.position+2 >= len(l.input) {
		return pgnToken{}, false
	}

	// Check for "O-O" pattern
	if l.peekChar() != '-' {
		return pgnToken{}, false
	}
	l.readChar() // skip O
	l.readChar() // skip -

	if l.ch != 'O' {
		// Reset if pattern doesn't match
		l.position = position
		l.readPosition = position + 1
		l.ch = l.input[position]
		return pgnToken{}, false
	}
	l.readChar() // skip O

	// Look ahead to see if this is queenside castling (O-O-O)
	if l.ch == '-' && l.peekChar() == 'O' {
		l.readChar() // skip -
		l.readChar() // skip O
		return pgnToken{Type: tokQueensideCastle, Value: castleQS}, true
	}

	return pgnToken{Type: tokKingsideCastle, Value: castleKS}, true
}

// readNullMove consumes a null-move token at the current position and returns
// it. Recognised spellings: "Z0", "Z1", "--", "@@", "0000". Returns (Token{}, false)
// if the current character does not start a null-move token. Safe to call from
// NextToken before the main dispatch because it never clashes with valid move
// squares, piece letters, or result tokens.
func (l *pgnLexer) readNullMove() (pgnToken, bool) {
	switch l.ch {
	case 'Z':
		next := l.peekChar()
		if next != '0' && next != '1' {
			return pgnToken{}, false
		}
		position := l.position
		l.readChar()
		l.readChar()
		return pgnToken{Type: tokNullMove, Value: l.input[position:l.position]}, true

	case '-':
		if l.peekChar() != '-' {
			return pgnToken{}, false
		}
		// Standalone "--" — readResult already consumed any result token
		// beginning with a digit ("1-0", "0-1", "1/2-1/2"), and readCastling
		// requires an uppercase O, so we cannot be inside those.
		l.readChar()
		l.readChar()
		return pgnToken{Type: tokNullMove, Value: "--"}, true

	case '@':
		if l.peekChar() != '@' {
			return pgnToken{}, false
		}
		l.readChar()
		l.readChar()
		return pgnToken{Type: tokNullMove, Value: "@@"}, true

	case '0':
		// "0000" — must be four zeros with nothing digit/dash-adjacent to
		// avoid clashing with result/castle tokens. readCastling consumes
		// "O-O" / "O-O-O" and readResult consumes "1-0" / "0-1" before we
		// reach here, so standalone "0000" can safely be claimed.
		if l.position+3 >= len(l.input) {
			return pgnToken{}, false
		}
		if l.input[l.position+1] != '0' ||
			l.input[l.position+2] != '0' ||
			l.input[l.position+3] != '0' {
			return pgnToken{}, false
		}
		l.readChar()
		l.readChar()
		l.readChar()
		l.readChar()
		return pgnToken{Type: tokNullMove, Value: "0000"}, true
	}
	return pgnToken{}, false
}

// NextToken reads the next token from the input stream.
// Returns an EOF token when the input is exhausted.
// Returns an ILLEGAL token for invalid input.
//
// The method handles all standard PGN notation including:
// - Move notation (e4, Nf3, O-O)
// - Comments ({comment} or ; comment)
// - Tags ([Event "World Championship"])
// - Move numbers and variations
// - Annotations ($1, !!, ?!)
//
// Example:
//
//	lexer := NewLexer("1. e4 {Strong move}")
//	token := lexer.NextToken() // NUMBER: "1"
//	token = lexer.NextToken()  // DOT: "."
//	token = lexer.NextToken()  // NOTATION: "e4"
//	token = lexer.NextToken()  // COMMENT: "Strong move"
//	token = lexer.NextToken()  // EOF
func (l *pgnLexer) NextToken() pgnToken {
	l.skipWhitespace()

	if l.inCommand {
		switch l.ch {
		case ']':
			l.inCommand = false
			l.readChar()
			return pgnToken{Type: tokCommandEnd, Value: "]"}
		case ',':
			l.readChar()
			return l.readCommandParam()
		default:
			// check if the previous token was a command start
			if l.inCommandParam {
				return l.readCommandParam()
			}
			return l.readCommandName()
		}
	}

	if l.inComment {
		if l.ch == '}' {
			l.inComment = false
			l.readChar()
			return pgnToken{Type: tokCommentEnd, Value: "}"}
		}
		return l.readComment()
	}

	if l.inTag && isLetter(l.ch) {
		return l.readTagKey()
	}

	// Null move tokens: "Z0", "Z1" (ChessBase / Scid), "--" (pgn-extract,
	// Scid), "@@" (some exporters), and "0000" (UCI convention).
	if tok, ok := l.readNullMove(); ok {
		return tok
	}

	switch l.ch {
	case '(':
		l.readChar()
		return pgnToken{Type: tokVariationStart, Value: "("}

	case ')':
		l.readChar()
		return pgnToken{Type: tokVariationEnd, Value: ")"}
	case '[':
		l.inTag = true
		l.readChar()
		return pgnToken{Type: tokTagStart, Value: "["}
	case ']':
		l.inTag = false
		l.readChar()
		return pgnToken{Type: tokTagEnd, Value: "]"}
	case '"':
		return l.readTagValue()
	case '{':
		l.readChar()
		l.inComment = true
		return pgnToken{Type: tokCommentStart, Value: "{"}
	case '}':
		l.readChar()
		return pgnToken{Type: tokCommentEnd, Value: "}"}
	case '.':
		if l.peekChar() == '.' && l.readPosition+1 < len(l.input) && l.input[l.readPosition+1] == '.' {
			l.readChar()
			l.readChar()
			l.readChar()
			return pgnToken{Type: tokEllipsis, Value: "..."}
		}
		l.readChar()
		return pgnToken{Type: tokDot, Value: "."}
	case 'x':
		l.readChar()
		return pgnToken{Type: tokCapture, Value: "x"}
	case '*':
		fallthrough
	case '-':
		return l.readResult()
	case '$', '!', '?':
		return l.readNAG()
	case 'O':
		// Check for castling
		if token, isCastling := l.readCastling(); isCastling {
			return token
		}
		// If not castling, treat as a regular piece move
		return l.readPieceMove()
	case '=':
		l.readChar()
		return pgnToken{Type: tokPromotion, Value: "="}
	case '+':
		l.readChar()
		return pgnToken{Type: tokCheck, Value: "+"}
	case '#':
		l.readChar()
		return pgnToken{Type: tokCheckmate, Value: "#"}
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		if l.inTag {
			return l.readTagValue()
		}

		// Look at previous characters to determine context
		if l.position > 0 && unicode.IsUpper(rune(l.input[l.position-1])) {
			// If preceded by a piece, it's a rank disambiguation
			return l.readRank()
		}

		// Look ahead to see if this number is followed by a dot or hyphen
		position := l.position
		for l.ch != 0 && isDigit(l.ch) {
			l.readChar()
		}
		switch l.ch {
		case '.':
			return pgnToken{Type: tokMoveNumber, Value: l.input[position:l.position]}
		case '-':
			l.position = position
			l.readPosition = position + 1
			l.ch = l.input[position]
			return l.readResult()
		default:
			// "1/2-1/2" draw result token, e.g. "1. e4 e5 1/2-1/2". The digit
			// run stopped at '/'; if the literal pattern starts at the run
			// start, reset and let readResult consume and validate it.
			if position+7 <= len(l.input) && l.input[position:position+7] == Draw.String() {
				l.position = position
				l.readPosition = position + 1
				l.ch = l.input[position]
				return l.readResult()
			}
			// Reset position and try again as a regular number
			l.position = position
			l.readPosition = position + 1
			l.ch = l.input[position]
			return l.readNumber()
		}
	case 0:
		return pgnToken{Type: tokEOF, Value: ""}
	default:
		if isLetter(l.ch) {
			if unicode.IsUpper(rune(l.ch)) {
				// If it follows a promotion token, it's a promotion piece
				if l.position > 0 && l.input[l.position-1] == '=' {
					return l.readPromotionPiece()
				}
				return l.readPieceMove()
			}
			return l.readMove()
		}
	}

	tok := pgnToken{Type: tokUndefined, Value: l.input[l.position : l.position+1]}
	l.readChar()
	return tok
}
