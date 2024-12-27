/*
Package chess provides PGN lexical analysis through a lexer that converts
PGN text into a stream of tokens. The lexer handles all standard PGN notation
including moves, annotations, comments, and game metadata.
The lexer provides token-by-token processing of PGN content with proper handling
of chess-specific notation and PGN syntax rules.
Example usage:

	// Create new lexer
	lexer := NewLexer("[Event \"World Championship\"] 1. e4 e5 {Opening}")

	// Process tokens
	for {
		token := lexer.NextToken()
		if token.Type == EOF {
			break
		}
		// Process token
	}
*/
package chess

import (
	"strings"
	"unicode"
)

// TokenType represents the type of token in PGN text.
type TokenType int

const (
	EOF TokenType = iota
	Undefined
	TagStart        // [
	TagEnd          // ]
	TagKey          // The key part of a tag (e.g., "Site")
	TagValue        // The value part of a tag (e.g., "Internet")
	MoveNumber      // 1, 2, 3, etc.
	DOT             // .
	ELLIPSIS        // ...
	PIECE           // N, B, R, Q, K
	SQUARE          // e4, e5, etc.
	CommentStart    // {
	CommentEnd      // }
	COMMENT         // The comment text
	RESULT          // 1-0, 0-1, 1/2-1/2
	CAPTURE         // 'x' in moves
	FILE            // a-h in moves when used as disambiguation
	RANK            // 1-8 in moves when used as disambiguation
	KingsideCastle  // 0-0
	QueensideCastle // 0-0-0
	PROMOTION       // = in moves
	PromotionPiece  // The piece being promoted to (Q, R, B, N)
	CHECK           // + in moves
	CHECKMATE       // # in moves
	NAG             // Numeric Annotation Glyph (e.g., $1, $2, etc.)
	VariationStart  // ( for starting a variation
	VariationEnd    // ) for ending a variation
	CommandStart    // [%
	CommandName     // The command name (e.g., clk, eval)
	CommandParam    // Command parameter
	CommandEnd      // ]
)

func (t TokenType) String() string {
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
	}

	if t < 0 || int(t) >= len(types) {
		return "Unknown"
	}

	return types[t]
}

// Token represents a lexical token from PGN text.
type Token struct {
	Error error
	Value string
	Type  TokenType
}

// Lexer provides lexical analysis of PGN text.
type Lexer struct {
	input          string
	position       int
	readPosition   int
	ch             byte
	inTag          bool
	inComment      bool
	inCommand      bool
	inCommandParam bool
}

// NewLexer creates a new Lexer for the provided input text.
// The lexer is initialized and ready to produce tokens through
// calls to NextToken().
//
// Example:
//
//	lexer := NewLexer("1. e4 e5")
func NewLexer(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readNumber() Token {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return Token{Type: MoveNumber, Value: l.input[position:l.position]}
}

func (l *Lexer) readCommandName() Token {
	position := l.position
	// Read alphanumeric characters until space
	for isAlphaNumeric(l.ch) {
		l.readChar()
	}
	l.inCommandParam = true
	return Token{Type: CommandName, Value: l.input[position:l.position]}
}

func (l *Lexer) readCommandParam() Token {
	l.skipWhitespace()

	// check for EOF
	if l.ch == 0 {
		return Token{Type: EOF, Value: ""}
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
			return Token{Type: EOF, Error: ErrUnterminatedQuote(position)}
		}
		value := l.input[position:l.position]
		l.readChar() // skip closing quote
		return Token{Type: CommandParam, Value: value}
	}

	// Read until comma or ] for non-quoted parameters
	// Allow colons and other characters within the parameter
	for l.ch != ',' && l.ch != ']' && l.ch != 0 && l.ch != '}' {
		l.readChar()
	}

	if l.ch == '}' && l.inCommand {
		return Token{Type: EOF, Error: ErrInvalidCommand(l.position)}
	}

	l.inCommandParam = l.ch == ',' // set flag if we are still in a command parameter
	return Token{Type: CommandParam, Value: strings.TrimSpace(l.input[position:l.position])}
}

func (l *Lexer) readNAG() Token {
	// Handle cases where NAG starts with '!' or '?'
	// This shouldn't happen from my understanding of the PGN spec but lichess pgn files have it.
	// TODO: Better NAG handling of different formats
	if l.ch == '!' || l.ch == '?' {
		value := string(l.ch)
		l.readChar() // Read the next character

		// Check if the next character is also '!' or '?'
		if l.ch == '!' || l.ch == '?' {
			value += string(l.ch) // Append the second character
			l.readChar()          // Move to the next character
		}

		return Token{Type: NAG, Value: value}
	}
	l.readChar() // skip the $ symbol
	position := l.position

	// Read all digits following the $
	for isDigit(l.ch) {
		l.readChar()
	}

	// Include the $ in the token value
	return Token{
		Type:  NAG,
		Value: "$" + l.input[position:l.position],
	}
}

func (l *Lexer) readResult() Token {
	position := l.position
	for !isWhitespace(l.ch) && l.ch != 0 {
		l.readChar()
	}
	result := l.input[position:l.position]
	if isResult(result) {
		return Token{Type: RESULT, Value: result}
	}
	return Token{Type: MoveNumber, Value: result}
}

func (l *Lexer) readRank() Token {
	rank := string(l.ch)
	if !isRank(l.ch) {
		l.readChar()
		return Token{Type: RANK, Error: ErrInvalidRank(l.position), Value: rank}
	}
	l.readChar()
	return Token{Type: RANK, Value: rank}
}

func (l *Lexer) readComment() Token {
	position := l.position

	// Look for command start sequence
	for l.ch != '}' && l.ch != 0 {
		if l.ch == '[' && l.peekChar() == '%' {
			if position != l.position {
				// Return accumulated comment text before the command
				return Token{Type: COMMENT, Value: strings.TrimSpace(l.input[position:l.position])}
			}
			// Start command processing
			l.readChar() // skip [
			l.readChar() // skip %
			l.inCommand = true
			// check for EOF after command start
			if l.ch == 0 {
				return Token{
					Type:  EOF,
					Error: ErrInvalidCommand(l.position),
				}
			}
			return Token{Type: CommandStart, Value: "[%"}
		}
		l.readChar()
	}

	// Check for unterminated comment
	if l.ch == 0 {
		l.readChar()
		return Token{
			Type:  EOF,
			Error: ErrUnterminatedComment(position),
		}
	}

	// Return remaining comment text if any
	if position != l.position {
		return Token{Type: COMMENT, Value: strings.TrimSpace(l.input[position:l.position])}
	}

	return Token{Type: CommentEnd, Value: "}"}
}

// Update readPieceMove to handle piece moves.
func (l *Lexer) readPieceMove() Token {
	// Capture just the piece
	piece := string(l.ch)
	if !isPiece(l.ch) {
		l.readChar()
		return Token{Type: PIECE, Error: ErrInvalidPiece(l.position), Value: piece}
	}
	l.readChar()

	// Return just the piece - the square or capture will be read in subsequent tokens
	return Token{Type: PIECE, Value: piece}
}

func (l *Lexer) readMove() Token {
	const disambiguationLength = 3

	position := l.position

	// Check for EOF early
	if l.ch == 0 {
		return Token{Type: EOF, Value: ""}
	}

	// For pawn captures
	if isFile(l.ch) {
		file := string(l.ch)
		l.readChar()

		// Check for capture
		if l.ch == 'x' {
			return Token{Type: FILE, Value: file}
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

	// If we read 3 characters, first one is disambiguation
	if length == disambiguationLength {
		l.readPosition = position + 1
		l.readChar()
		// Return just the first character as disambiguation
		return Token{Type: FILE, Value: string(l.input[position])}
	}

	// Validate the square (e.g., "e4")
	if length < 2 || !isFile(l.input[position]) || position+1 >= len(l.input) || !isDigit(l.input[position+1]) {
		l.readChar()
		return Token{Type: SQUARE, Value: "", Error: ErrInvalidSquare(position)}
	}

	return Token{Type: SQUARE, Value: l.input[position:l.position]}
}

func (l *Lexer) readPromotionPiece() Token {
	piece := string(l.ch)
	if !isPiece(l.ch) {
		l.readChar()
		return Token{Type: PromotionPiece, Error: ErrInvalidPiece(l.position), Value: piece}
	}
	l.readChar()
	return Token{Type: PromotionPiece, Value: piece}
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) readTagValue() Token {
	l.readChar() // skip opening quote
	position := l.position
	for l.ch != '"' && l.ch != 0 {
		l.readChar()
	}
	value := l.input[position:l.position]
	l.readChar() // skip closing quote
	return Token{Type: TagValue, Value: value}
}

func (l *Lexer) readTagKey() Token {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return Token{Type: TagKey, Value: l.input[position:l.position]}
}

func (l *Lexer) readCastling() (Token, bool) {
	position := l.position

	// First character should be uppercase 'O'
	if l.ch != 'O' {
		return Token{}, false
	}

	// Check if we have enough characters for at least kingside castling (O-O)
	if l.position+2 >= len(l.input) {
		return Token{}, false
	}

	// Check for "O-O" pattern
	if l.peekChar() != '-' {
		return Token{}, false
	}
	l.readChar() // skip O
	l.readChar() // skip -

	if l.ch != 'O' {
		// Reset if pattern doesn't match
		l.position = position
		l.readPosition = position + 1
		l.ch = l.input[position]
		return Token{}, false
	}
	l.readChar() // skip O

	// Look ahead to see if this is queenside castling (O-O-O)
	if l.ch == '-' && l.peekChar() == 'O' {
		l.readChar() // skip -
		l.readChar() // skip O
		return Token{Type: QueensideCastle, Value: "O-O-O"}, true
	}

	return Token{Type: KingsideCastle, Value: "O-O"}, true
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
func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if l.inCommand {
		switch l.ch {
		case ']':
			l.inCommand = false
			l.readChar()
			return Token{Type: CommandEnd, Value: "]"}
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
			return Token{Type: CommentEnd, Value: "}"}
		}
		return l.readComment()
	}

	if l.inTag && isLetter(l.ch) {
		return l.readTagKey()
	}

	switch l.ch {
	case '(':
		l.readChar()
		return Token{Type: VariationStart, Value: "("}

	case ')':
		l.readChar()
		return Token{Type: VariationEnd, Value: ")"}
	case '[':
		l.inTag = true
		l.readChar()
		return Token{Type: TagStart, Value: "["}
	case ']':
		l.inTag = false
		l.readChar()
		return Token{Type: TagEnd, Value: "]"}
	case '"':
		return l.readTagValue()
	case '{':
		l.readChar()
		l.inComment = true
		return Token{Type: CommentStart, Value: "{"}
	case '}':
		l.readChar()
		return Token{Type: CommentEnd, Value: "}"}
	case '.':
		if l.peekChar() == '.' && l.readPosition+1 < len(l.input) && l.input[l.readPosition+1] == '.' {
			l.readChar()
			l.readChar()
			l.readChar()
			return Token{Type: ELLIPSIS, Value: "..."}
		}
		l.readChar()
		return Token{Type: DOT, Value: "."}
	case 'x':
		l.readChar()
		return Token{Type: CAPTURE, Value: "x"}
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
		return Token{Type: PROMOTION, Value: "="}
	case '+':
		l.readChar()
		return Token{Type: CHECK, Value: "+"}
	case '#':
		l.readChar()
		return Token{Type: CHECKMATE, Value: "#"}
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
			return Token{Type: MoveNumber, Value: l.input[position:l.position]}
		case '-':
			l.position = position
			l.readPosition = position + 1
			l.ch = l.input[position]
			return l.readResult()
		default:
			// Reset position and try again as a regular number
			l.position = position
			l.readPosition = position + 1
			l.ch = l.input[position]
			return l.readNumber()
		}
	case 0:
		return Token{Type: EOF, Value: ""}
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

	tok := Token{Type: Undefined, Value: string(l.ch)}
	l.readChar()
	return tok
}
