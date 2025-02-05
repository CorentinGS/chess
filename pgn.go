/*
Package chess provides PGN (Portable Game Notation) parsing functionality,
supporting standard chess notation including moves, variations, comments,
annotations, and game metadata.
Example usage:

	// Create parser from tokens
	tokens := TokenizeGame(game)
	parser := NewParser(tokens)

	// Parse complete game
	game, err := parser.Parse()
*/
package chess

import (
	"errors"
	"fmt"
	"strconv"

	"golang.org/x/exp/maps"
)

// Parser holds the state needed during parsing.
type Parser struct {
	game        *Game
	currentMove *Move
	tokens      []Token
	errors      []ParserError
	position    int
}

// NewParser creates a new parser instance initialized with the given tokens.
// The parser starts with a root move containing the starting position.
//
// Example:
//
//	tokens := TokenizeGame(game)
//	parser := NewParser(tokens)
func NewParser(tokens []Token) *Parser {
	rootMove := &Move{
		position: StartingPosition(),
	}
	return &Parser{
		tokens: tokens,
		game: &Game{
			tagPairs:    make(TagPairs),
			pos:         StartingPosition(),
			rootMove:    rootMove, // Empty root move
			currentMove: rootMove,
		},
		currentMove: rootMove,
	}
}

// currentToken returns the current token being processed.
func (p *Parser) currentToken() Token {
	if p.position >= len(p.tokens) {
		return Token{Type: EOF}
	}
	return p.tokens[p.position]
}

// advance moves to the next token.
func (p *Parser) advance() {
	p.position++
}

// Parse processes all tokens and returns the complete game.
// This includes parsing header information (tags), moves,
// variations, comments, and the game result.
//
// Returns an error if the PGN is malformed or contains illegal moves.
//
// Example:
//
//	game, err := parser.Parse()
//	if err != nil {
//	    log.Fatal("Error parsing game:", err)
//	}
//	fmt.Printf("Event: %s\n", game.GetTagPair("Event"))
func (p *Parser) Parse() (*Game, error) {
	// Parse header section (tag pairs)
	if err := p.parseHeader(); err != nil {
		return nil, errors.New("parsing header")
	}

	// check if the game has a starting position
	if value, ok := p.game.tagPairs["FEN"]; ok {
		pos, err := decodeFEN(value)
		if err != nil {
			return nil, errors.New("invalid FEN")
		}
		p.game.rootMove.position = pos
		p.game.pos = pos
	}

	// Parse moves section
	if err := p.parseMoveText(); err != nil {
		return nil, err
	}

	return p.game, nil
}

func (p *Parser) parseHeader() error {
	for p.currentToken().Type == TagStart {
		if err := p.parseTagPair(); err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) parseTagPair() error {
	// Expect [
	if p.currentToken().Type != TagStart {
		return &ParserError{
			Message:    "expected tag start",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	p.advance()

	// Get key
	if p.currentToken().Type != TagKey {
		return &ParserError{
			Message:    "expected tag key",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	key := p.currentToken().Value
	p.advance()

	// Get value
	if p.currentToken().Type != TagValue {
		return &ParserError{
			Message:    "expected tag value",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}

	}
	value := p.currentToken().Value
	p.advance()

	// Expect ]
	if p.currentToken().Type != TagEnd {
		return &ParserError{
			Message:    "expected tag end",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	p.advance()

	// Store tag pair
	p.game.tagPairs[key] = value
	return nil
}

func (p *Parser) parseMoveText() error {
	var moveNumber uint64
	for p.position < len(p.tokens) {
		token := p.currentToken()

		switch token.Type {
		case MoveNumber:
			number, err := strconv.ParseUint(token.Value, 10, 32)
			if err == nil && p.currentMove != nil {
				moveNumber = number
			}
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
			}

		case ELLIPSIS:
			p.advance()

		case PIECE, SQUARE, FILE, KingsideCastle, QueensideCastle:
			move, err := p.parseMove()
			if err != nil {
				return err
			}
			if moveNumber > 0 {
				move.number = uint(moveNumber)
			}
			p.addMove(move)

		case CommentStart:
			comment, commandMap, err := p.parseComment()
			if err != nil {
				return err
			}
			if p.currentMove != nil {
				if p.currentMove.command != nil {
					maps.Copy(p.currentMove.command, commandMap)
				} else {
					p.currentMove.command = commandMap
				}
				if p.currentMove.comments != "" {
					p.currentMove.comments += " " + comment
				} else {
					p.currentMove.comments = comment
				}
			}

		case VariationStart:
			if err := p.parseVariation(); err != nil {
				return err
			}

		case RESULT:
			p.parseResult()
			return nil

		default:
			p.advance()
		}
	}
	return nil
}

// parseMove processes tokens until it has a complete move, then validates against legal moves.
func (p *Parser) parseMove() (*Move, error) {
	move := &Move{}

	// Handle castling first as it's a special case
	if p.currentToken().Type == KingsideCastle {
		move.tags = KingSideCastle
		for _, m := range p.game.pos.ValidMoves() {
			if m.HasTag(KingSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				move.position = p.game.pos.copy()
				if m.HasTag(Check) {
					move.AddTag(Check)
				}
				p.advance()
				return move, nil
			}
		}
		return nil, &ParserError{
			Message:    "illegal kingside castle",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	if p.currentToken().Type == QueensideCastle {
		move.tags = QueenSideCastle
		for _, m := range p.game.pos.ValidMoves() {
			if m.HasTag(QueenSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				move.position = p.game.pos
				if m.HasTag(Check) {
					move.AddTag(Check)
				}
				p.advance()
				return move, nil
			}
		}
		return nil, &ParserError{
			Message:    "illegal queenside castle",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	// Parse regular move
	var moveData struct {
		piece      string    // The piece type (if any)
		originFile string    // Disambiguation file
		originRank string    // Disambiguation rank
		destSquare string    // Destination square
		isCapture  bool      // Whether it's a capture
		promotion  PieceType // Promotion piece type
	}

	// First token could be piece, file (for pawn moves), or square
	switch p.currentToken().Type {
	case PIECE:
		moveData.piece = p.currentToken().Value
		p.advance()

		// Check for disambiguation
		if p.currentToken().Type == FILE {
			moveData.originFile = p.currentToken().Value
			p.advance()
		} else if p.currentToken().Type == RANK {
			moveData.originRank = p.currentToken().Value
			p.advance()
		}

	case FILE:
		moveData.originFile = p.currentToken().Value
		p.advance()
	}

	// Handle capture
	if p.currentToken().Type == CAPTURE {
		moveData.isCapture = true
		p.advance()
	}

	// Get destination square
	if p.currentToken().Type != SQUARE {
		return nil, &ParserError{
			Message:    "expected destination square",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	moveData.destSquare = p.currentToken().Value
	p.advance()

	// Handle promotion
	if p.currentToken().Type == PROMOTION {
		p.advance()
		if p.currentToken().Type != PromotionPiece {
			return nil, &ParserError{
				Message:    "expected promotion piece",
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
				Position:   p.position,
			}
		}
		moveData.promotion = parsePieceType(p.currentToken().Value)
		p.advance()
	}

	// Get target square
	targetSquare := parseSquare(moveData.destSquare)
	if targetSquare == NoSquare {
		return nil, &ParserError{
			Message:    "invalid destination square",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	// Find matching legal move
	var matchingMove *Move
	var err error
	validMoves := p.game.pos.ValidMoves()
	for _, m := range validMoves {
		//nolint:nestif // readability
		if m.S2() == targetSquare {
			pos := p.game.pos
			piece := pos.Board().Piece(m.S1())

			// Check piece type
			if moveData.piece != "" && piece.Type() != PieceTypeFromString(moveData.piece) || moveData.piece == "" && piece.Type() != Pawn {
				err = &ParserError{
					Message:    "piece type mismatch",
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
				continue
			}

			// Check disambiguation
			if moveData.originFile != "" && m.S1().File().String() != moveData.originFile {
				err = &ParserError{
					Message:    "origin file mismatch",
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
				continue
			}
			if moveData.originRank != "" && strconv.Itoa(int((m.S1()/8)+1)) != moveData.originRank {
				err = &ParserError{
					Message:    fmt.Sprintf("origin rank mismatch: %d", m.S1()/8+1),
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
				continue
			}

			// Check capture
			if moveData.isCapture != (m.HasTag(Capture) || m.HasTag(EnPassant)) {
				err = &ParserError{
					Message:    "capture mismatch",
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
				continue
			}

			// Check promotion
			if moveData.promotion != NoPieceType && m.promo != moveData.promotion {
				err = &ParserError{
					Message:    "promotion mismatch",
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
				continue
			}

			matchingMove = &m
			break
		}
	}

	if matchingMove == nil {
		if err != nil {
			return nil, &ParserError{
				Message:  fmt.Sprintf("no legal move found for position: %s", err.Error()),
				Position: p.position,
			}
		}
		return nil, &ParserError{
			Message:  "no legal move found for position",
			Position: p.position,
		}
	}

	// Copy the matched move details
	move.s1 = matchingMove.S1()
	move.s2 = matchingMove.S2()
	move.tags = matchingMove.tags
	move.promo = matchingMove.promo
	move.position = p.game.pos.copy() // Cache current position

	// Handle check/checkmate if present
	if p.currentToken().Type == CHECK {
		move.tags |= Check
		p.advance()
	}

	// Handle NAG if present
	if p.currentToken().Type == NAG {
		move.nag = p.currentToken().Value
		p.advance()
	}

	// Set move number for both white and black moves
	if p.game.pos != nil && p.game.pos.Turn() == Black {
		if parentMoveNum := p.currentMove.number; parentMoveNum > 0 {
			move.number = parentMoveNum
		}
	}

	return move, nil
}

func (p *Parser) parseComment() (string, map[string]string, error) {
	p.advance() // Consume "{"

	var comment string
	var commandMap map[string]string

	for p.currentToken().Type != CommentEnd && p.position < len(p.tokens) {
		switch p.currentToken().Type {
		case CommandStart:
			commands, err := p.parseCommand()
			if err != nil {
				return "", nil, err
			}

			// merge commands into commandMap
			if commandMap == nil {
				commandMap = make(map[string]string)
			}
			for k, v := range commands {
				commandMap[k] = v
			}

		case COMMENT:
			comment += p.currentToken().Value // Append plain comment text
		default:
			return "", nil, &ParserError{
				Message:    "unexpected token in comment",
				Position:   p.position,
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
			}
		}
		p.advance()
	}

	if p.position >= len(p.tokens) {
		return "", nil, &ParserError{
			Message:  "unterminated comment",
			Position: p.position,
		}
	}

	p.advance() // Consume "}"
	return comment, commandMap, nil
}

func (p *Parser) parseCommand() (map[string]string, error) {
	command := make(map[string]string)
	var key string

	// Consume the opening "["
	p.advance()

	for p.currentToken().Type != CommandEnd && p.position < len(p.tokens) {
		switch p.currentToken().Type {

		case CommandName:
			// The first token in a command is treated as the key
			key = p.currentToken().Value
		case CommandParam:
			// The second token is treated as the value for the current key
			if key != "" {
				command[key] = p.currentToken().Value
				key = "" // Reset key after assigning value
			}
		default:
			return nil, &ParserError{
				Message:    "unexpected token in command",
				Position:   p.position,
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
			}
		}
		p.advance()
	}

	if p.position >= len(p.tokens) {
		return nil, &ParserError{
			Message:  "unterminated command",
			Position: p.position,
		}
	}

	// p.advance() // Consume the closing "]"
	return command, nil
}

func (p *Parser) parseVariation() error {
	p.advance() // consume (

	// Save current state to restore later
	parentMove := p.currentMove
	oldPos := p.game.pos

	// For variations at game start, we attach to root
	variationParent := p.game.rootMove

	// Find the move this variation should branch from
	if parentMove != p.game.rootMove && parentMove.parent != nil {
		// If we're in the middle of the game, the variation branches from
		// the last move before the variation start
		variationParent = parentMove.parent
		// Reset position to where the variation starts
		if variationParent.parent != nil && variationParent.parent.position != nil {
			p.game.pos = variationParent.parent.position.copy()
			if newPos := p.game.pos.Update(variationParent); newPos != nil {
				p.game.pos = newPos
			}
		} else {
			p.game.pos = StartingPosition()
		}

	} else {
		// If we're at the start of the game, the variation branches from
		// the root move
		p.game.pos = StartingPosition()
	}

	// Set current move to the parent of the variation
	p.currentMove = variationParent

	isBlackMove := false

	for p.currentToken().Type != VariationEnd && p.position < len(p.tokens) {
		switch p.currentToken().Type {
		case MoveNumber:
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
				isBlackMove = false
			}

		case ELLIPSIS:
			p.advance()
			isBlackMove = true

		case VariationStart:
			if err := p.parseVariation(); err != nil {
				return err
			}

		case PIECE, SQUARE, FILE, KingsideCastle, QueensideCastle:
			if isBlackMove != (p.game.pos.Turn() == Black) {
				return &ParserError{
					Message:  "move color mismatch",
					Position: p.position,
				}
			}

			move, err := p.parseMove()
			if err != nil {
				return err
			}

			// Add move as child of current move
			move.parent = p.currentMove
			p.currentMove.children = append(p.currentMove.children, move)

			// Cache position before the move
			move.position = p.game.pos.copy()

			// Update position
			if newPos := p.game.pos.Update(move); newPos != nil {
				p.game.pos = newPos
			}

			move.position = p.game.pos.copy()

			// Update current move pointer
			p.currentMove = move
			isBlackMove = !isBlackMove

		default:
			p.advance()
		}
	}

	if p.position >= len(p.tokens) {
		return &ParserError{
			Message:  "unterminated variation",
			Position: p.position,
		}
	}

	p.advance() // consume )

	// Restore original state
	p.game.pos = oldPos
	p.currentMove = parentMove
	p.game.currentMove = p.currentMove

	return nil
}

func (p *Parser) parseResult() {
	result := p.currentToken().Value
	switch result {
	case "1-0":
		p.game.outcome = WhiteWon
	case "0-1":
		p.game.outcome = BlackWon
	case "1/2-1/2":
		p.game.outcome = Draw
	default:
		p.game.outcome = NoOutcome
	}
	p.advance()
}

func (p *Parser) addMove(move *Move) {
	// For the first move in the game
	if p.currentMove == p.game.rootMove {
		move.parent = p.game.rootMove
		p.game.rootMove.children = append(p.game.rootMove.children, move)
	} else {
		// Normal move in the main line
		move.parent = p.currentMove
		p.currentMove.children = append(p.currentMove.children, move)
	}

	// Update position
	if newPos := p.game.pos.Update(move); newPos != nil {
		p.game.pos = newPos
	}

	// Cache position before the move
	move.position = p.game.pos.copy()

	p.currentMove = move
}

// parsePieceType converts a piece character into a PieceType.
func parsePieceType(s string) PieceType {
	switch s {
	case "P":
		return Pawn
	case "N":
		return Knight
	case "B":
		return Bishop
	case "R":
		return Rook
	case "Q":
		return Queen
	case "K":
		return King
	default:
		return NoPieceType
	}
}

// parseSquare converts a square name (e.g., "e4") into a Square.
func parseSquare(s string) Square {
	const squareLen = 2
	if len(s) != squareLen {
		return NoSquare
	}

	file := int(s[0] - 'a')
	rank := int(s[1] - '1')

	// Validate file and rank are within bounds
	if file < 0 || file > 7 || rank < 0 || rank > 7 {
		return NoSquare
	}

	return Square(rank*8 + file)
}
