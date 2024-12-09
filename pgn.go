package chess

import (
	"fmt"
	"strconv"
	"strings"
)

// Parser holds the state needed during parsing
type Parser struct {
	tokens      []Token
	position    int
	game        *Game
	currentMove *Move
}

// NewParser creates a new Parser instance
func NewParser(tokens []Token) *Parser {
	rootMove := &Move{
		position: StartingPosition(),
	}
	return &Parser{
		tokens: tokens,
		game: &Game{
			tagPairs: make(TagPairs),
			pos:      StartingPosition(),
			rootMove: rootMove, // Empty root move
		},
		currentMove: rootMove,
	}
}

// currentToken returns the current token being processed
func (p *Parser) currentToken() Token {
	if p.position >= len(p.tokens) {
		return Token{Type: EOF}
	}
	return p.tokens[p.position]
}

// advance moves to the next token
func (p *Parser) advance() {
	p.position++
}

// Parse processes all tokens and returns the completed game
func (p *Parser) Parse() (*Game, error) {
	// Parse header section (tag pairs)
	if err := p.parseHeader(); err != nil {
		return nil, fmt.Errorf("parsing header: %w", err)
	}

	// check if the game has a starting position
	if value, ok := p.game.tagPairs["FEN"]; ok {
		pos, err := decodeFEN(value)
		if err != nil {
			return nil, fmt.Errorf("decoding FEN: %w", err)
		}
		p.game.rootMove.position = pos
		p.game.pos = pos
	}

	// Parse moves section
	if err := p.parseMoveText(); err != nil {
		return nil, fmt.Errorf("parsing moves: %w", err)
	}

	return p.game, nil
}

func (p *Parser) parseHeader() error {
	for p.currentToken().Type == TAG_START {
		if err := p.parseTagPair(); err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) parseTagPair() error {
	// Expect [
	if p.currentToken().Type != TAG_START {
		return fmt.Errorf("expected tag start, got %v", p.currentToken())
	}
	p.advance()

	// Get key
	if p.currentToken().Type != TAG_KEY {
		return fmt.Errorf("expected tag key, got %v", p.currentToken())
	}
	key := p.currentToken().Value
	p.advance()

	// Get value
	if p.currentToken().Type != TAG_VALUE {
		return fmt.Errorf("expected tag value, got %v", p.currentToken())
	}
	value := p.currentToken().Value
	p.advance()

	// Expect ]
	if p.currentToken().Type != TAG_END {
		return fmt.Errorf("expected tag end, got %v", p.currentToken())
	}
	p.advance()

	// Store tag pair
	p.game.tagPairs[key] = value
	return nil
}

func (p *Parser) parseMoveText() error {
	for p.position < len(p.tokens) {
		token := p.currentToken()

		switch token.Type {
		case MOVE_NUMBER:
			number, err := strconv.ParseUint(token.Value, 10, 32)
			if err == nil && p.currentMove != nil {
				p.currentMove.number = uint(number)
			}
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
			}

		case ELLIPSIS:
			p.advance()

		case PIECE, SQUARE, FILE, KINGSIDE_CASTLE, QUEENSIDE_CASTLE:
			move, err := p.parseMove()
			if err != nil {
				return err
			}
			p.addMove(move)

		case COMMENT_START:
			comment, err := p.parseComment()
			if err != nil {
				return err
			}
			if p.currentMove != nil {
				p.currentMove.comments = comment
			}

		case VARIATION_START:
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

func (p *Parser) peek() Token {
	if p.position+1 >= len(p.tokens) {
		return Token{Type: EOF}
	}
	return p.tokens[p.position+1]
}

// parseMove processes tokens until it has a complete move, then validates against legal moves
func (p *Parser) parseMove() (*Move, error) {
	move := &Move{}

	// Handle castling first as it's a special case
	if p.currentToken().Type == KINGSIDE_CASTLE {
		move.tags = KingSideCastle
		for _, m := range p.game.pos.ValidMoves() {
			if m.HasTag(KingSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				move.position = p.game.pos.copy()
				p.advance()
				return move, nil
			}
		}
		return nil, fmt.Errorf("illegal kingside castle")
	}

	if p.currentToken().Type == QUEENSIDE_CASTLE {
		move.tags = QueenSideCastle
		for _, m := range p.game.pos.ValidMoves() {
			if m.HasTag(QueenSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				move.position = p.game.pos
				p.advance()
				return move, nil
			}
		}
		return nil, fmt.Errorf("illegal queenside castle")
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
		return nil, fmt.Errorf("expected square, got %v", p.currentToken())
	}
	moveData.destSquare = p.currentToken().Value
	p.advance()

	// Handle promotion
	if p.currentToken().Type == PROMOTION {
		p.advance()
		if p.currentToken().Type != PROMOTION_PIECE {
			return nil, fmt.Errorf("expected promotion piece")
		}
		moveData.promotion = parsePieceType(p.currentToken().Value)
		p.advance()
	}

	// Get target square
	targetSquare := parseSquare(moveData.destSquare)
	if targetSquare == NoSquare {
		return nil, fmt.Errorf("invalid destination square")
	}

	// Find matching legal move
	var matchingMove *Move
	var err error
	for _, m := range p.game.pos.ValidMoves() {
		if m.S2() == targetSquare {
			pos := p.game.pos
			piece := pos.Board().Piece(m.S1())

			// Check piece type
			if moveData.piece != "" && piece.Type() != PieceTypeFromString(moveData.piece) || moveData.piece == "" && piece.Type() != Pawn {
				err = fmt.Errorf("piece type mismatch")
				continue
			}

			// Check disambiguation
			if moveData.originFile != "" && m.S1().File().String() != moveData.originFile {
				err = fmt.Errorf("origin file mismatch")
				continue
			}
			if moveData.originRank != "" && strconv.Itoa(int('1'+m.S1()/8)) != moveData.originRank {
				err = fmt.Errorf("origin rank mismatch")
				continue
			}

			// Check capture
			if moveData.isCapture != (m.HasTag(Capture) || m.HasTag(EnPassant)) {
				err = fmt.Errorf("capture mismatch")
				continue
			}

			// Check promotion
			if moveData.promotion != NoPieceType && m.promo != moveData.promotion {
				err = fmt.Errorf("promotion mismatch")
				continue
			}

			matchingMove = &m
			break

		}
	}

	if matchingMove == nil {
		if err != nil {
			return nil, fmt.Errorf("no legal move found for position: %w", err)
		}
		return nil, fmt.Errorf("no legal move found for position")
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
func (p *Parser) parseComment() (string, error) {
	p.advance() // consume {

	var comment string
	for p.currentToken().Type != COMMENT_END && p.position < len(p.tokens) {
		if p.currentToken().Type == COMMAND_START {
			command, err := p.parseCommand()
			if err != nil {
				return "", err
			}
			comment += command
		} else if p.currentToken().Type == COMMENT {
			comment += p.currentToken().Value
		}
		p.advance()
	}

	if p.position >= len(p.tokens) {
		return "", fmt.Errorf("unterminated comment")
	}

	p.advance() // consume }
	return comment, nil
}

func (p *Parser) parseCommand() (string, error) {
	var parts []string

	for p.currentToken().Type != COMMAND_END && p.position < len(p.tokens) {
		switch p.currentToken().Type {
		case COMMAND_NAME, COMMAND_PARAM:
			parts = append(parts, p.currentToken().Value)
		}
		p.advance()
	}

	if p.position >= len(p.tokens) {
		return "", fmt.Errorf("unterminated command")
	}

	return strings.Join(parts, " "), nil
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
		p.game.pos = variationParent.parent.position.copy()
		if newPos := p.game.pos.Update(variationParent); newPos != nil {
			p.game.pos = newPos
		}
	} else {
		// Reset to starting position for variations from root
		p.game.pos = StartingPosition()
	}

	// Set current move to the parent of the variation
	p.currentMove = variationParent

	isBlackMove := false

	for p.currentToken().Type != VARIATION_END && p.position < len(p.tokens) {
		switch p.currentToken().Type {
		case MOVE_NUMBER:
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
				isBlackMove = false
			}

		case ELLIPSIS:
			p.advance()
			isBlackMove = true

		case PIECE, SQUARE, FILE, KINGSIDE_CASTLE, QUEENSIDE_CASTLE:
			if isBlackMove != (p.game.pos.Turn() == Black) {
				return fmt.Errorf("move color mismatch in variation")
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

			// Update current move pointer
			p.currentMove = move
			isBlackMove = !isBlackMove

		default:
			p.advance()
		}
	}

	if p.position >= len(p.tokens) {
		return fmt.Errorf("unterminated variation")
	}

	p.advance() // consume )

	// Restore original state
	p.game.pos = oldPos
	p.currentMove = parentMove

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

// parsePieceType converts a piece character into a PieceType
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

// parseSquare converts a square name (e.g., "e4") into a Square
func parseSquare(s string) Square {
	if len(s) != 2 {
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

// Optional helper to convert in the other direction for debugging
func squareToString(sq Square) string {
	if sq == NoSquare {
		return "-"
	}

	file := 'a' + sq%8
	rank := '1' + sq/8

	return strconv.Itoa(int(file)) + strconv.Itoa(int(rank))
}
