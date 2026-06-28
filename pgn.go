/*
Package chess provides PGN (Portable Game Notation) parsing functionality,
supporting standard chess notation including moves, variations, comments,
annotations, and game metadata.
Example usage:

	// Create parser from tokens
	tokens := TokenizeGame(game)
	parser := newParser(tokens)

	// Parse complete game
	game, err := parser.Parse()
*/
package chess

import (
	"errors"
	"strconv"
	"strings"
)

// Parser holds the state needed during parsing.
type Parser struct {
	game         *Game
	currentMove  *MoveNode
	tokens       pgnTokenSource
	moveText     MoveTextCodec
	token        Token
	initErr      error
	errors       []ParserError
	position     int
	tagOutcome   Outcome
	tokenOutcome Outcome
}

type pgnTokenSource interface {
	NextToken() (Token, error)
}

type sliceTokenSource struct {
	tokens []Token
	pos    int
}

func (s *sliceTokenSource) NextToken() (Token, error) {
	if s.pos >= len(s.tokens) {
		return Token{Type: EOF}, nil
	}
	token := s.tokens[s.pos]
	s.pos++
	return token, nil
}

// newParser creates a new parser instance initialized with the given tokens.
// The parser starts with a root move containing the starting position.
//
// Example:
//
//	tokens := TokenizeGame(game)
//	parser := newParser(tokens)
func newParser(tokens []Token) *Parser {
	return newParserFromSource(&sliceTokenSource{tokens: tokens}, defaultPGNOptions())
}

func newParserFromSource(tokens pgnTokenSource, opts ...pgnOptions) *Parser {
	options := defaultPGNOptions()
	if len(opts) > 0 {
		options = opts[0]
	}
	pos := StartingPosition()
	tree := newMoveTree(pos)
	rootMove := tree.Root()
	parser := &Parser{
		tokens: tokens,
		game: &Game{
			tagPairs: make(TagPairs),
			tree:     tree,
			outcome:  NoOutcome,
			method:   NoMethod,
		},
		currentMove: rootMove,
		moveText:    options.moveTextCodec,
	}
	token, err := tokens.NextToken()
	if err != nil {
		parser.initErr = err
		parser.token = Token{Type: Undefined, Value: err.Error()}
		return parser
	}
	parser.token = token
	return parser
}

// currentToken returns the current token being processed.
func (p *Parser) currentToken() Token {
	return p.token
}

// advance moves to the next token.
func (p *Parser) advance() {
	p.position++
	token, err := p.tokens.NextToken()
	if err != nil {
		p.token = Token{Type: Undefined, Value: err.Error()}
		return
	}
	p.token = token
}

func (p *Parser) atEnd() bool {
	return p.currentToken().Type == EOF
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
	if p.initErr != nil {
		return nil, p.initErr
	}

	// Parse header section (tag pairs)
	if err := p.parseHeader(); err != nil {
		return nil, errors.New("chess: parsing header")
	}

	p.tagOutcome = outcomeFromResultString(p.game.tagPairs["Result"])

	// check if the game has a starting position
	if value, ok := p.game.tagPairs["FEN"]; ok {
		pos, err := decodeFEN(value)
		if err != nil {
			return nil, errors.New("chess: invalid FEN")
		}
		p.game.tree.setRootPosition(pos)
	}

	// Parse moves section
	if err := p.parseMoveText(); err != nil {
		return nil, err
	}
	p.game.evaluateTerminalPositionStatus()

	if err := p.resolveOutcome(); err != nil {
		return nil, err
	}
	p.game.tree.setCurrent(p.currentMove)

	return p.game, nil
}

func (p *Parser) resolveOutcome() error {
	boardMethod := p.game.method
	boardOutcome := p.game.outcome
	tagOutcome := normalizeOutcome(p.tagOutcome)
	tokenOutcome := normalizeOutcome(p.tokenOutcome)

	boardTerminal := boardMethod == Checkmate || boardMethod == Stalemate

	if boardTerminal {
		if tokenOutcome != NoOutcome && tokenOutcome != boardOutcome {
			return &ParserError{
				Message:  "movetext result token conflicts with board-derivable outcome",
				Position: p.position,
			}
		}
		if tagOutcome != NoOutcome && tagOutcome != boardOutcome {
			return &ParserError{
				Message:  "Result tag conflicts with board-derivable outcome",
				Position: p.position,
			}
		}
		p.game.outcome = boardOutcome
		p.game.method = boardMethod
		return nil
	}

	if tokenOutcome != NoOutcome {
		if tagOutcome != NoOutcome && tagOutcome != tokenOutcome {
			return &ParserError{
				Message:  "movetext result token conflicts with Result tag",
				Position: p.position,
			}
		}
		p.game.outcome = tokenOutcome
		p.game.method = NoMethod
		return nil
	}

	if tagOutcome != NoOutcome {
		p.game.outcome = tagOutcome
		p.game.method = NoMethod
		return nil
	}

	p.game.outcome = NoOutcome
	p.game.method = NoMethod
	return nil
}

func normalizeOutcome(o Outcome) Outcome {
	if o == UnknownOutcome {
		return NoOutcome
	}
	return o
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
	ply := 1
	for !p.atEnd() {
		token := p.currentToken()

		switch token.Type {
		case MoveNumber:
			number, err := strconv.ParseUint(token.Value, 10, 32)
			if err == nil && p.currentMove != nil {
				moveNumber = number
				ply = int((moveNumber-1)*2 + 1)
			}
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
			}

		case ELLIPSIS:
			p.advance()
			ply++

		case NullMove:
			p.addMove(NewNullMove(), uint(moveNumber))
			p.advance()
			ply++

		case PIECE, SQUARE, FILE, KingsideCastle, QueensideCastle:
			move, err := p.parseMove()
			if err != nil {
				return err
			}
			p.addMove(move, uint(moveNumber))
			ply++

			// Collect all NAGs and comments that follow the move
		collectLoop:
			for {
				tok := p.currentToken()
				switch tok.Type {
				case NAG:
					if nagErr := p.currentMove.AddNAG(tok.Value); nagErr != nil {
						return &ParserError{
							Message:    nagErr.Error(),
							TokenValue: tok.Value,
							TokenType:  NAG,
							Position:   p.position,
						}
					}
					p.advance()
				case CommentStart:
					block, err := p.parseComment()
					if err != nil {
						return err
					}
					if p.currentMove != nil {
						p.currentMove.addCommentBlock(block)
					}
				default:
					break collectLoop
				}
			}

		case CommentStart:
			block, err := p.parseComment()
			if err != nil {
				return err
			}
			if p.currentMove != nil {
				p.currentMove.addCommentBlock(block)
			}

		case VariationStart:
			if err := p.parseVariation(moveNumber, ply); err != nil {
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
func (p *Parser) parseMove() (Move, error) {
	move := Move{}

	// Handle castling first as it's a special case
	if p.currentToken().Type == KingsideCastle {
		move.tags = KingSideCastle
		var castles [2]Move
		count := castleMovesInto(p.game.currentPosition(), &castles, generateLegalAnnotated)
		for i := range count {
			m := castles[i]
			if m.HasTag(KingSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				if m.HasTag(Check) {
					move = move.WithTag(Check)
				}
				p.advance()
				return move, nil
			}
		}
		return Move{}, &ParserError{
			Message:    "illegal kingside castle",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	if p.currentToken().Type == QueensideCastle {
		move.tags = QueenSideCastle
		var castles [2]Move
		count := castleMovesInto(p.game.currentPosition(), &castles, generateLegalAnnotated)
		for i := range count {
			m := castles[i]
			if m.HasTag(QueenSideCastle) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				if m.HasTag(Check) {
					move = move.WithTag(Check)
				}
				p.advance()
				return move, nil
			}
		}
		return Move{}, &ParserError{
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
		} else if p.currentToken().Type == DeambiguationSquare {
			// Full square disambiguation (e.g., "Qe8f7" -> piece: Q, origin: e8, dest: f7)
			originSquare := p.currentToken().Value
			if len(originSquare) == 2 {
				moveData.originFile = string(originSquare[0])
				moveData.originRank = string(originSquare[1])
			}
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
		return Move{}, &ParserError{
			Message:    "expected destination square",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	moveData.destSquare = p.currentToken().Value
	p.advance()

	// Get target square before promotion handling so import-only promotion
	// spellings such as e8Q can be recognised without consuming ordinary
	// piece tokens after non-promotion moves.
	targetSquare := parseSquare(moveData.destSquare)
	if targetSquare == NoSquare {
		return Move{}, &ParserError{
			Message:    "invalid destination square",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	// Handle promotion
	if p.currentToken().Type == PROMOTION {
		p.advance()
		if p.currentToken().Type != PromotionPiece {
			return Move{}, &ParserError{
				Message:    "expected promotion piece",
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
				Position:   p.position,
			}
		}
		moveData.promotion = parsePieceType(p.currentToken().Value)
		p.advance()
	}
	if p.moveText.Policy() == MoveTextPolicyPGNImport &&
		moveData.promotion == NoPieceType &&
		moveData.piece == "" &&
		p.currentToken().Type == PIECE &&
		isPromotionDestination(targetSquare) {
		promo := parsePieceType(p.currentToken().Value)
		switch promo {
		case Queen, Rook, Bishop, Knight:
			moveData.promotion = promo
			p.advance()
		}
	}

	movePieceType := Pawn
	if moveData.piece != "" {
		movePieceType = PieceTypeFromString(moveData.piece)
	}

	matched, err := resolveSANMove(p.game.currentPosition(), sanMoveData{
		piece:      movePieceType,
		originFile: moveData.originFile,
		originRank: moveData.originRank,
		dest:       targetSquare,
		capture:    moveData.isCapture,
		promotion:  moveData.promotion,
		canonical:  p.moveText.Policy() == MoveTextPolicyStrict,
	})
	if err != nil {
		return Move{}, &ParserError{
			Message:  strings.TrimPrefix(err.Error(), "chess: "),
			Position: p.position,
		}
	}

	move.s1 = matched.S1()
	move.s2 = matched.S2()
	move.tags = matched.tags
	move.promo = matched.promo

	// Handle check/checkmate if present
	if p.currentToken().Type == CHECK {
		move.tags |= Check
		p.advance()
	}

	return move, nil
}

func isPromotionDestination(s Square) bool {
	return s.Rank() == Rank1 || s.Rank() == Rank8
}

func (p *Parser) parseComment() (CommentBlock, error) {
	p.advance() // Consume "{"

	block := CommentBlock{}

	for p.currentToken().Type != CommentEnd && !p.atEnd() {
		switch p.currentToken().Type {
		case CommandStart:
			command, err := p.parseCommand()
			if err != nil {
				return CommentBlock{}, err
			}
			block.Items = append(block.Items, command)

		case COMMENT:
			block.Items = append(block.Items, CommentItem{Kind: CommentText, Text: p.currentToken().Value})
		default:
			return CommentBlock{}, &ParserError{
				Message:    "unexpected token in comment",
				Position:   p.position,
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
			}
		}
		p.advance()
	}

	if p.atEnd() {
		return CommentBlock{}, &ParserError{
			Message:  "unterminated comment",
			Position: p.position,
		}
	}

	p.advance() // Consume "}"
	return block, nil
}

func (p *Parser) parseCommand() (CommentItem, error) {
	var key string
	var value string

	// Consume the opening "["
	p.advance()

	for p.currentToken().Type != CommandEnd && !p.atEnd() {
		switch p.currentToken().Type {

		case CommandName:
			if key != "" {
				return CommentItem{}, &ParserError{
					Message:    "duplicate command name in command",
					Position:   p.position,
					TokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
				}
			}
			// The first token in a command is treated as the key
			key = p.currentToken().Value
		case CommandParam:
			// The second token is treated as the value for the current key
			if key != "" && value == "" {
				value = p.currentToken().Value
			}
		default:
			return CommentItem{}, &ParserError{
				Message:    "unexpected token in command",
				Position:   p.position,
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
			}
		}
		p.advance()
	}

	if p.atEnd() {
		return CommentItem{}, &ParserError{
			Message:  "unterminated command",
			Position: p.position,
		}
	}

	return CommentItem{Kind: CommentCommand, Key: key, Value: value}, nil
}

func (p *Parser) parseVariation(parentMoveNumber uint64, parentPly int) error {
	p.advance() // consume (

	// Save current state to restore later
	parentMove := p.currentMove
	oldCurrent := p.game.tree.Current()

	// For variations at game start, we attach to root
	variationParent := p.game.tree.Root()

	// Find the move this variation should diverge from
	if parentMove != p.game.tree.Root() && parentMove.parent != nil {
		variationParent = parentMove.parent
	}

	p.currentMove = variationParent
	p.game.tree.setCurrent(variationParent)

	moveNumber := parentMoveNumber
	ply := parentPly
	isBlackMove := false

	for p.currentToken().Type != VariationEnd && !p.atEnd() {
		switch p.currentToken().Type {
		case MoveNumber:
			num, err := strconv.ParseUint(p.currentToken().Value, 10, 32)
			if err == nil {
				moveNumber = num
				ply = int((moveNumber-1)*2 + 1)
			}
			p.advance()
			if p.currentToken().Type == DOT {
				p.advance()
				isBlackMove = false
			}

		case ELLIPSIS:
			p.advance()
			isBlackMove = true
			ply++

		case NullMove:
			p.addMove(NewNullMove(), uint(moveNumber))
			p.advance()
			ply++
			isBlackMove = !isBlackMove

		case VariationStart:
			if err := p.parseVariation(moveNumber, ply); err != nil {
				return err
			}

		case CommentStart:
			block, err := p.parseComment()
			if err != nil {
				return err
			}
			if p.currentMove != nil {
				p.currentMove.addCommentBlock(block)
			}

		case NAG:
			if nagErr := p.currentMove.AddNAG(p.currentToken().Value); nagErr != nil {
				return &ParserError{
					Message:    nagErr.Error(),
					TokenValue: p.currentToken().Value,
					TokenType:  NAG,
					Position:   p.position,
				}
			}
			p.advance()

		case PIECE, SQUARE, FILE, KingsideCastle, QueensideCastle:
			if isBlackMove != (p.game.currentPosition().Turn() == Black) {
				return &ParserError{
					Message:  "move color mismatch",
					Position: p.position,
				}
			}

			move, err := p.parseMove()
			if err != nil {
				return err
			}

			p.addMove(move, uint(moveNumber))
			ply++
			isBlackMove = !isBlackMove

			// Collect all NAGs and comments that follow the move
		collectVariationAnnotations:
			for {
				tok := p.currentToken()
				switch tok.Type {
				case NAG:
					if nagErr := p.currentMove.AddNAG(tok.Value); nagErr != nil {
						return &ParserError{
							Message:    nagErr.Error(),
							TokenValue: tok.Value,
							TokenType:  NAG,
							Position:   p.position,
						}
					}
					p.advance()
				case CommentStart:
					block, err := p.parseComment()
					if err != nil {
						return err
					}
					if p.currentMove != nil {
						p.currentMove.addCommentBlock(block)
					}
				default:
					break collectVariationAnnotations
				}
			}

		default:
			p.advance()
		}
	}

	if p.atEnd() {
		return &ParserError{
			Message:  "unterminated variation",
			Position: p.position,
		}
	}

	p.advance() // consume )

	p.currentMove = parentMove
	p.game.tree.setCurrent(oldCurrent)

	return nil
}

func (p *Parser) parseResult() {
	p.tokenOutcome = outcomeFromResultString(p.currentToken().Value)
	p.advance()
}

func outcomeFromResultString(s string) Outcome {
	switch s {
	case "1-0":
		return WhiteWon
	case "0-1":
		return BlackWon
	case "1/2-1/2":
		return Draw
	default:
		return NoOutcome
	}
}

func (p *Parser) addMove(move Move, number uint) {
	node := &MoveNode{move: move, parent: p.currentMove, number: number}
	p.currentMove.children = append(p.currentMove.children, node)

	// Update position
	if newPos := p.game.currentPosition().Update(move); newPos != nil {
		node.position = newPos
	}

	p.currentMove = node
	p.game.tree.setCurrent(node)
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
