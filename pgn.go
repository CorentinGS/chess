/*
Package chess provides PGN (Portable Game Notation) parsing, including
moves, variations, comments, annotations, and game metadata.

Production PGN entry points:

	// Decode one game from an io.Reader.
	g, err := chess.ParsePGN(r)

	// Iterate every game in an io.Reader, with indices and offsets.
	for rec, err := range chess.PGNRecords(ctx, r) {
	    if err != nil {
	        // handle err
	    }
	    g, err := rec.Decode()
	}
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
	tokens       pgnTokenSource
	lexer        *Lexer
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

// lexerTokenSource adapts a Lexer to the pgnTokenSource interface so the
// parser can consume a raw PGN movetext stream without materialising every
// token up front.
type lexerTokenSource struct {
	lexer *Lexer
}

func (s *lexerTokenSource) NextToken() (Token, error) {
	return s.lexer.NextToken(), nil
}

// parsePGNText parses raw PGN movetext into a Game using the given options.
func parsePGNText(raw string, options pgnOptions) (*Game, error) {
	return newParserFromSource(&lexerTokenSource{lexer: NewLexer(raw)}, options).Parse()
}

// newParser creates a parser from a pre-built token slice. This is the test
// seam for parser state-machine unit tests in pgn_test.go.
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
	parser := &Parser{
		tokens: tokens,
		game: &Game{
			tagPairs: make(TagPairs),
			tree:     tree,
			outcome:  NoOutcome,
			method:   NoMethod,
		},
		moveText: options.moveTextCodec,
	}
	if source, ok := tokens.(*lexerTokenSource); ok {
		parser.lexer = source.lexer
		parser.token = parser.lexer.NextToken()
		return parser
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
// ponytail: dual branch skips one interface call on the hot lexer path; the
// slice branch is test-only and uncovered by the unit suite (pgn_test.go:878).
// Unify on p.tokens.NextToken() — interface cost (~1-2 ns) is parse-noise.
func (p *Parser) advance() {
	p.position++
	if p.lexer != nil {
		p.token = p.lexer.NextToken()
		return
	}
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

func (p *Parser) currentMove() *MoveNode {
	if p == nil || p.game == nil || p.game.tree == nil {
		return nil
	}
	return p.game.tree.Current()
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
	// Terminal-only policy: derive checkmate/stalemate from the final main-line
	// position but leave automatic draws unset, so the Result tag and movetext
	// token remain authoritative; resolveOutcome then arbitrates.
	p.game.outcome, p.game.method = classifyOutcome(p.game.currentPosition(), 0, outcomeRules{})

	if err := p.resolveOutcome(); err != nil {
		return nil, err
	}
	return p.game, nil
}

func (p *Parser) resolveOutcome() error {
	outcome, method, err := arbitratePGNOutcome(p.game.outcome, p.game.method, p.tagOutcome, p.tokenOutcome)
	if err != nil {
		return &ParserError{Message: err.Error(), Position: p.position}
	}
	p.game.outcome = outcome
	p.game.method = method
	return nil
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
			if err == nil && p.currentMove() != nil {
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
			if err = p.collectMoveAnnotations(); err != nil {
				return err
			}

		case CommentStart:
			block, err := p.parseComment()
			if err != nil {
				return err
			}
			if current := p.currentMove(); current != nil {
				current.addCommentBlock(block)
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

func (p *Parser) parseMove() (Move, error) {
	data := sanMoveData{
		piece:     Pawn,
		canonical: p.moveText.Policy() == MoveTextPolicyStrict,
	}

	// Castling: single token
	if p.currentToken().Type == KingsideCastle || p.currentToken().Type == QueensideCastle {
		data.castle = p.currentToken().Value
		p.advance()
	} else {
		// Regular move: piece? disambiguation? capture? square promotion? check?
		hasPiece := p.currentToken().Type == PIECE
		if hasPiece {
			data.piece = algebraicPieceType(p.currentToken().Value)
			p.advance()
		}

		// Optional disambiguation (file, rank, or full origin square)
		switch p.currentToken().Type {
		case FILE:
			data.originFile = p.currentToken().Value
			p.advance()
		case RANK:
			data.originRank = p.currentToken().Value
			p.advance()
		case DeambiguationSquare:
			if value := p.currentToken().Value; len(value) == 2 {
				data.originFile = value[:1]
				data.originRank = value[1:]
			}
			p.advance()
		}

		// Optional capture
		if p.currentToken().Type == CAPTURE {
			data.capture = true
			p.advance()
		}

		// Required destination square
		if p.currentToken().Type != SQUARE || len(p.currentToken().Value) != 2 {
			return Move{}, &ParserError{
				Message:    "expected destination square",
				TokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
				Position:   p.position,
			}
		}
		destSquare := p.currentToken().Value
		data.dest = squareFromFileRank(destSquare[0], destSquare[1])
		p.advance()

		// Promotion with "="
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
			data.promotion = algebraicPieceType(p.currentToken().Value)
			p.advance()
		} else if p.moveText.Policy() == MoveTextPolicyPGNImport &&
			!hasPiece &&
			p.currentToken().Type == PIECE &&
			(destSquare[1] == '1' || destSquare[1] == '8') {
			// Import-only promotion without "=" (e.g., e8Q).
			v := p.currentToken().Value
			if v == "Q" || v == "R" || v == "B" || v == "N" {
				data.promotion = algebraicPieceType(v)
				p.advance()
			}
		}
	}

	// Optional check/checkmate suffix
	if p.currentToken().Type == CHECK || p.currentToken().Type == CHECKMATE {
		p.advance()
	}

	move, err := resolveSANMove(p.game.currentPosition(), data)
	if err != nil {
		return Move{}, &ParserError{
			Message:  strings.TrimPrefix(ErrInvalidMoveText.Error()+": "+err.Error(), "chess: "),
			Position: p.position,
		}
	}
	return move, nil
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

// collectMoveAnnotations consumes all NAGs and comments immediately following
// the current move, attaching them to the current move node.
func (p *Parser) collectMoveAnnotations() error {
	for {
		tok := p.currentToken()
		switch tok.Type {
		case NAG:
			if nagErr := p.currentMove().AddNAG(tok.Value); nagErr != nil {
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
			if current := p.currentMove(); current != nil {
				current.addCommentBlock(block)
			}
		default:
			return nil
		}
	}
}

func (p *Parser) parseVariation(parentMoveNumber uint64, parentPly int) error {
	p.advance() // consume (

	// Save current state to restore later
	parentMove := p.currentMove()
	oldCurrent := p.game.tree.Current()

	// Restore the active cursor when this function returns, including on
	// every err path between here and the end. Previously the restore was
	// open-coded at the success branch only, leaking the cursor into the
	// abandoned variation subtree on any error.
	defer p.game.tree.setCurrent(oldCurrent)

	// For variations at game start, we attach to root
	variationParent := p.game.tree.Root()

	// Find the move this variation should diverge from
	if parentMove != p.game.tree.Root() && parentMove.parent != nil {
		variationParent = parentMove.parent
	}

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
			if current := p.currentMove(); current != nil {
				current.addCommentBlock(block)
			}

		case NAG:
			if nagErr := p.currentMove().AddNAG(p.currentToken().Value); nagErr != nil {
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
			if err = p.collectMoveAnnotations(); err != nil {
				return err
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

	return nil
}

func (p *Parser) parseResult() {
	p.tokenOutcome = outcomeFromResultString(p.currentToken().Value)
	p.advance()
}

func outcomeFromResultString(s string) Outcome {
	o, _ := ParseOutcome(s)
	return o
}

func (p *Parser) addMove(move Move, number uint) {
	parent := p.currentMove()
	node := &MoveNode{move: move, parent: parent, tree: p.game.tree, number: number}
	parent.children = append(parent.children, node)
	p.game.tree.setCurrent(node)
}
