package chess

import (
	"fmt"
	"strconv"
	"strings"
)

// pgnParser holds the state needed during parsing.
type pgnParser struct {
	game         *Game
	tokens       pgnTokenSource
	lexer        *pgnLexer
	moveText     MoveTextCodec
	token        pgnToken
	initErr      error
	errors       []ParserError
	position     int
	tagOutcome   Outcome
	tokenOutcome Outcome
}

type pgnTokenSource interface {
	NextToken() (pgnToken, error)
}

type sliceTokenSource struct {
	tokens []pgnToken
	pos    int
}

func (s *sliceTokenSource) NextToken() (pgnToken, error) {
	if s.pos >= len(s.tokens) {
		return pgnToken{Type: tokEOF}, nil
	}
	token := s.tokens[s.pos]
	s.pos++
	return token, nil
}

// lexerTokenSource adapts a Lexer to the pgnTokenSource interface so the
// parser can consume a raw PGN movetext stream without materialising every
// token up front.
type lexerTokenSource struct {
	lexer *pgnLexer
}

func (s *lexerTokenSource) NextToken() (pgnToken, error) {
	return s.lexer.NextToken(), nil
}

// parsePGNText parses raw PGN movetext into a Game using the given options.
func parsePGNText(raw string, options pgnOptions) (*Game, error) {
	return newParserFromSource(&lexerTokenSource{lexer: newPGNLexer(raw)}, options).Parse()
}

// newParser creates a parser from a pre-built token slice. This is the test
// seam for parser state-machine unit tests in pgn_test.go.
func newParser(tokens []pgnToken) *pgnParser {
	return newParserFromSource(&sliceTokenSource{tokens: tokens}, defaultPGNOptions())
}

func newParserFromSource(tokens pgnTokenSource, opts ...pgnOptions) *pgnParser {
	options := defaultPGNOptions()
	if len(opts) > 0 {
		options = opts[0]
	}
	pos := StartingPosition()
	tree := newMoveTree(pos)
	parser := &pgnParser{
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
		parser.token = pgnToken{Type: tokUndefined, Value: err.Error()}
		return parser
	}
	parser.token = token
	return parser
}

// currentToken returns the current token being processed.
func (p *pgnParser) currentToken() pgnToken {
	return p.token
}

// advance moves to the next token.
// ponytail: dual branch skips one interface call on the hot lexer path; the
// slice branch is test-only and uncovered by the unit suite (pgn_test.go:878).
// Unify on p.tokens.NextToken() — interface cost (~1-2 ns) is parse-noise.
func (p *pgnParser) advance() {
	p.position++
	if p.lexer != nil {
		p.token = p.lexer.NextToken()
		return
	}
	token, err := p.tokens.NextToken()
	if err != nil {
		p.token = pgnToken{Type: tokUndefined, Value: err.Error()}
		return
	}
	p.token = token
}

func (p *pgnParser) atEnd() bool {
	return p.currentToken().Type == tokEOF
}

func (p *pgnParser) currentMove() *MoveNode {
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
func (p *pgnParser) Parse() (*Game, error) {
	if p.initErr != nil {
		return nil, p.initErr
	}

	// Parse header section (tag pairs)
	if err := p.parseHeader(); err != nil {
		return nil, fmt.Errorf("%w: parsing header: %w", ErrInvalidPGN, err)
	}

	p.tagOutcome = outcomeFromResultString(p.game.tagPairs["Result"])

	// check if the game has a starting position
	if value, ok := p.game.tagPairs["FEN"]; ok {
		pos, err := decodeFEN(value)
		if err != nil {
			return nil, fmt.Errorf("%w: %w", ErrInvalidFEN, err)
		}
		if variantFromTag(p.game.tagPairs["Variant"]) == Chess960 {
			pos.variant = Chess960
		}
		p.game.tree.setRootPosition(pos)
	} else if variantFromTag(p.game.tagPairs["Variant"]) == Chess960 {
		// Chess960 declared without a FEN: default to the standard arrangement
		// (SP-518) treated as a Chess960 position.
		pos := StartingPosition()
		pos.variant = Chess960
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

func (p *pgnParser) resolveOutcome() error {
	outcome, method, err := arbitratePGNOutcome(p.game.outcome, p.game.method, p.tagOutcome, p.tokenOutcome)
	if err != nil {
		return &ParserError{Message: err.Error(), Position: p.position}
	}
	p.game.outcome = outcome
	p.game.method = method
	return nil
}

func (p *pgnParser) parseHeader() error {
	for p.currentToken().Type == tokTagStart {
		if err := p.parseTagPair(); err != nil {
			return err
		}
	}
	return nil
}

func (p *pgnParser) parseTagPair() error {
	// Expect [
	if p.currentToken().Type != tokTagStart {
		return &ParserError{
			Message:    "expected tag start",
			tokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	p.advance()

	// Get key
	if p.currentToken().Type != tokTagKey {
		return &ParserError{
			Message:    "expected tag key",
			tokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	key := p.currentToken().Value
	p.advance()

	// Get value
	if p.currentToken().Type != tokTagValue {
		return &ParserError{
			Message:    "expected tag value",
			tokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	value := p.currentToken().Value
	p.advance()

	// Expect ]
	if p.currentToken().Type != tokTagEnd {
		return &ParserError{
			Message:    "expected tag end",
			tokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}
	p.advance()

	// Store tag pair
	p.game.tagPairs[key] = value
	return nil
}

// variantFromTag maps a PGN Variant tag value to a Variant. Recognised Chess960
// aliases (case-insensitive) yield Chess960; anything else yields Standard.
func variantFromTag(s string) Variant {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "chess960", "chess 960", "fischer random", "fischerrandom",
		"fischerandom", "frc", "960":
		return Chess960
	}
	return Standard
}

func (p *pgnParser) parseMoveText() error {
	var moveNumber uint64
	ply := 1
	for !p.atEnd() {
		token := p.currentToken()

		switch token.Type {
		case tokMoveNumber:
			number, err := strconv.ParseUint(token.Value, 10, 32)
			if err == nil && p.currentMove() != nil {
				moveNumber = number
				ply = int((moveNumber-1)*2 + 1)
			}
			p.advance()
			if p.currentToken().Type == tokDot {
				p.advance()
			}

		case tokEllipsis:
			p.advance()
			ply++

		case tokNullMove:
			p.addMove(NewNullMove(), uint(moveNumber))
			p.advance()
			ply++

		case tokPiece, tokSquare, tokFile, tokKingsideCastle, tokQueensideCastle:
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

		case tokCommentStart:
			block, err := p.parseComment()
			if err != nil {
				return err
			}
			if current := p.currentMove(); current != nil {
				current.addCommentBlock(block)
			}

		case tokVariationStart:
			if err := p.parseVariation(moveNumber, ply); err != nil {
				return err
			}

		case tokResult:
			p.parseResult()
			return nil

		default:
			p.advance()
		}
	}
	return nil
}

func (p *pgnParser) parseMove() (Move, error) {
	data := sanMoveData{
		piece:     Pawn,
		canonical: p.moveText.Policy() == MoveTextPolicyStrict,
	}

	// Castling: single token
	if p.currentToken().Type == tokKingsideCastle || p.currentToken().Type == tokQueensideCastle {
		data.castle = p.currentToken().Value
		p.advance()
	} else {
		// Regular move: piece? disambiguation? capture? square promotion? check?
		hasPiece := p.currentToken().Type == tokPiece
		if hasPiece {
			data.piece = algebraicPieceType(p.currentToken().Value)
			p.advance()
		}

		// Optional disambiguation (file, rank, or full origin square)
		switch p.currentToken().Type {
		case tokFile:
			data.originFile = p.currentToken().Value
			p.advance()
		case tokRank:
			data.originRank = p.currentToken().Value
			p.advance()
		case tokDisambiguationSquare:
			if value := p.currentToken().Value; len(value) == 2 {
				data.originFile = value[:1]
				data.originRank = value[1:]
			}
			p.advance()
		}

		// Optional capture
		if p.currentToken().Type == tokCapture {
			data.capture = true
			p.advance()
		}

		// Required destination square
		if p.currentToken().Type != tokSquare || len(p.currentToken().Value) != 2 {
			return Move{}, &ParserError{
				Message:    "expected destination square",
				tokenType:  p.currentToken().Type,
				TokenValue: p.currentToken().Value,
				Position:   p.position,
			}
		}
		destSquare := p.currentToken().Value
		data.dest = squareFromFileRank(destSquare[0], destSquare[1])
		p.advance()

		// Promotion with "="
		if p.currentToken().Type == tokPromotion {
			p.advance()
			if p.currentToken().Type != tokPromotionPiece {
				return Move{}, &ParserError{
					Message:    "expected promotion piece",
					tokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
					Position:   p.position,
				}
			}
			data.promotion = algebraicPieceType(p.currentToken().Value)
			p.advance()
		} else if p.moveText.Policy() == MoveTextPolicyPGNImport &&
			!hasPiece &&
			p.currentToken().Type == tokPiece &&
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
	if p.currentToken().Type == tokCheck || p.currentToken().Type == tokCheckmate {
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

func (p *pgnParser) parseComment() (CommentBlock, error) {
	p.advance() // Consume "{"

	block := CommentBlock{}

	for p.currentToken().Type != tokCommentEnd && !p.atEnd() {
		switch p.currentToken().Type {
		case tokCommandStart:
			command, err := p.parseCommand()
			if err != nil {
				return CommentBlock{}, err
			}
			block.Items = append(block.Items, command)

		case tokComment:
			block.Items = append(block.Items, CommentItem{Kind: CommentText, Text: p.currentToken().Value})
		default:
			return CommentBlock{}, &ParserError{
				Message:    "unexpected token in comment",
				Position:   p.position,
				tokenType:  p.currentToken().Type,
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

func (p *pgnParser) parseCommand() (CommentItem, error) {
	var key string
	var value string

	// Consume the opening "["
	p.advance()

	for p.currentToken().Type != tokCommandEnd && !p.atEnd() {
		switch p.currentToken().Type {
		case tokCommandName:
			if key != "" {
				return CommentItem{}, &ParserError{
					Message:    "duplicate command name in command",
					Position:   p.position,
					tokenType:  p.currentToken().Type,
					TokenValue: p.currentToken().Value,
				}
			}
			// The first token in a command is treated as the key
			key = p.currentToken().Value
		case tokCommandParam:
			// The second token is treated as the value for the current key
			if key != "" && value == "" {
				value = p.currentToken().Value
			}
		default:
			return CommentItem{}, &ParserError{
				Message:    "unexpected token in command",
				Position:   p.position,
				tokenType:  p.currentToken().Type,
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
func (p *pgnParser) collectMoveAnnotations() error {
	for {
		tok := p.currentToken()
		switch tok.Type {
		case tokNAG:
			if nagErr := p.currentMove().AddNAG(tok.Value); nagErr != nil {
				return &ParserError{
					Message:    nagErr.Error(),
					TokenValue: tok.Value,
					tokenType:  tokNAG,
					Position:   p.position,
				}
			}
			p.advance()
		case tokCommentStart:
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

func (p *pgnParser) parseVariation(parentMoveNumber uint64, parentPly int) error {
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

	for p.currentToken().Type != tokVariationEnd && !p.atEnd() {
		switch p.currentToken().Type {
		case tokMoveNumber:
			num, err := strconv.ParseUint(p.currentToken().Value, 10, 32)
			if err == nil {
				moveNumber = num
				ply = int((moveNumber-1)*2 + 1)
			}
			p.advance()
			if p.currentToken().Type == tokDot {
				p.advance()
				isBlackMove = false
			}

		case tokEllipsis:
			p.advance()
			isBlackMove = true
			ply++

		case tokNullMove:
			p.addMove(NewNullMove(), uint(moveNumber))
			p.advance()
			ply++
			isBlackMove = !isBlackMove

		case tokVariationStart:
			if err := p.parseVariation(moveNumber, ply); err != nil {
				return err
			}

		case tokCommentStart:
			block, err := p.parseComment()
			if err != nil {
				return err
			}
			if current := p.currentMove(); current != nil {
				current.addCommentBlock(block)
			}

		case tokNAG:
			if nagErr := p.currentMove().AddNAG(p.currentToken().Value); nagErr != nil {
				return &ParserError{
					Message:    nagErr.Error(),
					TokenValue: p.currentToken().Value,
					tokenType:  tokNAG,
					Position:   p.position,
				}
			}
			p.advance()

		case tokPiece, tokSquare, tokFile, tokKingsideCastle, tokQueensideCastle:
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

func (p *pgnParser) parseResult() {
	p.tokenOutcome = outcomeFromResultString(p.currentToken().Value)
	p.advance()
}

func outcomeFromResultString(s string) Outcome {
	o, _ := ParseOutcome(s)
	return o
}

func (p *pgnParser) addMove(move Move, number uint) {
	parent := p.currentMove()
	node := &MoveNode{move: move, parent: parent, tree: p.game.tree, number: number}
	parent.children = append(parent.children, node)
	p.game.tree.setCurrent(node)
}
