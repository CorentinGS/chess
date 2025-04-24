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

	validMoves := p.game.pos.ValidMoves() // Potential alloc: slice for moves
	if len(validMoves) == 0 {
		return nil, &ParserError{
			Message:    "no legal moves available in the position",
			TokenType:  p.currentToken().Type,
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	currentTok := p.currentToken() // Read current token

	// --- Handle Castling ---
	var expectedCastleTag MoveTag = 0
	if currentTok.Type == KingsideCastle {
		expectedCastleTag = KingSideCastle
	} else if currentTok.Type == QueensideCastle {
		expectedCastleTag = QueenSideCastle
	}

	if expectedCastleTag != 0 {
		foundCastle := false
		// Iterate the pre-generated validMoves
		for _, m := range validMoves {
			if m.HasTag(expectedCastleTag) {
				move.s1 = m.S1()
				move.s2 = m.S2()
				move.tags = expectedCastleTag // Start with only the castle tag
				if m.HasTag(Check) {          // Add check tag if the move results in check
					move.AddTag(Check)
				}
				foundCastle = true
				break // Found the unique castling move
			}
		}

		if !foundCastle {
			return nil, &ParserError{
				Message:    fmt.Sprintf("illegal %s castle", currentTok.Value),
				TokenType:  currentTok.Type,
				TokenValue: currentTok.Value,
				Position:   p.position,
			}
		}
		p.advance() // Consume the O-O or O-O-O token

		move.position = p.game.pos.copy() // Alloc: Potentially large, copies position object

		// Check for optional Check/NAG suffix after castling
		p.parseOptionalSuffixes(move)

		return move, nil
	}

	// --- Parse Regular Move (non-castling) ---
	var moveData struct {
		piece      string    // The piece type char (e.g., "N", "B") - empty for pawns
		originFile string    // Disambiguation file ("a".."h")
		originRank string    // Disambiguation rank ("1".."8")
		destSquare string    // Destination square string ("e4")
		isCapture  bool      // Capture indicated by 'x'
		promotion  PieceType // Promotion piece type
	}
	var originRankInt = -1 // Use -1 for not present, avoids strconv in loop

	// Parse Piece or initial file/rank (simplified from original for clarity)
	switch currentTok.Type {
	case PIECE:
		moveData.piece = currentTok.Value
		p.advance()
		currentTok = p.currentToken()
		// Disambiguation checks (can be combined)
		if currentTok.Type == FILE {
			moveData.originFile = currentTok.Value
			p.advance()
			currentTok = p.currentToken()
			// Allow rank after file e.g. R1a2 - less common
			if currentTok.Type == RANK {
				moveData.originRank = currentTok.Value
				p.advance()
				currentTok = p.currentToken()
			}
		} else if currentTok.Type == RANK {
			moveData.originRank = currentTok.Value
			p.advance()
			currentTok = p.currentToken()
		} // Add square disambiguation if needed based on your notation support

	case FILE: // Pawn move starting with file (e.g., "exd5")
		moveData.originFile = currentTok.Value
		p.advance()
		currentTok = p.currentToken()
	} // Case SQUARE or others handled later

	// Handle Optional Capture 'x'
	if currentTok.Type == CAPTURE {
		moveData.isCapture = true
		p.advance()
		currentTok = p.currentToken()
	}

	// Get Destination Square (mandatory)
	if currentTok.Type != SQUARE {
		return nil, &ParserError{ // Alloc: error object
			Message:    "expected destination square",
			TokenType:  currentTok.Type,
			TokenValue: currentTok.Value,
			Position:   p.position,
		}
	}
	moveData.destSquare = currentTok.Value
	targetSquare := parseSquare(moveData.destSquare) // No allocation expected
	if targetSquare == NoSquare {
		return nil, &ParserError{ // Alloc: error object
			Message:    fmt.Sprintf("invalid destination square format: %s", moveData.destSquare), // Alloc: error string
			TokenType:  currentTok.Type,
			TokenValue: currentTok.Value,
			Position:   p.position,
		}
	}
	p.advance()
	currentTok = p.currentToken()

	// Handle Optional Promotion (e.g., "=Q")
	if currentTok.Type == PROMOTION {
		p.advance() // Consume '='
		currentTok = p.currentToken()
		if currentTok.Type != PromotionPiece {
			return nil, &ParserError{ // Alloc: error object
				Message:    "expected promotion piece type after '='",
				TokenType:  currentTok.Type,
				TokenValue: currentTok.Value,
				Position:   p.position,
			}
		}
		moveData.promotion = parsePieceType(currentTok.Value) // No allocation expected
		if moveData.promotion == NoPieceType {
			return nil, &ParserError{ // Alloc: error object
				Message:    fmt.Sprintf("invalid promotion piece type: %s", currentTok.Value), // Alloc: error string
				TokenType:  currentTok.Type,
				TokenValue: currentTok.Value,
				Position:   p.position,
			}
		}
		p.advance()
		// currentTok updated by parseOptionalSuffixes later
	}

	// --- Find the Matching Legal Move ---

	// Pre-parse integer rank for comparison (avoids strconv in the loop)
	if moveData.originRank != "" {
		rankNum, err := strconv.Atoi(moveData.originRank) // Potential minor alloc? Usually optimized.
		if err != nil || rankNum < 1 || rankNum > 8 {
			return nil, &ParserError{ // Alloc: error object
				Message:    fmt.Sprintf("invalid origin rank: %s", moveData.originRank), // Alloc: error string
				TokenType:  RANK,                                                        // Best guess
				TokenValue: moveData.originRank,
				Position:   p.position,
			}
		}
		originRankInt = rankNum - 1 // Convert to 0-7 index
	}

	var matchingMove *Move // Pointer to the move *in the validMoves slice*
	foundMatch := false

	board := p.game.pos.Board() // Get board once
	expectedPieceType := NoPieceType
	if moveData.piece != "" {
		expectedPieceType = PieceTypeFromString(moveData.piece) // No allocation expected
	} else {
		expectedPieceType = Pawn // Default to pawn if no piece specified
	}

	// Iterate the pre-generated validMoves
	for i := range validMoves {
		m := &validMoves[i] // Take address to avoid copying struct in loop

		// Skip castling moves now
		if m.HasTag(KingSideCastle) || m.HasTag(QueenSideCastle) {
			continue
		}

		// Must match destination square
		if m.S2() != targetSquare {
			continue
		}

		piece := board.Piece(m.S1()) // Get the piece being moved

		// Check piece type
		if piece.Type() != expectedPieceType {
			// Avoid allocating error strings in the loop unless it's the final error
			continue
		}

		// Check disambiguation file (string comparison is cheap)
		if moveData.originFile != "" && m.S1().File().String() != moveData.originFile {
			continue
		}

		// Check disambiguation rank (use pre-parsed int - faster)
		if originRankInt != -1 && int(m.S1().Rank()) != originRankInt { // Compare 0-7 ranks
			continue
		}

		// Check capture indication vs actual capture tags
		isActualCapture := m.HasTag(Capture) || m.HasTag(EnPassant)
		if moveData.isCapture != isActualCapture {
			continue
		}

		// Check promotion (must match if specified, must not exist if not specified)
		movePromo := m.promo // Assume m.promo uses 0 or NoPieceType for no promotion
		if moveData.promotion != NoPieceType && movePromo != moveData.promotion {
			continue // Promotion required but doesn't match
		}
		if moveData.promotion == NoPieceType && movePromo != NoPieceType && movePromo != 0 {
			continue // Promotion not specified but occurred
		}

		// If we get here, this move matches all criteria
		if foundMatch {
			// Ambiguous notation! Should not happen with valid SAN.
			// Construct error string only when ambiguity is confirmed.
			return nil, &ParserError{ // Alloc: error object
				Message:    "ambiguous move notation (multiple legal moves match)",
				TokenType:  p.currentToken().Type, // Or a generic type
				TokenValue: "(ambiguous)",         // Provide more context if possible
				Position:   p.position,
			}
		}
		matchingMove = m
		foundMatch = true
		// Optimization: break here assumes SAN guarantees non-ambiguity.
		// If ambiguity is possible, remove break and handle after loop.
		break
	} // End of validMoves loop

	// Check result of the loop
	if !foundMatch {
		// Construct the detailed error message *after* the loop fails
		// This avoids allocating error objects on every mismatch inside the loop.
		// Re-check the conditions to create a specific error message.
		// (This part adds slight overhead but avoids loop allocations for errors)
		var specificErrorMsg string = "no legal move matches the notation" // Default
		for i := range validMoves {
			m := &validMoves[i]
			if m.S2() != targetSquare {
				continue
			}
			piece := board.Piece(m.S1())
			if piece.Type() != expectedPieceType {
				specificErrorMsg = fmt.Sprintf("piece type mismatch (expected %v)", expectedPieceType)
				break
			}
			if moveData.originFile != "" && m.S1().File().String() != moveData.originFile {
				specificErrorMsg = fmt.Sprintf("origin file mismatch (expected %s)", moveData.originFile)
				break
			}
			if originRankInt != -1 && int(m.S1().Rank()) != originRankInt {
				specificErrorMsg = fmt.Sprintf("origin rank mismatch (expected %d)", originRankInt+1)
				break
			}
			isActualCapture := m.HasTag(Capture) || m.HasTag(EnPassant)
			if moveData.isCapture != isActualCapture {
				specificErrorMsg = fmt.Sprintf("capture mismatch (expected %t)", moveData.isCapture)
				break
			}
			movePromo := m.promo
			if moveData.promotion != NoPieceType && movePromo != moveData.promotion {
				specificErrorMsg = fmt.Sprintf("promotion mismatch (expected %v)", moveData.promotion)
				break
			}
			if moveData.promotion == NoPieceType && movePromo != NoPieceType && movePromo != 0 {
				specificErrorMsg = "unexpected promotion"
				break
			}
		}

		return nil, &ParserError{ // Alloc: error object
			Message:    specificErrorMsg,      // Alloc: error string (now allocated only once on failure)
			TokenType:  p.currentToken().Type, // Token where parsing stopped/failed
			TokenValue: p.currentToken().Value,
			Position:   p.position,
		}
	}

	// Copy the matched move details into the result 'move'
	move.s1 = matchingMove.S1()
	move.s2 = matchingMove.S2()
	move.tags = matchingMove.tags // Copy all tags (capture, check, etc.)
	move.promo = matchingMove.promo

	// --- CRITICAL ALLOCATION POINT ---
	// Consider if copying the entire position is truly necessary.
	move.position = p.game.pos.copy() // Alloc: Potentially large, copies position object

	// Parse optional suffixes like check (+) or NAGs ($1)
	p.parseOptionalSuffixes(move) // Call helper

	// Move number setting is handled in addMove or parseMoveText
	// p.setMoveNumber(move) // Removed from here

	return move, nil
}

// Helper function to parse optional suffixes (Check, NAG)
func (p *Parser) parseOptionalSuffixes(move *Move) {
	currentTok := p.currentToken()

	// Handle Check (+) or Checkmate (#)
	if currentTok.Type == CHECK {
		// Optional: Verify check tag matches generated move, or just consume token.
		// move.AddTag(Check) // Usually redundant if generator is correct
		p.advance()
		currentTok = p.currentToken()
	}

	// Handle NAG (Numeric Annotation Glyph, e.g., $1, $2)
	if currentTok.Type == NAG {
		move.nag = currentTok.Value
		p.advance()
	}
}

// Note: setMoveNumber helper removed as logic seems better placed in addMove or parseMoveText
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
