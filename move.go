package chess

import "strings"

// CommentItemKind identifies the kind of item inside a PGN comment block.
type CommentItemKind int

const (
	// CommentText is plain text inside a PGN comment block.
	CommentText CommentItemKind = iota
	// CommentCommand is a command annotation like [%clk 0:05:00].
	CommentCommand
)

// CommentItem is one ordered item inside a PGN comment block.
type CommentItem struct {
	Kind  CommentItemKind
	Text  string
	Key   string
	Value string
}

// CommentBlock represents one PGN {...} block.
type CommentBlock struct {
	Items []CommentItem
}

// A MoveTag represents a notable consequence of a move.
type MoveTag uint16

const (
	// KingSideCastle indicates that the move is a king side castle.
	KingSideCastle MoveTag = 1 << iota
	// QueenSideCastle indicates that the move is a queen side castle.
	QueenSideCastle
	// Capture indicates that the move captures a piece.
	Capture
	// EnPassant indicates that the move captures via en passant.
	EnPassant
	// Check indicates that the move puts the opposing player in check.
	Check
	// inCheck indicates that the move puts the moving player in check and
	// is therefore invalid.
	inCheck
)

// A Move is the movement of a piece from one square to another.
type Move struct {
	parent             *Move
	position           *Position // Position after the move
	nag                string
	comments           string
	command            map[string]string // Store commands as key-value pairs
	commentBlocks      []CommentBlock
	structuredComments bool
	children           []*Move // Main line and variations
	number             uint
	tags               MoveTag
	s1                 Square
	s2                 Square
	promo              PieceType
}

// String returns a string useful for debugging.  String doesn't return
// algebraic notation.
func (m *Move) String() string {
	return m.s1.String() + m.s2.String() + m.promo.String()
}

// S1 returns the origin square of the move.
func (m *Move) S1() Square {
	return m.s1
}

// S2 returns the destination square of the move.
func (m *Move) S2() Square {
	return m.s2
}

// Promo returns promotion piece type of the move.
func (m *Move) Promo() PieceType {
	return m.promo
}

// HasTag returns true if the move contains the MoveTag given.
func (m *Move) HasTag(tag MoveTag) bool {
	return (tag & m.tags) > 0
}

// AddTag adds the given MoveTag to the move's tags using a bitwise OR operation.
// Multiple tags can be combined by calling AddTag multiple times.
func (m *Move) AddTag(tag MoveTag) {
	m.tags |= tag
}

func (m *Move) GetCommand(key string) (string, bool) {
	for i := len(m.commentBlocks) - 1; i >= 0; i-- {
		block := m.commentBlocks[i]
		for j := len(block.Items) - 1; j >= 0; j-- {
			item := block.Items[j]
			if item.Kind == CommentCommand && item.Key == key {
				return item.Value, true
			}
		}
	}

	if m.command == nil {
		m.command = make(map[string]string)
		return "", false
	}
	value, ok := m.command[key]
	return value, ok
}

func (m *Move) SetCommand(key, value string) {
	if m.command == nil {
		m.command = make(map[string]string)
	}
	m.command[key] = value

	if !m.structuredComments {
		m.rebuildLegacyCommentBlock()
		return
	}

	m.ensureCommentBlocksFromLegacy()
	for blockIdx := len(m.commentBlocks) - 1; blockIdx >= 0; blockIdx-- {
		for itemIdx := len(m.commentBlocks[blockIdx].Items) - 1; itemIdx >= 0; itemIdx-- {
			item := &m.commentBlocks[blockIdx].Items[itemIdx]
			if item.Kind == CommentCommand && item.Key == key {
				item.Value = value
				return
			}
		}
	}

	if len(m.commentBlocks) == 0 {
		m.commentBlocks = append(m.commentBlocks, CommentBlock{})
	}
	last := len(m.commentBlocks) - 1
	m.commentBlocks[last].Items = append(m.commentBlocks[last].Items, CommentItem{
		Kind:  CommentCommand,
		Key:   key,
		Value: value,
	})
}

func (m *Move) SetComment(comment string) {
	commands := m.command
	m.comments = comment
	m.commentBlocks = nil
	m.structuredComments = false
	if comment != "" || len(commands) > 0 {
		m.rebuildLegacyCommentBlockWithCommands(commands)
	}
}

func (m *Move) AddComment(comment string) {
	if !m.structuredComments {
		m.comments += comment
		m.rebuildLegacyCommentBlock()
		return
	}

	m.ensureCommentBlocksFromLegacy()
	if len(m.commentBlocks) == 0 {
		m.commentBlocks = []CommentBlock{{Items: []CommentItem{{Kind: CommentText, Text: comment}}}}
		m.syncLegacyAnnotationsFromBlocks()
		return
	}

	lastBlock := len(m.commentBlocks) - 1
	for i := len(m.commentBlocks[lastBlock].Items) - 1; i >= 0; i-- {
		item := &m.commentBlocks[lastBlock].Items[i]
		if item.Kind == CommentText {
			item.Text += comment
			m.syncLegacyAnnotationsFromBlocks()
			return
		}
	}
	m.commentBlocks[lastBlock].Items = append(m.commentBlocks[lastBlock].Items, CommentItem{Kind: CommentText, Text: comment})
	m.syncLegacyAnnotationsFromBlocks()
}

func (m *Move) Comments() string {
	if len(m.commentBlocks) > 0 {
		return flattenCommentText(m.commentBlocks)
	}
	return m.comments
}

// CommentBlocks returns a defensive copy of the move's structured PGN comment blocks.
func (m *Move) CommentBlocks() []CommentBlock {
	m.ensureCommentBlocksFromLegacy()
	blocks := make([]CommentBlock, len(m.commentBlocks))
	for i, block := range m.commentBlocks {
		blocks[i].Items = append([]CommentItem(nil), block.Items...)
	}
	return blocks
}

func (m *Move) NAG() string {
	return m.nag
}

func (m *Move) SetNAG(nag string) {
	m.nag = nag
}

func (m *Move) Parent() *Move {
	return m.parent
}

func (m *Move) Position() *Position {
	return m.position
}

func (m *Move) Children() []*Move {
	return m.children
}

func (m *Move) Number() int {
	ret := int(m.number)
	if ret == 0 { // 0 indicates the 'dummy' rootMove
		ret = 1
	}

	return ret
}

// FullMoveNumber returns the full move number (increments after Black's move).
func (m *Move) FullMoveNumber() int {
	return m.Number()
}

// Ply returns the half-move number (increments every move).
func (m *Move) Ply() int {
	if m == nil {
		return 0
	}
	if m.position == nil {
		return 0
	}
	moveNumber := int(m.number)
	// we reverse the color because the position is after the move has been played
	if m.position.turn == Black {
		// After the move, it's White's turn, so the move was by Black
		return (moveNumber-1)*2 + 1
	}
	// After the move, it's Black's turn, so the move was by White
	return (moveNumber)*2 + 0
}

// Clone returns a deep copy of a move.
//
// Per-field exceptions:
//
//	parent: not copied; the clone'd move has no parent
//	children: not copied; the clone'd move has no children
func (m *Move) Clone() *Move {
	ret := &Move{}
	ret.parent = nil
	ret.position = m.position.copy()
	ret.nag = m.nag
	ret.comments = m.comments
	ret.commentBlocks = copyCommentBlocks(m.commentBlocks)
	ret.structuredComments = m.structuredComments
	ret.children = make([]*Move, 0)
	ret.number = m.number
	ret.tags = m.tags
	ret.s1 = m.s1
	ret.s2 = m.s2
	ret.promo = m.promo

	ret.command = make(map[string]string)
	for k, v := range m.command {
		ret.command[k] = v
	}

	return ret
}

func (m *Move) addCommentBlock(block CommentBlock) {
	if len(block.Items) == 0 {
		return
	}
	m.commentBlocks = append(m.commentBlocks, block)
	m.structuredComments = true
	m.syncLegacyAnnotationsFromBlocks()
}

func (m *Move) hasAnnotations() bool {
	return len(m.commentBlocks) > 0 || m.comments != "" || len(m.command) > 0
}

func (m *Move) syncLegacyAnnotationsFromBlocks() {
	m.comments = flattenCommentText(m.commentBlocks)
	m.command = make(map[string]string)
	for _, block := range m.commentBlocks {
		for _, item := range block.Items {
			if item.Kind == CommentCommand {
				m.command[item.Key] = item.Value
			}
		}
	}
	if len(m.command) == 0 {
		m.command = nil
	}
}

func (m *Move) ensureCommentBlocksFromLegacy() {
	if len(m.commentBlocks) > 0 || (m.comments == "" && len(m.command) == 0) {
		return
	}

	block := CommentBlock{}
	if m.comments != "" {
		block.Items = append(block.Items, CommentItem{Kind: CommentText, Text: m.comments})
	}
	for _, key := range sortedCommandKeys(m.command) {
		block.Items = append(block.Items, CommentItem{Kind: CommentCommand, Key: key, Value: m.command[key]})
	}
	m.commentBlocks = []CommentBlock{block}
}

func (m *Move) rebuildLegacyCommentBlock() {
	m.rebuildLegacyCommentBlockWithCommands(m.command)
}

func (m *Move) rebuildLegacyCommentBlockWithCommands(commands map[string]string) {
	block := CommentBlock{}
	if m.comments != "" {
		block.Items = append(block.Items, CommentItem{Kind: CommentText, Text: m.comments})
	}
	for _, key := range sortedCommandKeys(commands) {
		block.Items = append(block.Items, CommentItem{Kind: CommentCommand, Key: key, Value: commands[key]})
	}
	if len(block.Items) == 0 {
		m.commentBlocks = nil
		return
	}
	m.commentBlocks = []CommentBlock{block}
}

func flattenCommentText(blocks []CommentBlock) string {
	var comments []string
	for _, block := range blocks {
		var blockText strings.Builder
		for _, item := range block.Items {
			if item.Kind == CommentText {
				blockText.WriteString(item.Text)
			}
		}
		if blockText.Len() > 0 {
			comments = append(comments, blockText.String())
		}
	}
	return strings.Join(comments, " ")
}

func copyCommentBlocks(src []CommentBlock) []CommentBlock {
	blocks := make([]CommentBlock, len(src))
	for i, block := range src {
		blocks[i].Items = append([]CommentItem(nil), block.Items...)
	}
	return blocks
}

func (m *Move) cloneChildren(srcChildren []*Move) {
	if len(srcChildren) == 0 {
		return
	}

	for _, srcMv := range srcChildren {
		dstMv := srcMv.Clone()
		dstMv.parent = m
		dstMv.cloneChildren(srcMv.children)
		m.children = append(m.children, dstMv)
	}
}
