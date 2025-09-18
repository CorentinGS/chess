package chess

import "strings"

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
	parent   *Move
	position *Position // Position after the move
	nag      string
	comments string
	command  map[string]string // Store commands as key-value pairs
	children []*Move           // Main line and variations
	number   uint
	tags     MoveTag
	s1       Square
	s2       Square
	promo    PieceType
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
}

func (m *Move) SetComment(comment string) {
	m.comments = comment
}

func (m *Move) AddComment(comment string) {
	comments := strings.Builder{}
	comments.WriteString(m.comments)
	comments.WriteString(comment)
	m.comments = comments.String()
}

func (m *Move) Comments() string {
	return m.comments
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
