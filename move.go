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
	return int(m.number)
}
