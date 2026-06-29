package chess

const (
	numOfSquaresInBoard = 64
	numOfSquaresInRow   = 8
)

// A Square is one of the 64 rank and file combinations that make up a chess board.
type Square int8

// File returns the square's file.
func (sq Square) File() File {
	return File(sq % numOfSquaresInRow)
}

// Rank returns the square's rank.
func (sq Square) Rank() Rank {
	return Rank(sq / numOfSquaresInRow)
}

func (sq Square) String() string {
	return sq.File().String() + sq.Rank().String()
}

func (sq Square) Bytes() [2]byte {
	return [2]byte{sq.File().Byte(), sq.Rank().Byte()}
}

// NewSquare creates a new Square from a File and a Rank.
func NewSquare(f File, r Rank) Square {
	return Square(int8(r)*numOfSquaresInRow + int8(f))
}

func (sq Square) color() Color {
	if ((sq / 8) % 2) == (sq % 2) { //nolint:mnd // this is a formula to determine the color of a square
		return Black
	}
	return White
}

const (
	NoSquare Square = iota - 1
	A1
	B1
	C1
	D1
	E1
	F1
	G1
	H1
	A2
	B2
	C2
	D2
	E2
	F2
	G2
	H2
	A3
	B3
	C3
	D3
	E3
	F3
	G3
	H3
	A4
	B4
	C4
	D4
	E4
	F4
	G4
	H4
	A5
	B5
	C5
	D5
	E5
	F5
	G5
	H5
	A6
	B6
	C6
	D6
	E6
	F6
	G6
	H6
	A7
	B7
	C7
	D7
	E7
	F7
	G7
	H7
	A8
	B8
	C8
	D8
	E8
	F8
	G8
	H8
)

const (
	fileChars = "abcdefgh"
	rankChars = "12345678"
)

// A Rank is the rank of a square.
type Rank int8

const (
	Rank1 Rank = iota
	Rank2
	Rank3
	Rank4
	Rank5
	Rank6
	Rank7
	Rank8
)

func (r Rank) String() string {
	return rankChars[r : r+1] // r+1 is exclusive
}

func (r Rank) Byte() byte {
	return rankChars[r]
}

// A File is the file of a square.
type File int8

const (
	FileA File = iota
	FileB
	FileC
	FileD
	FileE
	FileF
	FileG
	FileH
)

func (f File) String() string {
	return fileChars[f : f+1]
}

func (f File) Byte() byte {
	return fileChars[f]
}

// SquareFromString converts a 2-character square notation (e.g., "e4") to a Square.
// Returns NoSquare if the string is not a valid square.
func SquareFromString(s string) Square {
	if len(s) != 2 {
		return NoSquare
	}
	f := int(s[0] - 'a')
	r := int(s[1] - '1')
	if f < 0 || f > 7 || r < 0 || r > 7 {
		return NoSquare
	}
	return NewSquare(File(f), Rank(r))
}
