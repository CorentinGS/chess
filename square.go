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
	switch s {
	case "a1":
		return A1
	case "a2":
		return A2
	case "a3":
		return A3
	case "a4":
		return A4
	case "a5":
		return A5
	case "a6":
		return A6
	case "a7":
		return A7
	case "a8":
		return A8
	case "b1":
		return B1
	case "b2":
		return B2
	case "b3":
		return B3
	case "b4":
		return B4
	case "b5":
		return B5
	case "b6":
		return B6
	case "b7":
		return B7
	case "b8":
		return B8
	case "c1":
		return C1
	case "c2":
		return C2
	case "c3":
		return C3
	case "c4":
		return C4
	case "c5":
		return C5
	case "c6":
		return C6
	case "c7":
		return C7
	case "c8":
		return C8
	case "d1":
		return D1
	case "d2":
		return D2
	case "d3":
		return D3
	case "d4":
		return D4
	case "d5":
		return D5
	case "d6":
		return D6
	case "d7":
		return D7
	case "d8":
		return D8
	case "e1":
		return E1
	case "e2":
		return E2
	case "e3":
		return E3
	case "e4":
		return E4
	case "e5":
		return E5
	case "e6":
		return E6
	case "e7":
		return E7
	case "e8":
		return E8
	case "f1":
		return F1
	case "f2":
		return F2
	case "f3":
		return F3
	case "f4":
		return F4
	case "f5":
		return F5
	case "f6":
		return F6
	case "f7":
		return F7
	case "f8":
		return F8
	case "g1":
		return G1
	case "g2":
		return G2
	case "g3":
		return G3
	case "g4":
		return G4
	case "g5":
		return G5
	case "g6":
		return G6
	case "g7":
		return G7
	case "g8":
		return G8
	case "h1":
		return H1
	case "h2":
		return H2
	case "h3":
		return H3
	case "h4":
		return H4
	case "h5":
		return H5
	case "h6":
		return H6
	case "h7":
		return H7
	case "h8":
		return H8
	default:
		return NoSquare
	}
}
