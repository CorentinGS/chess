package chess_test

import (
	"testing"

	"github.com/corentings/chess/v3"
)

type newSquareTest struct {
	f  chess.File
	r  chess.Rank
	sq chess.Square
}

func TestNewSquare(t *testing.T) {
	testCases := []newSquareTest{
		{chess.FileA, chess.Rank1, chess.A1},
		{chess.FileA, chess.Rank8, chess.A8},
		{chess.FileH, chess.Rank1, chess.H1},
		{chess.FileH, chess.Rank8, chess.H8},
		{chess.FileB, chess.Rank4, chess.B4},
		{chess.FileE, chess.Rank8, chess.E8},
		{chess.FileH, chess.Rank3, chess.H3},
		{chess.FileD, chess.Rank7, chess.D7},
	}

	for _, testCase := range testCases {
		square := chess.NewSquare(testCase.f, testCase.r)
		if square != testCase.sq {
			t.Fatalf("expected %s, got %s", testCase.sq.String(), square.String())
		}
	}
}
