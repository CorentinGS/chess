package chess_test

import (
	"bytes"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPieceString(t *testing.T) {
	tables := []struct {
		piece chess.PieceType
		str   string
	}{
		{chess.King, "k"},
		{chess.Queen, "q"},
		{chess.Rook, "r"},
		{chess.Bishop, "b"},
		{chess.Knight, "n"},
		{chess.Pawn, "p"},
	}

	for _, table := range tables {
		if table.piece.String() != table.str {
			t.Errorf("String version of piece was incorrect.")
		}
	}
}

func TestBytesForPieceTypes(t *testing.T) {
	tests := []struct {
		pieceType chess.PieceType
		expected  []byte
	}{
		{chess.King, []byte{'k'}},
		{chess.Queen, []byte{'q'}},
		{chess.Rook, []byte{'r'}},
		{chess.Bishop, []byte{'b'}},
		{chess.Knight, []byte{'n'}},
		{chess.Pawn, []byte{'p'}},
		{chess.NoPieceType, []byte{}},
	}

	for _, tt := range tests {
		if !bytes.Equal(tt.pieceType.Bytes(), tt.expected) {
			t.Fatalf("expected %v but got %v", tt.expected, tt.pieceType.Bytes())
		}
	}
}
