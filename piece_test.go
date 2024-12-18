package chess

import (
	"bytes"
	"testing"
)

func TestPieceString(t *testing.T) {
	tables := []struct {
		piece PieceType
		str   string
	}{
		{King, "k"},
		{Queen, "q"},
		{Rook, "r"},
		{Bishop, "b"},
		{Knight, "n"},
		{Pawn, "p"},
	}

	for _, table := range tables {
		if table.piece.String() != table.str {
			t.Errorf("String version of piece was incorrect.")
		}
	}
}

func TestBytesForPieceTypes(t *testing.T) {
	tests := []struct {
		pieceType PieceType
		expected  []byte
	}{
		{King, []byte{'k'}},
		{Queen, []byte{'q'}},
		{Rook, []byte{'r'}},
		{Bishop, []byte{'b'}},
		{Knight, []byte{'n'}},
		{Pawn, []byte{'p'}},
		{NoPieceType, []byte{}},
	}

	for _, tt := range tests {
		if !bytes.Equal(tt.pieceType.Bytes(), tt.expected) {
			t.Fatalf("expected %v but got %v", tt.expected, tt.pieceType.Bytes())
		}
	}
}
