package chess

import (
	"bytes"
	"encoding/binary"
	"errors"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

// Helper function to create a test book entry
func createEntry(key uint64, move, weight uint16, learn uint32) []byte {
	buf := make([]byte, 16)
	binary.BigEndian.PutUint64(buf[0:8], key)
	binary.BigEndian.PutUint16(buf[8:10], move)
	binary.BigEndian.PutUint16(buf[10:12], weight)
	binary.BigEndian.PutUint32(buf[12:16], learn)
	return buf
}

// Helper function to create test book data
func createTestBook(entries []PolyglotEntry) []byte {
	var buf bytes.Buffer
	for _, entry := range entries {
		buf.Write(createEntry(entry.Key, entry.Move, entry.Weight, entry.Learn))
	}
	return buf.Bytes()
}

func TestBytesBookSource(t *testing.T) {
	testEntries := []PolyglotEntry{
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
		{Key: 2, Move: 200, Weight: 20, Learn: 0},
	}
	bookData := createTestBook(testEntries)

	source := NewBytesBookSource(bookData)

	// Test Size
	size, err := source.Size()
	if err != nil {
		t.Errorf("Size() error = %v", err)
	}
	if size != 32 { // 2 entries * 16 bytes
		t.Errorf("Size() = %v, want %v", size, 32)
	}

	// Test Read
	buf := make([]byte, 16)
	n, err := source.Read(buf)
	if err != nil {
		t.Errorf("Read() error = %v", err)
	}
	if n != 16 {
		t.Errorf("Read() = %v bytes, want %v", n, 16)
	}

	// Test EOF
	source.index = 32
	n, err = source.Read(buf)
	if !errors.Is(err, io.EOF) {
		t.Errorf("Read() error = %v, want EOF", err)
	}
}

func TestReaderBookSource(t *testing.T) {
	testEntries := []PolyglotEntry{
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
		{Key: 2, Move: 200, Weight: 20, Learn: 0},
	}
	bookData := createTestBook(testEntries)
	reader := bytes.NewReader(bookData)

	source, err := NewReaderBookSource(reader)
	if err != nil {
		t.Fatalf("NewReaderBookSource() error = %v", err)
	}

	size, err := source.Size()
	if err != nil || size != 32 {
		t.Errorf("Size() = %v, %v, want 32, nil", size, err)
	}

	buf := make([]byte, 16)
	n, err := source.Read(buf)
	if err != nil || n != 16 {
		t.Errorf("Read() = %v, %v, want 16, nil", n, err)
	}
}

func TestFileBookSource(t *testing.T) {
	// Create temporary test file
	testEntries := []PolyglotEntry{
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
		{Key: 2, Move: 200, Weight: 20, Learn: 0},
	}
	bookData := createTestBook(testEntries)

	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "test.bin")
	if err := os.WriteFile(tmpFile, bookData, 0666); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	osFile, _ := os.Open(tmpFile)

	source, _ := NewReaderBookSource(osFile)

	size, err := source.Size()
	if err != nil || size != 32 {
		t.Errorf("Size() = %v, %v, want 32, nil", size, err)
	}

	buf := make([]byte, 16)
	n, err := source.Read(buf)
	if err != nil || n != 16 {
		t.Errorf("Read() = %v, %v, want 16, nil", n, err)
	}
}

func TestLoadFromSource(t *testing.T) {
	testEntries := []PolyglotEntry{
		{Key: 2, Move: 200, Weight: 20, Learn: 0}, // Intentionally unsorted
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
	}
	bookData := createTestBook(testEntries)
	source := NewBytesBookSource(bookData)

	book, err := LoadFromSource(source)
	if err != nil {
		t.Fatalf("LoadFromSource() error = %v", err)
	}

	if len(book.entries) != 2 {
		t.Errorf("LoadFromSource() loaded %v entries, want 2", len(book.entries))
	}

	// Verify entries are sorted
	if book.entries[0].Key != 1 || book.entries[1].Key != 2 {
		t.Error("LoadFromSource() entries not properly sorted")
	}
}

func TestFindMoves(t *testing.T) {
	book := &PolyglotBook{
		entries: []PolyglotEntry{
			{Key: 1, Move: 100, Weight: 10, Learn: 0},
			{Key: 1, Move: 101, Weight: 20, Learn: 0}, // Same position, different move
			{Key: 2, Move: 200, Weight: 30, Learn: 0},
		},
	}

	tests := []struct {
		name    string
		hash    uint64
		want    []PolyglotEntry
		wantLen int
	}{
		{
			name: "Multiple moves",
			hash: 1,
			want: []PolyglotEntry{
				{Key: 1, Move: 101, Weight: 20, Learn: 0}, // Higher weight should be first
				{Key: 1, Move: 100, Weight: 10, Learn: 0},
			},
			wantLen: 2,
		},
		{
			name:    "Single move",
			hash:    2,
			want:    []PolyglotEntry{{Key: 2, Move: 200, Weight: 30, Learn: 0}},
			wantLen: 1,
		},
		{
			name:    "No moves",
			hash:    3,
			want:    nil,
			wantLen: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := book.FindMoves(tt.hash)
			if len(got) != tt.wantLen {
				t.Errorf("FindMoves() returned %v moves, want %v", len(got), tt.wantLen)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("FindMoves() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDecodeMove(t *testing.T) {
	tests := []struct {
		name string
		move uint16
		want PolyglotMove
	}{
		{
			name: "Regular move",
			move: uint16((1 << 9) | (2 << 6) | (3 << 3) | 4), // from a2 to d3
			want: PolyglotMove{
				FromFile: 2, FromRank: 1,
				ToFile: 4, ToRank: 3,
				Promotion: 0, CastlingMove: false,
			},
		},
		{
			name: "Castling move white kingside",
			move: uint16((4 << 6) | (0 << 9) | (7 << 0) | (0 << 3)), // e1h1
			want: PolyglotMove{
				FromFile: 4, FromRank: 0,
				ToFile: 7, ToRank: 0,
				Promotion: 0, CastlingMove: true,
			},
		},
		{
			name: "Promotion move",
			move: uint16(((1 << 9) | (6 << 6) | (1 << 3) | 7) | (4 << 12)), // promotion to queen
			want: PolyglotMove{
				FromFile: 6, FromRank: 1,
				ToFile: 7, ToRank: 1,
				Promotion: 4, CastlingMove: false,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := DecodeMove(tt.move)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("DecodeMove() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetRandomMove(t *testing.T) {
	book := &PolyglotBook{
		entries: []PolyglotEntry{
			{Key: 1, Move: 100, Weight: 10, Learn: 0},
			{Key: 1, Move: 101, Weight: 20, Learn: 0},
		},
	}

	// Test with existing position
	move := book.GetRandomMove(1)
	if move == nil {
		t.Error("GetRandomMove() returned nil for existing position")
	}

	// Test with non-existing position
	move = book.GetRandomMove(999)
	if move != nil {
		t.Error("GetRandomMove() returned move for non-existing position")
	}
}

func TestInvalidBookData(t *testing.T) {
	// Test invalid file size
	invalidData := []byte{0x00, 0x01, 0x02} // Not multiple of 16
	_, err := LoadFromBytes(invalidData)
	if err == nil {
		t.Error("LoadFromBytes() should fail with invalid file size")
	}

	// Test empty book
	emptyData := []byte{}
	book, err := LoadFromBytes(emptyData)
	if err != nil {
		t.Error("LoadFromBytes() should handle empty book")
	}
	if len(book.entries) != 0 {
		t.Error("Empty book should have no entries")
	}
}

func TestChessMoveToPolyglotTests(t *testing.T) {
	tests := []struct {
		name     string
		move     Move
		expected PolyglotMove
	}{
		{
			name: "Valid move",
			move: Move{
				s1:    A1,
				s2:    A2,
				promo: NoPieceType,
			},
			expected: PolyglotMove{
				FromFile:     0,
				FromRank:     0,
				ToFile:       0,
				ToRank:       1,
				Promotion:    0,
				CastlingMove: false,
			},
		},
		{
			name: "Move with promotion",
			move: Move{
				s1:    G7,
				s2:    H8,
				promo: Queen,
			},
			expected: PolyglotMove{
				FromFile:  6,
				FromRank:  6,
				ToFile:    7,
				ToRank:    7,
				Promotion: 4,
			},
		},
		{
			name: "Castling move",
			move: Move{
				s1:    E1,
				s2:    H1,
				promo: NoPieceType,
			},
			expected: PolyglotMove{
				FromFile:     4,
				FromRank:     0,
				ToFile:       7,
				ToRank:       0,
				Promotion:    0,
				CastlingMove: true,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := MoveToPolyglot(tt.move)
			decoded := DecodeMove(result)
			if decoded != tt.expected {
				t.Fatalf("expected %v but got %v", tt.expected, decoded)
			}
		})
	}
}

func TestPolyglotMoveEncode(t *testing.T) {
	tests := []struct {
		name     string
		move     PolyglotMove
		expected uint16
	}{
		{
			name: "Valid move",
			move: PolyglotMove{
				FromFile:     0,
				FromRank:     0,
				ToFile:       0,
				ToRank:       1,
				Promotion:    0,
				CastlingMove: false,
			},
			expected: uint16((0 << 9) | (0 << 6) | (1 << 3) | 0),
		},
		{
			name: "Move with promotion",
			move: PolyglotMove{
				FromFile:  6,
				FromRank:  6,
				ToFile:    7,
				ToRank:    7,
				Promotion: 4,
			},
			expected: uint16(((6 << 9) | (6 << 6) | (7 << 3) | 7) | (4 << 12)),
		},
		{
			name: "Castling move",
			move: PolyglotMove{
				FromFile:     4,
				FromRank:     0,
				ToFile:       7,
				ToRank:       0,
				Promotion:    0,
				CastlingMove: true,
			},
			expected: uint16((4 << 6) | (0 << 9) | (7 << 0) | (0 << 3)),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.move.Encode()
			if result != tt.expected {
				t.Fatalf("expected %v but got %v", tt.expected, result)
			}
		})
	}
}

func TestPolyglotMoveToMoveConversion(t *testing.T) {
	tests := []struct {
		name     string
		polyMove PolyglotMove
		expected Move
	}{
		{
			name: "Regular move",
			polyMove: PolyglotMove{
				FromFile: 0, FromRank: 0,
				ToFile: 0, ToRank: 1,
				Promotion: 0, CastlingMove: false,
			},
			expected: Move{
				s1:    A1,
				s2:    A2,
				promo: NoPieceType,
			},
		},
		{
			name: "Promotion move",
			polyMove: PolyglotMove{
				FromFile: 6, FromRank: 6,
				ToFile: 7, ToRank: 7,
				Promotion: 4, CastlingMove: false,
			},
			expected: Move{
				s1:    G7,
				s2:    H8,
				promo: Queen,
			},
		},
		{
			name: "Castling move",
			polyMove: PolyglotMove{
				FromFile: 4, FromRank: 0,
				ToFile: 7, ToRank: 0,
				Promotion: 0, CastlingMove: true,
			},
			expected: Move{
				s1:    E1,
				s2:    H1,
				promo: NoPieceType,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.polyMove.ToMove()
			if !reflect.DeepEqual(result, tt.expected) {
				t.Fatalf("expected %v but got %v", tt.expected, result)
			}
		})
	}
}

func TestNewPolyglotBookFromMap(t *testing.T) {
	// Create a map of position hash to slice of MoveWithWeight.
	// Here we use two different position hashes.
	pos1 := uint64(1)
	pos2 := uint64(2)

	// Create moves using the default Move type.
	// For example, move from A1 to A2.
	move1 := Move{
		s1:    A1,
		s2:    A2,
		promo: NoPieceType,
	}
	// Another move, say from B1 to B2.
	move2 := Move{
		s1:    B1,
		s2:    B2,
		promo: NoPieceType,
	}
	// A move with promotion, for example from G7 to H8 promoting to Queen.
	move3 := Move{
		s1:    G7,
		s2:    H8,
		promo: Queen,
	}

	mw1 := MoveWithWeight{Move: move1, Weight: 100}
	mw2 := MoveWithWeight{Move: move2, Weight: 200}
	mw3 := MoveWithWeight{Move: move3, Weight: 300}

	m := map[uint64][]MoveWithWeight{
		pos1: {mw1, mw2},
		pos2: {mw3},
	}

	book := NewPolyglotBookFromMap(m)
	if len(book.entries) != 3 {
		t.Fatalf("Expected 3 entries, got %d", len(book.entries))
	}
	// Check that the entries are sorted by key (all entries for pos1 come before pos2).
	if book.entries[0].Key != pos1 || book.entries[1].Key != pos1 || book.entries[2].Key != pos2 {
		t.Errorf("Entries not sorted by key as expected: got keys [%d, %d, %d]",
			book.entries[0].Key, book.entries[1].Key, book.entries[2].Key)
	}
}

func TestAddMove(t *testing.T) {
	book := &PolyglotBook{entries: []PolyglotEntry{}}
	pos := uint64(5)
	// Create a move (for example, from D1 to D2).
	move := Move{
		s1:    D1,
		s2:    D2,
		promo: NoPieceType,
	}
	book.AddMove(pos, move, 150)
	if len(book.entries) != 1 {
		t.Fatalf("Expected 1 entry, got %d", len(book.entries))
	}
	entry := book.entries[0]
	if entry.Key != pos {
		t.Errorf("Expected position key %d, got %d", pos, entry.Key)
	}
	if entry.Weight != 150 {
		t.Errorf("Expected weight 150, got %d", entry.Weight)
	}

	// Add another move with a lower position hash to test sorting.
	pos2 := uint64(3)
	move2 := Move{
		s1:    C1,
		s2:    C2,
		promo: NoPieceType,
	}
	book.AddMove(pos2, move2, 100)
	if len(book.entries) != 2 {
		t.Fatalf("Expected 2 entries, got %d", len(book.entries))
	}
	// After sorting, the first entry should have key pos2.
	if book.entries[0].Key != pos2 {
		t.Errorf("Expected first entry key %d, got %d", pos2, book.entries[0].Key)
	}
}

func TestUpdateMove(t *testing.T) {
	pos := uint64(10)
	move := Move{
		s1:    E2,
		s2:    E4,
		promo: NoPieceType,
	}
	book := &PolyglotBook{entries: []PolyglotEntry{
		{Key: pos, Move: MoveToPolyglot(move), Weight: 100, Learn: 0},
	}}
	// Update the move's weight.
	err := book.UpdateMove(pos, move, 250)
	if err != nil {
		t.Fatalf("UpdateMove returned error: %v", err)
	}
	found := false
	for _, entry := range book.entries {
		if entry.Key == pos && entry.Move == MoveToPolyglot(move) {
			if entry.Weight != 250 {
				t.Errorf("Expected weight 250, got %d", entry.Weight)
			}
			found = true
		}
	}
	if !found {
		t.Errorf("Move not found after update")
	}

	// Try updating a move that does not exist.
	nonExistentMove := Move{
		s1:    A1,
		s2:    A1,
		promo: NoPieceType,
	}
	err = book.UpdateMove(pos, nonExistentMove, 300)
	if err == nil {
		t.Errorf("Expected error for non-existent move update, got nil")
	}
}

func TestDeleteMoves(t *testing.T) {
	pos1 := uint64(100)
	pos2 := uint64(200)
	move1 := Move{
		s1:    B2,
		s2:    B3,
		promo: NoPieceType,
	}
	move2 := Move{
		s1:    B3,
		s2:    B4,
		promo: NoPieceType,
	}
	move3 := Move{
		s1:    C2,
		s2:    C3,
		promo: NoPieceType,
	}
	book := &PolyglotBook{
		entries: []PolyglotEntry{
			{Key: pos1, Move: MoveToPolyglot(move1), Weight: 100, Learn: 0},
			{Key: pos1, Move: MoveToPolyglot(move2), Weight: 200, Learn: 0},
			{Key: pos2, Move: MoveToPolyglot(move3), Weight: 300, Learn: 0},
		},
	}
	book.DeleteMoves(pos1)
	if len(book.entries) != 1 {
		t.Fatalf("Expected 1 entry after deletion, got %d", len(book.entries))
	}
	if book.entries[0].Key != pos2 {
		t.Errorf("Expected remaining entry key %d, got %d", pos2, book.entries[0].Key)
	}
}

func TestGetChessMoves(t *testing.T) {
	pos := uint64(50)
	move1 := Move{
		s1:    D2,
		s2:    D4,
		promo: NoPieceType,
	}
	move2 := Move{
		s1:    G7,
		s2:    H8,
		promo: Queen, // promotion move
	}
	book := &PolyglotBook{
		entries: []PolyglotEntry{
			{Key: pos, Move: MoveToPolyglot(move1), Weight: 120, Learn: 0},
			{Key: pos, Move: MoveToPolyglot(move2), Weight: 220, Learn: 0},
		},
	}
	moves, err := book.GetChessMoves(pos)
	if err != nil {
		t.Fatalf("GetChessMoves returned error: %v", err)
	}
	if len(moves) != 2 {
		t.Fatalf("Expected 2 moves, got %d", len(moves))
	}
	// Since the conversion functions (ChessMoveToPolyglot, DecodeMove, PolyglotToChessMove)
	// should produce a round-trip, the moves should equal the originals.
	if !reflect.DeepEqual(moves[1], move1) {
		t.Errorf("First move mismatch: expected %+v, got %+v", move1, moves[0])
	}
	if !reflect.DeepEqual(moves[0], move2) {
		t.Errorf("Second move mismatch: expected %+v, got %+v", move2, moves[1])
	}

	// Test error case: request moves for a non-existent position.
	_, err = book.GetChessMoves(999)
	if err == nil {
		t.Errorf("Expected error for non-existent position, got nil")
	}
}

func TestToMoveMap(t *testing.T) {
	pos1 := uint64(10)
	pos2 := uint64(20)
	move1 := Move{
		s1:    A2,
		s2:    A3,
		promo: NoPieceType,
	}
	move2 := Move{
		s1:    A3,
		s2:    A4,
		promo: NoPieceType,
	}
	move3 := Move{
		s1:    B2,
		s2:    B3,
		promo: NoPieceType,
	}
	book := &PolyglotBook{
		entries: []PolyglotEntry{
			{Key: pos1, Move: MoveToPolyglot(move1), Weight: 50, Learn: 0},
			{Key: pos1, Move: MoveToPolyglot(move2), Weight: 75, Learn: 0},
			{Key: pos2, Move: MoveToPolyglot(move3), Weight: 100, Learn: 0},
		},
	}

	moveMap := book.ToMoveMap()
	if len(moveMap) != 2 {
		t.Fatalf("Expected move map with 2 keys, got %d", len(moveMap))
	}
	movesPos1, ok := moveMap[pos1]
	if !ok {
		t.Fatalf("Position %d not found in move map", pos1)
	}
	if len(movesPos1) != 2 {
		t.Errorf("Expected 2 moves for position %d, got %d", pos1, len(movesPos1))
	}
	movesPos2, ok := moveMap[pos2]
	if !ok {
		t.Fatalf("Position %d not found in move map", pos2)
	}
	if len(movesPos2) != 1 {
		t.Errorf("Expected 1 move for position %d, got %d", pos2, len(movesPos2))
	}
}
