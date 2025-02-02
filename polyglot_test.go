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
			move: uint16(((1 << 9) | (2 << 6) | (3 << 3) | 4)), // from a2 to d3
			want: PolyglotMove{
				FromFile: 2, FromRank: 1,
				ToFile: 4, ToRank: 3,
				Promotion: 0, CastlingMove: false,
			},
		},
		{
			name: "Castling move white kingside",
			move: uint16(((4 << 6) | (0 << 9) | (7 << 0) | (0 << 3))), // e1h1
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
