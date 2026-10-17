package polyglot

import (
	"bytes"
	"encoding/binary"
	"reflect"
	"testing"

	"github.com/corentings/chess/v3"
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
func createTestBook(entries []Entry) []byte {
	var buf bytes.Buffer
	for _, entry := range entries {
		buf.Write(createEntry(entry.Key, entry.Move, entry.Weight, entry.Learn))
	}
	return buf.Bytes()
}

func TestLoadFromBytes(t *testing.T) {
	// Unsorted on purpose: parser must sort by Key.
	testEntries := []Entry{
		{Key: 2, Move: 200, Weight: 20, Learn: 0},
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
	}
	bookData := createTestBook(testEntries)

	book, err := LoadBytes(bookData)
	if err != nil {
		t.Fatalf("LoadBytes() error = %v", err)
	}

	if len(book.entries) != 2 {
		t.Fatalf("LoadBytes() loaded %v entries, want 2", len(book.entries))
	}
	if book.entries[0].Key != 1 || book.entries[1].Key != 2 {
		t.Error("LoadBytes() entries not sorted by Key")
	}
}

func TestLoadFromReader(t *testing.T) {
	testEntries := []Entry{
		{Key: 1, Move: 100, Weight: 10, Learn: 0},
		{Key: 2, Move: 200, Weight: 20, Learn: 0},
	}
	bookData := createTestBook(testEntries)

	book, err := Load(bytes.NewReader(bookData))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}

	if len(book.entries) != 2 {
		t.Fatalf("Load() loaded %v entries, want 2", len(book.entries))
	}
	if book.entries[0].Key != 1 || book.entries[1].Key != 2 {
		t.Error("Load() entries not sorted by Key")
	}
}

func TestFindMoves(t *testing.T) {
	book := &Book{
		entries: []Entry{
			{Key: 1, Move: 100, Weight: 10, Learn: 0},
			{Key: 1, Move: 101, Weight: 20, Learn: 0}, // Same position, different move
			{Key: 2, Move: 200, Weight: 30, Learn: 0},
		},
	}

	tests := []struct {
		name    string
		hash    uint64
		want    []Entry
		wantLen int
	}{
		{
			name: "Multiple moves",
			hash: 1,
			want: []Entry{
				{Key: 1, Move: 101, Weight: 20, Learn: 0}, // Higher weight should be first
				{Key: 1, Move: 100, Weight: 10, Learn: 0},
			},
			wantLen: 2,
		},
		{
			name:    "Single move",
			hash:    2,
			want:    []Entry{{Key: 2, Move: 200, Weight: 30, Learn: 0}},
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
		want Move
	}{
		{
			name: "Regular move",
			move: uint16((1 << 9) | (2 << 6) | (3 << 3) | 4), // from a2 to d3
			want: Move{
				FromFile: 2, FromRank: 1,
				ToFile: 4, ToRank: 3,
				Promotion: 0, CastlingMove: false,
			},
		},
		{
			name: "Castling move white kingside",
			move: uint16((4 << 6) | (0 << 9) | (7 << 0) | (0 << 3)), // e1h1
			want: Move{
				FromFile: 4, FromRank: 0,
				ToFile: 7, ToRank: 0,
				Promotion: 0, CastlingMove: true,
			},
		},
		{
			name: "Promotion move",
			move: uint16(((1 << 9) | (6 << 6) | (1 << 3) | 7) | (4 << 12)), // promotion to queen
			want: Move{
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

func TestRandomMove(t *testing.T) {
	book := &Book{
		entries: []Entry{
			{Key: 1, Move: 100, Weight: 10, Learn: 0},
			{Key: 1, Move: 101, Weight: 20, Learn: 0},
		},
	}

	// Test with existing position
	move, err := book.RandomMove(1)
	if err != nil {
		t.Fatalf("RandomMove() error = %v", err)
	}
	if move == nil {
		t.Error("RandomMove() returned nil for existing position")
	}

	// Test with non-existing position
	move, err = book.RandomMove(999)
	if err != nil {
		t.Fatalf("RandomMove() error = %v", err)
	}
	if move != nil {
		t.Error("RandomMove() returned move for non-existing position")
	}
}

func TestRandomMoveWithZeroTotalWeight(t *testing.T) {
	book := &Book{
		entries: []Entry{
			{Key: 1, Move: 100, Weight: 0, Learn: 0},
			{Key: 1, Move: 101, Weight: 0, Learn: 0},
		},
	}

	move, err := book.RandomMove(1)
	if err != nil {
		t.Fatalf("RandomMove() error = %v", err)
	}
	if move != nil {
		t.Fatalf("RandomMove() = %v, want nil", move)
	}
}

func TestInvalidBookData(t *testing.T) {
	// Test invalid file size
	invalidData := []byte{0x00, 0x01, 0x02} // Not multiple of 16
	_, err := LoadBytes(invalidData)
	if err == nil {
		t.Error("LoadBytes() should fail with invalid file size")
	}

	// Test empty book
	emptyData := []byte{}
	book, err := LoadBytes(emptyData)
	if err != nil {
		t.Error("LoadBytes() should handle empty book")
	}
	if len(book.entries) != 0 {
		t.Error("Empty book should have no entries")
	}
}

func TestChessMoveToPolyglotTests(t *testing.T) {
	tests := []struct {
		name     string
		move     chess.Move
		expected Move
	}{
		{
			name: "Valid move",
			move: chess.NewMove(chess.A1, chess.A2),
			expected: Move{
				FromFile:     0,
				FromRank:     0,
				ToFile:       0,
				ToRank:       1,
				Promotion:    0,
				CastlingMove: false,
			},
		},
		{
			name: "chess.Move with promotion",
			move: chess.NewMove(chess.G7, chess.H8, chess.Queen),
			expected: Move{
				FromFile:  6,
				FromRank:  6,
				ToFile:    7,
				ToRank:    7,
				Promotion: 4,
			},
		},
		{
			name: "Castling move",
			move: chess.NewMove(chess.E1, chess.H1),
			expected: Move{
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
			result := EncodeMove(tt.move)
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
		move     Move
		expected uint16
	}{
		{
			name: "Valid move",
			move: Move{
				FromFile:     0,
				FromRank:     0,
				ToFile:       0,
				ToRank:       1,
				Promotion:    0,
				CastlingMove: false,
			},
			expected: uint16((0 << 9) | (0 << 6) | (1 << 3)),
		},
		{
			name: "chess.Move with promotion",
			move: Move{
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
			move: Move{
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
		polyMove Move
		expected chess.Move
	}{
		{
			name: "Regular move",
			polyMove: Move{
				FromFile: 0, FromRank: 0,
				ToFile: 0, ToRank: 1,
				Promotion: 0, CastlingMove: false,
			},
			expected: chess.NewMove(chess.A1, chess.A2),
		},
		{
			name: "Promotion move",
			polyMove: Move{
				FromFile: 6, FromRank: 6,
				ToFile: 7, ToRank: 7,
				Promotion: 4, CastlingMove: false,
			},
			expected: chess.NewMove(chess.G7, chess.H8, chess.Queen),
		},
		{
			name: "Castling move",
			polyMove: Move{
				FromFile: 4, FromRank: 0,
				ToFile: 7, ToRank: 0,
				Promotion: 0, CastlingMove: true,
			},
			expected: chess.NewMove(chess.E1, chess.G1).WithTag(chess.KingSideCastle),
		},
		{
			name: "Long castling move",
			polyMove: Move{
				FromFile: 4, FromRank: 0,
				ToFile: 0, ToRank: 0,
				Promotion: 0, CastlingMove: true,
			},
			expected: chess.NewMove(chess.E1, chess.C1).WithTag(chess.QueenSideCastle),
		},
		{
			name: "Long castling move black",
			polyMove: Move{
				FromFile: 4, FromRank: 7,
				ToFile: 0, ToRank: 7,
				Promotion: 0, CastlingMove: true,
			},
			expected: chess.NewMove(chess.E8, chess.C8).WithTag(chess.QueenSideCastle),
		},
		{
			name: "castling  move black",
			polyMove: Move{
				FromFile: 4, FromRank: 7,
				ToFile: 7, ToRank: 7,
				Promotion: 0, CastlingMove: true,
			},
			expected: chess.NewMove(chess.E8, chess.G8).WithTag(chess.KingSideCastle),
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
	// Create a map of position hash to slice of WeightedMove.
	// Here we use two different position hashes.
	pos1 := uint64(1)
	pos2 := uint64(2)

	// Create moves using the default chess.Move type.
	// For example, move from chess.A1 to chess.A2.
	move1 := chess.NewMove(chess.A1, chess.A2)
	// Another move, say from chess.B1 to chess.B2.
	move2 := chess.NewMove(chess.B1, chess.B2)
	// A move with promotion, for example from chess.G7 to chess.H8 promoting to chess.Queen.
	move3 := chess.NewMove(chess.G7, chess.H8, chess.Queen)

	mw1 := WeightedMove{Move: move1, Weight: 100}
	mw2 := WeightedMove{Move: move2, Weight: 200}
	mw3 := WeightedMove{Move: move3, Weight: 300}

	m := map[uint64][]WeightedMove{
		pos1: {mw1, mw2},
		pos2: {mw3},
	}

	book := NewBook(m)
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
	book := &Book{entries: []Entry{}}
	pos := uint64(5)
	// Create a move (for example, from chess.D1 to chess.D2).
	move := chess.NewMove(chess.D1, chess.D2)
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
	move2 := chess.NewMove(chess.C1, chess.C2)
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
	move := chess.NewMove(chess.E2, chess.E4)
	book := &Book{entries: []Entry{
		{Key: pos, Move: EncodeMove(move), Weight: 100, Learn: 0},
	}}
	// Update the move's weight.
	err := book.UpdateMove(pos, move, 250)
	if err != nil {
		t.Fatalf("UpdateMove returned error: %v", err)
	}
	found := false
	for _, entry := range book.entries {
		if entry.Key == pos && entry.Move == EncodeMove(move) {
			if entry.Weight != 250 {
				t.Errorf("Expected weight 250, got %d", entry.Weight)
			}
			found = true
		}
	}
	if !found {
		t.Errorf("chess.Move not found after update")
	}

	// Try updating a move that does not exist.
	nonExistentMove := chess.NewMove(chess.A1, chess.A1)
	err = book.UpdateMove(pos, nonExistentMove, 300)
	if err == nil {
		t.Errorf("Expected error for non-existent move update, got nil")
	}
}

func TestDeleteMoves(t *testing.T) {
	pos1 := uint64(100)
	pos2 := uint64(200)
	move1 := chess.NewMove(chess.B2, chess.B3)
	move2 := chess.NewMove(chess.B3, chess.B4)
	move3 := chess.NewMove(chess.C2, chess.C3)
	book := &Book{
		entries: []Entry{
			{Key: pos1, Move: EncodeMove(move1), Weight: 100, Learn: 0},
			{Key: pos1, Move: EncodeMove(move2), Weight: 200, Learn: 0},
			{Key: pos2, Move: EncodeMove(move3), Weight: 300, Learn: 0},
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

func TestMoves(t *testing.T) {
	pos := uint64(50)
	move1 := chess.NewMove(chess.D2, chess.D4)
	move2 := chess.NewMove(chess.G7, chess.H8, chess.Queen) // promotion move
	book := &Book{
		entries: []Entry{
			{Key: pos, Move: EncodeMove(move1), Weight: 120, Learn: 0},
			{Key: pos, Move: EncodeMove(move2), Weight: 220, Learn: 0},
		},
	}
	moves, err := book.Moves(pos)
	if err != nil {
		t.Fatalf("Moves returned error: %v", err)
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
	_, err = book.Moves(999)
	if err == nil {
		t.Errorf("Expected error for non-existent position, got nil")
	}
}

func TestToMoveMap(t *testing.T) {
	pos1 := uint64(10)
	pos2 := uint64(20)
	move1 := chess.NewMove(chess.A2, chess.A3)
	move2 := chess.NewMove(chess.A3, chess.A4)
	move3 := chess.NewMove(chess.B2, chess.B3)
	book := &Book{
		entries: []Entry{
			{Key: pos1, Move: EncodeMove(move1), Weight: 50, Learn: 0},
			{Key: pos1, Move: EncodeMove(move2), Weight: 75, Learn: 0},
			{Key: pos2, Move: EncodeMove(move3), Weight: 100, Learn: 0},
		},
	}

	moveMap := book.WeightedMoves()
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

func BenchmarkToMoveMap(b *testing.B) {
	pos1 := uint64(10)
	pos2 := uint64(20)
	move1 := chess.NewMove(chess.A2, chess.A3)
	move2 := chess.NewMove(chess.A3, chess.A4)
	move3 := chess.NewMove(chess.B2, chess.B3)
	book := &Book{
		entries: []Entry{
			{Key: pos1, Move: EncodeMove(move1), Weight: 50, Learn: 0},
			{Key: pos1, Move: EncodeMove(move2), Weight: 75, Learn: 0},
			{Key: pos2, Move: EncodeMove(move3), Weight: 100, Learn: 0},
		},
	}

	b.ReportAllocs()

	for range b.N {
		_ = book.WeightedMoves()
	}
}

func TestBookKeyedByPositionZobristHash(t *testing.T) {
	pos := chess.StartingPosition()
	if got, want := pos.ZobristHash(), uint64(0x463b96181691fc9c); got != want {
		t.Fatalf("starting position hash = %#x, want Polyglot key %#x", got, want)
	}
	e4 := chess.NewMove(chess.E2, chess.E4)
	book := NewBook(map[uint64][]WeightedMove{pos.ZobristHash(): {{Move: e4, Weight: 1}}})
	moves, err := book.Moves(pos.ZobristHash())
	if err != nil {
		t.Fatal(err)
	}
	if len(moves) != 1 || !pos.IsLegal(moves[0]) || moves[0].S1() != chess.E2 || moves[0].S2() != chess.E4 {
		t.Fatalf("Moves(start) = %v, want [e2e4]", moves)
	}
}
