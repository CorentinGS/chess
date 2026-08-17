package chess_test

import (
	"context"
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNRecordsStreamsRecordsWithIndexAndOffset(t *testing.T) {
	pgn := "\n\n" + decoderGameOne + "\n" + decoderGameTwo
	var records []chess.PGNRecord

	for record, err := range chess.PGNRecords(context.Background(), strings.NewReader(pgn)) {
		if err != nil {
			t.Fatalf("PGNRecords error: %v", err)
		}
		records = append(records, record)
	}

	if len(records) != 2 {
		t.Fatalf("len(records) = %d, want 2", len(records))
	}
	if records[0].Index != 1 || records[1].Index != 2 {
		t.Fatalf("record indexes = %d,%d want 1,2", records[0].Index, records[1].Index)
	}
	if records[0].Offset != 2 {
		t.Fatalf("first offset = %d, want 2", records[0].Offset)
	}
	if !strings.Contains(records[0].Raw, `[Event "one"]`) {
		t.Fatalf("first record raw does not contain first game: %q", records[0].Raw)
	}
}

func TestPGNRecordDecodeReturnsGame(t *testing.T) {
	var record chess.PGNRecord
	for r, err := range chess.PGNRecords(context.Background(), strings.NewReader(decoderGameOne)) {
		if err != nil {
			t.Fatalf("PGNRecords error: %v", err)
		}
		record = r
	}

	game, err := record.Decode()
	if err != nil {
		t.Fatalf("Decode error: %v", err)
	}
	if got := game.GetTagPair("Event"); got != "one" {
		t.Fatalf("Event tag = %q, want one", got)
	}
}

func TestPGNRecordsHandlesLargeRecord(t *testing.T) {
	largeComment := strings.Repeat("a", 128*1024)
	pgn := strings.Replace(decoderGameOne, "1. e4 *", "1. e4 {"+largeComment+"} *", 1)

	count := 0
	for record, err := range chess.PGNRecords(context.Background(), strings.NewReader(pgn)) {
		if err != nil {
			t.Fatalf("PGNRecords error: %v", err)
		}
		count++
		if len(record.Raw) < 128*1024 {
			t.Fatalf("record Raw len = %d, want at least %d", len(record.Raw), 128*1024)
		}
	}
	if count != 1 {
		t.Fatalf("record count = %d, want 1", count)
	}
}

func TestPGNRecordsHonorsCanceledContext(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	for _, err := range chess.PGNRecords(ctx, strings.NewReader(decoderGameOne)) {
		if !errors.Is(err, context.Canceled) {
			t.Fatalf("PGNRecords error = %v, want context.Canceled", err)
		}
		return
	}
	t.Fatal("PGNRecords did not yield context cancellation")
}

func TestPGNDecoderUsesRecordOffsets(t *testing.T) {
	dec := chess.NewPGNDecoder(strings.NewReader("\n\n" + decoderGameOne))
	if _, err := dec.Decode(); err != nil {
		t.Fatalf("Decode error: %v", err)
	}
	if dec.Index() != 1 {
		t.Fatalf("Index = %d, want 1", dec.Index())
	}
	if dec.Offset() != 2 {
		t.Fatalf("Offset = %d, want 2", dec.Offset())
	}
	if _, err := dec.Decode(); !errors.Is(err, io.EOF) {
		t.Fatalf("final Decode error = %v, want io.EOF", err)
	}
}
