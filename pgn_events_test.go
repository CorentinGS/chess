package chess_test

import (
	"context"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNEventsStreamsTagsMovesCommentsAndGameEnd(t *testing.T) {
	pgn := `[Event "events"]
[Site "somewhere"]
[Result "*"]

1. e4 {hello [%clk 0:01:00]} $1 (1. d4) *
`

	var kinds []chess.PGNEventKind
	var tagEvent chess.PGNEvent
	var commentEvent chess.PGNEvent
	var nagEvent chess.PGNEvent
	var moveValues []string

	for event, err := range chess.PGNEvents(strings.NewReader(pgn)) {
		if err != nil {
			t.Fatalf("PGNEvents error: %v", err)
		}
		kinds = append(kinds, event.Kind)
		switch event.Kind {
		case chess.PGNTag:
			if event.Name == "Event" {
				tagEvent = event
			}
		case chess.PGNMove:
			moveValues = append(moveValues, event.Move)
		case chess.PGNComment:
			commentEvent = event
		case chess.PGNNAG:
			nagEvent = event
		}
	}

	if tagEvent.Value != "events" {
		t.Fatalf("Event tag value = %q, want events", tagEvent.Value)
	}
	if commentEvent.Value != "hello " {
		t.Fatalf("comment value = %q, want hello with trailing space", commentEvent.Value)
	}
	if nagEvent.NAG != "$1" {
		t.Fatalf("NAG = %q, want $1", nagEvent.NAG)
	}
	if strings.Join(moveValues, ",") != "e4,d4" {
		t.Fatalf("moves = %v, want [e4 d4]", moveValues)
	}
	if len(kinds) == 0 || kinds[len(kinds)-1] != chess.PGNGameEnd {
		t.Fatalf("last event kind = %v, want PGNGameEnd", kinds)
	}
}

func TestPGNEventsStreamsMultipleGamesWithIndexes(t *testing.T) {
	var endIndexes []int64
	for event, err := range chess.PGNEvents(strings.NewReader(decoderGameOne + "\n" + decoderGameTwo)) {
		if err != nil {
			t.Fatalf("PGNEvents error: %v", err)
		}
		if event.Kind == chess.PGNGameEnd {
			endIndexes = append(endIndexes, event.Index)
		}
	}
	if len(endIndexes) != 2 || endIndexes[0] != 1 || endIndexes[1] != 2 {
		t.Fatalf("game end indexes = %v, want [1 2]", endIndexes)
	}
}

func TestPGNRecordTagsExtractsTagsWithoutDecodingGame(t *testing.T) {
	var record chess.PGNRecord
	for r, err := range chess.PGNRecords(context.Background(), strings.NewReader(decoderGameOne)) {
		if err != nil {
			t.Fatalf("PGNRecords error: %v", err)
		}
		record = r
	}

	tags, err := record.Tags()
	if err != nil {
		t.Fatalf("Tags error: %v", err)
	}
	if tags["Event"] != "one" {
		t.Fatalf("Event tag = %q, want one", tags["Event"])
	}
	if _, ok := tags["White"]; !ok {
		t.Fatal("White tag missing")
	}
}

func TestPGNRecordTagsOnTruncatedTag(t *testing.T) {
	var record chess.PGNRecord
	for r, err := range chess.PGNRecords(context.Background(), strings.NewReader(`[Event "x"`)) {
		if err != nil {
			t.Fatalf("PGNRecords error: %v", err)
		}
		record = r
	}

	tags, err := record.Tags()
	if err != nil {
		t.Fatalf("Tags error: %v", err)
	}
	if len(tags) != 0 {
		t.Fatalf("tags = %v, want empty map", tags)
	}
}
