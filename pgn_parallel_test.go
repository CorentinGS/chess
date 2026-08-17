package chess_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestDecodePGNGamesParallelDecodesAllGames(t *testing.T) {
	ctx := context.Background()
	results := chess.DecodePGNGamesParallel(ctx, strings.NewReader(decoderGameOne+"\n"+decoderGameTwo), chess.PGNParallelOptions{
		Workers: 2,
		Buffer:  1,
	})

	events := map[string]bool{}
	for result := range results {
		if result.Err != nil {
			t.Fatalf("parallel result error: %v", result.Err)
		}
		events[result.Game.GetTagPair("Event")] = true
	}
	if !events["one"] || !events["two"] || len(events) != 2 {
		t.Fatalf("decoded events = %v, want one and two", events)
	}
}

func TestDecodePGNGamesParallelOrderedPreservesPGNOrder(t *testing.T) {
	ctx := context.Background()
	results := chess.DecodePGNGamesParallel(ctx, strings.NewReader(decoderGameOne+"\n"+decoderGameTwo), chess.PGNParallelOptions{
		Workers: 2,
		Buffer:  2,
		Ordered: true,
	})

	var events []string
	var indexes []int64
	for result := range results {
		if result.Err != nil {
			t.Fatalf("parallel result error: %v", result.Err)
		}
		events = append(events, result.Game.GetTagPair("Event"))
		indexes = append(indexes, result.Index)
	}
	if strings.Join(events, ",") != "one,two" {
		t.Fatalf("events = %v, want [one two]", events)
	}
	if len(indexes) != 2 || indexes[0] != 1 || indexes[1] != 2 {
		t.Fatalf("indexes = %v, want [1 2]", indexes)
	}
}

func TestDecodePGNGamesParallelReportsPerGameErrors(t *testing.T) {
	bad := `[Event "bad"]
[Result "*"]

1. NotAMove *
`
	ctx := context.Background()
	results := chess.DecodePGNGamesParallel(ctx, strings.NewReader(decoderGameOne+"\n"+bad+"\n"+decoderGameTwo), chess.PGNParallelOptions{
		Workers: 2,
		Buffer:  2,
		Ordered: true,
	})

	var good int
	var failed []int64
	for result := range results {
		if result.Err != nil {
			failed = append(failed, result.Index)
			continue
		}
		good++
	}
	if good != 2 {
		t.Fatalf("good result count = %d, want 2", good)
	}
	if len(failed) != 1 || failed[0] != 2 {
		t.Fatalf("failed indexes = %v, want [2]", failed)
	}
}

func TestDecodePGNGamesParallelHonorsCanceledContext(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	results := chess.DecodePGNGamesParallel(ctx, strings.NewReader(decoderGameOne), chess.PGNParallelOptions{Workers: 1, Buffer: 1})
	result, ok := <-results
	if !ok {
		t.Fatal("result channel closed without cancellation result")
	}
	if !errors.Is(result.Err, context.Canceled) {
		t.Fatalf("result error = %v, want context.Canceled", result.Err)
	}
	if _, ok := <-results; ok {
		t.Fatal("result channel should close after cancellation")
	}
}
