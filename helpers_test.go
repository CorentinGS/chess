package chess_test

import (
	"io"
	"os"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func mustReadFile(fname string) string {
	f, err := os.Open(fname)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	b, err := io.ReadAll(f)
	if err != nil {
		panic(err)
	}
	return string(b)
}

func mustPGNOption(t *testing.T, pgn string) func(*chess.Game) {
	t.Helper()
	opt, err := chess.PGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatal(err)
	}
	return opt
}

func mustParseSingleGame(t *testing.T, pgn string) *chess.Game {
	t.Helper()
	game, err := chess.ParsePGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatalf("failed to parse pgn: %v", err)
	}
	if game == nil {
		t.Fatal("expected game")
	}
	return game
}

func withMinimalTags(moveText string) string {
	return `[Event "Test"]
[Site "Internet"]
[Date "2026.06.19"]
[Round "1"]
[White "White"]
[Black "Black"]
[Result "*"]

` + moveText
}
