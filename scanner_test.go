package chess

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"testing"
)

func TestScanner(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}
	defer file.Close()

	scanner := NewScanner(file)

	// Test first game
	game, err := scanner.ScanGame()
	if err != nil {
		t.Fatalf("Failed to read first game: %v", err)
	}

	tokens, err := TokenizeGame(game)
	if err != nil {
		t.Fatalf("Failed to tokenize first game: %v", err)
	}

	expectedFirstGame := []struct {
		typ   TokenType
		value string
	}{
		{TagStart, "["},
		{TagKey, "Event"},
		{TagValue, "Example"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Site"},
		{TagValue, "Internet"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Date"},
		{TagValue, "2023.12.06"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Round"},
		{TagValue, "1"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "White"},
		{TagValue, "Player1"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Black"},
		{TagValue, "Player2"},
		{TagEnd, "]"},
		{TagStart, "["},
		{TagKey, "Result"},
		{TagValue, "1-0"},
		{TagEnd, "]"},
		{MoveNumber, "1"},
		{DOT, "."},
		{SQUARE, "e4"},
		{SQUARE, "e5"},
		{MoveNumber, "2"},
		{DOT, "."},
		{PIECE, "N"},
		{SQUARE, "f3"},
		{PIECE, "N"},
		{SQUARE, "c6"},
		{MoveNumber, "3"},
		{DOT, "."},
		{PIECE, "B"},
		{SQUARE, "b5"},
		{CommentStart, "{"},
		{COMMENT, "This is the Ruy Lopez."},
		{CommentEnd, "}"},
		{MoveNumber, "3"},
		{ELLIPSIS, "..."},
		{SQUARE, "a6"},
		{RESULT, "1-0"},
	}

	if len(tokens) != len(expectedFirstGame) {
		t.Errorf("Expected %d tokens, got %d", len(expectedFirstGame), len(tokens))
		return
	}

	for i, expected := range expectedFirstGame {
		if i >= len(tokens) {
			t.Errorf("Missing token at position %d, expected {%v, %q}", i, expected.typ, expected.value)
			continue
		}

		if tokens[i].Type != expected.typ || tokens[i].Value != expected.value {
			t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
				i, expected.typ, expected.value, tokens[i].Type, tokens[i].Value)
		}
	}

	// Test second game (if exists)
	_, err = scanner.ScanGame()
	if err != nil && !errors.Is(err, io.EOF) {
		t.Errorf("Unexpected error reading second game: %v", err)
	}

	// Test HasNext functionality by counting games
	file.Seek(0, 0) // Reset file to beginning
	scanner = NewScanner(file)

	var gameCount int
	for scanner.HasNext() {
		game, err := scanner.ScanGame()
		if err != nil {
			t.Fatalf("Error reading game %d: %v", gameCount+1, err)
		}

		tokens, err = TokenizeGame(game)
		if err != nil {
			t.Fatalf("Error tokenizing game %d: %v", gameCount+1, err)
		}

		if len(tokens) == 0 {
			t.Errorf("Game %d has no tokens", gameCount+1)
		}

		gameCount++
	}

	expectedGames := 4
	if gameCount != expectedGames {
		t.Errorf("Expected %d games, got %d", expectedGames, gameCount)
	}
}

func TestScannerEmptyFile(t *testing.T) {
	tmpfile, err := os.CreateTemp("", "empty.pgn")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpfile.Name())
	defer tmpfile.Close()

	scanner := NewScanner(tmpfile)

	if scanner.HasNext() {
		t.Error("Expected HasNext() to return false for empty file")
	}

	game, err := scanner.ScanGame()
	if !errors.Is(err, io.EOF) {
		t.Errorf("Expected EOF error for empty file, got %v", err)
	}
	if game != nil {
		t.Error("Expected nil game for empty file")
	}
}

func TestSequentialProcessing(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}
	defer file.Close()

	scanner := NewScanner(file)
	var games []*GameScanned
	var allTokens [][]Token

	// Read all games using ScanGame in a loop
	for {
		game, err := scanner.ScanGame()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			t.Fatalf("Failed to scan game: %v", err)
		}
		games = append(games, game)

		tokens, err := TokenizeGame(game)
		if err != nil {
			t.Fatalf("Failed to tokenize game: %v", err)
		}
		allTokens = append(allTokens, tokens)
	}

	if len(games) != 4 {
		t.Errorf("Expected 4 games, got %d", len(games))
	}

	for i, tokens := range allTokens {
		if len(tokens) == 0 {
			t.Errorf("Game %d has no tokens", i+1)
		}
	}
}

// Additional test to verify HasNext doesn't consume games.
func TestHasNextDoesntConsume(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}
	defer file.Close()

	scanner := NewScanner(file)

	// Call HasNext multiple times
	for i := range 3 {
		if !scanner.HasNext() {
			t.Errorf("Expected HasNext() to return true on call %d", i+1)
		}
	}

	// Should still be able to read the first game
	game, err := scanner.ScanGame()
	if err != nil {
		t.Fatalf("Failed to read first game after HasNext calls: %v", err)
	}

	tokens, err := TokenizeGame(game)
	if err != nil {
		t.Fatalf("Failed to tokenize first game: %v", err)
	}

	if len(tokens) == 0 {
		t.Error("First game has no tokens after multiple HasNext calls")
	}
}
