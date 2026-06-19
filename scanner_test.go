package chess_test

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestScanner(t *testing.T) {
	openMultiGame := func() *os.File {
		t.Helper()
		file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
		if err != nil {
			t.Fatalf("Failed to open fixture file: %v", err)
		}
		return file
	}

	t.Run("tokenize_first_game", func(t *testing.T) {
		file := openMultiGame()
		defer file.Close()
		scanner := chess.NewScanner(file)

		game, err := scanner.ScanGame()
		if err != nil {
			t.Fatalf("Failed to read first game: %v", err)
		}

		tokens, err := chess.TokenizeGame(game)
		if err != nil {
			t.Fatalf("Failed to tokenize first game: %v", err)
		}

		expectedFirstGame := []struct {
			typ   chess.TokenType
			value string
		}{
			{chess.TagStart, "["},
			{chess.TagKey, "Event"},
			{chess.TagValue, "Example"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "Site"},
			{chess.TagValue, "Internet"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "Date"},
			{chess.TagValue, "2023.12.06"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "Round"},
			{chess.TagValue, "1"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "White"},
			{chess.TagValue, "Player1"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "Black"},
			{chess.TagValue, "Player2"},
			{chess.TagEnd, "]"},
			{chess.TagStart, "["},
			{chess.TagKey, "Result"},
			{chess.TagValue, "1-0"},
			{chess.TagEnd, "]"},
			{chess.MoveNumber, "1"},
			{chess.DOT, "."},
			{chess.SQUARE, "e4"},
			{chess.SQUARE, "e5"},
			{chess.MoveNumber, "2"},
			{chess.DOT, "."},
			{chess.PIECE, "N"},
			{chess.SQUARE, "f3"},
			{chess.PIECE, "N"},
			{chess.SQUARE, "c6"},
			{chess.MoveNumber, "3"},
			{chess.DOT, "."},
			{chess.PIECE, "B"},
			{chess.SQUARE, "b5"},
			{chess.CommentStart, "{"},
			{chess.COMMENT, "This is the Ruy Lopez."},
			{chess.CommentEnd, "}"},
			{chess.MoveNumber, "3"},
			{chess.ELLIPSIS, "..."},
			{chess.SQUARE, "a6"},
			{chess.RESULT, "1-0"},
		}

		if len(tokens) != len(expectedFirstGame) {
			t.Fatalf("Expected %d tokens, got %d", len(expectedFirstGame), len(tokens))
		}

		for i, expected := range expectedFirstGame {
			if tokens[i].Type != expected.typ || tokens[i].Value != expected.value {
				t.Errorf("Token %d - Expected {%v, %q}, got {%v, %q}",
					i, expected.typ, expected.value, tokens[i].Type, tokens[i].Value)
			}
		}
	})

	t.Run("reads_second_game", func(t *testing.T) {
		file := openMultiGame()
		defer file.Close()
		scanner := chess.NewScanner(file)
		_, _ = scanner.ScanGame()
		_, err := scanner.ScanGame()
		if err != nil && !errors.Is(err, io.EOF) {
			t.Errorf("Unexpected error reading second game: %v", err)
		}
	})

	t.Run("has_next_counts_all_games", func(t *testing.T) {
		file := openMultiGame()
		defer file.Close()
		scanner := chess.NewScanner(file)

		var gameCount int
		for scanner.HasNext() {
			game, err := scanner.ScanGame()
			if err != nil {
				t.Fatalf("Error reading game %d: %v", gameCount+1, err)
			}

			tokens, err := chess.TokenizeGame(game)
			if err != nil {
				t.Fatalf("Error tokenizing game %d: %v", gameCount+1, err)
			}

			if len(tokens) == 0 {
				t.Errorf("Game %d has no tokens", gameCount+1)
			}

			gameCount++
		}

		const expectedGames = 4
		if gameCount != expectedGames {
			t.Errorf("Expected %d games, got %d", expectedGames, gameCount)
		}
	})
}

func TestScanner_EmptyInputReturnsEOFAndFalseHasNext(t *testing.T) {
	tmpfile, err := os.CreateTemp("", "empty.pgn")
	if err != nil {
		t.Fatalf("Failed to create temp file: %v", err)
	}
	defer os.Remove(tmpfile.Name())
	defer tmpfile.Close()

	scanner := chess.NewScanner(tmpfile)

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

func TestScanner_ReadsAllGamesInScanLoop(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}
	defer file.Close()

	scanner := chess.NewScanner(file)
	var games []*chess.GameScanned
	var allTokens [][]chess.Token

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

		tokens, err := chess.TokenizeGame(game)
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
func TestScanner_HasNextDoesntConsume(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "multi_game.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}
	defer file.Close()

	scanner := chess.NewScanner(file)

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

	tokens, err := chess.TokenizeGame(game)
	if err != nil {
		t.Fatalf("Failed to tokenize first game: %v", err)
	}

	if len(tokens) == 0 {
		t.Error("First game has no tokens after multiple HasNext calls")
	}
}

func validateExpand(t *testing.T, scanner *chess.Scanner, expectedLastLines []string,
	expectedFinalPos []string,
) {
	count := 0
	for scanner.HasNext() {
		game, err := scanner.ParseNext()
		if err != nil {
			t.Fatalf("fail to parse game %v: %s", count+1, err.Error())
		}

		if game == nil {
			t.Fatalf("game is nil")
		}
		if count >= len(expectedLastLines) {
			t.Fatalf("expected %v games but found at least %v",
				len(expectedLastLines), count+1)
		}
		lines := strings.Split(game.String(), "\n")
		if len(lines) == 0 {
			t.Fatalf("split game %v output blank", count+1)
		}

		lastLine := lines[len(lines)-1]
		if lastLine != expectedLastLines[count] {
			t.Errorf("game output not correct\n\tExpected:'%v'\n\tGot:     '%v'\n",
				expectedLastLines[count], lastLine)
		}
		fen := game.Position().XFENString()
		if fen != expectedFinalPos[count] {
			t.Errorf("game position not correct\n\tExpected:'%v'\n\tGot:     '%v'\n",
				expectedFinalPos[count], fen)
		}
		count++
	}

	if count != len(expectedLastLines) {
		t.Fatalf("expected %v games but found only %v",
			len(expectedLastLines), count)
	}
}

func TestScanner_VariationExpansion(t *testing.T) {
	tests := []struct {
		name             string
		fixture          string
		expandVariations bool
		lastLines        []string
		finalPos         []string
	}{
		{"variations_expand", "fixtures/pgns/variations.pgn", true,
			[]string{
				"1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Nxd4 *",
				"1. e4 e5 2. Nc3 Nf6 3. f4 *",
				"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 dxe5 5. Qxd8+ Kxd8 *",
				"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. Nf3 Nbd7 *",
				"1. e3 e5 *",
			},
			[]string{
				"r1bqkbnr/pppp1ppp/2n5/8/3NP3/8/PPP2PPP/RNBQKB1R b KQkq - 0 4",
				"rnbqkb1r/pppp1ppp/5n2/4p3/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 0 3",
				"rnbk1b1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1B1KBNR w KQ - 0 6",
				"r1bqkb1r/pppn1ppp/3p1n2/4p3/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 2 5",
				"rnbqkbnr/pppp1ppp/8/4p3/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
			}},
		{"variations_no_expand", "fixtures/pgns/variations.pgn", false,
			[]string{
				"1. e4 (1. e3 e5) 1... e5 (1... d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 (4. Nf3 Nbd7) 4... dxe5 5. Qxd8+ Kxd8) 2. Nf3 (2. Nc3 Nf6 3. f4) 2... Nc6 3. d4 exd4 4. Nxd4 1-0",
			},
			[]string{
				"r1bqkbnr/pppp1ppp/2n5/8/3NP3/8/PPP2PPP/RNBQKB1R b KQkq - 0 4",
			}},
		{"multi_frompos_no_expand", "fixtures/pgns/multi_frompos_games.pgn", false,
			[]string{
				"1. d4 d5 2. c4 c6 { [%eval 0.21]} *",
				"3. Nf3 (3. Nc3 Nf6 4. Nf3) 3... Nf6 4. Nc3 { [%eval 0.16]} *",
				"4... a6 { [%eval 0.19]} *",
				"5. cxd5 (5. e3 e6 6. cxd5 cxd5) 5... cxd5 6. e3 e6 { [%eval 0.11]} *",
			},
			[]string{
				"rnbqkbnr/pp2pppp/2p5/3p4/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3",
				"rnbqkb1r/pp2pppp/2p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R b KQkq - 3 4",
				"rnbqkb1r/1p2pppp/p1p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5",
				"rnbqkb1r/1p3ppp/p3pn2/3p4/3P4/2N1PN2/PP3PPP/R1BQKB1R w KQkq - 0 7",
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pgn := mustReadFile(tt.fixture)
			scanner := chess.NewScanner(strings.NewReader(pgn))
			if tt.expandVariations {
				scanner = chess.NewScanner(strings.NewReader(pgn), chess.WithExpandVariations())
			}
			validateExpand(t, scanner, tt.lastLines, tt.finalPos)
		})
	}
}

func TestScanner_PreservesEscapedQuoteInTagValue(t *testing.T) {
	const escapedQuoteGame = `[Event "Internet Section 08A g/8'+2\""]
[Site "Dos Hermanas"]
[Date "2004.03.08"]
[Round "6"]
[White "Di Berardino, Diego Rafael"]
[Black "Mar, Fernando"]
[Result "1/2-1/2"]
[ECO "B96"]

1. e4 c5 2. Nf3 1/2-1/2
`

	scanner := chess.NewScanner(strings.NewReader(escapedQuoteGame))
	if !scanner.HasNext() {
		t.Fatal("scanner should report one game")
	}
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("BUG: parser rejects spec-legal escaped quote in tag value: %v", err)
	}

	// The in-memory value should contain a literal double quote.
	if game.GetTagPair("Event") != `Internet Section 08A g/8'+2"` {
		t.Fatalf("expected unescaped quote in tag value, got %q", game.GetTagPair("Event"))
	}

	// Round-trip: String() must emit the escaped form again.
	pgn := game.String()
	if !strings.Contains(pgn, `[Event "Internet Section 08A g/8'+2\""]`) {
		t.Fatalf("round-trip PGN does not contain escaped quote: %s", pgn)
	}
}

func TestScanner_GameBoundaries(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		wantGames   int
		wantOutcome chess.Outcome
	}{
		{"empty", "", 0, chess.NoOutcome},
		{"whitespace_only", "   \n\t  ", 0, chess.NoOutcome},
		{"result_only", "1-0\n", 1, chess.WhiteWon},
		{"star_result_only_not_detected", "*\n", 0, chess.NoOutcome},
		{"tagless_game", "1. e4 e5 1-0\n", 1, chess.WhiteWon},
		{"tags_only", "[Event \"x\"]\n[Site \"y\"]\n\n", 1, chess.NoOutcome},
		{"crlf_no_tag_separator", "1. e4 e5 1-0\r\n1. d4 d5 0-1\r\n", 1, chess.WhiteWon},
		{"bom_prefix", "\xEF\xBB\xBF1. e4 e5 1-0\n", 0, chess.NoOutcome},
		{"no_blank_line_separator", "1. e4 e5 1-0\n[Event \"z\"]\n\n1. d4 d5 0-1\n", 1, chess.BlackWon},
		{"trailing_garbage", "1. e4 e5 1-0\nGARBAGE\n", 1, chess.WhiteWon},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			scanner := chess.NewScanner(strings.NewReader(tt.input))
			count := 0
			var lastOutcome chess.Outcome
			for scanner.HasNext() {
				g, err := scanner.ParseNext()
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				lastOutcome = g.Outcome()
				count++
			}
			if count != tt.wantGames {
				t.Errorf("game count = %d, want %d", count, tt.wantGames)
			}
			if tt.wantGames > 0 && lastOutcome != tt.wantOutcome {
				t.Errorf("outcome = %s, want %s", lastOutcome, tt.wantOutcome)
			}
		})
	}
}

func TestScanner_TokenizeGameNil(t *testing.T) {
	tokens, err := chess.TokenizeGame(nil)
	if err != nil {
		t.Fatalf("TokenizeGame(nil) error = %v", err)
	}
	if len(tokens) != 0 {
		t.Errorf("TokenizeGame(nil) = %d tokens, want 0", len(tokens))
	}
}

func TestScanner_RoundTrip(t *testing.T) {
	pgn := mustReadFile("fixtures/pgns/variations.pgn")
	scanner := chess.NewScanner(strings.NewReader(pgn))
	for scanner.HasNext() {
		g, err := scanner.ParseNext()
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		rendered := g.String()
		reparsed := mustParseSingleGame(t, rendered)
		if reparsed.String() != rendered {
			t.Errorf("round-trip mismatch:\nfirst:\n%s\nsecond:\n%s", rendered, reparsed.String())
		}
	}
}
