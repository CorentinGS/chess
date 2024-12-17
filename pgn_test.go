package chess

import (
	"fmt"
	"io"
	"os"
	"strings"
	"testing"
)

type pgnTest struct {
	PostPos *Position
	PGN     string
}

var validPGNs = []pgnTest{
	{
		PostPos: unsafeFEN("4r3/6P1/2p2P1k/1p6/pP2p1R1/P1B5/2P2K2/3r4 b - - 0 45"),
		PGN:     mustParsePGN("fixtures/pgns/0001.pgn"),
	},
	{
		PostPos: unsafeFEN("4r3/6P1/2p2P1k/1p6/pP2p1R1/P1B5/2P2K2/3r4 b - - 0 45"),
		PGN:     mustParsePGN("fixtures/pgns/0002.pgn"),
	},
	{
		PostPos: unsafeFEN("2r2rk1/pp1bBpp1/2np4/2pp2p1/1bP5/1P4P1/P1QPPPBP/3R1RK1 b - - 0 3"),
		PGN:     mustParsePGN("fixtures/pgns/0003.pgn"),
	},
	{
		PostPos: unsafeFEN("r3kb1r/2qp1pp1/b1n1p2p/pp2P3/5n1B/1PPQ1N2/P1BN1PPP/R3K2R w KQkq - 1 14"),
		PGN:     mustParsePGN("fixtures/pgns/0004.pgn"),
	},
	{
		PostPos: unsafeFEN("8/8/6p1/4R3/6kQ/r2P1pP1/5P2/6K1 b - - 3 42"),
		PGN:     mustParsePGN("fixtures/pgns/0011.pgn"),
	},
	{
		PostPos: StartingPosition(),
		PGN:     mustParsePGN("fixtures/pgns/0012.pgn"),
	},
}

type commentTest struct {
	PGN         string
	MoveNumber  int
	CommentText string
}

var _ = []commentTest{
	{
		PGN:         mustParsePGN("fixtures/pgns/0005.pgn"),
		MoveNumber:  7,
		CommentText: `(-0.25 → 0.39) Inaccuracy. cxd4 was best. [%eval 0.39] [%clk 0:05:05]`,
	},
	{
		PGN:         mustParsePGN("fixtures/pgns/0009.pgn"),
		MoveNumber:  5,
		CommentText: `This opening is called the Ruy Lopez.`,
	},
	{
		PGN:         mustParsePGN("fixtures/pgns/0010.pgn"),
		MoveNumber:  5,
		CommentText: `This opening is called the Ruy Lopez.`,
	},
}

func BenchmarkPGN(b *testing.B) {
	pgn := mustParsePGN("fixtures/pgns/0001.pgn")
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		opt, _ := PGN(strings.NewReader(pgn))
		NewGame(opt)
	}
}

func mustParsePGN(fname string) string {
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

func TestGamesFromPGN(t *testing.T) {
	for idx, test := range validPGNs {
		reader := strings.NewReader(test.PGN)
		scanner := NewScanner(reader)
		scannedGame, err := scanner.ScanGame()
		if err != nil {
			t.Fatalf("fail to scan game from valid pgn %d: %s", idx, err.Error())
		}

		tokens, err := TokenizeGame(scannedGame)
		if err != nil {
			t.Fatalf("fail to tokenize game from valid pgn %d: %s", idx, err.Error())
		}

		parser := NewParser(tokens)
		game, err := parser.Parse()
		if err != nil {
			t.Fatalf("fail to read games from valid pgn %d: %s", idx, err.Error())
		}

		if game == nil {
			t.Fatalf("game is nil")
		}
	}
}

func TestGameWithVariations(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/variations.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)
	scannedGame, err := scanner.ScanGame()
	if err != nil {
		t.Fatalf("fail to scan game from valid pgn: %s", err.Error())
	}

	tokens, err := TokenizeGame(scannedGame)
	if err != nil {
		t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
	}

	parser := NewParser(tokens)
	game, err := parser.Parse()
	if err != nil {
		t.Fatalf("fail to read games from valid pgn: %s", err.Error())
	}

	if game == nil {
		t.Fatalf("game is nil")
	}

	if len(game.Moves()) != 7 {
		t.Fatalf("game moves are not correct, expected 7, got %d", len(game.Moves()))
	}

}

func TestSingleGameFromPGN(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/single_game.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)

	scannedGame, err := scanner.ScanGame()
	if err != nil {
		t.Fatalf("fail to scan game from valid pgn: %s", err.Error())
	}

	tokens, err := TokenizeGame(scannedGame)
	if err != nil {
		t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
	}

	parser := NewParser(tokens)
	game, err := parser.Parse()
	if err != nil {
		t.Fatalf("fail to read games from valid pgn: %s", err.Error())
	}

	if game == nil {
		t.Fatalf("game is nil")
	}

	if game.tagPairs["Event"] != "Example" {
		t.Fatalf("game event is not correct")
	}

	if game.tagPairs["Site"] != "Internet" {
		t.Fatalf("game site is not correct")
	}

	if game.tagPairs["Date"] != "2023.12.06" {
		t.Fatalf("game date is not correct")
	}

	if game.tagPairs["Round"] != "1" {
		t.Fatalf("game round is not correct")
	}

	if game.tagPairs["White"] != "Player1" {
		t.Fatalf("game white is not correct")
	}

	if game.tagPairs["Black"] != "Player2" {
		t.Fatalf("game black is not correct")
	}

	if game.tagPairs["Result"] != "1-0" {
		t.Fatalf("game result is not correct")
	}

	// Check moves
	if len(game.Moves()) != 6 {
		t.Fatalf("game moves are not correct, expected 6, got %d", len(game.Moves()))
	}

	if game.Moves()[0].String() != "e2e4" {
		t.Fatalf("game move 1 is not correct, expected e4, got %s", game.Moves()[0].String())
	}

	// print all moves
	moves := game.Moves()

	if moves[4].comments == "" {
		t.Fatalf("game move 6 is not correct, expected comment, got %s", moves[5].comments)
	}
}

func TestBigPgn(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/big.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)
	count := 0

	for scanner.HasNext() {
		count++
		t.Run(fmt.Sprintf("big pgn : %d", count), func(t *testing.T) {
			scannedGame, err := scanner.ScanGame()
			if err != nil {
				t.Fatalf("fail to scan game from valid pgn: %s", err.Error())
			}

			tokens, err := TokenizeGame(scannedGame)
			if err != nil {
				t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
			}

			raw := scannedGame.Raw

			parser := NewParser(tokens)
			game, err := parser.Parse()
			if err != nil {
				t.Fatalf("fail to read games from valid pgn: %s | %s", err.Error(), raw[:min(200, len(raw))])
			}

			if game == nil {
				t.Fatalf("game is nil")
			}
		})
	}
}

func TestBigBigPgn(t *testing.T) {
	t.Skip("This test is too slow")
	pgn := mustParsePGN("fixtures/pgns/big_big.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)
	count := 0

	for scanner.HasNext() {
		count++
		t.Run(fmt.Sprintf("bigbig pgn : %d", count), func(t *testing.T) {
			scannedGame, err := scanner.ScanGame()
			if err != nil {
				t.Fatalf("fail to scan game from valid pgn: %s", err.Error())
			}

			tokens, err := TokenizeGame(scannedGame)
			if err != nil {
				t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
			}

			raw := scannedGame.Raw

			parser := NewParser(tokens)
			game, err := parser.Parse()
			if err != nil {
				t.Fatalf("fail to read games from valid pgn: %s | %s", err.Error(), raw[:min(200, len(raw))])
			}

			if game == nil {
				t.Fatalf("game is nil")
			}
		})
	}
}

func TestCompleteGame(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/complete_game.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)
	scannedGame, err := scanner.ScanGame()
	if err != nil {
		t.Fatalf("fail to scan game from valid pgn: %s", err.Error())
	}

	t.Log(scannedGame.Raw)

	tokens, err := TokenizeGame(scannedGame)
	if err != nil {
		t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
	}

	t.Log(tokens[80])

	parser := NewParser(tokens)
	game, err := parser.Parse()
	if err != nil {
		t.Fatalf("fail to read games from valid pgn: %s", err.Error())
	}

	if game == nil {
		t.Fatalf("game is nil")
	}

	if game.tagPairs["Event"] != "Rated blitz game" {
		t.Fatalf("game event is not correct")
	}

	if game.tagPairs["Site"] != "https://lichess.org/ASZaQYyr" {
		t.Fatalf("game site is not correct")
	}

	if game.tagPairs["Date"] != "2024.12.07" {
		t.Fatalf("game date is not correct")
	}

	if game.tagPairs["White"] != "dangerouschess07" {
		t.Fatalf("game white is not correct")
	}

	if game.tagPairs["Black"] != "GABUZYAN_CHESSMOOD" {
		t.Fatalf("game black is not correct")
	}

	if game.tagPairs["Result"] != "0-1" {
		t.Fatalf("game result is not correct")
	}

	// Check moves
	if len(game.Moves()) != 104 {
		t.Fatalf("game moves are not correct, expected 52, got %d", len(game.Moves()))
	}

	if game.Moves()[0].String() != "d2d4" {
		t.Fatalf("game move 1 is not correct, expected d4, got %s", game.Moves()[0].String())
	}

	if game.Moves()[0].comments != "" {
		t.Fatalf("game move 1 is not correct, expected no comment, got %s", game.Moves()[0].comments)
	}

	// print all moves
	moves := game.Moves()

	if moves[4].comments == "" {
		t.Fatalf("game move 6 is not correct, expected comment, got %s", moves[5].comments)
	}
}
