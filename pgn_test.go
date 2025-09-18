package chess

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
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
		game, err := scanner.ParseNext()
		if err != nil {
			t.Fatalf("fail to parse game from valid pgn %d: %s", idx, err.Error())
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
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game from pgn: %s", err.Error())
	}
	if game == nil {
		t.Fatalf("game is nil")
	}

	if len(game.Moves()) != 7 {
		t.Fatalf("game moves are not correct, expected 7, got %d", len(game.Moves()))
	}

	lines := strings.Split(game.String(), "\n")
	if len(lines) == 0 {
		t.Fatalf("game output blank")
	}

	const expectedLastLine = "1. e4 (1. e3 e5) 1... e5 (1... d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 (4. Nf3 Nbd7) 4... dxe5 5. Qxd8+ Kxd8) 2. Nf3 (2. Nc3 Nf6 3. f4) 2... Nc6 3. d4 exd4 4. Nxd4 *"
	lastLine := lines[len(lines)-1]
	if lastLine != expectedLastLine {
		t.Fatalf("game output not correct\n\tExpected:'%v'\n\tGot:     '%v'\n",
			expectedLastLine, lastLine)
	}
}

func TestSingleGameFromPGN(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/single_game.pgn")
	reader := strings.NewReader(pgn)

	scanner := NewScanner(reader)

	game, err := scanner.ParseNext()
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

	for i, move := range game.Moves() {
		// check move number for each move
		// Get the full move number
		fullMoveNumber := (i / 2) + 1
		if move.Number() != fullMoveNumber {
			t.Fatalf("game move %d is not correct, expected full move number %d, got %d", i, fullMoveNumber, move.Number())
		}
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

			// check moves number
			if len(game.Moves()) == 0 {
				t.Fatalf("game moves are not correct, expected 0, got %d", len(game.Moves()))
			}

			if game.GetTagPair("Variant") == "From Position" {
				t.Skip("Skipping test for From Position")
			}

			for i, move := range game.Moves() {
				// check move number for each move
				// Get the full move number
				fullMoveNumber := (i / 2) + 1
				if move.Number() != fullMoveNumber {
					t.Log(game.Moves())
					t.Log(game)
					t.Fatalf("game move %d is not correct, expected full move number %d, got %d", i, fullMoveNumber, move.Number())
				}
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
	game, err := scanner.ParseNext()
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

	if game.Moves()[0].command["eval"] != "0.17" {
		t.Fatalf("game move 1 is not correct, expected eval, got %s", game.Moves()[0].command["eval"])
	}

	if moves[6].comments != "A57 Benko Gambit Declined: Main Line" {
		t.Fatalf("game move 4 is not correct, expected comment, got %s", moves[6].comments)
	}

	if moves[44].nag != "?!" {
		t.Fatalf("game move 44 is not correct, expected nag '!?', got %s", moves[44].nag)
	}
}

func TestLichessMultipleCommand(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "lichess_multiple_command.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}

	scanner := NewScanner(file)

	// Test first game
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to read games from valid pgn: %s", err.Error())
	}

	if game == nil {
		t.Fatalf("game is nil")
	}

	if game.tagPairs["Event"] != "Rated blitz game" {
		t.Fatalf("game event is not correct")
	}

	// Check if move one has the correct command
	if game.Moves()[0].command["eval"] != "0.0" {
		t.Fatalf("game move 1 is not correct, expected eval, got %s", game.Moves()[0].command["eval"])
	}

	// Check for clock also
	if game.Moves()[0].command["clk"] != "0:03:00" {
		t.Fatalf("game move 1 is not correct, expected clock, got %s", game.Moves()[0].command["clk"])
	}

	// Check move 5 for comment and eval
	if game.Moves()[4].comments != "E00 Catalan Opening" {
		t.Fatalf("game move 5 is not correct, expected comment, got %s", game.Moves()[4].comments)
	}

	if game.Moves()[4].command["eval"] != "0.14" {
		t.Fatalf("game move 5 is not correct, expected eval, got %s", game.Moves()[4].command["eval"])
	}

	// check for clock
	if game.Moves()[4].command["clk"] != "0:02:58" {
		t.Fatalf("game move 5 is not correct, expected clock, got %s", game.Moves()[4].command["clk"])
	}
}

func TestParseMoveWithNAGAndComment(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 $1 {Good move} e5 {Solid} $2 2. Nf3 $3 {Another comment} Nc6 $4 {Yet another}`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	moves := game.Moves()
	if len(moves) < 4 {
		t.Fatalf("expected at least 4 moves, got %d", len(moves))
	}

	if moves[0].nag == "" || moves[0].comments == "" {
		t.Errorf("move 1 should have both NAG and comment, got nag: '%s', comment: '%s'", moves[0].nag, moves[0].comments)
	}
	if moves[1].nag == "" || moves[1].comments == "" {
		t.Errorf("move 2 should have both NAG and comment, got nag: '%s', comment: '%s'", moves[1].nag, moves[1].comments)
	}
	if moves[2].nag == "" || moves[2].comments == "" {
		t.Errorf("move 3 should have both NAG and comment, got nag: '%s', comment: '%s'", moves[2].nag, moves[2].comments)
	}
	if moves[3].nag == "" || moves[3].comments == "" {
		t.Errorf("move 4 should have both NAG and comment, got nag: '%s', comment: '%s'", moves[3].nag, moves[3].comments)
	}
}

func TestVariationMoveNumbers(t *testing.T) {
	pgn := `[Event "VariationTest"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 (3. Bc4 Nf6 4. d3) a6 4. Ba4 Nf6 5. O-O Be7 1-0`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Helper to recursively check move numbers
	var checkMoveNumbers func(m *Move, expectedNum int)
	checkMoveNumbers = func(m *Move, expectedNum int) {
		fullMove := (expectedNum-1)/2 + 1
		for _, child := range m.children {
			if child.number != 0 && int(child.Ply()) != expectedNum {
				t.Errorf("move %s: expected move number %d, got %d", child.String(), expectedNum, child.Ply())
			}
			if child.FullMoveNumber() != fullMove {
				t.Errorf("move %s: expected full move number %d, got %d", child.String(), fullMove, child.FullMoveNumber())
			}
			// If this move starts a variation, the move number should be correct for the branch
			checkMoveNumbers(child, expectedNum+1)
		}
	}

	t.Logf("Root move number: %d", game.rootMove.number)
	t.Logf("Root move ply: %d", game.rootMove.Ply())
	t.Logf("Root move full move number: %d", game.rootMove.FullMoveNumber())
	t.Logf("Second move: %v", game.rootMove.Children()[0])

	// Mainline starts at move 1
	checkMoveNumbers(game.rootMove, 1)

	// Check specific variation branch
	mainMoves := game.Moves()
	if len(mainMoves) < 3 {
		t.Fatalf("expected at least 3 mainline moves, got %d", len(mainMoves))
	}
	variation := mainMoves[3].children // 3. Bb5 (3. Bc4 ...)
	if len(variation) == 0 {
		t.Fatalf("expected a variation at move 3, got none")
	}
	if variation[0].number != 3 {
		t.Errorf("variation first move: expected move number 3, got %d", variation[0].FullMoveNumber())
	}
	if len(variation[0].children) > 0 && variation[0].children[0].number != 3 {
		t.Errorf("variation reply: expected move number 3 or 4, got %d", variation[0].children[0].Ply())
	}
}
