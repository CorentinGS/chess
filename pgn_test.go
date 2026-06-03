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

func TestVariationComments(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "*"]

1. e4 {main line comment} e5 (1... d5 {variation comment on d5} 2. exd5 {variation comment on exd5} Qxd5) 2. Nf3 Nc6 *`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Check main line comment on 1. e4
	mainMoves := game.Moves()
	if mainMoves[0].comments != "main line comment" {
		t.Errorf("expected main line comment on e4, got %q", mainMoves[0].comments)
	}

	// 1... e5 is mainMoves[1], and its parent (rootMove child for e4) should have
	// a second child which is the variation 1... d5
	e4Move := game.rootMove.children[0] // 1. e4
	if len(e4Move.children) < 2 {
		t.Fatalf("expected at least 2 children on e4 (main line e5 + variation d5), got %d", len(e4Move.children))
	}

	// First child is the main line 1... e5
	// Second child is the variation 1... d5
	d5Move := e4Move.children[1]
	if d5Move.comments != "variation comment on d5" {
		t.Errorf("expected 'variation comment on d5' on 1...d5, got %q", d5Move.comments)
	}

	// d5's first child should be 2. exd5
	if len(d5Move.children) == 0 {
		t.Fatalf("expected children on d5 variation move")
	}
	exd5Move := d5Move.children[0]
	if exd5Move.comments != "variation comment on exd5" {
		t.Errorf("expected 'variation comment on exd5' on 2.exd5, got %q", exd5Move.comments)
	}
}

func TestVariationNAGs(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "*"]

1. e4 e5 (1... d5 $1 {great move} 2. exd5 $6 Qxd5 $2) 2. Nf3 *`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Find the variation: 1... d5
	e4Move := game.rootMove.children[0]
	if len(e4Move.children) < 2 {
		t.Fatalf("expected variation on e4, got %d children", len(e4Move.children))
	}

	d5Move := e4Move.children[1]
	if d5Move.nag != "$1" {
		t.Errorf("expected NAG '$1' on 1...d5, got %q", d5Move.nag)
	}
	if d5Move.comments != "great move" {
		t.Errorf("expected comment 'great move' on 1...d5, got %q", d5Move.comments)
	}

	if len(d5Move.children) == 0 {
		t.Fatalf("expected children on d5")
	}
	exd5Move := d5Move.children[0]
	if exd5Move.nag != "$6" {
		t.Errorf("expected NAG '$6' on 2.exd5, got %q", exd5Move.nag)
	}

	if len(exd5Move.children) == 0 {
		t.Fatalf("expected children on exd5")
	}
	qxd5Move := exd5Move.children[0]
	if qxd5Move.nag != "$2" {
		t.Errorf("expected NAG '$2' on 2...Qxd5, got %q", qxd5Move.nag)
	}
}

func TestVariationCommands(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "*"]

1. e4 e5 (1... d5 {good move [%eval -0.5] [%clk 0:05:00]} 2. exd5) 2. Nf3 *`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	e4Move := game.rootMove.children[0]
	if len(e4Move.children) < 2 {
		t.Fatalf("expected variation on e4, got %d children", len(e4Move.children))
	}

	d5Move := e4Move.children[1]
	if d5Move.comments != "good move" {
		t.Errorf("expected comment 'good move' on 1...d5, got %q", d5Move.comments)
	}
	if d5Move.command["eval"] != "-0.5" {
		t.Errorf("expected eval command '-0.5' on 1...d5, got %q", d5Move.command["eval"])
	}
	if d5Move.command["clk"] != "0:05:00" {
		t.Errorf("expected clk command '0:05:00' on 1...d5, got %q", d5Move.command["clk"])
	}
}

func TestNestedVariationComments(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 {Ruy Lopez} (3. Bc4 {Italian Game} Nf6 (3... Bc5 {Giuoco Piano}) 4. d3) 3... a6 *`

	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Main line: 3. Bb5 should have comment "Ruy Lopez"
	mainMoves := game.Moves()
	// Moves: e4, e5, Nf3, Nc6, Bb5, a6 => index 4 is Bb5
	if len(mainMoves) < 5 {
		t.Fatalf("expected at least 5 main line moves, got %d", len(mainMoves))
	}
	bb5Move := mainMoves[4]
	if bb5Move.comments != "Ruy Lopez" {
		t.Errorf("expected 'Ruy Lopez' comment on 3.Bb5, got %q", bb5Move.comments)
	}

	// Variation: 3. Bc4 should have comment "Italian Game"
	nc6Move := mainMoves[3] // parent of Bb5 and Bc4
	if len(nc6Move.children) < 2 {
		t.Fatalf("expected variation at move 3, got %d children", len(nc6Move.children))
	}
	bc4Move := nc6Move.children[1]
	if bc4Move.comments != "Italian Game" {
		t.Errorf("expected 'Italian Game' comment on 3.Bc4, got %q", bc4Move.comments)
	}

	// Nested variation: 3... Bc5 should have comment "Giuoco Piano"
	if len(bc4Move.children) < 2 {
		t.Fatalf("expected nested variation on Bc4, got %d children", len(bc4Move.children))
	}
	bc5Move := bc4Move.children[1]
	if bc5Move.comments != "Giuoco Piano" {
		t.Errorf("expected 'Giuoco Piano' comment on 3...Bc5, got %q", bc5Move.comments)
	}
}

func TestRoundTripWithVariationsAndCommandAnnotations(t *testing.T) {
	pgn := `[Event "Test"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "*"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 {Ruy Lopez} (3. Bc4 {Italian Game} Nf6 (3... Bc5 {Giuoco Piano}) 4. d3) 3... a6 *`

	base := NewScanner(strings.NewReader(pgn))
	if !base.HasNext() {
		t.Fatal("expected one game in input pgn")
	}
	game, err := base.ParseNext()
	if err != nil {
		t.Fatalf("failed to parse input pgn: %v", err)
	}

	var walk func(*Move)
	walk = func(m *Move) {
		if m == nil {
			return
		}
		if m.Comments() != "" {
			m.SetCommand("eval", "0.25")
		}
		for _, child := range m.Children() {
			walk(child)
		}
	}
	walk(game.GetRootMove())

	roundTrip := game.String()

	if strings.Contains(roundTrip, "{Ruy Lopez} { [%eval 0.25] }") {
		t.Fatalf("expected comment and command to be merged in one comment block, got: %s", roundTrip)
	}

	if !strings.Contains(roundTrip, "{Ruy Lopez [%eval 0.25]}") {
		t.Fatalf("expected merged comment+command block, got: %s", roundTrip)
	}
	if !strings.Contains(roundTrip, "{Italian Game [%eval 0.25]}") {
		t.Fatalf("expected merged comment+command block in variation, got: %s", roundTrip)
	}
	if !strings.Contains(roundTrip, "{Giuoco Piano [%eval 0.25]}") {
		t.Fatalf("expected merged comment+command block in nested variation, got: %s", roundTrip)
	}
}

func TestPGNAnnotationFidelityRoundTrip(t *testing.T) {
	pgn := withMinimalTags(`1. e4 {Good move [%clk 0:05:00]} {second [%eval 0.25]} *`)

	game := mustParseSingleGame(t, pgn)
	move := game.Moves()[0]
	if move.Comments() != "Good move second" {
		t.Fatalf("expected flattened comments, got %q", move.Comments())
	}
	if got, ok := move.GetCommand("clk"); !ok || got != "0:05:00" {
		t.Fatalf("expected clk command, got %q, %v", got, ok)
	}

	roundTrip := game.String()
	if strings.Contains(roundTrip, "{Good move }") || strings.Contains(roundTrip, "{ [%clk") {
		t.Fatalf("expected mixed comment and command to stay in one block, got %s", roundTrip)
	}

	reparsed := mustParseSingleGame(t, roundTrip)
	blocks := reparsed.Moves()[0].CommentBlocks()
	if len(blocks) != 2 {
		t.Fatalf("expected 2 comment blocks, got %#v", blocks)
	}
	assertCommentItem(t, blocks[0].Items[0], CommentText, "Good move", "", "")
	assertCommentItem(t, blocks[0].Items[1], CommentCommand, "", "clk", "0:05:00")
	assertCommentItem(t, blocks[1].Items[0], CommentText, "second", "", "")
	assertCommentItem(t, blocks[1].Items[1], CommentCommand, "", "eval", "0.25")
}

func TestPGNAnnotationFidelityPreservesOrderAndDuplicateCommands(t *testing.T) {
	pgn := withMinimalTags(`1. e4 {before [%clk 0:05:00] middle [%clk 0:04:59] after} *`)

	game := mustParseSingleGame(t, pgn)
	roundTrip := game.String()
	reparsed := mustParseSingleGame(t, roundTrip)
	move := reparsed.Moves()[0]

	if got, ok := move.GetCommand("clk"); !ok || got != "0:04:59" {
		t.Fatalf("expected last duplicate clk command, got %q, %v", got, ok)
	}
	move.SetCommand("clk", "0:04:00")
	if got, ok := move.GetCommand("clk"); !ok || got != "0:04:00" {
		t.Fatalf("expected SetCommand to update last duplicate clk command, got %q, %v", got, ok)
	}

	blocks := move.CommentBlocks()
	if len(blocks) != 1 || len(blocks[0].Items) != 5 {
		t.Fatalf("expected one block with 5 ordered items, got %#v", blocks)
	}
	assertCommentItem(t, blocks[0].Items[0], CommentText, "before", "", "")
	assertCommentItem(t, blocks[0].Items[1], CommentCommand, "", "clk", "0:05:00")
	assertCommentItem(t, blocks[0].Items[2], CommentText, "middle", "", "")
	assertCommentItem(t, blocks[0].Items[3], CommentCommand, "", "clk", "0:04:00")
	assertCommentItem(t, blocks[0].Items[4], CommentText, "after", "", "")
}

func TestPGNAnnotationFidelityCommandUsesFirstParameter(t *testing.T) {
	game := mustParseSingleGame(t, withMinimalTags(`1. e4 {[%command 1:45:12,Nf6,"very interesting, but wrong"]} *`))
	move := game.Moves()[0]

	if got, ok := move.GetCommand("command"); !ok || got != "1:45:12" {
		t.Fatalf("expected first command parameter, got %q, %v", got, ok)
	}

	blocks := move.CommentBlocks()
	if len(blocks) != 1 || len(blocks[0].Items) != 1 {
		t.Fatalf("expected one command item, got %#v", blocks)
	}
	assertCommentItem(t, blocks[0].Items[0], CommentCommand, "", "command", "1:45:12")
}

func TestPGNAnnotationFidelityVariationsAndExpansion(t *testing.T) {
	pgn := withMinimalTags(`1. e4 e5 (1... c5 {Sicilian [%eval 0.12]} 2. Nf3 {develop [%clk 0:04:58]}) 2. Nf3 *`)

	game := mustParseSingleGame(t, pgn)
	roundTrip := game.String()
	reparsed := mustParseSingleGame(t, roundTrip)

	e4 := reparsed.Moves()[0]
	if len(e4.Children()) < 2 {
		t.Fatalf("expected e4 variation, got %d children", len(e4.Children()))
	}
	c5 := e4.Children()[1]
	blocks := c5.CommentBlocks()
	if len(blocks) != 1 || len(blocks[0].Items) != 2 {
		t.Fatalf("expected variation annotation block, got %#v", blocks)
	}
	assertCommentItem(t, blocks[0].Items[0], CommentText, "Sicilian", "", "")
	assertCommentItem(t, blocks[0].Items[1], CommentCommand, "", "eval", "0.12")

	scanner := NewScanner(strings.NewReader(roundTrip), WithExpandVariations())
	for scanner.HasNext() {
		if _, err := scanner.ParseNext(); err != nil {
			t.Fatalf("expanded annotated variation should parse: %v", err)
		}
	}
}

func TestPGNAnnotationFidelityLegacyAPIsAndDefensiveCopies(t *testing.T) {
	game := NewGame()
	if err := game.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	move := game.Moves()[0]
	move.SetComment("Good move")
	move.SetCommand("clk", "0:05:00")
	move.SetCommand("clk", "0:04:59")
	move.AddComment("!")

	if move.Comments() != "Good move!" {
		t.Fatalf("expected legacy comments, got %q", move.Comments())
	}
	if got, ok := move.GetCommand("clk"); !ok || got != "0:04:59" {
		t.Fatalf("expected updated clk command, got %q, %v", got, ok)
	}
	if !strings.Contains(game.String(), "{Good move! [%clk 0:04:59]}") {
		t.Fatalf("expected legacy comment serialization, got %s", game.String())
	}

	parsed := mustParseSingleGame(t, withMinimalTags(`1. e4 {Good [%clk 0:05:00]} *`))
	parsedMove := parsed.Moves()[0]
	blocks := parsedMove.CommentBlocks()
	blocks[0].Items[0].Text = "mutated"
	blocks[0].Items = append(blocks[0].Items, CommentItem{Kind: CommentCommand, Key: "eval", Value: "9"})

	blocks = parsedMove.CommentBlocks()
	assertCommentItem(t, blocks[0].Items[0], CommentText, "Good", "", "")
	if strings.Contains(parsed.String(), "mutated") || strings.Contains(parsed.String(), "[%eval 9]") {
		t.Fatalf("mutating CommentBlocks result changed move state: %s", parsed.String())
	}
}

func mustParseSingleGame(t *testing.T, pgn string) *Game {
	t.Helper()
	scanner := NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
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
[Date "2026.06.02"]
[Round "1"]
[White "White"]
[Black "Black"]
[Result "*"]

` + moveText
}

func assertCommentItem(t *testing.T, item CommentItem, kind CommentItemKind, text, key, value string) {
	t.Helper()
	if item.Kind != kind || item.Text != text || item.Key != key || item.Value != value {
		t.Fatalf("unexpected comment item: got %#v", item)
	}
}

func TestParserAnnotationErrors(t *testing.T) {
	t.Run("UnexpectedTokenInComment", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommentStart, Value: "{"},
			{Type: MoveNumber, Value: "1"},
			{Type: CommentEnd, Value: "}"},
		})
		_, err := parser.parseComment()
		if err == nil {
			t.Fatal("expected parseComment error")
		}
	})

	t.Run("UnterminatedComment", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommentStart, Value: "{"},
			{Type: COMMENT, Value: "missing close"},
		})
		_, err := parser.parseComment()
		if err == nil {
			t.Fatal("expected unterminated comment error")
		}
	})

	t.Run("CommandErrorPropagatesFromComment", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommentStart, Value: "{"},
			{Type: CommandStart, Value: "[%"},
			{Type: MoveNumber, Value: "1"},
			{Type: CommandEnd, Value: "]"},
			{Type: CommentEnd, Value: "}"},
		})
		_, err := parser.parseComment()
		if err == nil {
			t.Fatal("expected command parse error")
		}
	})

	t.Run("UnexpectedTokenInCommand", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommandStart, Value: "[%"},
			{Type: MoveNumber, Value: "1"},
			{Type: CommandEnd, Value: "]"},
		})
		_, err := parser.parseCommand()
		if err == nil {
			t.Fatal("expected parseCommand error")
		}
	})

	t.Run("UnterminatedCommand", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommandStart, Value: "[%"},
			{Type: CommandName, Value: "clk"},
		})
		_, err := parser.parseCommand()
		if err == nil {
			t.Fatal("expected unterminated command error")
		}
	})
}

func TestParserVariationErrors(t *testing.T) {
	t.Run("UnterminatedVariation", func(t *testing.T) {
		parser := NewParser([]Token{{Type: VariationStart, Value: "("}})
		if err := parser.parseVariation(1, 1); err == nil {
			t.Fatal("expected unterminated variation error")
		}
	})

	t.Run("MoveColorMismatch", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: VariationStart, Value: "("},
			{Type: MoveNumber, Value: "1"},
			{Type: DOT, Value: "."},
			{Type: ELLIPSIS, Value: "..."},
			{Type: SQUARE, Value: "e5"},
			{Type: VariationEnd, Value: ")"},
		})
		if err := parser.parseVariation(1, 1); err == nil {
			t.Fatal("expected move color mismatch error")
		}
	})

	t.Run("CommentErrorInsideVariation", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: VariationStart, Value: "("},
			{Type: CommentStart, Value: "{"},
			{Type: MoveNumber, Value: "1"},
			{Type: VariationEnd, Value: ")"},
		})
		if err := parser.parseVariation(1, 1); err == nil {
			t.Fatal("expected variation comment error")
		}
	})
}

func TestParseResultDraw(t *testing.T) {
	parser := NewParser([]Token{{Type: RESULT, Value: "1/2-1/2"}})
	parser.parseResult()
	if parser.game.Outcome() != Draw {
		t.Fatalf("expected draw outcome, got %s", parser.game.Outcome())
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

// BenchmarkPGNWithVariations measures parsing performance for a PGN with many nested variations.
func BenchmarkPGNWithVariations(b *testing.B) {
	pgn := `[Event "Variation Benchmark"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 (3. Bc4 Nf6 4. d3) a6 4. Ba4 Nf6 5. O-O Be7 1-0`

	scanner := NewScanner(strings.NewReader(pgn))

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Reset scanner for each iteration
		scanner = NewScanner(strings.NewReader(pgn))
		_, err := scanner.ParseNext()
		if err != nil {
			b.Fatalf("parse error: %v", err)
		}
	}
}
