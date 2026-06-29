package chess

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
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
	for range b.N {
		opt, _ := PGN(strings.NewReader(pgn))
		NewGame(opt)
	}
}

func TestNewParserSharesStartingPosition(t *testing.T) {
	parser := NewParser(nil)
	if parser.game.currentPosition() != parser.game.MoveTree().Root().position {
		t.Fatal("parser game and root move should share the starting position")
	}
}

type errorTokenSource struct {
	err error
}

func (s errorTokenSource) NextToken() (Token, error) {
	return Token{}, s.err
}

func TestParserReportsInitialTokenSourceError(t *testing.T) {
	wantErr := errors.New("token source failed")

	_, err := newParserFromSource(errorTokenSource{err: wantErr}).Parse()
	if !errors.Is(err, wantErr) {
		t.Fatalf("Parse() error = %v, want %v", err, wantErr)
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

func isKnownInconsistentPgn(err error) bool {
	if err == nil {
		return false
	}
	return strings.Contains(err.Error(), "conflicts with")
}

func TestGamesFromPGN(t *testing.T) {
	for idx, test := range validPGNs {
		reader := strings.NewReader(test.PGN)
		game, err := NewPGNDecoder(reader).Decode()
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

	game, err := NewPGNDecoder(reader).Decode()
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

	const expectedLastLine = "1. e4 (1. e3 e5) 1... e5 (1... d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 (4. Nf3 Nbd7) 4... dxe5 5. Qxd8+ Kxd8) 2. Nf3 (2. Nc3 Nf6 3. f4) 2... Nc6 3. d4 exd4 4. Nxd4 1-0"
	lastLine := lines[len(lines)-1]
	if lastLine != expectedLastLine {
		t.Fatalf("game output not correct\n\tExpected:'%v'\n\tGot:     '%v'\n",
			expectedLastLine, lastLine)
	}
}

func TestSingleGameFromPGN(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/single_game.pgn")
	reader := strings.NewReader(pgn)

	game, err := NewPGNDecoder(reader).Decode()
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

	for i, move := range game.MoveTree().MainLine() {
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
	moves := game.MoveTree().MainLine()

	if moves[4].Comments() == "" {
		t.Fatalf("game move 6 is not correct, expected comment, got %s", moves[5].Comments())
	}
}

func TestBigPgn(t *testing.T) {
	pgn := mustParsePGN("fixtures/pgns/big.pgn")

	count := 0
	for record, err := range PGNRecords(context.Background(), strings.NewReader(pgn)) {
		count++
		t.Run(fmt.Sprintf("big pgn : %d", count), func(t *testing.T) {
			if err != nil {
				if isKnownInconsistentPgn(err) {
					t.Skipf("skipping inconsistent real-world PGN: %s", err.Error())
					return
				}
				t.Fatalf("fail to read record: %s", err.Error())
			}

			raw := record.Raw
			tokens, err := TokenizeGame(&GameScanned{Raw: raw})
			if err != nil {
				t.Fatalf("fail to tokenize game from valid pgn: %s", err.Error())
			}

			parser := NewParser(tokens)
			game, err := parser.Parse()
			if err != nil {
				if isKnownInconsistentPgn(err) {
					t.Skipf("skipping inconsistent real-world PGN: %s", err.Error())
					return
				}
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

			for i, move := range game.MoveTree().MainLine() {
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
	pgn := mustParsePGN("fixtures/pgns/big_big.pgn")
	reader := strings.NewReader(pgn)

	dec := NewPGNDecoder(reader)
	count := 0

	for {
		count++
		game, err := dec.Decode()
		if errors.Is(err, io.EOF) {
			break
		}
		t.Run(fmt.Sprintf("bigbig pgn : %d", count), func(t *testing.T) {
			if err != nil {
				t.Fatalf("fail to read games from valid pgn: %s", err.Error())
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

	game, err := NewPGNDecoder(reader).Decode()
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

	if game.MoveTree().MainLine()[0].Comments() != "" {
		t.Fatalf("game move 1 is not correct, expected no comment, got %s", game.MoveTree().MainLine()[0].Comments())
	}

	// print all moves
	moves := game.MoveTree().MainLine()

	if got, ok := game.MoveTree().MainLine()[0].GetCommand("eval"); !ok || got != "0.17" {
		t.Fatalf("game move 1 is not correct, expected eval, got %s", got)
	}

	if moves[6].Comments() != "A57 Benko Gambit Declined: Main Line" {
		t.Fatalf("game move 4 is not correct, expected comment, got %s", moves[6].Comments())
	}

	if got := moves[44].NAGs(); !reflect.DeepEqual(got, []string{"$6"}) {
		t.Fatalf("game move 44 is not correct, expected NAG '$6', got %v", got)
	}
}

func TestLichessMultipleCommand(t *testing.T) {
	file, err := os.Open(filepath.Join("fixtures/pgns", "lichess_multiple_command.pgn"))
	if err != nil {
		t.Fatalf("Failed to open fixture file: %v", err)
	}

	dec := NewPGNDecoder(file)

	// Test first game
	game, err := dec.Decode()
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
	if got, ok := game.MoveTree().MainLine()[0].GetCommand("eval"); !ok || got != "0.0" {
		t.Fatalf("game move 1 is not correct, expected eval, got %s", got)
	}

	// Check for clock also
	if got, ok := game.MoveTree().MainLine()[0].GetCommand("clk"); !ok || got != "0:03:00" {
		t.Fatalf("game move 1 is not correct, expected clock, got %s", got)
	}

	// Check move 5 for comment and eval
	if game.MoveTree().MainLine()[4].Comments() != "E00 Catalan Opening" {
		t.Fatalf("game move 5 is not correct, expected comment, got %s", game.MoveTree().MainLine()[4].Comments())
	}

	if got, ok := game.MoveTree().MainLine()[4].GetCommand("eval"); !ok || got != "0.14" {
		t.Fatalf("game move 5 is not correct, expected eval, got %s", got)
	}

	// check for clock
	if got, ok := game.MoveTree().MainLine()[4].GetCommand("clk"); !ok || got != "0:02:58" {
		t.Fatalf("game move 5 is not correct, expected clock, got %s", got)
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	moves := game.MoveTree().MainLine()
	if len(moves) < 4 {
		t.Fatalf("expected at least 4 moves, got %d", len(moves))
	}

	if len(moves[0].NAGs()) == 0 || moves[0].Comments() == "" {
		t.Errorf("move 1 should have both NAG and comment, got nags: '%v', comment: '%s'", moves[0].NAGs(), moves[0].Comments())
	}
	if len(moves[1].NAGs()) == 0 || moves[1].Comments() == "" {
		t.Errorf("move 2 should have both NAG and comment, got nags: '%v', comment: '%s'", moves[1].NAGs(), moves[1].Comments())
	}
	if len(moves[2].NAGs()) == 0 || moves[2].Comments() == "" {
		t.Errorf("move 3 should have both NAG and comment, got nags: '%v', comment: '%s'", moves[2].NAGs(), moves[2].Comments())
	}
	if len(moves[3].NAGs()) == 0 || moves[3].Comments() == "" {
		t.Errorf("move 4 should have both NAG and comment, got nags: '%v', comment: '%s'", moves[3].NAGs(), moves[3].Comments())
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Check main line comment on 1. e4
	mainMoves := game.MoveTree().MainLine()
	if mainMoves[0].Comments() != "main line comment" {
		t.Errorf("expected main line comment on e4, got %q", mainMoves[0].Comments())
	}

	// 1... e5 is mainMoves[1], and its parent (rootMove child for e4) should have
	// a second child which is the variation 1... d5
	e4Move := game.MoveTree().Root().children[0] // 1. e4
	if len(e4Move.children) < 2 {
		t.Fatalf("expected at least 2 children on e4 (main line e5 + variation d5), got %d", len(e4Move.children))
	}

	// First child is the main line 1... e5
	// Second child is the variation 1... d5
	d5Move := e4Move.children[1]
	if d5Move.Comments() != "variation comment on d5" {
		t.Errorf("expected 'variation comment on d5' on 1...d5, got %q", d5Move.Comments())
	}

	// d5's first child should be 2. exd5
	if len(d5Move.children) == 0 {
		t.Fatalf("expected children on d5 variation move")
	}
	exd5Move := d5Move.children[0]
	if exd5Move.Comments() != "variation comment on exd5" {
		t.Errorf("expected 'variation comment on exd5' on 2.exd5, got %q", exd5Move.Comments())
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Find the variation: 1... d5
	e4Move := game.MoveTree().Root().children[0]
	if len(e4Move.children) < 2 {
		t.Fatalf("expected variation on e4, got %d children", len(e4Move.children))
	}

	d5Move := e4Move.children[1]
	if got := d5Move.NAGs(); !reflect.DeepEqual(got, []string{"$1"}) {
		t.Errorf("expected NAG '$1' on 1...d5, got %v", got)
	}
	if d5Move.Comments() != "great move" {
		t.Errorf("expected comment 'great move' on 1...d5, got %q", d5Move.Comments())
	}

	if len(d5Move.children) == 0 {
		t.Fatalf("expected children on d5")
	}
	exd5Move := d5Move.children[0]
	if got := exd5Move.NAGs(); !reflect.DeepEqual(got, []string{"$6"}) {
		t.Errorf("expected NAG '$6' on 2.exd5, got %v", got)
	}

	if len(exd5Move.children) == 0 {
		t.Fatalf("expected children on exd5")
	}
	qxd5Move := exd5Move.children[0]
	if got := qxd5Move.NAGs(); !reflect.DeepEqual(got, []string{"$2"}) {
		t.Errorf("expected NAG '$2' on 2...Qxd5, got %v", got)
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	e4Move := game.MoveTree().Root().children[0]
	if len(e4Move.children) < 2 {
		t.Fatalf("expected variation on e4, got %d children", len(e4Move.children))
	}

	d5Move := e4Move.children[1]
	if d5Move.Comments() != "good move" {
		t.Errorf("expected comment 'good move' on 1...d5, got %q", d5Move.Comments())
	}
	if got, ok := d5Move.GetCommand("eval"); !ok || got != "-0.5" {
		t.Errorf("expected eval command '-0.5' on 1...d5, got %q", got)
	}
	if got, ok := d5Move.GetCommand("clk"); !ok || got != "0:05:00" {
		t.Errorf("expected clk command '0:05:00' on 1...d5, got %q", got)
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Main line: 3. Bb5 should have comment "Ruy Lopez"
	mainMoves := game.MoveTree().MainLine()
	// Moves: e4, e5, Nf3, Nc6, Bb5, a6 => index 4 is Bb5
	if len(mainMoves) < 5 {
		t.Fatalf("expected at least 5 main line moves, got %d", len(mainMoves))
	}
	bb5Move := mainMoves[4]
	if bb5Move.Comments() != "Ruy Lopez" {
		t.Errorf("expected 'Ruy Lopez' comment on 3.Bb5, got %q", bb5Move.Comments())
	}

	// Variation: 3. Bc4 should have comment "Italian Game"
	nc6Move := mainMoves[3] // parent of Bb5 and Bc4
	if len(nc6Move.children) < 2 {
		t.Fatalf("expected variation at move 3, got %d children", len(nc6Move.children))
	}
	bc4Move := nc6Move.children[1]
	if bc4Move.Comments() != "Italian Game" {
		t.Errorf("expected 'Italian Game' comment on 3.Bc4, got %q", bc4Move.Comments())
	}

	// Nested variation: 3... Bc5 should have comment "Giuoco Piano"
	if len(bc4Move.children) < 2 {
		t.Fatalf("expected nested variation on Bc4, got %d children", len(bc4Move.children))
	}
	bc5Move := bc4Move.children[1]
	if bc5Move.Comments() != "Giuoco Piano" {
		t.Errorf("expected 'Giuoco Piano' comment on 3...Bc5, got %q", bc5Move.Comments())
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("failed to parse input pgn: %v", err)
	}

	var walk func(*MoveNode)
	walk = func(m *MoveNode) {
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
	walk(game.MoveTree().Root())

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
	move := game.MoveTree().MainLine()[0]
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
	blocks := reparsed.MoveTree().MainLine()[0].CommentBlocks()
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
	move := reparsed.MoveTree().MainLine()[0]

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
	move := game.MoveTree().MainLine()[0]

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

	e4 := reparsed.MoveTree().MainLine()[0]
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

	dec := NewPGNDecoder(strings.NewReader(roundTrip), WithPGNExpandVariations())
	for {
		if _, err := dec.Decode(); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			t.Fatalf("expanded annotated variation should parse: %v", err)
		}
	}
}

func TestPGNAnnotationFidelityLegacyAPIsAndDefensiveCopies(t *testing.T) {
	game := NewGame()
	if _, err := game.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	move := game.MoveTree().MainLine()[0]
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
	parsedMove := parsed.MoveTree().MainLine()[0]
	blocks := parsedMove.CommentBlocks()
	blocks[0].Items[0].Text = "mutated"
	blocks[0].Items = append(blocks[0].Items, CommentItem{Kind: CommentCommand, Key: "eval", Value: "9"})

	blocks = parsedMove.CommentBlocks()
	assertCommentItem(t, blocks[0].Items[0], CommentText, "Good", "", "")
	if strings.Contains(parsed.String(), "mutated") || strings.Contains(parsed.String(), "[%eval 9]") {
		t.Fatalf("mutating CommentBlocks result changed move state: %s", parsed.String())
	}
}

// TestPGNCommentCommandMergingIssue104 is a regression test for issue #104:
// a parsed text comment followed by SetCommand must merge into a single
// comment block rather than emitting two separate {} blocks. v3's
// SetCommand appends to the last block when no matching key exists, so
// this pins the single-block contract. See the reporter's v2 scenario.
func TestPGNCommentCommandMergingIssue104(t *testing.T) {
	game := mustParseSingleGame(t, withMinimalTags(`1. e4 {Ruy Lopez} *`))
	move := game.MoveTree().MainLine()[0]
	move.SetCommand("eval", "0.25")

	roundTrip := game.String()
	want := "{Ruy Lopez [%eval 0.25]}"
	if !strings.Contains(roundTrip, want) {
		t.Fatalf("expected merged single block %q in:\n%s", want, roundTrip)
	}
	// The two-block failure mode would look like "{Ruy Lopez} {[%eval 0.25]}".
	if strings.Contains(roundTrip, "{Ruy Lopez} {") {
		t.Fatalf("expected single merged block, got two separate blocks:\n%s", roundTrip)
	}
}

func mustParseSingleGame(t *testing.T, pgn string) *Game {
	t.Helper()
	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
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

	t.Run("ParseMoveJoinsMismatchReasons", func(t *testing.T) {
		// Position: white rook on a1 and bishop on c1, both can reach e1.
		// Asking for "Nxe1" (knight captures e1) should fail with reasons
		// from both candidates joined into a single ParserError.
		opt, err := FEN("1k6/8/8/8/8/8/8/1BR4K w - - 0 1")
		if err != nil {
			t.Fatalf("FEN: %v", err)
		}
		g := NewGame(opt)
		parser := NewParser([]Token{
			{Type: PIECE, Value: "N"},
			{Type: CAPTURE, Value: "x"},
			{Type: SQUARE, Value: "e1"},
		})
		parser.game = g
		_, err = parser.parseMove()
		if err == nil {
			t.Fatal("expected parser error for Nxe1")
		}
		var pe *ParserError
		if !errors.As(err, &pe) {
			t.Fatalf("expected *ParserError, got %T: %v", err, err)
		}
		if !strings.Contains(pe.Message, "piece type mismatch") {
			t.Errorf("expected joined message to contain 'piece type mismatch', got %q", pe.Message)
		}
	})

	t.Run("DuplicateCommandName", func(t *testing.T) {
		parser := NewParser([]Token{
			{Type: CommandStart, Value: "[%"},
			{Type: CommandName, Value: "clk"},
			{Type: CommandParam, Value: "0:05:00"},
			{Type: CommandName, Value: "eval"},
			{Type: CommandParam, Value: "0.24"},
			{Type: CommandEnd, Value: "]"},
		})
		_, err := parser.parseCommand()
		if err == nil {
			t.Fatal("expected parseCommand error for duplicate command name")
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
	if err := parser.resolveOutcome(); err != nil {
		t.Fatal(err)
	}
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

	game, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %v", err)
	}

	// Helper to recursively check move numbers
	var checkMoveNumbers func(m *MoveNode, expectedNum int)
	checkMoveNumbers = func(m *MoveNode, expectedNum int) {
		fullMove := (expectedNum-1)/2 + 1
		for _, child := range m.children {
			if child.number != 0 && child.Ply() != expectedNum {
				t.Errorf("move %s: expected move number %d, got %d", child.Move().String(), expectedNum, child.Ply())
			}
			if child.FullMoveNumber() != fullMove {
				t.Errorf("move %s: expected full move number %d, got %d", child.Move().String(), fullMove, child.FullMoveNumber())
			}
			// If this move starts a variation, the move number should be correct for the variation
			checkMoveNumbers(child, expectedNum+1)
		}
	}

	t.Logf("Root move number: %d", game.MoveTree().Root().number)
	t.Logf("Root move ply: %d", game.MoveTree().Root().Ply())
	t.Logf("Root move full move number: %d", game.MoveTree().Root().FullMoveNumber())
	t.Logf("Second move: %v", game.MoveTree().Root().Children()[0])

	// Mainline starts at move 1
	checkMoveNumbers(game.MoveTree().Root(), 1)

	// Check specific variation
	mainMoves := game.MoveTree().MainLine()
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

	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		_, err := NewPGNDecoder(strings.NewReader(pgn)).Decode()
		if err != nil {
			b.Fatalf("parse error: %v", err)
		}
	}
}

// readPGNFixture returns the raw bytes of a fixture PGN file.
func readPGNFixture(name string) []byte {
	data, err := os.ReadFile(filepath.Join("fixtures", "pgns", name))
	if err != nil {
		panic(err)
	}
	return data
}

// pgnFixtureMeta reports size and game count for a fixture file.
type pgnFixtureMeta struct {
	path    string
	size    int64
	games   int
	perGame int // approximate bytes per game
}

func pgnMeta(name string) pgnFixtureMeta {
	data := readPGNFixture(name)
	games := bytes.Count(data, []byte("[Event "))
	return pgnFixtureMeta{
		path:    filepath.Join("fixtures", "pgns", name),
		size:    int64(len(data)),
		games:   games,
		perGame: len(data) / max(games, 1),
	}
}

// BenchmarkPGN_Stream_Big parses the 1000-game lichess big.pgn fixture once per
// iteration via the streaming PGNDecoder API. Equivalent in workload to the
// chess-library example (full SAN validation + move tree building).
func BenchmarkPGN_Stream_Big(b *testing.B) {
	benchStreamFixture(b, "big.pgn")
}

// BenchmarkPGN_Stream_BigBig parses the 10000-game lichess big_big.pgn fixture.
func BenchmarkPGN_Stream_BigBig(b *testing.B) {
	benchStreamFixture(b, "big_big.pgn")
}

// BenchmarkPGN_Stream_Variations parses a small PGN rich in variations to
// exercise the variation path separately.
func BenchmarkPGN_Stream_Variations(b *testing.B) {
	pgn := []byte(`[Event "Variation Benchmark"]
[Site "Internet"]
[Date "2023.12.06"]
[Round "1"]
[White "Player1"]
[Black "Player2"]
[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 (3. Bc4 Nf6 4. d3) a6 4. Ba4 Nf6 5. O-O Be7 1-0`)

	b.SetBytes(int64(len(pgn)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		dec := NewPGNDecoder(bytes.NewReader(pgn))
		for {
			_, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				b.Fatalf("parse error: %v", err)
			}
		}
	}
}

func benchStreamFixture(b *testing.B, name string) {
	b.Helper()
	data := readPGNFixture(name)
	meta := pgnMeta(name)
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		dec := NewPGNDecoder(bytes.NewReader(data))
		games := 0
		errs := 0
		for {
			_, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				// Real-world lichess data has ~0.7% games with a result
				// inconsistent with the board-derivable outcome (e.g. "Black
				// wins on time" when the final position is checkmate).
				// Skip those rather than aborting the benchmark.
				errs++
				continue
			}
			games++
		}
		if games+errs != meta.games {
			b.Fatalf("expected %d games total, parsed %d with %d errors", meta.games, games, errs)
		}
	}
}
