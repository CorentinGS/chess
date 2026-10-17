package chess_test

import (
	"strings"
	"testing"
	"time"

	"github.com/corentings/chess/v3"
	"github.com/corentings/chess/v3/uci"
)

// This file adds black-box regression tests for closed GitHub issues that the
// existing test suite did not already cover. Each test exercises only the
// public API of github.com/corentings/chess/v3 and /v3/uci, and pins the
// behavior promised in the original issue's resolution so any v3 regression
// fails loudly.

// #76: When parsing a game that ends in checkmate from PGN, the game method
// was NoMethod instead of Checkmate.
// https://github.com/CorentinGS/chess/issues/76
func TestRegressionIssue76_PGNCheckmateMethod(t *testing.T) {
	g, err := chess.ParsePGN(strings.NewReader("1. f4 e5 2. g4 Qh4# 0-1"))
	if err != nil {
		t.Fatalf("ParsePGN failed: %v", err)
	}
	if got := g.Outcome(); got != chess.BlackWon {
		t.Fatalf("Outcome() = %v, want %v", got, chess.BlackWon)
	}
	if got := g.Method(); got != chess.Checkmate {
		t.Fatalf("Method() = %v, want %v", got, chess.Checkmate)
	}
}

// #29: Engine bestmove was decoded with a nil position, so it lost tags like
// KingSideCastle and SAN encoded as "Kg1" instead of "O-O".
// https://github.com/CorentinGS/chess/issues/29
func TestRegressionIssue29_EngineBestMoveHasCastleTag(t *testing.T) {
	fake := &uci.FakeAdapter{
		Responses: map[string][]string{
			"go": {"info depth 10 score cp 50 nodes 1000 pv e8g8", "bestmove e8g8"},
		},
	}
	eng := uci.NewWithAdapter(fake)
	defer eng.Close()

	// Black king-side castle is legal: king on e8, rook on h8, both squares
	// f8/g8 empty and not attacked. White rook on h1 targets h8 — irrelevant
	// to castling through g8/f8 because castling rules check attacks on
	// e8/f8/g8 only.
	f, err := chess.FEN("r3k2r/8/8/8/8/8/8/4K2R b kq - 0 1")
	if err != nil {
		t.Fatal(err)
	}
	g := chess.NewGame(f)
	if err := eng.Run(uci.CmdPosition{Position: g.Position()}, uci.CmdGo{MoveTime: 100 * time.Millisecond}); err != nil {
		t.Fatalf("Run failed: %v", err)
	}
	results := eng.SearchResults()
	if !results.BestMove.HasTag(chess.KingSideCastle) {
		t.Fatalf("expected KingSideCastle tag on engine best move e8g8, got move=%s",
			results.BestMove)
	}
	san, err := chess.SAN().Encode(g.Position(), results.BestMove)
	if err != nil {
		t.Fatalf("SAN encode failed: %v", err)
	}
	if san != "O-O" {
		t.Fatalf("SAN = %q, want %q", san, "O-O")
	}
}

// #34: UCI decode did not tag king/queen-side castles, so downstream
// SAN encoding was wrong.
// https://github.com/CorentinGS/chess/issues/34
func TestRegressionIssue34_UCINotationDecodeTagsCastle(t *testing.T) {
	for _, tt := range []struct {
		name   string
		fen    string
		uci    string
		want   chess.MoveTag
		sanOut string
	}{
		{
			name:   "black_kingside",
			fen:    "rnb1k2r/ppp3pp/4pn2/3q4/3P4/2P2N2/P1PB1PPP/R2QKB1R b KQkq - 3 8",
			uci:    "e8g8",
			want:   chess.KingSideCastle,
			sanOut: "O-O",
		},
		{
			name:   "black_queenside",
			fen:    "r3kbnr/pppq1ppp/2npb3/3p4/3P4/2NPB3/PPPQ1PPP/R3KBNR b KQkq - 4 6",
			uci:    "e8c8",
			want:   chess.QueenSideCastle,
			sanOut: "O-O-O",
		},
	} {
		t.Run(tt.name, func(t *testing.T) {
			f, err := chess.FEN(tt.fen)
			if err != nil {
				t.Fatal(err)
			}
			g := chess.NewGame(f)
			m, err := chess.UCI().Decode(g.Position(), tt.uci)
			if err != nil {
				t.Fatalf("decode %q: %v", tt.uci, err)
			}
			if !m.HasTag(tt.want) {
				t.Fatalf("%s: missing tag %v (got move=%s)", tt.uci, tt.want, m)
			}
			san, err := chess.SAN().Encode(g.Position(), m)
			if err != nil {
				t.Fatalf("%s: SAN encode error: %v", tt.uci, err)
			}
			if san != tt.sanOut {
				t.Fatalf("%s: SAN = %q, want %q", tt.uci, san, tt.sanOut)
			}
		})
	}
}

// #104: Inside a variation, alternatives to the variation's next move were
// written twice, once before that move with the wrong move number, so
// String() produced PGN that could not be re-parsed.
// https://github.com/CorentinGS/chess/issues/104
func TestRegressionIssue104_NestedVariationRoundTrip(t *testing.T) {
	tests := []struct {
		name     string
		movetext string
	}{
		{"alternative to second move of variation", "1. e4 (1. d4 d5 (1... Nf6) 2. c4) 1... e5 *"},
		{"alternative to second move, deeper", "1. e4 e5 2. Nf3 (2. Nc3 Nf6 (2... Nc6 3. Bc4) 3. f4) 2... Nc6 *"},
		{"nested at depth two", "1. e4 (1. d4 d5 (1... Nf6 2. c4 (2. Nf3 g6 (2... e6) 3. g3)) 2. c4) 1... e5 *"},
		{"multiple alternatives", "1. e4 (1. d4 d5 (1... Nf6) (1... f5) 2. c4) 1... e5 *"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			in := "[Event \"x\"]\n\n" + tt.movetext + "\n"
			game, err := chess.ParsePGN(strings.NewReader(in))
			if err != nil {
				t.Fatalf("parse input: %v", err)
			}

			out := game.String()
			gotMovetext := strings.TrimSpace(out[strings.LastIndex(out, "\n\n")+2:])
			if gotMovetext != tt.movetext {
				t.Errorf("movetext not preserved\nwant: %s\ngot:  %s", tt.movetext, gotMovetext)
			}

			if _, err := chess.ParsePGN(strings.NewReader(out)); err != nil {
				t.Errorf("re-parse output: %v\noutput: %s", err, out)
			}
		})
	}
}

// #104: The issue's reproducer. A study with comments and nested variations,
// annotated with SetCommand, must still expand to every variation after a
// String() round trip.
// https://github.com/CorentinGS/chess/issues/104
func TestRegressionIssue104_AnnotatedVariationsRoundTripExpanded(t *testing.T) {
	const pgn = `[Event "Test"]
[Site "?"]
[Date "2024.01.01"]
[Round "?"]
[White "White"]
[Black "Black"]
[Result "*"]

1. e4 {Best by test.} c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 Nf6 5. Nc3 a6 6. g3 {A quiet line.} (6. Be3 {Active.} e5 (6... e6) 7. Nf3 (7. Nb3 Be6 8. f3 {English Attack.}) 7... Be7 8. Bc4 O-O 9. O-O Be6) 6... e5 (6... e6 7. Bg2 Be7 8. O-O Qc7 9. Be3 O-O) 7. Nde2 (7. Nb3 Be7 8. Bg2 (8. a4 Nc6) 8... O-O 9. O-O b5) (7. Nf3 Be7 8. Bg2 O-O 9. O-O b5) (7. Nf5 d5) 7... Be7 8. Bg2 (8. a4 Nc6) 8... O-O 9. O-O b5 10. Nd5 Nbd7 11. Nec3 Nb6 *
`

	countExpanded := func(pgn string) int {
		n := 0
		for _, err := range chess.PGNGames(strings.NewReader(pgn), chess.WithPGNExpandVariations()) {
			if err != nil {
				t.Fatalf("parse expanded game %d: %v\n%s", n+1, err, pgn)
			}
			n++
		}
		return n
	}

	want := countExpanded(pgn)
	if want != 10 {
		t.Fatalf("input expands to %d games, want 10", want)
	}

	game, err := chess.ParsePGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatalf("parse input: %v", err)
	}
	var annotate func(n *chess.MoveNode)
	annotate = func(n *chess.MoveNode) {
		n.SetCommand("eval", "0.25")
		for _, child := range n.Children() {
			annotate(child)
		}
	}
	annotate(game.MoveTree().Root())

	if got := countExpanded(game.String()); got != want {
		t.Errorf("round trip expands to %d games, want %d", got, want)
	}
}
