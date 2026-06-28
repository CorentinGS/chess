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
	f, err := chess.FEN("r3k2r/8/8/8/8/8/8/4K2R b KQkq - 0 1")
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
	alg := chess.AlgebraicNotation{}
	if san := alg.Encode(g.Position(), results.BestMove); san != "O-O" {
		t.Fatalf("SAN = %q, want %q", san, "O-O")
	}
}

// #34: UCINotation.Decode did not tag king/queen-side castles, so downstream
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
			uciDec := chess.UCINotation{}
			m, err := uciDec.Decode(g.Position(), tt.uci)
			if err != nil {
				t.Fatalf("decode %q: %v", tt.uci, err)
			}
			if !m.HasTag(tt.want) {
				t.Fatalf("%s: missing tag %v (got move=%s)", tt.uci, tt.want, m)
			}
			alg := chess.AlgebraicNotation{}
			if san := alg.Encode(g.Position(), m); san != tt.sanOut {
				t.Fatalf("%s: SAN = %q, want %q", tt.uci, san, tt.sanOut)
			}
		})
	}
}