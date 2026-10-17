package uci_test

import (
	"testing"
	"time"

	"github.com/corentings/chess/v3"
	"github.com/corentings/chess/v3/uci"
)

// #29: Engine bestmove was decoded with a nil position, so it lost tags like
// KingSideCastle and SAN encoded as "Kg1" instead of "O-O".
// https://github.com/CorentinGS/chess/issues/29
func TestRegressionIssue29_EngineBestMoveHasCastleTag(t *testing.T) {
	fake := &fakeAdapter{
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
