package chess_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNResultTagSetsOutcomeWhenNoMovetextToken(t *testing.T) {
	pgn := `[Result "1-0"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 *`

	opt, err := chess.PGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatal(err)
	}
	g := chess.NewGame(opt)

	if g.Outcome() != chess.WhiteWon {
		t.Errorf("outcome = %s, want WhiteWon (from Result tag)", g.Outcome())
	}
	if g.Method() != chess.NoMethod {
		t.Errorf("method = %s, want NoMethod (Method not set by tag alone)", g.Method())
	}
}

func TestPGNMovetextTokenWinsOverResultTag(t *testing.T) {
	pgn := `1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 1-0`

	opt, err := chess.PGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatal(err)
	}
	g := chess.NewGame(opt)

	if g.Outcome() != chess.WhiteWon {
		t.Errorf("outcome = %s, want WhiteWon (movetext token only)", g.Outcome())
	}
}

func TestPGNTagTokenConflictReturnsError(t *testing.T) {
	pgn := `[Result "0-1"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 1-0`

	_, err := chess.PGN(strings.NewReader(pgn))
	var pe *chess.ParserError
	if !errors.As(err, &pe) {
		t.Fatalf("expected *ParserError on tag-vs-token conflict, got %T (%v)", err, err)
	}
}

func TestPGNBoardCheckmateOverridesResultTag(t *testing.T) {
	pgn := `[Result "0-1"]

1. e4 e5 2. Bc4 Nc6 3. Qh5 Nf6?? 4. Qxf7# 1-0`

	_, err := chess.PGN(strings.NewReader(pgn))
	var pe *chess.ParserError
	if !errors.As(err, &pe) {
		t.Fatalf("expected *ParserError on board-vs-tag mismatch, got %T (%v)", err, err)
	}
}
