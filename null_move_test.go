package chess_test

import (
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

// Null move specification
//
// A null move flips the side to move without moving any piece. It is used in
// engines for null-move pruning and in PGN annotations to indicate a side
// passed. The standard PGN format does not define null moves, so this package
// emits "Z0" (the convention used by ChessBase and Scid) and accepts several
// common spellings when reading.
//
// Notation contract:
//   - SAN encode       : "Z0"
//   - Long/UCI encode  : "0000"
//   - SAN decode       : "Z0", "Z1", "--", "@@"
//   - UCI decode       : "0000"
//
// Validation contract:
//   - Null moves are never part of a position's legal moves (ValidMoves).
//   - Game.Move() rejects a null move.
//   - Game.UnsafeMove(NullMove()) and Game.NullMove() accept it.
//
// State changes after a null move:
//   - Turn flips.
//   - halfMoveClock increments by 1 (treated like a quiet move).
//   - moveCount increments by 1 when Black was the side to move.
//   - en-passant square is cleared (the capture right expires).
//   - castling rights are unchanged.
//   - pieces are unchanged.
//   - inCheck is recomputed for the new side to move.
//   - Zobrist hash reflects the new turn and cleared en-passant.

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------

func TestNullMove_ConstructorHasNullTag(t *testing.T) {
	m := chess.NewNullMove()
	if !m.HasTag(chess.Null) {
		t.Fatalf("NullMove() must carry the Null MoveTag")
	}
}

// ------------------------------------------------------------------
// Notation encoding
// ------------------------------------------------------------------

func TestNullMove_SANEncode(t *testing.T) {
	pos := chess.StartingPosition()
	got := chess.AlgebraicNotation{}.Encode(pos, chess.NewNullMove())
	if got != "Z0" {
		t.Fatalf("SAN Encode = %q, want %q", got, "Z0")
	}
}

func TestNullMove_LongAlgebraicEncode(t *testing.T) {
	pos := chess.StartingPosition()
	got := chess.LongAlgebraicNotation{}.Encode(pos, chess.NewNullMove())
	if got != "0000" {
		t.Fatalf("LongAlgebraic Encode = %q, want %q", got, "0000")
	}
}

func TestNullMove_UCIEncode(t *testing.T) {
	pos := chess.StartingPosition()
	got := chess.UCINotation{}.Encode(pos, chess.NewNullMove())
	if got != "0000" {
		t.Fatalf("UCI Encode = %q, want %q", got, "0000")
	}
}

// ------------------------------------------------------------------
// Notation decoding
// ------------------------------------------------------------------

func TestNullMove_SANDecode(t *testing.T) {
	pos := chess.StartingPosition()
	cases := []string{"Z0", "Z1", "--", "@@"}
	for _, s := range cases {
		t.Run(s, func(t *testing.T) {
			m, err := chess.AlgebraicNotation{}.Decode(pos, s)
			if err != nil {
				t.Fatalf("SAN Decode(%q) error: %v", s, err)
			}
			if !m.HasTag(chess.Null) {
				t.Fatalf("SAN Decode(%q) returned move without Null tag", s)
			}
		})
	}
}

func TestNullMove_UCIDecode(t *testing.T) {
	pos := chess.StartingPosition()
	m, err := chess.UCINotation{}.Decode(pos, "0000")
	if err != nil {
		t.Fatalf("UCI Decode error: %v", err)
	}
	if !m.HasTag(chess.Null) {
		t.Fatalf("UCI Decode of '0000' must return null move")
	}
}

func TestNullMove_LongAlgebraicDecode(t *testing.T) {
	pos := chess.StartingPosition()
	m, err := chess.LongAlgebraicNotation{}.Decode(pos, "0000")
	if err != nil {
		t.Fatalf("LongAlgebraic Decode error: %v", err)
	}
	if !m.HasTag(chess.Null) {
		t.Fatalf("LongAlgebraic Decode of '0000' must return null move")
	}
}

// ------------------------------------------------------------------
// Position.Update behaviour
// ------------------------------------------------------------------

func TestNullMove_UpdateFlipsTurn(t *testing.T) {
	pos := chess.StartingPosition()
	if pos.Turn() != chess.White {
		t.Fatalf("starting position Turn=%v, want White", pos.Turn())
	}
	next := pos.Update(chess.NewNullMove())
	if next.Turn() != chess.Black {
		t.Fatalf("after null move, Turn=%v, want Black", next.Turn())
	}
	// Original is immutable.
	if pos.Turn() != chess.White {
		t.Fatalf("Update must not mutate the receiver; Turn=%v", pos.Turn())
	}
}

func TestNullMove_UpdateFlipsTurnTwice(t *testing.T) {
	pos := chess.StartingPosition()
	pos = pos.Update(chess.NewNullMove())
	pos = pos.Update(chess.NewNullMove())
	if pos.Turn() != chess.White {
		t.Fatalf("after two null moves, Turn=%v, want White", pos.Turn())
	}
}

func TestNullMove_UpdateIncrementsHalfMoveClock(t *testing.T) {
	pos := chess.StartingPosition()
	if pos.HalfMoveClock() != 0 {
		t.Fatalf("starting halfMoveClock=%d, want 0", pos.HalfMoveClock())
	}
	next := pos.Update(chess.NewNullMove())
	if next.HalfMoveClock() != 1 {
		t.Fatalf("after null move halfMoveClock=%d, want 1", next.HalfMoveClock())
	}
}

func TestNullMove_UpdateDoesNotChangeBoard(t *testing.T) {
	pos := chess.StartingPosition()
	next := pos.Update(chess.NewNullMove())
	if next.Board().Draw() != pos.Board().Draw() {
		t.Fatalf("board changed after null move:\nbefore=%s\nafter=%s",
			pos.Board().Draw(), next.Board().Draw())
	}
}

func TestNullMove_UpdatePreservesCastleRights(t *testing.T) {
	pos := chess.StartingPosition()
	next := pos.Update(chess.NewNullMove())
	if next.CastleRights() != pos.CastleRights() {
		t.Fatalf("castle rights changed: before=%q after=%q",
			pos.CastleRights(), next.CastleRights())
	}
}

func TestNullMove_UpdateClearsEnPassant(t *testing.T) {
	fen := "rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
	fenOpt, err := chess.FEN(fen)
	if err != nil {
		t.Fatalf("FEN parse: %v", err)
	}
	g := chess.NewGame(fenOpt)
	pos := g.Position()
	if pos.EnPassantSquare() == chess.NoSquare {
		t.Fatalf("setup: en passant square should be set, got %v", pos.EnPassantSquare())
	}
	next := pos.Update(chess.NewNullMove())
	if next.EnPassantSquare() != chess.NoSquare {
		t.Fatalf("after null move, en passant square = %v, want NoSquare",
			next.EnPassantSquare())
	}
}

func TestNullMove_UpdateKeepsEnPassantCleared(t *testing.T) {
	pos := chess.StartingPosition()
	if pos.EnPassantSquare() != chess.NoSquare {
		t.Fatalf("starting position should have no en passant square")
	}
	next := pos.Update(chess.NewNullMove())
	if next.EnPassantSquare() != chess.NoSquare {
		t.Fatalf("after null move, en passant square should remain NoSquare")
	}
}

func TestNullMove_UpdateChangesHash(t *testing.T) {
	pos := chess.StartingPosition()
	next := pos.Update(chess.NewNullMove())
	if pos.ZobristHash() == next.ZobristHash() {
		t.Fatal("Zobrist hash must change when side to move flips")
	}
	// Hash must be deterministic: another null move from the new position
	// produces the same hash.
	next2 := next.Update(chess.NewNullMove())
	if next2.ZobristHash() != pos.ZobristHash() {
		t.Fatal("two successive null moves must return to the original hash")
	}
}

func TestNullMove_UpdateIncrementsMoveCountAfterBlack(t *testing.T) {
	// Starting with FEN where it's Black to move at move 5.
	fen := "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
	fenOpt, err := chess.FEN(fen)
	if err != nil {
		t.Fatalf("FEN parse: %v", err)
	}
	g := chess.NewGame(fenOpt)
	pos := g.Position()
	if pos.Turn() != chess.Black {
		t.Fatalf("setup: Turn=%v, want Black", pos.Turn())
	}
	// moveCount is not exported; verify via FEN round trip.
	beforeFen := pos.String()
	next := pos.Update(chess.NewNullMove())
	afterFen := next.String()
	if beforeFen == afterFen {
		t.Fatalf("FEN must change after null move\nbefore=%s\nafter=%s",
			beforeFen, afterFen)
	}
	// The "fullmove" component (last token in FEN) must increment by 1
	// because Black's null move ends a full move.
	beforeFull := fenField(beforeFen, 5)
	afterFull := fenField(afterFen, 5)
	if afterFull != beforeFull+1 {
		t.Fatalf("fullmove before=%d after=%d, want +1", beforeFull, afterFull)
	}
}

// ------------------------------------------------------------------
// Game insertion API
// ------------------------------------------------------------------

func TestNullMove_GameMoveRejectsNull(t *testing.T) {
	g := chess.NewGame()
	if err := g.Move(chess.NewNullMove(), nil); err == nil {
		t.Fatal("Game.Move must reject a null move")
	}
}

func TestNullMove_UnsafeMoveAcceptsNull(t *testing.T) {
	g := chess.NewGame()
	if err := g.UnsafeMove(chess.NewNullMove(), nil); err != nil {
		t.Fatalf("Game.UnsafeMove(NullMove()) error: %v", err)
	}
	if g.Position().Turn() != chess.Black {
		t.Fatalf("after null move Turn=%v, want Black", g.Position().Turn())
	}
}

func TestNullMove_GameNullMoveMethod(t *testing.T) {
	g := chess.NewGame()
	node, err := g.NullMove()
	if err != nil {
		t.Fatalf("Game.NullMove error: %v", err)
	}
	if node == nil {
		t.Fatal("Game.NullMove returned nil node")
	}
	if !node.Move().HasTag(chess.Null) {
		t.Fatal("node move must carry the Null tag")
	}
	if g.Position().Turn() != chess.Black {
		t.Fatalf("after Game.NullMove Turn=%v, want Black", g.Position().Turn())
	}
}

func TestNullMove_AppearsInMoveHistory(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.NullMove(); err != nil {
		t.Fatalf("Game.NullMove: %v", err)
	}
	history := g.MoveHistory()
	if len(history) != 1 {
		t.Fatalf("len(MoveHistory)=%d, want 1", len(history))
	}
	if !history[0].Move.HasTag(chess.Null) {
		t.Fatal("MoveHistory entry must carry Null tag")
	}
}

// ------------------------------------------------------------------
// ValidMoves contract: a null move is not legal.
// ------------------------------------------------------------------

func TestNullMove_NeverInValidMoves(t *testing.T) {
	g := chess.NewGame()
	for _, m := range g.ValidMoves() {
		if m.HasTag(chess.Null) {
			t.Fatal("ValidMoves must never return a null move")
		}
	}
}

// ------------------------------------------------------------------
// PGN read: token recognition
// ------------------------------------------------------------------

func TestNullMove_PGNRead_Z0(t *testing.T) {
	// After "1. e4" it's Black's turn; Black plays Z0 (pass), then it's
	// White's turn at move 2; White plays Nf3.
	pgn := withMinimalTags("1. e4 Z0 2. Nf3 *")
	g := mustParseSingleGame(t, pgn)
	moves := g.Moves()
	if len(moves) != 3 {
		t.Fatalf("len(Moves)=%d, want 3 (e4, Z0, Nf3)", len(moves))
	}
	if !moves[1].HasTag(chess.Null) {
		t.Fatal("second move must be a null move")
	}
	if g.Position().Turn() != chess.Black {
		t.Fatalf("after 1.e4 Z0 2.Nf3, Turn=%v, want Black", g.Position().Turn())
	}
}

func TestNullMove_PGNRead_DoubleDash(t *testing.T) {
	// '--' must be parsed as a null move.
	pgn := withMinimalTags("1. e4 -- 2. Nf3 *")
	g := mustParseSingleGame(t, pgn)
	moves := g.Moves()
	if len(moves) != 3 {
		t.Fatalf("len(Moves)=%d, want 3", len(moves))
	}
	if !moves[1].HasTag(chess.Null) {
		t.Fatal("'--' must be parsed as a null move")
	}
}

func TestNullMove_PGNRead_AtStart(t *testing.T) {
	pgn := withMinimalTags("1. Z0 e5 *")
	g := mustParseSingleGame(t, pgn)
	moves := g.Moves()
	if len(moves) != 2 {
		t.Fatalf("len(Moves)=%d, want 2", len(moves))
	}
	if !moves[0].HasTag(chess.Null) {
		t.Fatal("first move must be a null move")
	}
}

func TestNullMove_PGNRead_TwoConsecutiveNulls(t *testing.T) {
	// White passes, Black passes: side to move is back to White at move 2.
	pgn := withMinimalTags("1. Z0 Z0 *")
	g := mustParseSingleGame(t, pgn)
	moves := g.Moves()
	if len(moves) != 2 {
		t.Fatalf("len(Moves)=%d, want 2", len(moves))
	}
	for i, m := range moves {
		if !m.HasTag(chess.Null) {
			t.Fatalf("move %d must be null", i)
		}
	}
	if g.Position().Turn() != chess.White {
		t.Fatalf("after 1.Z0 Z0, Turn=%v, want White", g.Position().Turn())
	}
}

// ------------------------------------------------------------------
// PGN write + read round trip
// ------------------------------------------------------------------

func TestNullMove_PGNWrite(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.NullMove(); err != nil {
		t.Fatalf("NullMove: %v", err)
	}
	if err := g.PushMove("e5", nil); err != nil {
		t.Fatalf("PushMove e5: %v", err)
	}
	rendered := g.String()
	if !strings.Contains(rendered, " Z0 ") {
		t.Fatalf("rendered PGN should contain Z0, got:\n%s", rendered)
	}
}

func TestNullMove_PGNWriteReadRoundTrip(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.NullMove(); err != nil {
		t.Fatalf("NullMove: %v", err)
	}
	if err := g.PushMove("e5", nil); err != nil {
		t.Fatalf("PushMove e5: %v", err)
	}
	if err := g.PushMove("Nf3", nil); err != nil {
		t.Fatalf("PushMove Nf3: %v", err)
	}
	if _, err := g.NullMove(); err != nil {
		t.Fatalf("NullMove (tail): %v", err)
	}
	rendered := g.String()

	g2, err := chess.PGN(strings.NewReader(rendered))
	if err != nil {
		t.Fatalf("re-parse: %v", err)
	}
	loaded := chess.NewGame(g2)
	moves := loaded.Moves()
	if len(moves) != 4 {
		t.Fatalf("len(Moves)=%d, want 4", len(moves))
	}
	if !moves[0].HasTag(chess.Null) {
		t.Fatal("round-tripped first move must be null")
	}
	if !moves[3].HasTag(chess.Null) {
		t.Fatal("round-tripped last move must be null")
	}
	// Z0 e5 Nf3 Z0: White pass, Black e5, White Nf3, Black pass -> White to move.
	if loaded.Position().Turn() != chess.White {
		t.Fatalf("after round-trip, Turn=%v, want White", loaded.Position().Turn())
	}
}

// ------------------------------------------------------------------
// helpers
// ------------------------------------------------------------------

// fenField splits a FEN string and returns the 0-indexed field as an int.
func fenField(fen string, idx int) int {
	parts := strings.Split(fen, " ")
	if idx >= len(parts) {
		return 0
	}
	n := 0
	for _, c := range parts[idx] {
		if c < '0' || c > '9' {
			return 0
		}
		n = n*10 + int(c-'0')
	}
	return n
}
