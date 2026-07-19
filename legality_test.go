package chess

import (
	"reflect"
	"testing"
)

// TestLegalityPinnedPiece pins a queen to the king on the same file as a
// rook: a move off the pin file exposes the king.
func TestLegalityPinnedPiece(t *testing.T) {
	// White king on e1, white queen on e4, black rook on e8. The queen
	// blocks the rook's attack on the e-file. Moving the queen off the
	// file exposes the king to the rook; capturing the rook is legal.
	pos := mustPosition(t, "4r3/8/8/8/4Q3/8/8/4K3 w - - 0 1")

	lg := newLegality(pos, generateLegalAnnotated)

	mOff := Move{s1: E4, s2: D4}
	if _, ok := lg.legal(mOff); ok {
		t.Errorf("queen off the pin file must be illegal, got legal")
	}

	mCapture := Move{s1: E4, s2: E8}
	tag, ok := lg.legal(mCapture)
	if !ok {
		t.Errorf("queen capturing the pinning rook must be legal, got illegal")
	}
	if tag&Capture == 0 {
		t.Errorf("queen capturing the pinning rook must carry the Capture tag, got tag=%b", tag)
	}
}

// TestLegalitySingleCheck covers the "capture or step out of check" rule
// when the side to move is in single check.
func TestLegalitySingleCheck(t *testing.T) {
	// White king on e1 in single check from black rook on e2. The king can
	// capture the checker (e2 not defended by the black king on e8), and
	// can step to d1 or f1 (off the rook's rank/file). d2 and f2 lie on
	// the rook's rank-2 attack and must be rejected.
	pos := mustPosition(t, "4k3/8/8/8/8/8/4r3/4K3 w - - 0 1")

	lg := newLegality(pos, generateLegalAnnotated)

	cases := []struct {
		name string
		move Move
		want bool
	}{
		{"capture the checker", Move{s1: E1, s2: E2}, true},
		{"step out to d1", Move{s1: E1, s2: D1}, true},
		{"step out to f1", Move{s1: E1, s2: F1}, true},
		{"step into rook's rank (d2)", Move{s1: E1, s2: D2}, false},
		{"step into rook's rank (f2)", Move{s1: E1, s2: F2}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, ok := lg.legal(tc.move)
			if ok != tc.want {
				t.Errorf("legal(%s) ok=%v, want %v", tc.move, ok, tc.want)
			}
		})
	}
}

// TestLegalityDoubleCheck restricts non-King moves in a double check.
func TestLegalityDoubleCheck(t *testing.T) {
	// White king on e1 in double check from a knight on c2 and a rook on e8.
	// No single interposition can block both — only king moves are legal.
	pos := mustPosition(t, "4r3/8/8/8/8/8/2nP4/4K3 w - - 0 1")

	lg := newLegality(pos, generateLegalOnly)

	mKing := Move{s1: E1, s2: D1}
	if _, ok := lg.legal(mKing); !ok {
		t.Errorf("king move in double check must be legal, got illegal")
	}

	// A pawn push cannot resolve either checker; it must be rejected.
	mPawn := Move{s1: D2, s2: D3}
	if _, ok := lg.legal(mPawn); ok {
		t.Errorf("non-king move in double check must be illegal, got legal")
	}
}

// TestLegalityEnPassantDiscoveredCheck: a pawn en passant capture that
// opens a discovered check on the moving side's king must be illegal.
func TestLegalityEnPassantDiscoveredCheck(t *testing.T) {
	// White king on h5, white pawn on f5, black pawn on g5 (just moved from
	// g7-g5, giving EP target g6), black rook on a5. Before the move the
	// f5 pawn shields the king from the rook along rank 5. En-passant
	// f5xg6 removes BOTH the f5 pawn (moves to g6) and the g5 pawn (captured),
	// opening rank 5 and exposing the king to the rook.
	pos := mustPosition(t, "7k/8/8/r4PpK/8/8/8/8 w - g6 0 1")

	lg := newLegality(pos, generateLegalAnnotated)

	mEP := Move{s1: F5, s2: G6}
	_, ok := lg.legal(mEP)
	if ok {
		t.Errorf("en passant that exposes own king must be illegal, got legal")
	}
}

// TestLegalityAnnotatesCheck covers the generateLegalAnnotated mode:
// legal moves that give check carry the Check tag.
func TestLegalityAnnotatesCheck(t *testing.T) {
	// White queen on d5 with a clear line to the black king on h5; the
	// Qd5-h5 diagonal has no blockers.
	pos := mustPosition(t, "7k/8/8/3Q4/8/8/8/K7 w - - 0 1")

	lg := newLegality(pos, generateLegalAnnotated)

	mCheck := Move{s1: D5, s2: H5}
	tag, ok := lg.legal(mCheck)
	if !ok {
		t.Errorf("Qd5-h5 must be legal, got illegal")
	}
	if tag&Check == 0 {
		t.Errorf("Qd5-h5 must carry the Check tag, got tag=%b", tag)
	}
}

// TestLegalityNotMutable is the regression fence for the no-mutation
// invariant. legality must never touch any Position field, including the
// scalars (moveCount, halfMoveClock, enPassantSquare) that a leaked
// applyMove-style mutation would corrupt.
func TestLegalityNotMutable(t *testing.T) {
	fens := []string{
		startFEN,                              // quiet
		"4r3/8/8/8/4Q3/8/8/4K3 w - - 0 1",     // pinned piece
		"4k3/8/8/8/8/8/4r3/4K3 w - - 0 1",     // single check
		"4r3/8/8/8/8/8/2nP4/4K3 w - - 0 1",    // double check
		"7k/8/8/r4PpK/8/8/8/8 w - g6 0 1",     // ep discovered check
		"7k/8/8/3Q4/8/8/8/K7 w - - 0 1",       // gives check
		"4k3/8/8/8/8/8/4PPP1/4K1R1 w - - 0 1", // castling candidate
	}
	moves := []Move{
		{s1: E2, s2: E4},
		{s1: G1, s2: F3},
		{s1: E1, s2: G1},
	}
	for _, fen := range fens {
		for _, mode := range []moveGenerationMode{generateLegalOnly, generateLegalAnnotated, generateUnsafeOnly} {
			pos := mustPosition(t, fen)
			before := posSnapshot(pos)
			lg := newLegality(pos, mode)
			for _, m := range moves {
				_, _ = lg.legal(m)
			}
			after := posSnapshot(pos)
			if !reflect.DeepEqual(before, after) {
				t.Errorf("legality mutated pos for fen=%q mode=%v\nbefore=%+v\nafter=%+v",
					fen, mode, before, after)
			}
		}
	}
}

type posFields struct {
	board           Board
	turn            Color
	castleRights    CastleRights
	enPassantSquare Square
	inCheck         bool
	validMoves      []Move
	status          Method
	statusCached    bool
	hash            uint64
	moveCount       int
	halfMoveClock   int
}

func posSnapshot(pos *Position) posFields {
	return posFields{
		board:           pos.board,
		turn:            pos.turn,
		castleRights:    pos.castleRights,
		enPassantSquare: pos.enPassantSquare,
		inCheck:         pos.inCheck,
		validMoves:      pos.validMoves,
		status:          pos.status,
		statusCached:    pos.statusCached,
		hash:            pos.hash,
		moveCount:       pos.moveCount,
		halfMoveClock:   pos.halfMoveClock,
	}
}
