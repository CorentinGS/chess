package chess

import "testing"

// TestApplyMoveDifferential runs the copy-on-write applier (Position.Update) and
// the in-place applier (Position.makeMove) in lockstep over the canonical perft
// positions and asserts they reach byte-identical positions after every move.
//
// Perft node counts are blind to the half-move clock and full-move number, so a
// drift in those fields would not change node counts (the existing perft suite
// would stay green) yet would corrupt the 50/75-move draw rules. Position.String
// emits the full FEN, which includes both counters, so this comparison is the
// guard against that drift class. The hash is intentionally excluded: Update
// computes it incrementally while makeMove leaves it stale, and FEN does not
// encode it.
func TestApplyMoveDifferential(t *testing.T) {
	const depth = 3
	fens := []struct{ name, fen string }{
		{"startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"},
		{"kiwipete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"},
		{"pos3", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"},
		{"pos4", "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"},
		{"pos5", "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"},
		{"pos6", "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"},
	}

	for _, c := range fens {
		t.Run(c.name, func(t *testing.T) {
			opt, err := FEN(c.fen)
			if err != nil {
				t.Fatalf("FEN decode: %v", err)
			}
			root := NewGame(opt).Position()
			// Two independent positions with their own *Board: cow advances via
			// Update (COW, never mutates its receiver), while inplace mutates and
			// restores via make/unmakeMove. They must not share a board pointer.
			cow := root
			opt2, err := FEN(c.fen)
			if err != nil {
				t.Fatalf("FEN decode (inplace): %v", err)
			}
			inplace := NewGame(opt2).Position()
			walkLockstep(t, cow, inplace, depth)
		})
	}
}

func walkLockstep(t *testing.T, cow, inplace *Position, depth int) {
	visitLegalMoves(cow, generateLegalOnly, func(m Move) bool {
		nextCow := cow.Update(m)
		undo := inplace.makeMove(m)

		if nextCow.String() != inplace.String() {
			t.Errorf("FEN drift after %s at remaining depth %d:\n  Update  : %s\n  makeMove: %s",
				m, depth, nextCow.String(), inplace.String())
			inplace.unmakeMove(undo)
			return true // stop iterating this node
		}

		if depth > 1 {
			walkLockstep(t, nextCow, inplace, depth-1)
		}
		inplace.unmakeMove(undo)
		return false
	})
}
