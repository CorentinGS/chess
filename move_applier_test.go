package chess

import (
	"testing"
)

// TestComputeMoveEffect pins down the single interpretation of a move's
// physical facts (en-passant captured square, castle rook squares, capture
// target, landing piece). Board.update and updateHash both read this struct
// instead of each re-reading MoveTags; this test is the focused guard so any
// future drift in the descriptor surfaces here, not at perft.
func TestComputeMoveEffect(t *testing.T) {
	cases := []struct {
		name string
		fen  string
		m    Move
		want moveEffect
	}{
		{
			name: "quiet pawn push",
			fen:  "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1",
			m:    Move{s1: E2, s2: E4},
			want: moveEffect{
				moving:   WhitePawn,
				landing:  WhitePawn,
				capSq:    NoSquare,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "normal capture",
			fen:  "3k4/8/8/4Q3/8/8/8/4K3 w - - 0 1",
			m:    Move{s1: E5, s2: D8, tags: Capture},
			want: moveEffect{
				moving:   WhiteQueen,
				landing:  WhiteQueen,
				capPiece: BlackKing,
				capSq:    D8,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "white en passant captures pawn at s2-8",
			fen:  "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1",
			m:    Move{s1: E5, s2: D6, tags: EnPassant | Capture},
			want: moveEffect{
				moving:   WhitePawn,
				landing:  WhitePawn,
				capPiece: BlackPawn,
				capSq:    D5,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "black en passant captures pawn at s2+8",
			fen:  "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1",
			m:    Move{s1: D4, s2: E3, tags: EnPassant | Capture},
			want: moveEffect{
				moving:   BlackPawn,
				landing:  BlackPawn,
				capPiece: WhitePawn,
				capSq:    E4,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "white king-side castle",
			fen:  "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1",
			m:    Move{s1: E1, s2: G1, tags: KingSideCastle},
			want: moveEffect{
				moving:   WhiteKing,
				landing:  WhiteKing,
				capSq:    NoSquare,
				rookFrom: H1,
				rookTo:   F1,
			},
		},
		{
			name: "white queen-side castle",
			fen:  "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1",
			m:    Move{s1: E1, s2: C1, tags: QueenSideCastle},
			want: moveEffect{
				moving:   WhiteKing,
				landing:  WhiteKing,
				capSq:    NoSquare,
				rookFrom: A1,
				rookTo:   D1,
			},
		},
		{
			name: "black king-side castle",
			fen:  "r3k2r/8/8/8/8/8/8/4K3 b KQ - 0 1",
			m:    Move{s1: E8, s2: G8, tags: KingSideCastle},
			want: moveEffect{
				moving:   BlackKing,
				landing:  BlackKing,
				capSq:    NoSquare,
				rookFrom: H8,
				rookTo:   F8,
			},
		},
		{
			name: "black queen-side castle",
			fen:  "r3k2r/8/8/8/8/8/8/4K3 b KQ - 0 1",
			m:    Move{s1: E8, s2: C8, tags: QueenSideCastle},
			want: moveEffect{
				moving:   BlackKing,
				landing:  BlackKing,
				capSq:    NoSquare,
				rookFrom: A8,
				rookTo:   D8,
			},
		},
		{
			name: "promotion to queen (no capture)",
			fen:  "4k3/P7/8/8/8/8/8/4K3 w - - 0 1",
			m:    Move{s1: A7, s2: A8, promo: Queen},
			want: moveEffect{
				moving:   WhitePawn,
				landing:  WhiteQueen,
				capSq:    NoSquare,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "promotion-capture",
			fen:  "3rk3/4P3/8/8/8/8/8/4K3 w - - 0 1",
			m:    Move{s1: E7, s2: D8, promo: Queen, tags: Capture},
			want: moveEffect{
				moving:   WhitePawn,
				landing:  WhiteQueen,
				capPiece: BlackRook,
				capSq:    D8,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
		{
			name: "degenerate move with empty origin returns zero effect",
			fen:  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
			m:    Move{s1: E4, s2: E4},
			want: moveEffect{capSq: NoSquare, rookFrom: NoSquare, rookTo: NoSquare},
		},
		{
			name: "en passant tag with occupied destination falls through to normal capture",
			fen:  "4k3/8/8/3q4/4P3/8/8/4K3 w - d6 0 1",
			m:    Move{s1: E4, s2: D5, tags: EnPassant | Capture},
			want: moveEffect{
				moving:   WhitePawn,
				landing:  WhitePawn,
				capPiece: BlackQueen,
				capSq:    D5,
				rookFrom: NoSquare,
				rookTo:   NoSquare,
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			pos, err := decodeFEN(tc.fen)
			if err != nil {
				t.Fatalf("FEN decode: %v", err)
			}
			got := computeMoveEffect(&pos.board, tc.m)
			if got.moving != tc.want.moving ||
				got.landing != tc.want.landing ||
				got.capPiece != tc.want.capPiece ||
				got.capSq != tc.want.capSq ||
				got.rookFrom != tc.want.rookFrom ||
				got.rookTo != tc.want.rookTo {
				t.Errorf("computeMoveEffect(%s, %s):\n  got  %+v\n  want %+v",
					tc.fen, tc.m, got, tc.want)
			}
		})
	}
}

// TestUnsafeTransitionConsistency guards the two invariants that unsafe moves
// through Position.Update (and thus Game.UnsafeMove / Game.UnsafePushMoveText)
// must still uphold:
//
//   - Special-move effects (en-passant captured square, castle rook squares)
//     are driven by the moving piece's color, not by Position.turn. Otherwise
//     an unsafe castling or en-passant move played against the wrong turn
//     produces rook moves / captures on the wrong rank and silently corrupts
//     bitboards and mailbox.
//   - Capturing onto a square occupied by a friendly piece (which Game.Move
//     rejects) is applied without leaving whiteSqs/blackSqs inconsistent with
//     the piece bitboards. The capture removal happens before the mover's
//     aggregate occupancy shifts to s2, so the same square can be cleared and
//     re-added under the mover's color without an aggregate-occupancy gap.
func TestUnsafeTransitionConsistency(t *testing.T) {
	t.Run("black king-side castle with white nominally to move moves H8 rook, not H1", func(t *testing.T) {
		opt, err := FEN("r3k2r/8/8/8/8/8/8/4K3 w KQkq - 0 1")
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(opt)
		if _, err := g.UnsafeMove(Move{s1: E8, s2: G8, tags: KingSideCastle}, nil); err != nil {
			t.Fatalf("UnsafeMove() error = %v", err)
		}
		b := g.Position().Board()
		if err := verifyMailboxConsistency(b); err != nil {
			t.Fatalf("board inconsistent: %v", err)
		}
		if got := b.Piece(F8); got != BlackRook {
			t.Errorf("piece on F8 = %v, want BlackRook", got)
		}
		if got := b.Piece(H8); got != NoPiece {
			t.Errorf("piece on H8 = %v, want empty (rook moved off)", got)
		}
	})

	t.Run("black en passant tagged while white to move still removes pawn at s2+8", func(t *testing.T) {
		opt, err := FEN("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1")
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(opt)
		// Move the white pawn to D6 with an EnPassant tag. The descriptor must
		// derive capSq from the moving piece's color (white), so the captured
		// pawn sits on D5 regardless of the position's nominal turn.
		if _, err := g.UnsafeMove(Move{s1: E5, s2: D6, tags: EnPassant | Capture}, nil); err != nil {
			t.Fatalf("UnsafeMove() error = %v", err)
		}
		b := g.Position().Board()
		if err := verifyMailboxConsistency(b); err != nil {
			t.Fatalf("board inconsistent: %v", err)
		}
		if got := b.Piece(D5); got != NoPiece {
			t.Errorf("piece on D5 = %v, want empty (e.p. captured)", got)
		}
	})

	t.Run("king moving onto friendly square preserves mailbox invariants", func(t *testing.T) {
		g := NewGame()
		// Startpos: white king on E1, white pawn on E2. UnsafeMove E1->E2 is
		// illegal but unsafe APIs accept it. The king should end up on E2,
		// with the original pawn gone and the white aggregate occupancy
		// consistent with the mailbox and per-piece bitboards.
		if _, err := g.UnsafeMove(Move{s1: E1, s2: E2}, nil); err != nil {
			t.Fatalf("UnsafeMove() error = %v", err)
		}
		b := g.Position().Board()
		if err := verifyMailboxConsistency(b); err != nil {
			t.Fatalf("board inconsistent: %v", err)
		}
		if got := b.Piece(E2); got != WhiteKing {
			t.Errorf("piece on E2 = %v, want WhiteKing", got)
		}
		if got := b.Piece(E1); got != NoPiece {
			t.Errorf("piece on E1 = %v, want empty (king left)", got)
		}
	})

	t.Run("en passant tag with occupied destination captures the piece on s2, not s2±8", func(t *testing.T) {
		opt, err := FEN("4k3/8/8/3q4/4P3/8/8/4K3 w - d6 0 1")
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(opt)
		// White pawn on E4 pushing to D5 (where the black queen sits) but
		// tagged as en-passant. Real e.p. would clear D5's adjacent square
		// instead; a misuse of the tag must demote to a normal capture
		// rather than ghost the queen in the per-piece bitboards.
		if _, err := g.UnsafeMove(Move{s1: E4, s2: D5, tags: EnPassant | Capture}, nil); err != nil {
			t.Fatalf("UnsafeMove() error = %v", err)
		}
		b := g.Position().Board()
		if err := verifyMailboxConsistency(b); err != nil {
			t.Fatalf("board inconsistent: %v", err)
		}
		if got := b.Piece(D5); got != WhitePawn {
			t.Errorf("piece on D5 = %v, want WhitePawn", got)
		}
		// The queen's bitboard must reflect the capture; the incremental
		// hash should match a full recompute.
		pre := g.Position()
		if got, want := pre.ZobristHash(), pre.computeHash(); got != want {
			t.Errorf("hash drift: incremental=%x recompute=%x", got, want)
		}
	})
}

// TestApplyMoveDifferential runs the copy-on-write applier (Position.Update) and
// the in-place applier (Position.makeMove) in lockstep over the canonical perft
// positions and asserts:
//   - the two paths reach byte-identical FENs after every move, and
//   - on the copy-on-write path, the incremental Zobrist hash equals a full
//     recompute at every node walked.
//
// The hash guard is the depth-multiplied form of TestZobristHashIncrementalCorrectness:
// now that Board.update and updateHash share the moveEffect descriptor, this
// is a structural check (drift is impossible) rather than a regression check,
// but we still surface it at depth so a future change to either consumer would
// fail loudly here rather than silently corrupt transposition tables.
//
// What the FEN comparison guards: perft node counts are blind to the half-move
// clock and full-move number, so a drift in those fields would not change node
// counts yet would corrupt the 50/75-move draw rules. Position.String emits
// the full FEN, which includes both counters, so this comparison is the guard
// against that drift class.
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
			// Two independent positions with their own *Board: cow advances
			// immutably via Update (which copies the board each call), while
			// inplace mutates and restores via make/unmakeMove.
			cow := NewGame(opt).Position()
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

		// On the copy-on-write path, the incremental hash from updateHash
		// must equal the full recompute at every node. The inplace path
		// leaves the hash stale by design, so we only check the cow side.
		if got, want := nextCow.ZobristHash(), nextCow.computeHash(); got != want {
			t.Errorf("hash drift after %s at remaining depth %d: incremental=%x recompute=%x",
				m, depth, got, want)
			inplace.unmakeMove(undo)
			return true
		}

		if depth > 1 {
			walkLockstep(t, nextCow, inplace, depth-1)
		}
		inplace.unmakeMove(undo)
		return false
	})
}
