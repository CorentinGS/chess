package chess

import (
	"strings"
	"testing"
)

// rank1BackRank returns the piece types on rank 1 indexed by file 0..7.
func rank1BackRank(b *Board) [8]PieceType {
	var r [8]PieceType
	for f := range 8 {
		r[f] = b.Piece(NewSquare(File(f), Rank1)).Type()
	}
	return r
}

func TestChess960SetupAllIndicesValid(t *testing.T) {
	seen := make(map[string]struct{}, 960)
	for i := range 960 {
		s, err := Chess960Setup(i)
		if err != nil {
			t.Fatalf("Chess960Setup(%d): unexpected error: %v", i, err)
		}
		if s.Variant != Chess960 {
			t.Errorf("index %d: Variant = %v, want Chess960", i, s.Variant)
		}
		pos, err := NewPosition(s)
		if err != nil {
			t.Errorf("index %d: NewPosition failed: %v", i, err)
			continue
		}

		// Two rooks on rank 1 with the king strictly between them, and the two
		// bishops on opposite-colored squares.
		var kingFile, bishopA, bishopB int = -1, -1, -1
		var rookFiles [2]int
		ri := 0
		for f := range 8 {
			pt := pos.Board().Piece(NewSquare(File(f), Rank1)).Type()
			switch pt {
			case King:
				kingFile = f
			case Rook:
				if ri >= 2 {
					t.Fatalf("index %d: more than two rooks on rank 1", i)
				}
				rookFiles[ri] = f
				ri++
			case Bishop:
				if bishopA < 0 {
					bishopA = f
				} else {
					bishopB = f
				}
			}
		}
		if ri != 2 {
			t.Errorf("index %d: want two rooks on rank 1, got %d", i, ri)
			continue
		}
		if !(rookFiles[0] < kingFile && kingFile < rookFiles[1]) {
			t.Errorf("index %d: king file %d not strictly between rooks %d and %d", i, kingFile, rookFiles[0], rookFiles[1])
		}
		if bishopA%2 == bishopB%2 {
			t.Errorf("index %d: bishops on files %d and %d are not on opposite colors", i, bishopA, bishopB)
		}

		// Every index must map to a unique starting arrangement.
		key := pos.Board().String()
		if _, dup := seen[key]; dup {
			t.Errorf("index %d: duplicate starting arrangement %q", i, key)
		}
		seen[key] = struct{}{}
	}
	if len(seen) != 960 {
		t.Errorf("unique starts = %d, want 960", len(seen))
	}
}

func TestChess960SetupIndex518IsStandard(t *testing.T) {
	s, err := Chess960Setup(518)
	if err != nil {
		t.Fatalf("Chess960Setup(518): %v", err)
	}
	pos, err := NewPosition(s)
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	std := StartingPosition()
	want := rank1BackRank(std.Board())
	got := rank1BackRank(pos.Board())
	// SP-518 is the standard arrangement R N B Q K B N R.
	if got != want {
		t.Errorf("SP-518 rank 1 = %v, want standard %v", got, want)
	}
}

func TestChess960SetupOutOfRange(t *testing.T) {
	for _, idx := range []int{-1, 960, 1000} {
		if _, err := Chess960Setup(idx); err == nil {
			t.Errorf("Chess960Setup(%d): want error, got nil", idx)
		}
	}
}

func TestChess960SetupFullBoardShape(t *testing.T) {
	// A starting position has a mirrored black back rank on rank 8 and pawns
	// on ranks 2 and 7.
	pos, err := NewPosition(mustChess960Setup(t, 0))
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	for f := range 8 {
		w := pos.Board().Piece(NewSquare(File(f), Rank1))
		b := pos.Board().Piece(NewSquare(File(f), Rank8))
		if w.Type() != b.Type() {
			t.Errorf("file %d: white %v != black %v on mirrored back ranks", f, w, b)
		}
		if pos.Board().Piece(NewSquare(File(f), Rank2)) != WhitePawn {
			t.Errorf("file %d: missing white pawn on rank 2", f)
		}
		if pos.Board().Piece(NewSquare(File(f), Rank7)) != BlackPawn {
			t.Errorf("file %d: missing black pawn on rank 7", f)
		}
	}
}

func mustChess960Setup(t *testing.T, index int) Setup {
	t.Helper()
	s, err := Chess960Setup(index)
	if err != nil {
		t.Fatalf("Chess960Setup(%d): %v", index, err)
	}
	return s
}

// shredderRights builds the Shredder-FEN castling rights string for a starting
// position's board: uppercase file letters for white rooks, lowercase for black.
func shredderRights(b *Board) string {
	var out []byte
	for f := range 8 {
		if b.Piece(NewSquare(File(f), Rank1)) == WhiteRook {
			out = append(out, byte('A'+f))
		}
		if b.Piece(NewSquare(File(f), Rank8)) == BlackRook {
			out = append(out, byte('a'+f))
		}
	}
	if len(out) == 0 {
		return "-"
	}
	return string(out)
}

func TestFENCHess960RightsInferVariant(t *testing.T) {
	for _, idx := range []int{0, 1, 2, 314, 517, 518, 519, 958, 959} {
		s := mustChess960Setup(t, idx)
		shredder := shredderRights(&s.Board)
		fen := s.Board.String() + " w " + shredder + " - 0 1"
		setup, err := decodeFENSetup(fen)
		if err != nil {
			t.Errorf("idx %d: decodeFENSetup(%q): %v", idx, fen, err)
			continue
		}
		if setup.Variant != Chess960 {
			t.Errorf("idx %d: Variant = %v, want Chess960 (rights %q)", idx, setup.Variant, shredder)
		}
		want := NewCastleRights(true, true, true, true)
		if setup.CastleRights != want {
			t.Errorf("idx %d: CastleRights = %v, want full rights (rights %q)", idx, setup.CastleRights, shredder)
		}
	}
}

func TestFENStandardRightsInferStandard(t *testing.T) {
	cases := []string{
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
		"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1",
		"8/8/8/8/8/8/8/4K2k w - - 0 1",
	}
	for _, fen := range cases {
		setup, err := decodeFENSetup(fen)
		if err != nil {
			t.Errorf("decodeFENSetup(%q): %v", fen, err)
			continue
		}
		if setup.Variant != Standard {
			t.Errorf("decodeFENSetup(%q): Variant = %v, want Standard", fen, setup.Variant)
		}
	}
}

func TestFormCastleRightsWithVariantMapping(t *testing.T) {
	// Standard back rank: king e1 (file 4), rooks a1 (0) and h1 (7).
	std := StartingPosition()
	board := std.Board()
	cr, variant, err := formCastleRightsWithVariant("AH", board)
	if err != nil {
		t.Fatalf("formCastleRightsWithVariant(AH): %v", err)
	}
	if variant != Chess960 {
		t.Errorf("variant = %v, want Chess960", variant)
	}
	// File A (0) < king file (4) => queenside; file H (7) > king file => kingside.
	if !cr.CanCastle(White, QueenSide) || !cr.CanCastle(White, KingSide) {
		t.Errorf("white rights = %v, want both sides", cr)
	}
	if cr.CanCastle(Black, KingSide) || cr.CanCastle(Black, QueenSide) {
		t.Errorf("black rights set unexpectedly: %v", cr)
	}

	// '-' yields no rights and Standard.
	cr, variant, err = formCastleRightsWithVariant("-", board)
	if err != nil || variant != Standard || cr != (CastleRights{}) {
		t.Errorf("'-' => cr=%v variant=%v err=%v, want empty/Standard/nil", cr, variant, err)
	}

	// Invalid character is rejected.
	if _, _, err := formCastleRightsWithVariant("AX", board); err == nil {
		t.Errorf("formCastleRightsWithVariant(AX): want error, got nil")
	}
}

func TestChess960FENRoundTrip(t *testing.T) {
	for i := range 960 {
		pos, err := NewPosition(mustChess960Setup(t, i))
		if err != nil {
			t.Fatalf("idx %d: NewPosition: %v", i, err)
		}
		fen := pos.String()
		pos2, err := decodeFEN(fen)
		if err != nil {
			t.Errorf("idx %d: decodeFEN(%q): %v", i, fen, err)
			continue
		}
		if pos2.Variant() != Chess960 {
			t.Errorf("idx %d: round-trip Variant = %v, want Chess960 (fen %q)", i, pos2.Variant(), fen)
		}
		if pos2.CastleRights() != pos.CastleRights() {
			t.Errorf("idx %d: round-trip rights %v != %v (fen %q)", i, pos2.CastleRights(), pos.CastleRights(), fen)
		}
		if pos2.Board().String() != pos.Board().String() {
			t.Errorf("idx %d: round-trip board changed (fen %q)", i, fen)
		}
	}
}

func TestStandardFENByteIdentical(t *testing.T) {
	want := "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	if got := StartingPosition().String(); got != want {
		t.Errorf("standard String() = %q, want %q", got, want)
	}
}

func TestChess960PerftSP518MatchesStandard(t *testing.T) {
	// SP-518 is the standard starting position; the Chess960 move-generation and
	// application paths must reproduce the canonical perft node counts exactly.
	pos, err := NewPosition(mustChess960Setup(t, 518))
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		depth int
		want  uint64
	}{
		{1, 20},
		{2, 400},
		{3, 8902},
		{4, 197281},
	}
	for _, c := range cases {
		if got := pos.Perft(c.depth); got != c.want {
			t.Errorf("SP-518 perft(%d) = %d, want %d", c.depth, got, c.want)
		}
	}
}

func TestChess960PerftAllStartsNoCrash(t *testing.T) {
	// Exercises the full make/unmake castling path across every indexed start.
	for i := range 960 {
		pos, err := NewPosition(mustChess960Setup(t, i))
		if err != nil {
			t.Fatalf("idx %d: %v", i, err)
		}
		if n := pos.Perft(2); n == 0 {
			t.Errorf("idx %d: perft(2) = 0", i)
		}
	}
}

func TestChess960CastlingKingSide(t *testing.T) {
	// White king on c1, rooks on a1 (queen side) and e1 (king side). King-side
	// castle moves the king c1->g1 and the rook e1->f1; only Shredder rights are
	// present, so this also exercises variant inference.
	pos, err := decodeFEN("4k3/8/8/8/8/8/8/R2KR3 w EA - 0 1")
	if err != nil {
		t.Fatalf("decodeFEN: %v", err)
	}
	if pos.Variant() != Chess960 {
		t.Fatalf("variant = %v, want Chess960", pos.Variant())
	}
	var castle Move
	found := false
	for _, m := range pos.ValidMoves() {
		if m.HasTag(KingSideCastle) {
			castle = m
			found = true
		}
	}
	if !found {
		t.Fatal("no king-side castle move generated")
	}
	if castle.S1() != D1 || castle.S2() != G1 {
		t.Errorf("castle = %s%s, want d1g1", castle.S1(), castle.S2())
	}
	if san := (algebraicNotation{}).Encode(pos, castle); san != "O-O" {
		t.Errorf("SAN = %q, want O-O", san)
	}
	pos2 := pos.Update(castle)
	if pos2.Board().Piece(G1) != WhiteKing {
		t.Errorf("g1 = %v, want WhiteKing", pos2.Board().Piece(G1))
	}
	if pos2.Board().Piece(F1) != WhiteRook {
		t.Errorf("f1 = %v, want WhiteRook", pos2.Board().Piece(F1))
	}
	if pos2.Board().Piece(E1) != NoPiece {
		t.Errorf("e1 = %v, want empty after rook moves", pos2.Board().Piece(E1))
	}
	if pos2.Board().Piece(D1) != NoPiece {
		t.Errorf("d1 = %v, want empty after king moves", pos2.Board().Piece(D1))
	}
	if pos2.CastleRights().CanCastle(White, KingSide) {
		t.Error("king-side right not consumed")
	}
	if pos2.CastleRights().CanCastle(White, QueenSide) {
		t.Error("king move should also consume the queen-side right")
	}
}

func TestChess960PGNRoundTrip(t *testing.T) {
	// Build a Chess960 game from a non-standard start, render to PGN, and
	// reparse: the variant and starting position must survive the round trip.
	pos, err := NewPosition(mustChess960Setup(t, 247))
	if err != nil {
		t.Fatal(err)
	}
	opt, err := FEN(pos.String())
	if err != nil {
		t.Fatalf("FEN option: %v", err)
	}
	g := NewGame(opt)
	if g.Variant() != Chess960 {
		t.Fatalf("game variant = %v, want Chess960", g.Variant())
	}
	pgn := g.String()
	if !strings.Contains(pgn, `[Variant "Chess960"]`) {
		t.Errorf("PGN missing [Variant \"Chess960\"]:\n%s", pgn)
	}
	if !strings.Contains(pgn, `[SetUp "1"]`) {
		t.Errorf("PGN missing [SetUp \"1\"]:\n%s", pgn)
	}
	g2, err := ParsePGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatalf("reparse PGN: %v\n%s", err, pgn)
	}
	if g2.Variant() != Chess960 {
		t.Errorf("round-trip variant = %v, want Chess960", g2.Variant())
	}
}

func TestChess960PGNVariantTagAlias(t *testing.T) {
	// The "Fischer Random" alias must select the Chess960 variant, and a Shredder
	// FEN reconstructs the start.
	pgn := `[Variant "Fischer Random"]` + "\n" +
		`[SetUp "1"]` + "\n" +
		`[FEN "rnbkrqnb/pppppppp/8/8/8/8/PPPPPPPP/RNBKRQNB w AEae - 0 1"]` + "\n\n*\n"
	g, err := ParsePGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatalf("ParsePGN: %v", err)
	}
	if g.Variant() != Chess960 {
		t.Errorf("variant = %v, want Chess960", g.Variant())
	}
}

func TestStandardPGNHasNoVariantTag(t *testing.T) {
	if strings.Contains(NewGame().String(), "Variant") {
		t.Error("standard game PGN should not contain a Variant tag")
	}
}

func TestChess960BinaryRoundTrip(t *testing.T) {
	pos, err := NewPosition(mustChess960Setup(t, 500))
	if err != nil {
		t.Fatal(err)
	}
	data, err := pos.MarshalBinary()
	if err != nil {
		t.Fatalf("MarshalBinary: %v", err)
	}
	if len(data) != 101 {
		t.Errorf("binary size = %d, want 101", len(data))
	}
	var pos2 Position
	if err := pos2.UnmarshalBinary(data); err != nil {
		t.Fatalf("UnmarshalBinary: %v", err)
	}
	if pos2.Variant() != Chess960 {
		t.Errorf("round-trip variant = %v, want Chess960", pos2.Variant())
	}
	if pos2.ZobristHash() != pos.ZobristHash() {
		t.Errorf("hash mismatch: %d != %d", pos2.ZobristHash(), pos.ZobristHash())
	}
}

// TestChess960CastlingEdgeCases covers the geometric case classes the castling
// move generator must handle: a king whose path crosses the rook destination
// (queenside, king h1, rook a1), a stationary rook (kingside, rook already
// on f1), the king-adjacent-to-destination case (king f1, rook h1), and a
// kingside path blocked by a non-castling piece. Positions are built via
// Setup so the board placement is unambiguous; the resulting position's FEN
// still round-trips through decodeFEN, so the same machinery the rest of the
// library uses is exercised.
func TestChess960CastlingEdgeCases(t *testing.T) {
	type want struct {
		legal    bool
		side     MoveTag // KingSideCastle or QueenSideCastle (ignored when !legal)
		kingFrom Square
		kingTo   Square
		rookFrom Square
		rookTo   Square
	}

	cases := []struct {
		name string
		set  Setup
		want want
	}{
		{
			// King h1 with queen-side rook on a1. King's transit h1->c1 covers
			// g1,f1,e1,d1; the rook's destination d1 lies on the king's
			// transit — a king-path-contains-rook-destination crossing.
			name: "king_h1_queenside_crossing",
			set: makeChess960Set(t,
				map[Square]Piece{H1: WhiteKing, A1: WhiteRook, A8: BlackRook, E8: BlackKing},
				NewCastleRights(false, true, false, false),
			),
			want: want{legal: true, side: QueenSideCastle, kingFrom: H1, kingTo: C1, rookFrom: A1, rookTo: D1},
		},
		{
			name: "stationary_rook_kingside",
			set: makeChess960Set(t,
				map[Square]Piece{B1: WhiteKing, F1: WhiteRook, A8: BlackRook, E8: BlackKing},
				NewCastleRights(true, false, false, false),
			),
			want: want{legal: true, side: KingSideCastle, kingFrom: B1, kingTo: G1, rookFrom: F1, rookTo: F1},
		},
		{
			name: "king_adjacent_kingside",
			set: makeChess960Set(t,
				map[Square]Piece{F1: WhiteKing, H1: WhiteRook, A8: BlackRook, E8: BlackKing},
				NewCastleRights(true, false, false, false),
			),
			want: want{legal: true, side: KingSideCastle, kingFrom: F1, kingTo: G1, rookFrom: H1, rookTo: F1},
		},
		{
			// Blocked: bishop on e1 sits on the kingside transit of king a1
			// with the kingside rook on h1; no kingside castle should be legal.
			name: "blocked_kingside_no_castle",
			set: makeChess960Set(t,
				map[Square]Piece{A1: WhiteKing, H1: WhiteRook, E1: WhiteBishop, H8: BlackRook, A8: BlackRook, E8: BlackKing},
				NewCastleRights(true, false, false, false),
			),
			want: want{legal: false, side: KingSideCastle},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			pos, err := NewPosition(tc.set)
			if err != nil {
				t.Fatalf("NewPosition: %v", err)
			}
			if pos.Variant() != Chess960 {
				t.Fatalf("variant = %v, want Chess960", pos.Variant())
			}
			// Round-trip through FEN to exercise the same codecs the library uses.
			fen := pos.String()
			pos, err = decodeFEN(fen)
			if err != nil {
				t.Fatalf("round-trip decodeFEN(%q): %v", fen, err)
			}
			var castle Move
			seen := false
			for _, m := range pos.ValidMoves() {
				if m.HasTag(tc.want.side) {
					castle, seen = m, true
				}
			}
			if !tc.want.legal {
				if seen {
					t.Errorf("castle move generated, want none: %s%s", castle.S1(), castle.S2())
				}
				return
			}
			if !seen {
				// Fall back: scan for either castle tag.
				for _, m := range pos.ValidMoves() {
					if m.HasTag(KingSideCastle) || m.HasTag(QueenSideCastle) {
						castle, seen = m, true
						break
					}
				}
				if !seen {
					t.Fatal("no castle move generated")
				}
			}
			if castle.S1() != tc.want.kingFrom || castle.S2() != tc.want.kingTo {
				t.Errorf("king %s%s, want %s%s", castle.S1(), castle.S2(), tc.want.kingFrom, tc.want.kingTo)
			}
			pos2 := pos.Update(castle)
			if pos2.Board().Piece(tc.want.kingTo) != WhiteKing {
				t.Errorf("%s = %v, want WhiteKing", tc.want.kingTo, pos2.Board().Piece(tc.want.kingTo))
			}
			if pos2.Board().Piece(tc.want.rookTo) != WhiteRook {
				t.Errorf("%s = %v, want WhiteRook", tc.want.rookTo, pos2.Board().Piece(tc.want.rookTo))
			}
			if tc.want.rookFrom != tc.want.rookTo && pos2.Board().Piece(tc.want.rookFrom) != NoPiece {
				t.Errorf("%s = %v, want empty", tc.want.rookFrom, pos2.Board().Piece(tc.want.rookFrom))
			}
		})
	}
}

// makeChess960Set builds a Setup from a piece map (any subset of the board),
// with the castling rights provided, fullmove number set to 1, and variant
// set to Chess960. Rank 1 and rank 8 pieces intended for the opponent must
// be included explicitly in the map.
func makeChess960Set(t *testing.T, pieces map[Square]Piece, rights CastleRights) Setup {
	t.Helper()
	b, err := NewBoard(pieces)
	if err != nil {
		t.Fatalf("makeChess960Set: NewBoard: %v", err)
	}
	return Setup{
		Board:        *b,
		Turn:         White,
		CastleRights: rights,
		EnPassant:    NoSquare,
		FullMoveNo:   1,
		Variant:      Chess960,
	}
}

// TestFormCastleRightsWithVariantRejectsOrphanedFile ensures a Shredder rights
// string whose named file has no rook on the back rank is rejected at parse
// time rather than silently dropping the right. The king on c1 with rooks on
// a1 and e1 cannot have rights "CH" (c1 has no rook, h1 has no rook).
func TestFormCastleRightsWithVariantRejectsOrphanedFile(t *testing.T) {
	pos := StartingPosition()
	for _, rights := range []string{"CH", "DA", "Ax"} {
		if _, _, err := formCastleRightsWithVariant(rights, pos.Board()); err == nil {
			t.Errorf("formCastleRightsWithVariant(%q): want error, got nil", rights)
		}
	}
	if _, _, err := formCastleRightsWithVariant("-", pos.Board()); err != nil {
		t.Errorf("formCastleRightsWithVariant(-): %v", err)
	}
}

// TestUCIEncodeChess960CastleKingToRook verifies that UCI castling in a
// Chess960 position is encoded king-to-rook (e.g. "e1h1"), the form engines
// expect when the UCI_Chess960 option is set. SP-518 is the standard
// arrangement marked Chess960: king e1, rooks a1/h1.
func TestUCIEncodeChess960CastleKingToRook(t *testing.T) {
	pos, err := NewPosition(mustChess960Setup(t, 518))
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	for _, tc := range []struct {
		name string
		m    Move
		want string
	}{
		{"kingside", Move{s1: E1, s2: G1, tags: KingSideCastle}, "e1h1"},
		{"queenside", Move{s1: E1, s2: C1, tags: QueenSideCastle}, "e1a1"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got, err := UCI().Encode(pos, tc.m)
			if err != nil {
				t.Fatalf("Encode: %v", err)
			}
			if got != tc.want {
				t.Errorf("Encode(%v) = %q, want %q", tc.m, got, tc.want)
			}
		})
	}
}

// TestUCIEncodeChess960CastleNonStandardRook checks a Chess960 position whose
// castling rook is not on a1/h1: king c1 with the king-side rook on h1 encodes
// the castle as "c1h1", not the king's destination "c1g1".
func TestUCIEncodeChess960CastleNonStandardRook(t *testing.T) {
	pos, err := NewPosition(makeChess960Set(t, map[Square]Piece{
		C1: WhiteKing, H1: WhiteRook, E8: BlackKing,
	}, NewCastleRights(true, false, false, false)))
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	got, err := UCI().Encode(pos, Move{s1: C1, s2: G1, tags: KingSideCastle})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if got != "c1h1" {
		t.Errorf("Encode = %q, want %q", got, "c1h1")
	}
}

// TestUCIDecodeChess960AcceptsBothCastleForms verifies the codec reads both the
// king-to-rook form ("c1h1", from UCI_Chess960 engines) and the
// king-to-destination form ("c1g1", legacy) as the same canonical castle.
func TestUCIDecodeChess960AcceptsBothCastleForms(t *testing.T) {
	pos, err := NewPosition(makeChess960Set(t, map[Square]Piece{
		C1: WhiteKing, H1: WhiteRook, B8: BlackKing,
	}, NewCastleRights(true, false, false, false)))
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	for _, tc := range []struct {
		name string
		in   string
	}{
		{"king-to-rook", "c1h1"},
		{"king-to-destination", "c1g1"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			m, err := UCI().Decode(pos, tc.in)
			if err != nil {
				t.Fatalf("Decode(%q): %v", tc.in, err)
			}
			if m.S1() != C1 || m.S2() != G1 {
				t.Errorf("Decode(%q) = %v-%v, want c1-g1", tc.in, m.S1(), m.S2())
			}
			if !m.HasTag(KingSideCastle) {
				t.Errorf("Decode(%q): missing KingSideCastle tag", tc.in)
			}
		})
	}
}

// TestUCIEncodeStandardCastleIsKingToDestination guards the standard-variant
// path: even with castling tags, a Standard position emits the king's
// destination square (e1g1), never king-to-rook.
func TestUCIEncodeStandardCastleIsKingToDestination(t *testing.T) {
	pos := unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
	got, err := UCI().Encode(pos, Move{s1: E1, s2: G1, tags: KingSideCastle})
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if got != "e1g1" {
		t.Errorf("Encode = %q, want %q (standard must stay king-to-destination)", got, "e1g1")
	}
}

// TestUCIDecodeChess960QueenSideCastleForms verifies both Chess960 UCI
// spellings resolve a queen-side castle whose rook is left of the king.
func TestUCIDecodeChess960QueenSideCastleForms(t *testing.T) {
	pos, err := NewPosition(makeChess960Set(t, map[Square]Piece{
		B1: WhiteRook, F1: WhiteKing, H8: BlackKing,
	}, NewCastleRights(false, true, false, false)))
	if err != nil {
		t.Fatalf("NewPosition: %v", err)
	}
	for _, tc := range []struct {
		name string
		in   string
	}{
		{"king-to-rook", "f1b1"},
		{"king-to-destination", "f1c1"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			m, err := UCI().Decode(pos, tc.in)
			if err != nil {
				t.Fatalf("Decode(%q): %v", tc.in, err)
			}
			if m.S1() != F1 || m.S2() != C1 {
				t.Errorf("Decode(%q) = %v-%v, want f1-c1", tc.in, m.S1(), m.S2())
			}
			if !m.HasTag(QueenSideCastle) {
				t.Errorf("Decode(%q): missing QueenSideCastle tag", tc.in)
			}
		})
	}
}
