package chess

import (
	"strings"
	"testing"
)

type decodeTest struct {
	Codec MoveTextCodec
	Pos   *Position
	Text  string
}

var invalidDecodeTests = []decodeTest{
	{
		// opening for white
		Codec: SAN(),
		Pos:   unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
		Text:  "e5",
	},
	{
		// http://en.lichess.org/W91M4jms#14
		Codec: SAN(),
		Pos:   unsafeFEN("rn1qkb1r/pp3ppp/2p1pn2/3p4/2PP4/2NQPN2/PP3PPP/R1B1K2R b KQkq - 0 7"),
		Text:  "Nd7",
	},
	{
		// http://en.lichess.org/W91M4jms#17
		Codec: SAN(),
		Pos:   unsafeFEN("r2qk2r/pp1n1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P4PPP/R1B1K2R w KQkq - 1 9"),
		Text:  "O-O-O-O",
	},
	{
		// http://en.lichess.org/W91M4jms#23
		Codec: SAN(),
		Pos:   unsafeFEN("3r1rk1/pp1nqppp/2pbpn2/3p4/2PP4/1PNQPN2/PB3PPP/3RR1K1 b - - 5 12"),
		Text:  "dx4",
	},
	{
		// should not assume pawn for unknown piece type "n"
		Codec: SAN(),
		Pos:   unsafeFEN("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"),
		Text:  "nf3",
	},
	{
		// disambiguation should not allow for this since it is not a capture
		Codec: SAN(),
		Pos:   unsafeFEN("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 2"),
		Text:  "bf4",
	},
}

func TestInvalidDecoding(t *testing.T) {
	for _, test := range invalidDecodeTests {
		if _, err := test.Codec.Decode(test.Pos, test.Text); err == nil {
			t.Fatalf("starting from board\n%s\n expected move notation %s to be invalid", test.Pos.board.Draw(), test.Text)
		}
	}
}

func TestAlgebraicDisambiguation(t *testing.T) {
	tests := []struct {
		name string
		fen  string
		move Move
		want string
	}{
		{
			// Two white rooks, one on a2 and one on d5, both can move to a5
			// (same destination, different file and rank). Standard SAN
			// requires only the file: Raa5 and Rda5.
			name: "file disambiguation, no shared file or rank",
			fen:  "1k6/8/8/3R4/8/8/R7/K7 w - - 0 1",
			move: Move{s1: A2, s2: A5},
			want: "Raa5",
		},
		{
			// Same position, the other rook: should encode Rda5.
			name: "file disambiguation, no shared file or rank, other rook",
			fen:  "1k6/8/8/3R4/8/8/R7/K7 w - - 0 1",
			move: Move{s1: D5, s2: A5},
			want: "Rda5",
		},
	}
	codec := SAN()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := unsafeFEN(tt.fen)
			got, err := codec.Encode(pos, tt.move)
			if err != nil {
				t.Fatalf("Encode() error = %v", err)
			}
			if got != tt.want {
				t.Fatalf("Encode = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAlgebraicNotationDecodeRoundTripsLegalMoves(t *testing.T) {
	positions := []*Position{
		startPos,
		midPos,
		complexPos,
		unsafeFEN("r3k2r/pppq1ppp/2npbn2/3Np3/2B1P3/2N2Q2/PPP2PPP/R3K2R w KQkq - 0 10"),
	}
	codec := SAN()

	for _, pos := range positions {
		for _, move := range pos.ValidMovesUnsafe() {
			encoded, err := codec.Encode(pos, move)
			if err != nil {
				t.Fatalf("Encode(%s) from %s: %v", move, pos, err)
			}
			decoded, err := codec.Decode(pos, encoded)
			if err != nil {
				t.Fatalf("Decode(%q) from %s: %v", encoded, pos, err)
			}
			if decoded.s1 != move.s1 || decoded.s2 != move.s2 || decoded.promo != move.promo {
				t.Fatalf("Decode(%q) = %s, want %s", encoded, decoded, move)
			}
		}
	}
}

func TestEncodeUCINotation(t *testing.T) {
	codec := UCI()
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	move := Move{s1: E2, s2: E4}
	expected := "e2e4"
	result, err := codec.Encode(pos, move)
	if err != nil {
		t.Fatalf("Encode() error = %v", err)
	}
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithPromotion(t *testing.T) {
	codec := UCI()
	pos := unsafeFEN("8/P7/8/8/8/8/8/8 w - - 0 1")
	move := Move{s1: A7, s2: A8, promo: Queen}
	expected := "a7a8q"
	result, err := codec.Encode(pos, move)
	if err != nil {
		t.Fatalf("Encode() error = %v", err)
	}
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithInvalidMove(t *testing.T) {
	codec := UCI()
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	move := Move{s1: E2, s2: E5}
	expected := "e2e5"
	result, err := codec.Encode(pos, move)
	if err != nil {
		t.Fatalf("Encode() error = %v", err)
	}
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestNotationHandlesInvalidPooledStringBuilder(t *testing.T) {
	tests := []struct {
		name string
		run  func() string
		want string
	}{
		{
			name: "uci encode",
			run: func() string {
				s, err := UCI().Encode(nil, Move{s1: E2, s2: E4})
				if err != nil {
					t.Fatalf("UCI().Encode() error = %v", err)
				}
				return s
			},
			want: "e2e4",
		},
		{
			name: "algebraic encode",
			run: func() string {
				pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
				s, err := SAN().Encode(pos, Move{s1: E2, s2: E4})
				if err != nil {
					t.Fatalf("SAN().Encode() error = %v", err)
				}
				return s
			},
			want: "e4",
		},
		{
			name: "form source square",
			run: func() string {
				pos := unsafeFEN("1k6/8/8/3R4/8/8/R7/K7 w - - 0 1")
				return formS1(pos, Move{s1: A2, s2: A5})
			},
			want: "a",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			stringPool.Put("not a string builder")
			if got := tt.run(); got != tt.want {
				t.Fatalf("got %q, want %q", got, tt.want)
			}
		})
	}
}

func TestUCINotationGrammar(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		wantErr bool
	}{
		{
			name:    "invalid UCI notation length",
			input:   "e2e",
			wantErr: true,
		},
		{
			name:    "invalid squares in UCI notation",
			input:   "e9e4",
			wantErr: true,
		},
		{
			name:    "invalid promotion piece",
			input:   "a7a8x",
			wantErr: true,
		},
		{
			name:    "invalid source file character",
			input:   "i1e4",
			wantErr: true,
		},
		{
			name:    "invalid destination file character",
			input:   "e1i4",
			wantErr: true,
		},
		{
			name:    "invalid destination rank character",
			input:   "e1e0",
			wantErr: true,
		},
		{
			name:    "invalid source rank character",
			input:   "e0e4",
			wantErr: true,
		},
	}

	codec := UCI()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := codec.ValidateSyntax(tt.input)
			if (err != nil) != tt.wantErr {
				t.Fatalf("ValidateSyntax() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestUCINotationDecode(t *testing.T) {
	moveWithCheckCapture := Move{s1: D1, s2: D8, tags: Check}.WithTag(Capture)

	tests := []struct {
		name        string
		pos         *Position
		input       string
		want        Move
		wantErr     bool
		expectedPos *Position
	}{
		{
			name:        "valid move without promotion",
			pos:         unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
			input:       "e2e4",
			want:        Move{s1: E2, s2: E4},
			expectedPos: unsafeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"),
			wantErr:     false,
		},
		{
			name:        "valid move with promotion",
			pos:         unsafeFEN("8/P7/8/8/8/8/8/8 w - - 0 1"),
			input:       "a7a8q",
			want:        Move{s1: A7, s2: A8, promo: Queen},
			expectedPos: unsafeFEN("Q7/8/8/8/8/8/8/8 b - - 0 1"),
			wantErr:     false,
		},
		{
			name:        "valid move with capture",
			pos:         unsafeFEN("rnbqkb1r/ppp2ppp/3p1n2/4P3/4P3/2N5/PPP2PPP/R1BQKBNR b KQkq - 0 4"),
			input:       "d6e5",
			want:        Move{s1: D6, s2: E5, tags: Capture},
			wantErr:     false,
			expectedPos: unsafeFEN("rnbqkb1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 5"),
		},
		{
			name:        "valid move with check only",
			pos:         unsafeFEN("rnbqkb1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 5"),
			input:       "f1b5",
			want:        Move{s1: F1, s2: B5, tags: Check},
			expectedPos: unsafeFEN("rnbqkb1r/ppp2ppp/5n2/1B2p3/4P3/2N5/PPP2PPP/R1BQK1NR b KQkq - 1 5"),
			wantErr:     false,
		},
		{
			name:        "valid move with check and capture",
			pos:         unsafeFEN("rnbqkb1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 5"),
			input:       "d1d8",
			want:        moveWithCheckCapture,
			wantErr:     false,
			expectedPos: unsafeFEN("rnbQkb1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1B1KBNR b KQkq - 0 5"),
		},
		{
			name:        "valid move with castle with check",
			pos:         unsafeFEN("r4b1r/ppp3pp/8/4p3/2Pq4/3P1Q2/PP3PPP/1k2K2R w K - 2 19"),
			input:       "e1g1",
			want:        Move{s1: E1, s2: G1, tags: Check | KingSideCastle},
			wantErr:     false,
			expectedPos: unsafeFEN("r4b1r/ppp3pp/8/4p3/2Pq4/3P1Q2/PP3PPP/1k3RK1 b - - 3 19"),
		},
		{
			name:        "valid en passant move with check",
			pos:         unsafeFEN("r3k1r1/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRK1R1 b q f3 0 2"),
			input:       "e4f3",
			want:        Move{s1: E4, s2: F3, tags: Check | EnPassant},
			wantErr:     false,
			expectedPos: unsafeFEN("r3k1r1/pbppqpb1/1pn3p1/7p/1N4n1/1PP2p1N/PB1P2PP/2QRK1R1 w q - 0 3"),
		},
		{
			name:        "valid en passant move",
			pos:         unsafeFEN("rnbqkbnr/ppp2ppp/4p3/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"),
			input:       "e5d6",
			want:        Move{s1: E5, s2: D6, tags: EnPassant},
			wantErr:     false,
			expectedPos: unsafeFEN("rnbqkbnr/ppp2ppp/3Pp3/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3"),
		},
	}

	codec := UCI()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := codec.Decode(tt.pos, tt.input)
			if (err != nil) != tt.wantErr {
				t.Errorf("Decode() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !tt.wantErr && (got.String() != tt.want.String() || got.promo != tt.want.promo || got.tags != tt.want.tags) {
				t.Errorf("Decode() = %v (%d), want %v (%d)", got, got.tags, tt.want, tt.want.tags)
			}

			if !tt.wantErr && tt.expectedPos != nil && tt.pos.Update(got).String() != tt.expectedPos.String() {
				t.Errorf("Decode() position = %v, want %v", tt.pos.Update(got).String(), tt.expectedPos)
			}
		})
	}
}

// Common test positions for consistent benchmarking
var (
	// Initial position
	startPos = unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	// Middle game position
	midPos = unsafeFEN("r1bqk2r/ppp2ppp/2np1n2/2b1p3/2B1P3/2PP1N2/PP3PPP/RNBQK2R w KQkq - 0 6")
	// Complex position with multiple piece interactions
	complexPos = unsafeFEN("r1n1k2r/pP1pqpb1/b3pnp1/2pPN3/1p2P3/2N2Q1p/PP1BBPPP/R3K2R w KQkq c6 0 2")
)

// Test moves for each position
var (
	startMoves = []Move{
		{s1: E2, s2: E4}, // e4
		{s1: G1, s2: F3}, // Nf3
		{s1: B1, s2: C3}, // Nc3
	}
	midMoves = []Move{
		{s1: E1, s2: G1, tags: KingSideCastle},  // O-O
		{s1: F3, s2: E5, tags: Capture},         // Nxe5
		{s1: C4, s2: F7, tags: Check | Capture}, // d4+
	}
	complexMoves = []Move{
		{s1: B7, s2: B8, promo: Knight},                // b8=N
		{s1: B7, s2: A8, promo: Bishop, tags: Capture}, // bxa8=B
		{s1: B7, s2: C8, promo: Rook, tags: Check},     // bxc8=R+
		{s1: D5, s2: C6, tags: EnPassant},              // dxc6

	}
)

func mustEncode(t testing.TB, codec MoveTextCodec, pos *Position, m Move) string {
	t.Helper()
	s, err := codec.Encode(pos, m)
	if err != nil {
		t.Fatalf("Encode(%s) from %s: %v", m, pos, err)
	}
	return s
}

// Benchmarks for UCI notation
func BenchmarkUCIEncode(b *testing.B) {
	codec := UCI()
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		if _, err := codec.Encode(pos, move); err != nil {
			b.Fatalf("Encode error: %v", err)
		}
	}
}

func BenchmarkUCIDecode(b *testing.B) {
	codec := UCI()
	samples := []struct {
		pos  *Position
		text string
	}{
		{startPos, "e2e4"},
		{midPos, "e1g1"},
		{complexPos, "e5f7"},
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		sample := samples[i%len(samples)]
		_, err := codec.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmarks for Algebraic notation
func BenchmarkAlgebraicEncode(b *testing.B) {
	codec := SAN()
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		if _, err := codec.Encode(pos, move); err != nil {
			b.Fatalf("Encode error: %v", err)
		}
	}
}

func BenchmarkAlgebraicDecode(b *testing.B) {
	codec := SAN()
	samples := []struct {
		pos  *Position
		text string
	}{
		{startPos, "e4"},
		{midPos, "O-O"},
		{complexPos, "Nxf7+"},
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		sample := samples[i%len(samples)]
		_, err := codec.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmarks for Long Algebraic notation
func BenchmarkLongAlgebraicEncode(b *testing.B) {
	codec := LongAlgebraic()
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		if _, err := codec.Encode(pos, move); err != nil {
			b.Fatalf("Encode error: %v", err)
		}
	}
}

func BenchmarkLongAlgebraicDecode(b *testing.B) {
	codec := LongAlgebraic()
	samples := []struct {
		pos  *Position
		text string
	}{
		{startPos, "e2e4"},
		{midPos, "O-O"},
		{complexPos, "Ne5xf7"},
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		sample := samples[i%len(samples)]
		_, err := codec.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmark specific scenarios
func BenchmarkAlgebraicDecodeComplex(b *testing.B) {
	codec := SAN()
	pos := complexPos
	moves := []string{
		"Nxf7",    // Capture with check
		"O-O-O",   // Castling
		"bxc8=Q+", // Promotion with check
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_, err := codec.Decode(pos, moves[i%len(moves)])
		if err != nil {
			b.Fatalf("error decoding %s: %s", moves[i%len(moves)], err)
		}
	}
}

func TestPromotionWithCheck(t *testing.T) {
	promoPos := unsafeFEN("8/1P2k3/8/8/8/8/8/8 w - - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen, tags: Check}

	got := mustEncode(t, SAN(), promoPos, promoMove)
	if got != "b8=Q" {
		t.Fatalf("SAN: Expected 'b8=Q', got '%s'", got)
	}

	got = mustEncode(t, LongAlgebraic(), promoPos, promoMove)
	if got != "b7b8=Q" {
		t.Fatalf("LongAlgebraic: Expected 'b7b8=Q', got '%s'", got)
	}
}

func TestPromotionWithCheckFromIssue84(t *testing.T) {
	promoPos := unsafeFEN("8/1P2k3/8/8/8/8/8/8 w - - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen}.WithTag(Check)

	got := mustEncode(t, SAN(), promoPos, promoMove)
	if got != "b8=Q" {
		t.Fatalf("SAN: Expected 'b8=Q', got '%s'", got)
	}

	got = mustEncode(t, LongAlgebraic(), promoPos, promoMove)
	if got != "b7b8=Q" {
		t.Fatalf("LongAlgebraic: Expected 'b7b8=Q', got '%s'", got)
	}
}

// Issue 84's original failure was an encoded promotion missing its check
// suffix. The codec now recomputes the Check tag from the resulting position
// (not the input), so this test uses a position where the promotion genuinely
// delivers check: a pawn on b7 promoting on b8 attacks the black king on d8
// along the 8th rank.
func TestPromotionDeliversGenuineCheck(t *testing.T) {
	promoPos := unsafeFEN("3k4/1P6/8/8/8/8/8/4K3 w - - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen}

	got := mustEncode(t, SAN(), promoPos, promoMove)
	if got != "b8=Q+" {
		t.Fatalf("SAN: Expected 'b8=Q+', got '%s'", got)
	}

	got = mustEncode(t, LongAlgebraic(), promoPos, promoMove)
	if got != "b7b8=Q+" {
		t.Fatalf("LongAlgebraic: Expected 'b7b8=Q+', got '%s'", got)
	}
}

func TestNullMoveEncodeDoesNotRequirePosition(t *testing.T) {
	got, err := SAN().Encode(nil, NewNullMove())
	if err != nil {
		t.Fatalf("SAN().Encode(nil, null) error = %v", err)
	}
	if got != "Z0" {
		t.Fatalf("SAN().Encode(nil, null) = %q, want %q", got, "Z0")
	}

	got, err = LongAlgebraic().Encode(nil, NewNullMove())
	if err != nil {
		t.Fatalf("LongAlgebraic().Encode(nil, null) error = %v", err)
	}
	if got != "0000" {
		t.Fatalf("LongAlgebraic().Encode(nil, null) = %q, want %q", got, "0000")
	}

	got, err = UCI().Encode(nil, NewNullMove())
	if err != nil {
		t.Fatalf("UCI().Encode(nil, null) error = %v", err)
	}
	if got != "0000" {
		t.Fatalf("UCI().Encode(nil, null) = %q, want %q", got, "0000")
	}
}

func TestIssue84FullGame(t *testing.T) {
	const pgn = `[Event "?"]
[Site "?"]
[Date "????.??.??"]
[Round "?"]
[White "?"]
[Black "?"]
[Result "*"]
1. e4 c5 2. Nf3 Nc6 3. Bb5 g6 4. Bxc6 dxc6 5. d3 Bg7 6. h3 e5 7. a3 Nf6
8. Nc3 Nd7 9. Be3 Qe7 10. Qd2 O-O 11. O-O f5 12. exf5 gxf5 13. Bh6 Qf6
14. Bxg7 Qxg7 15. Qg5 Qxg5 16. Nxg5 Re8 17. Rae1 h6 18. Nf3 c4 19. dxc4 Kf7
20. Rd1 Nf6 21. Rd6 e4 22. Nh4 Be6 23. b3 Rg8 24. Ne2 Rad8 25. Rfd1 Rxd6
26. Rxd6 Ke7 27. c5 Ne8 28. Rd1 Nf6 29. Nd4 f4 30. Nhf5+ Bxf5 31. Nxf5+ Ke6
32. Nxh6 Rg5 33. b4 Rd5 34. Rxd5 cxd5 35. c3 Nd7 36. Ng4 Kf5 37. Kf1 Nf8
38. Ke2 Ne6 39. Kd2 Ng5 40. Nh6+ Kg6 41. Ng4 Kf5 42. Nh2 Ke5 43. a4 Ne6
44. Nf1 a5 45. g3 axb4 46. cxb4 d4 47. gxf4+ Nxf4 48. a5 Nxh3 49. c6 Kd6
50. cxb7 Kc7 51. f3 exf3 52. b8=Q+ Kxb8 53. Kd3 Kb7 54. Kxd4 Ka6 55. Kc5 Nf4
56. Kc6 Nd3 57. b5+ Kxa5 58. b6 Nb4+ 59. Kc5 *`

	reader := strings.NewReader(pgn)
	pgnObj, err := PGN(reader)
	if err != nil {
		t.Fatalf("Failed to parse PGN: %v", err)
	}
	game := NewGame(pgnObj)
	moves := game.MoveTree().MainLine()

	for i, mv := range moves {
		moveNum := (i / 2) + 1
		color := "W"
		if i%2 == 1 {
			color = "B"
		}

		if _, err := LongAlgebraic().Encode(mv.Position(), mv.Move()); err != nil {
			t.Fatalf("Error: LongAlgebraic().Encode panic at half-move %d (%d. %s): %v",
				i, moveNum, color, err)
		}

		if _, err := SAN().Encode(mv.Position(), mv.Move()); err != nil {
			t.Fatalf("Error: SAN().Encode panic at half-move %d (%d. %s): %v",
				i, moveNum, color, err)
		}
	}
}

// Benchmark promotion scenarios
func BenchmarkPromotionEncoding(b *testing.B) {
	promoPos := unsafeFEN("rnbqkbnr/pPpppppp/8/8/8/8/P1PPPPPP/RNBQKBNR w KQkq - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen, tags: Check}
	codecs := []MoveTextCodec{
		UCI(),
		SAN(),
		LongAlgebraic(),
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		codec := codecs[i%len(codecs)]
		if _, err := codec.Encode(promoPos, promoMove); err != nil {
			b.Fatalf("Encode error: %v", err)
		}
	}
}
