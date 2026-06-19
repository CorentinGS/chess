package chess

import (
	"fmt"
	"strings"
	"testing"
)

type notationDecodeTest struct {
	N       Notation
	Pos     *Position
	Text    string
	PostPos *Position
}

var invalidDecodeTests = []notationDecodeTest{
	{
		// opening for white
		N:    AlgebraicNotation{},
		Pos:  unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
		Text: "e5",
	},
	{
		// http://en.lichess.org/W91M4jms#14
		N:    AlgebraicNotation{},
		Pos:  unsafeFEN("rn1qkb1r/pp3ppp/2p1pn2/3p4/2PP4/2NQPN2/PP3PPP/R1B1K2R b KQkq - 0 7"),
		Text: "Nd7",
	},
	{
		// http://en.lichess.org/W91M4jms#17
		N:       AlgebraicNotation{},
		Pos:     unsafeFEN("r2qk2r/pp1n1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P4PPP/R1B1K2R w KQkq - 1 9"),
		Text:    "O-O-O-O",
		PostPos: unsafeFEN("r2qk2r/pp1n1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P4PPP/R1B2RK1 b kq - 2 9"),
	},
	{
		// http://en.lichess.org/W91M4jms#23
		N:    AlgebraicNotation{},
		Pos:  unsafeFEN("3r1rk1/pp1nqppp/2pbpn2/3p4/2PP4/1PNQPN2/PB3PPP/3RR1K1 b - - 5 12"),
		Text: "dx4",
	},
	{
		// should not assume pawn for unknown piece type "n"
		N:    AlgebraicNotation{},
		Pos:  unsafeFEN("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 2"),
		Text: "nf3",
	},
	{
		// disambiguation should not allow for this since it is not a capture
		N:    AlgebraicNotation{},
		Pos:  unsafeFEN("rnbqkbnr/ppp1pppp/8/3p4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 2"),
		Text: "bf4",
	},
}

func TestInvalidDecoding(t *testing.T) {
	for _, test := range invalidDecodeTests {
		if _, err := test.N.Decode(test.Pos, test.Text); err == nil {
			t.Fatalf("starting from board\n%s\n expected move notation %s to be invalid", test.Pos.board.Draw(), test.Text)
		}
	}
}

func TestEncodeUCINotation(t *testing.T) {
	notation := UCINotation{}
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	move := Move{s1: E2, s2: E4}
	expected := "e2e4"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithPromotion(t *testing.T) {
	notation := UCINotation{}
	pos := unsafeFEN("8/P7/8/8/8/8/8/8 w - - 0 1")
	move := Move{s1: A7, s2: A8, promo: Queen}
	expected := "a7a8q"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithInvalidMove(t *testing.T) {
	notation := UCINotation{}
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	move := Move{s1: E2, s2: E5}
	expected := "e2e5"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
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
			name:    "invalid UCI notation length",
			pos:     nil,
			input:   "e2e",
			wantErr: true,
		},
		{
			name:    "invalid squares in UCI notation",
			pos:     nil,
			input:   "e9e4",
			wantErr: true,
		},
		{
			name:    "invalid promotion piece",
			pos:     nil,
			input:   "a7a8x",
			wantErr: true,
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

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			notation := UCINotation{}
			got, err := notation.Decode(tt.pos, tt.input)
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

// Benchmarks for UCI Notation
func BenchmarkUCIEncode(b *testing.B) {
	notation := UCINotation{}
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		notation.Encode(pos, move)
	}
}

func BenchmarkUCIDecode(b *testing.B) {
	notation := UCINotation{}
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
		_, err := notation.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmarks for Algebraic Notation
func BenchmarkAlgebraicEncode(b *testing.B) {
	notation := AlgebraicNotation{}
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		notation.Encode(pos, move)
	}
}

func BenchmarkAlgebraicDecode(b *testing.B) {
	notation := AlgebraicNotation{}
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
		_, err := notation.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmarks for Long Algebraic Notation
func BenchmarkLongAlgebraicEncode(b *testing.B) {
	notation := LongAlgebraicNotation{}
	positions := []*Position{startPos, midPos, complexPos}
	moves := [][]Move{startMoves, midMoves, complexMoves}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		pos := positions[i%len(positions)]
		move := moves[i%len(moves)][i%len(moves[i%len(moves)])]
		notation.Encode(pos, move)
	}
}

func BenchmarkLongAlgebraicDecode(b *testing.B) {
	notation := LongAlgebraicNotation{}
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
		_, err := notation.Decode(sample.pos, sample.text)
		if err != nil {
			b.Fatalf("error decoding %s: %s", sample.text, err)
		}
	}
}

// Benchmark specific scenarios
func BenchmarkAlgebraicDecodeComplex(b *testing.B) {
	notation := AlgebraicNotation{}
	pos := complexPos
	moves := []string{
		"Nxf7",    // Capture with check
		"O-O-O",   // Castling
		"bxc8=Q+", // Promotion with check
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		_, err := notation.Decode(pos, moves[i%len(moves)])
		if err != nil {
			b.Fatalf("error decoding %s: %s", moves[i%len(moves)], err)
		}
	}
}

func TestPromotionWithCheck(t *testing.T) {
	promoPos := unsafeFEN("8/1P2k3/8/8/8/8/8/8 w - - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen, tags: Check}

	algebraicNotation := AlgebraicNotation{}
	result := algebraicNotation.Encode(promoPos, promoMove)
	if result != "b8=Q+" {
		t.Fatalf("Expected 'b8=Q+', got '%s'", result)
	}

	longAlgebraicNotation := LongAlgebraicNotation{}
	result = longAlgebraicNotation.Encode(promoPos, promoMove)
	if result != "b7b8=Q+" {
		t.Fatalf("Expected 'b7b8=Q+', got '%s'", result)
	}
}

func TestPromotionWithCheckFromIssue84(t *testing.T) {
	promoPos := unsafeFEN("8/1P2k3/8/8/8/8/8/8 w - - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen}.WithTag(Check)

	algebraicNotation := AlgebraicNotation{}
	result := algebraicNotation.Encode(promoPos, promoMove)
	if result != "b8=Q+" {
		t.Fatalf("Algebraic Notation: Expected 'b8=Q+', got '%s'", result)
	}

	longAlgebraicNotation := LongAlgebraicNotation{}
	result = longAlgebraicNotation.Encode(promoPos, promoMove)
	if result != "b7b8=Q+" {
		t.Fatalf("Long Algebraic Notation: Expected 'b7b8=Q+', got '%s'", result)
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
	moves := game.MoveNodes()

	for i, mv := range moves {
		moveNum := (i / 2) + 1
		color := "W"
		if i%2 == 1 {
			color = "B"
		}

		_, err := safeEncode(LongAlgebraicNotation{}, mv.Position(), mv.Move())
		if err != nil {
			t.Fatalf("Error: LongAlgebraicNotation.Encode panic at half-move %d (%d. %s): %v",
				i, moveNum, color, err)
		}

		_, err = safeEncode(AlgebraicNotation{}, mv.Position(), mv.Move())
		if err != nil {
			t.Fatalf("Error: AlgebraicNotation.Encode panic at half-move %d (%d. %s): %v",
				i, moveNum, color, err)
		}
	}
}

func safeEncode(notation Encoder, pos *Position, mv Move) (s string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
		}
	}()
	return notation.Encode(pos, mv), nil
}

// Benchmark promotion scenarios
func BenchmarkPromotionEncoding(b *testing.B) {
	promoPos := unsafeFEN("rnbqkbnr/pPpppppp/8/8/8/8/P1PPPPPP/RNBQKBNR w KQkq - 0 1")
	promoMove := Move{s1: B7, s2: B8, promo: Queen, tags: Check}
	notations := []Notation{
		UCINotation{},
		AlgebraicNotation{},
		LongAlgebraicNotation{},
	}

	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		notation := notations[i%len(notations)]
		notation.Encode(promoPos, promoMove)
	}
}
