package chess

import (
	"testing"
)

type _ struct {
	Pos1        *Position `json:"pos1"`
	Pos2        *Position `json:"pos2"`
	AlgText     string    `json:"alg_text"`
	LongAlgText string    `json:"long_alg_text"`
	UCIText     string    `json:"uci_text"`
	Description string    `json:"description"`
}

/*
TODO: Fix this test for new notation system
func TestValidDecoding(t *testing.T) {
	f, err := os.Open("fixtures/valid_notation_tests.json")
	if err != nil {
		t.Fatal(err)
		return
	}

	var validTests []validNotationTest
	if err := json.NewDecoder(f).Decode(&validTests); err != nil {
		t.Fatal(err)
		return
	}

	for _, test := range validTests {
		for i, n := range []Notation{AlgebraicNotation{}, LongAlgebraicNotation{}, UCINotation{}} {
			var moveText string
			switch i {
			case 0:
				moveText = test.AlgText
			case 1:
				moveText = test.LongAlgText
			case 2:
				moveText = test.UCIText
			}
			m, err := n.Decode(test.Pos1, moveText)
			if err != nil {
				movesStrList := []string{}
				for _, m := range test.Pos1.ValidMoves() {
					s := n.Encode(test.Pos1, &m)
					movesStrList = append(movesStrList, s)
				}
				t.Fatalf("starting from board \n%s\n expected move to be valid error - %s %s\n", test.Pos1.board.Draw(), err.Error(), strings.Join(movesStrList, ","))
			}
			postPos := test.Pos1.Update(m)
			if test.Pos2.String() != postPos.String() {
				t.Fatalf("starting from board \n%s%s\n after move %s\n expected board to be %s\n%s\n but was %s\n%s\n",
					test.Pos1.String(),
					test.Pos1.board.Draw(), m.String(), test.Pos2.String(),
					test.Pos2.board.Draw(), postPos.String(), postPos.board.Draw())
			}
		}
	}
}

*/

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
	move := &Move{s1: E2, s2: E4}
	expected := "e2e4"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithPromotion(t *testing.T) {
	notation := UCINotation{}
	pos := unsafeFEN("8/P7/8/8/8/8/8/8 w - - 0 1")
	move := &Move{s1: A7, s2: A8, promo: Queen}
	expected := "a7a8q"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
	}
}

func TestEncodeUCINotationWithInvalidMove(t *testing.T) {
	notation := UCINotation{}
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	move := &Move{s1: E2, s2: E5}
	expected := "e2e5"
	result := notation.Encode(pos, move)
	if result != expected {
		t.Fatalf("expected %s, got %s", expected, result)
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
	startMoves = []*Move{
		{s1: E2, s2: E4}, // e4
		{s1: G1, s2: F3}, // Nf3
		{s1: B1, s2: C3}, // Nc3
	}
	midMoves = []*Move{
		{s1: E1, s2: G1, tags: KingSideCastle},  // O-O
		{s1: F3, s2: E5, tags: Capture},         // Nxe5
		{s1: C4, s2: F7, tags: Check | Capture}, // d4+
	}
	complexMoves = []*Move{
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
	moves := [][]*Move{startMoves, midMoves, complexMoves}

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
	moves := [][]*Move{startMoves, midMoves, complexMoves}

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
	moves := [][]*Move{startMoves, midMoves, complexMoves}

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

// Benchmark promotion scenarios
func BenchmarkPromotionEncoding(b *testing.B) {
	promoPos := unsafeFEN("rnbqkbnr/pPpppppp/8/8/8/8/P1PPPPPP/RNBQKBNR w KQkq - 0 1")
	promoMove := &Move{s1: B7, s2: B8, promo: Queen, tags: Check}
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
