package chess_test

import (
	"bytes"
	"errors"
	"regexp"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNRenderer_RenderMatchesGameString(t *testing.T) {
	g := chess.NewGame()
	g.AddTagPair("Event", "Test Event")
	g.AddTagPair("Site", "Test Site")
	g.AddTagPair("Date", "2024.01.01")
	g.AddTagPair("Round", "1")
	g.AddTagPair("White", "Player A")
	g.AddTagPair("Black", "Player B")
	g.AddTagPair("Result", "*")

	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}

	got := g.String()
	rendered := chess.DefaultPGNRenderer.Render(g)
	if got != rendered {
		t.Errorf("Game.String() and DefaultPGNRenderer.Render disagree:\n%s\nvs\n%s", got, rendered)
	}
}

func TestGame_WritePGNMatchesString(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("d4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("d5", nil); err != nil {
		t.Fatal(err)
	}

	var buf bytes.Buffer
	if err := g.WritePGN(&buf); err != nil {
		t.Fatal(err)
	}

	if buf.String() != g.String() {
		t.Errorf("WritePGN output differs from String():\n%s\nvs\n%s", buf.String(), g.String())
	}
}

type errWriter struct {
	err error
}

func (w *errWriter) Write(p []byte) (int, error) {
	return 0, w.err
}

func TestPGNRenderer_RenderGameToPropagatesWriterError(t *testing.T) {
	g := chess.NewGame()
	want := errors.New("disk full")
	w := &errWriter{err: want}

	err := chess.DefaultPGNRenderer.RenderGameTo(g, w)
	if !errors.Is(err, want) {
		t.Errorf("expected error %v, got %v", want, err)
	}
}

func TestPGNRenderer_EndsEmptyGameWithNoOutcome(t *testing.T) {
	g := chess.NewGame()
	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.HasSuffix(out, string(chess.NoOutcome)) {
		t.Errorf("expected output to end with NoOutcome %q, got %q", chess.NoOutcome, out)
	}
}

func TestPGNRenderer_MoveNumberUsesDotAndSpace(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("Nf3", nil); err != nil {
		t.Fatal(err)
	}

	out := chess.DefaultPGNRenderer.Render(g)

	for _, pattern := range []string{`\b1\. e4\b`, ` 2\. Nf3\b`} {
		re := regexp.MustCompile(pattern)
		if !re.MatchString(out) {
			t.Errorf("expected output to match %q, got %q", pattern, out)
		}
	}
	if strings.Contains(out, "1.e4") || strings.Contains(out, "2.Nf3") {
		t.Errorf("expected a space between move number and move, got %q", out)
	}
}

func TestPGNRenderer_TrailingSpaceAfterMoves(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.HasSuffix(out, " *") {
		t.Errorf("expected a single trailing space before outcome token in %q", out)
	}
}

func TestPGNRenderer_NoTrailingSpaceForEmptyGame(t *testing.T) {
	g := chess.NewGame()
	out := chess.DefaultPGNRenderer.Render(g)
	if strings.HasSuffix(out, " *") {
		t.Errorf("empty game must not gain a trailing space before outcome: %q", out)
	}
}

func TestPGNRenderer_EscapesCommentEndBrace(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.MoveTree().Root().Children()[0].SetComment("keeps } inside")

	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, `keeps \} inside`) {
		t.Fatalf("rendered PGN did not escape comment brace: %q", out)
	}

	parsed := chess.NewGame(mustPGNOption(t, out))
	if got := parsed.MoveTree().Root().Children()[0].Comments(); got != "keeps } inside" {
		t.Fatalf("round-tripped comment = %q, want %q", got, "keeps } inside")
	}
}

func TestPGNRenderer_EscapesTagValueQuotes(t *testing.T) {
	g := chess.NewGame()
	g.AddTagPair("White", `A "B" C`)
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}

	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, `A \"B\" C`) {
		t.Fatalf("rendered PGN did not escape tag value quotes: %q", out)
	}
}

func TestPGNRenderer_EmitsCommandAnnotation(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.MoveTree().Root().Children()[0].SetCommand("clk", "0:01:23")

	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, "[%clk 0:01:23]") {
		t.Fatalf("rendered PGN did not contain command annotation: %q", out)
	}
}

func TestPGNRenderer_OrdersTagsBySevenTagRosterThenAlpha(t *testing.T) {
	g := chess.NewGame()
	g.AddTagPair("Result", "*")
	g.AddTagPair("Zebra", "z")
	g.AddTagPair("Date", "2026.06.19")
	g.AddTagPair("Black", "B")
	g.AddTagPair("Alpha", "a")
	g.AddTagPair("Round", "1")
	g.AddTagPair("White", "A")
	g.AddTagPair("Event", "E")
	g.AddTagPair("Site", "S")
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}

	out := chess.DefaultPGNRenderer.Render(g)
	idx := func(tag string) int { return strings.Index(out, "["+tag+" \"") }
	order := []string{"Event", "Site", "Date", "Round", "White", "Black", "Result", "Alpha", "Zebra"}
	for i := 1; i < len(order); i++ {
		if idx(order[i]) <= idx(order[i-1]) {
			t.Fatalf("tags out of order: %q (idx %d) should come after %q (idx %d)\n%s",
				order[i], idx(order[i]), order[i-1], idx(order[i-1]), out)
		}
	}
}

func TestPGNRenderer_DoesNotMutateGame(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.MoveTree().Root().Children()[0].SetComment("a comment")
	g.AddTagPair("Custom", "value")

	before := g.String()
	beforePos := g.Position().XFENString()
	_ = chess.DefaultPGNRenderer.Render(g)
	if g.String() != before {
		t.Errorf("Render mutated game string:\nbefore:\n%s\nafter:\n%s", before, g.String())
	}
	if g.Position().XFENString() != beforePos {
		t.Errorf("Render mutated position: before %q after %q", beforePos, g.Position().XFENString())
	}
}

func TestPGNRenderer_RoundTripsVariations(t *testing.T) {
	pgn := withMinimalTags("1. e4 {main} e5 (1...c5 {sicilian} 2. Nf3) 2. Nf3 *")
	g := mustParseSingleGame(t, pgn)

	first := chess.DefaultPGNRenderer.Render(g)
	reparsed := mustParseSingleGame(t, first)
	second := chess.DefaultPGNRenderer.Render(reparsed)

	if first != second {
		t.Fatalf("render not idempotent for variations:\nfirst:\n%s\nsecond:\n%s", first, second)
	}
	if !strings.Contains(first, "c5") {
		t.Fatalf("variation move c5 missing from render: %q", first)
	}
}

func TestPGNRenderer_IsIdempotentOnAnnotations(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.MoveTree().Root().Children()[0].SetComment("best move")
	if err := g.MoveTree().Root().Children()[0].SetNAGs([]string{"$1"}); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}

	first := chess.DefaultPGNRenderer.Render(g)
	reparsed := chess.NewGame(mustPGNOption(t, first))
	second := chess.DefaultPGNRenderer.Render(reparsed)

	if first != second {
		t.Fatalf("render not idempotent:\nfirst:\n%s\nsecond:\n%s", first, second)
	}
}

func TestPGNRenderer_WritesNAGsBeforeComments(t *testing.T) {
	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	move := g.MoveTree().Root().Children()[0]
	move.SetComment("best move")
	// Multiple NAGs are kept in order and rendered as canonical "$N" before
	// the comment, even when imported via symbolic spellings.
	if err := move.SetNAGs([]string{"!", "$1406"}); err != nil {
		t.Fatal(err)
	}

	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, "e4 $1 $1406 {best move}") {
		t.Fatalf("expected NAGs before comment, got:\n%s", out)
	}
}
