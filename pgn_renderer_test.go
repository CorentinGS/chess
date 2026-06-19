package chess

import (
	"bytes"
	"errors"
	"strings"
	"testing"
)

func TestPGNRendererRenderMatchesGameString(t *testing.T) {
	g := NewGame()
	g.tagPairs["Event"] = "Test Event"
	g.tagPairs["Site"] = "Test Site"
	g.tagPairs["Date"] = "2024.01.01"
	g.tagPairs["Round"] = "1"
	g.tagPairs["White"] = "Player A"
	g.tagPairs["Black"] = "Player B"
	g.tagPairs["Result"] = "*"

	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}

	got := g.String()
	rendered := DefaultPGNRenderer.Render(g)
	if got != rendered {
		t.Errorf("Game.String() and DefaultPGNRenderer.Render disagree:\n%s\nvs\n%s", got, rendered)
	}
}

func TestGameWritePGNMatchesGameString(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("d4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("d5", nil); err != nil {
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

func TestPGNRendererRenderGameToPropagatesWriterError(t *testing.T) {
	g := NewGame()
	want := errors.New("disk full")
	w := &errWriter{err: want}

	err := DefaultPGNRenderer.RenderGameTo(g, w)
	if !errors.Is(err, want) {
		t.Errorf("expected error %v, got %v", want, err)
	}
}

func TestPGNRendererRenderAnnotatesEmptyGame(t *testing.T) {
	g := NewGame()
	out := DefaultPGNRenderer.Render(g)
	if !strings.HasSuffix(out, string(NoOutcome)) {
		t.Errorf("expected output to end with NoOutcome %q, got %q", NoOutcome, out)
	}
}

func TestPGNRendererEscapesCommentEndBrace(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.currentMove.SetComment("keeps } inside")

	out := DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, `keeps \} inside`) {
		t.Fatalf("rendered PGN did not escape comment brace: %q", out)
	}

	parsed := NewGame(mustPGNOption(t, out))
	if got := parsed.currentMove.Comments(); got != "keeps } inside" {
		t.Fatalf("round-tripped comment = %q, want %q", got, "keeps } inside")
	}
}

func mustPGNOption(t *testing.T, pgn string) func(*Game) {
	t.Helper()
	opt, err := PGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatal(err)
	}
	return opt
}
