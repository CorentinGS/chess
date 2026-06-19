package chess_test

import (
	"bytes"
	"errors"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNRendererRenderMatchesGameString(t *testing.T) {
	g := chess.NewGame()
	g.AddTagPair("Event", "Test Event")
	g.AddTagPair("Site", "Test Site")
	g.AddTagPair("Date", "2024.01.01")
	g.AddTagPair("Round", "1")
	g.AddTagPair("White", "Player A")
	g.AddTagPair("Black", "Player B")
	g.AddTagPair("Result", "*")

	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}

	got := g.String()
	rendered := chess.DefaultPGNRenderer.Render(g)
	if got != rendered {
		t.Errorf("Game.String() and DefaultPGNRenderer.Render disagree:\n%s\nvs\n%s", got, rendered)
	}
}

func TestGameWritePGNMatchesGameString(t *testing.T) {
	g := chess.NewGame()
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
	g := chess.NewGame()
	want := errors.New("disk full")
	w := &errWriter{err: want}

	err := chess.DefaultPGNRenderer.RenderGameTo(g, w)
	if !errors.Is(err, want) {
		t.Errorf("expected error %v, got %v", want, err)
	}
}

func TestPGNRendererRenderAnnotatesEmptyGame(t *testing.T) {
	g := chess.NewGame()
	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.HasSuffix(out, string(chess.NoOutcome)) {
		t.Errorf("expected output to end with NoOutcome %q, got %q", chess.NoOutcome, out)
	}
}

func TestPGNRendererEscapesCommentEndBrace(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.GetRootMove().Children()[0].SetComment("keeps } inside")

	out := chess.DefaultPGNRenderer.Render(g)
	if !strings.Contains(out, `keeps \} inside`) {
		t.Fatalf("rendered PGN did not escape comment brace: %q", out)
	}

	parsed := chess.NewGame(mustPGNOption(t, out))
	if got := parsed.GetRootMove().Children()[0].Comments(); got != "keeps } inside" {
		t.Fatalf("round-tripped comment = %q, want %q", got, "keeps } inside")
	}
}

func mustPGNOption(t *testing.T, pgn string) func(*chess.Game) {
	t.Helper()
	opt, err := chess.PGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatal(err)
	}
	return opt
}
