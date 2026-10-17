package image

import (
	"bytes"
	"encoding/xml"
	"errors"
	"image/color"
	"io"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestSVGDefaultRender(t *testing.T) {
	svg := renderSVG(t, chess.StartingPosition(), nil)

	if !strings.HasPrefix(svg, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg ") {
		t.Fatalf("expected SVG to start with XML declaration and root element, got %q", svg[:min(len(svg), 80)])
	}
	assertValidXML(t, svg)
	assertContains(t, svg, `width="360"`)
	assertContains(t, svg, `height="360"`)
	assertContains(t, svg, `viewBox="0 0 360 360"`)
	assertCount(t, svg, `<rect class="square `, 64)
	assertContains(t, svg, `class="piece white rook" data-square="a1"`)
	assertContains(t, svg, `<text class="coordinate file"`)
	assertContains(t, svg, `<text class="coordinate rank"`)
}

func TestSVGOptions(t *testing.T) {
	svg := renderSVG(t, chess.StartingPosition(), &SVGOptions{
		LightSquare: color.RGBA{R: 1, G: 2, B: 3, A: 255},
		DarkSquare:  color.RGBA{R: 4, G: 5, B: 6, A: 255},
		Marks: map[chess.Square]color.Color{
			chess.D2:       color.RGBA{R: 255, G: 255, B: 0, A: 1},
			chess.D4:       color.RGBA{R: 255, G: 255, B: 0, A: 1},
			chess.NoSquare: color.RGBA{R: 255, G: 0, B: 0, A: 1},
		},
		Perspective: chess.Black,
		SquareSize:  90,
	})

	assertValidXML(t, svg)
	assertContains(t, svg, `width="720"`)
	assertContains(t, svg, `height="720"`)
	assertContains(t, svg, `viewBox="0 0 720 720"`)
	assertContains(t, svg, `fill="#010203"`)
	assertContains(t, svg, `fill="#040506"`)
	assertCount(t, svg, `<rect class="mark"`, 2)
	assertContains(t, svg, `class="mark" data-square="d2"`)
	assertContains(t, svg, `class="mark" data-square="d4"`)
	assertContains(t, svg, `fill-opacity="0.2"`)
	assertContains(t, svg, `class="piece white rook" data-square="a1" transform="translate(630 0) scale(2)"`)
	assertContains(t, svg, `<text class="coordinate rank" x="4" y="22" font-size="22"`)
	assertContains(t, svg, `<text class="coordinate file" x="715" y="714" text-anchor="end" font-size="22"`)
}

func TestSVGHideCoordinates(t *testing.T) {
	svg := renderSVG(t, chess.StartingPosition(), &SVGOptions{HideCoordinates: true})

	assertValidXML(t, svg)
	if strings.Contains(svg, `<text class="coordinate `) {
		t.Fatalf("expected coordinates to be hidden, got %s", svg)
	}
}

func TestSVGValidation(t *testing.T) {
	var buf bytes.Buffer
	if err := SVG(&buf, nil, nil); err == nil || err.Error() != "image: nil position" {
		t.Fatalf("expected nil position error, got %v", err)
	}
	if err := SVG(nil, chess.StartingPosition(), nil); err == nil || err.Error() != "image: nil writer" {
		t.Fatalf("expected nil writer error, got %v", err)
	}

	defaultSVG := renderSVG(t, chess.StartingPosition(), nil)
	invalidPerspectiveSVG := renderSVG(t, chess.StartingPosition(), &SVGOptions{Perspective: chess.NoColor})
	if invalidPerspectiveSVG != defaultSVG {
		t.Fatal("expected invalid perspective to render from white perspective")
	}

	defaultSizeSVG := renderSVG(t, chess.StartingPosition(), &SVGOptions{SquareSize: -1})
	assertContains(t, defaultSizeSVG, `width="360"`)
}

func TestSVGWriterError(t *testing.T) {
	errBoom := errors.New("boom")
	err := SVG(errorWriter{err: errBoom}, chess.StartingPosition(), nil)
	if !errors.Is(err, errBoom) {
		t.Fatalf("expected writer error %v, got %v", errBoom, err)
	}
}

func TestEmbeddedPiecesLoaded(t *testing.T) {
	if len(pieceSVGs) != 12 {
		t.Fatalf("expected 12 embedded piece SVGs, got %d", len(pieceSVGs))
	}
	for _, p := range []chess.Piece{
		chess.WhiteKing, chess.WhiteQueen, chess.WhiteRook, chess.WhiteBishop, chess.WhiteKnight, chess.WhitePawn,
		chess.BlackKing, chess.BlackQueen, chess.BlackRook, chess.BlackBishop, chess.BlackKnight, chess.BlackPawn,
	} {
		if pieceSVGs[pieceFileName(p)] == "" {
			t.Fatalf("expected embedded piece SVG for %s", pieceFileName(p))
		}
	}
}

func TestColorToHex(t *testing.T) {
	got := colorToHex(color.RGBA64{R: 0x1234, G: 0x5678, B: 0x9abc, A: 0xffff})
	if got != "#12569a" {
		t.Fatalf("expected #12569a, got %s", got)
	}
}

func BenchmarkSVGDefault(b *testing.B) {
	pos := chess.StartingPosition()
	for b.Loop() {
		if err := SVG(&bytes.Buffer{}, pos, nil); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkSVGMarkedCustomSize(b *testing.B) {
	pos := chess.StartingPosition()
	opts := &SVGOptions{
		Marks: map[chess.Square]color.Color{
			chess.D2: color.RGBA{R: 255, G: 255, B: 0, A: 255},
			chess.D4: color.RGBA{R: 255, G: 255, B: 0, A: 255},
		},
		Perspective: chess.Black,
		SquareSize:  90,
	}
	for b.Loop() {
		if err := SVG(&bytes.Buffer{}, pos, opts); err != nil {
			b.Fatal(err)
		}
	}
}

type errorWriter struct {
	err error
}

func (w errorWriter) Write([]byte) (int, error) {
	return 0, w.err
}

func renderSVG(t *testing.T, pos *chess.Position, opts *SVGOptions) string {
	t.Helper()
	var buf bytes.Buffer
	if err := SVG(&buf, pos, opts); err != nil {
		t.Fatal(err)
	}
	return buf.String()
}

func assertValidXML(t *testing.T, svg string) {
	t.Helper()
	decoder := xml.NewDecoder(strings.NewReader(svg))
	for {
		_, err := decoder.Token()
		if err == nil {
			continue
		}
		if errors.Is(err, io.EOF) {
			return
		}
		t.Fatalf("expected valid XML, got %v", err)
	}
}

func assertContains(t *testing.T, s, want string) {
	t.Helper()
	if !strings.Contains(s, want) {
		t.Fatalf("expected output to contain %q", want)
	}
}

func assertCount(t *testing.T, s, substr string, want int) {
	t.Helper()
	if got := strings.Count(s, substr); got != want {
		t.Fatalf("expected %d occurrences of %q, got %d", want, substr, got)
	}
}
