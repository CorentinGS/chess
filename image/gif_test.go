package image_test

import (
	"bytes"
	"errors"
	stdimage "image"
	"image/color"
	"image/draw"
	"image/gif"
	"testing"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/image"
)

func TestGIFUsesChangedFrameBounds(t *testing.T) {
	// The initial position includes both colors of all six piece types.
	game := chess.NewGame()
	boards := []*chess.Board{game.Position().Board()}
	move, err := (chess.UCINotation{}).Decode(game.Position(), "e2e4")
	if err != nil {
		t.Fatal(err)
	}
	if err := game.Move(move, nil); err != nil {
		t.Fatal(err)
	}
	boards = append(boards, game.Position().Board())
	boards = append(boards, game.Position().Board())

	var buf bytes.Buffer
	if err := image.GIF(&buf, boards, 5); err != nil {
		t.Fatal(err)
	}

	animation, err := gif.DecodeAll(&buf)
	if err != nil {
		t.Fatal(err)
	}
	if len(animation.Image) != len(boards) {
		t.Fatalf("expected %d frames, got %d", len(boards), len(animation.Image))
	}
	if animation.LoopCount != 0 {
		t.Errorf("loop count = %d, want 0 (infinite loop)", animation.LoopCount)
	}
	for i, got := range animation.Delay {
		if got != 5 {
			t.Errorf("frame %d delay = %d, want 5", i, got)
		}
	}

	if got, want := animation.Image[0].Bounds(), stdimage.Rect(0, 0, 360, 360); got != want {
		t.Errorf("first frame bounds = %v, want %v", got, want)
	}
	if got, want := animation.Image[1].Bounds(), stdimage.Rect(180, 180, 225, 315); got != want {
		t.Errorf("second frame bounds = %v, want changed-square bounds %v", got, want)
	}
	if got, want := animation.Image[2].Bounds(), stdimage.Rect(0, 0, 1, 1); got != want {
		t.Errorf("unchanged frame bounds = %v, want %v", got, want)
	}

	composed := compositeGIFFrames(animation)
	if !samePixels(composed[1], composed[2]) {
		t.Error("unchanged frame changed the displayed board")
	}
}

func TestGIFRejectsInvalidInput(t *testing.T) {
	board := chess.NewGame().Position().Board()
	tests := []struct {
		name   string
		boards []*chess.Board
		delay  int
	}{
		{name: "no boards", delay: 5},
		{name: "nil board", boards: []*chess.Board{nil}, delay: 5},
		{name: "negative delay", boards: []*chess.Board{board}, delay: -1},
		{name: "delay too large", boards: []*chess.Board{board}, delay: 65536},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := image.GIF(&bytes.Buffer{}, tt.boards, tt.delay); err == nil {
				t.Fatal("GIF returned nil error for invalid input")
			}
		})
	}
}

func TestGIFReturnsWriterError(t *testing.T) {
	want := errors.New("write failed")
	err := image.GIF(errorWriter{err: want}, []*chess.Board{chess.NewGame().Position().Board()}, 5)
	if !errors.Is(err, want) {
		t.Fatalf("GIF error = %v, want wrapped %v", err, want)
	}
}

type errorWriter struct {
	err error
}

func (w errorWriter) Write([]byte) (int, error) {
	return 0, w.err
}

func TestGIFMoveUpdatesSourceAndDestinationSquares(t *testing.T) {
	game := chess.NewGame()
	boards := []*chess.Board{game.Position().Board()}
	move, err := (chess.UCINotation{}).Decode(game.Position(), "e2e4")
	if err != nil {
		t.Fatal(err)
	}
	if err := game.Move(move, nil); err != nil {
		t.Fatal(err)
	}
	boards = append(boards, game.Position().Board())

	animation := encodeGIF(t, boards, 5)
	composed := compositeGIFFrames(animation)
	e2 := stdimage.Pt(4*45+22, 6*45+22)
	e4 := stdimage.Pt(4*45+22, 4*45+22)
	if composed[0].At(e2.X, e2.Y) == composed[1].At(e2.X, e2.Y) {
		t.Error("source square e2 did not change after the move")
	}
	if composed[0].At(e4.X, e4.Y) == composed[1].At(e4.X, e4.Y) {
		t.Error("destination square e4 did not change after the move")
	}
}

func TestGIFOptionsCustomizeBoard(t *testing.T) {
	board := chess.NewGame().Position().Board()
	light := color.RGBA{R: 240, G: 30, B: 20, A: 255}
	dark := color.RGBA{R: 20, G: 40, B: 230, A: 255}
	mark := color.RGBA{R: 20, G: 230, B: 40, A: 255}

	base := encodeGIF(t, []*chess.Board{board}, 5)
	var buf bytes.Buffer
	if err := image.GIF(&buf, []*chess.Board{board}, 5,
		image.SquareColors(light, dark),
		image.MarkSquares(mark, chess.E4),
	); err != nil {
		t.Fatal(err)
	}
	customized := decodeGIF(t, &buf)
	baseFrame := compositeGIFFrames(base)[0]
	customFrame := compositeGIFFrames(customized)[0]

	// D5 and E4 are light squares; D4 is dark from White's perspective.
	d5 := stdimage.Pt(3*45+22, 3*45+22)
	e4 := stdimage.Pt(4*45+22, 4*45+22)
	d4 := stdimage.Pt(3*45+22, 4*45+22)
	if got, want := rgbaAt(customFrame, d5), light; got != want {
		t.Errorf("custom light-square color at d5 = %v, want %v", got, want)
	}
	if got := rgbaAt(customFrame, e4); got == rgbaAt(baseFrame, e4) {
		t.Error("marking e4 did not change its rendered color")
	}
	if got, want := rgbaAt(customFrame, d4), dark; got != want {
		t.Errorf("custom dark-square color at d4 = %v, want %v", got, want)
	}
	if got := rgbaAt(customFrame, e4); got == light {
		t.Error("marked e4 has the unmarked light-square color")
	}
}

func TestGIFPerspectiveChangesRenderedBoard(t *testing.T) {
	board := chess.NewGame().Position().Board()
	whitePerspective := encodeGIF(t, []*chess.Board{board}, 5)
	var buf bytes.Buffer
	if err := image.GIF(&buf, []*chess.Board{board}, 5, image.Perspective(chess.Black)); err != nil {
		t.Fatal(err)
	}
	blackPerspective := decodeGIF(t, &buf)
	if samePixels(compositeGIFFrames(whitePerspective)[0], compositeGIFFrames(blackPerspective)[0]) {
		t.Error("Black perspective produced the same image as White perspective")
	}
}

func encodeGIF(t *testing.T, boards []*chess.Board, delay int) *gif.GIF {
	t.Helper()
	var buf bytes.Buffer
	if err := image.GIF(&buf, boards, delay); err != nil {
		t.Fatal(err)
	}
	return decodeGIF(t, &buf)
}

func decodeGIF(t *testing.T, buf *bytes.Buffer) *gif.GIF {
	t.Helper()
	animation, err := gif.DecodeAll(buf)
	if err != nil {
		t.Fatal(err)
	}
	return animation
}

func compositeGIFFrames(animation *gif.GIF) []*stdimage.RGBA {
	bounds := stdimage.Rect(0, 0, animation.Config.Width, animation.Config.Height)
	canvas := stdimage.NewRGBA(bounds)
	frames := make([]*stdimage.RGBA, 0, len(animation.Image))
	for _, frame := range animation.Image {
		draw.Draw(canvas, frame.Bounds(), frame, frame.Bounds().Min, draw.Over)
		copy := stdimage.NewRGBA(bounds)
		draw.Draw(copy, bounds, canvas, bounds.Min, draw.Src)
		frames = append(frames, copy)
	}
	return frames
}

func samePixels(a, b stdimage.Image) bool {
	if a.Bounds() != b.Bounds() {
		return false
	}
	bounds := a.Bounds()
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			if a.At(x, y) != b.At(x, y) {
				return false
			}
		}
	}
	return true
}

func rgbaAt(img stdimage.Image, point stdimage.Point) color.RGBA {
	r, g, b, a := img.At(point.X, point.Y).RGBA()
	return color.RGBA{R: uint8(r >> 8), G: uint8(g >> 8), B: uint8(b >> 8), A: uint8(a >> 8)}
}
