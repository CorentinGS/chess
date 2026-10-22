package image

import (
	"errors"
	"fmt"
	stdimage "image"
	"image/color"
	"image/color/palette"
	"image/draw"
	"image/gif"
	"io"
	"strings"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/image/internal"
	"github.com/srwiley/oksvg"
	"github.com/srwiley/rasterx"
	"golang.org/x/image/font"
	"golang.org/x/image/font/basicfont"
	"golang.org/x/image/math/fixed"
)

// GIF writes an animated GIF of the given board positions. Delay is measured in
// hundredths of a second, as in the standard library's image/gif package.
// GIF also takes options which can customize the board output.
func GIF(w io.Writer, boards []*chess.Board, delay int, opts ...func(*encoder)) error {
	if len(boards) == 0 {
		return errors.New("image: GIF requires at least one board")
	}
	if delay < 0 || delay > 0xffff {
		return errors.New("image: GIF delay must be between 0 and 65535 hundredths of a second")
	}
	for _, board := range boards {
		if board == nil {
			return errors.New("image: GIF cannot encode a nil board")
		}
	}

	e := newEncoder(w, opts)
	pieceSprites, err := loadPieceSprites()
	if err != nil {
		return err
	}
	background := drawBoardBackground(e)
	canvas := stdimage.NewRGBA(background.Bounds())
	draw.Draw(canvas, canvas.Bounds(), background, background.Bounds().Min, draw.Src)
	framePalette := gifPalette(e)

	frames := &gif.GIF{
		Image:     make([]*stdimage.Paletted, 0, len(boards)),
		Delay:     make([]int, 0, len(boards)),
		LoopCount: 0,
		Config: stdimage.Config{
			ColorModel: framePalette,
			Width:      boardWidth,
			Height:     boardHeight,
		},
	}
	var previous map[chess.Square]chess.Piece
	for _, board := range boards {
		current := board.SquareMap()
		changedBounds := stdimage.Rectangle{}
		ranks, files := boardOrder(e.perspective)
		for row, rank := range ranks {
			for column, file := range files {
				square := chess.NewSquare(file, rank)
				piece := current[square]
				if previous != nil && previous[square] == piece {
					continue
				}
				squareBounds := stdimage.Rect(column*sqWidth, row*sqHeight, (column+1)*sqWidth, (row+1)*sqHeight)
				redrawGIFSquare(canvas, background, squareBounds, square, row, column, piece, e, pieceSprites)
				changedBounds = changedBounds.Union(squareBounds)
			}
		}
		if changedBounds.Empty() {
			changedBounds = stdimage.Rect(0, 0, 1, 1)
		}
		frames.Image = append(frames.Image, quantizeFrame(canvas, changedBounds, framePalette))
		frames.Delay = append(frames.Delay, delay)
		previous = current
	}

	if err := gif.EncodeAll(w, frames); err != nil {
		return fmt.Errorf("image: encode GIF: %w", err)
	}
	return nil
}

func loadPieceSprites() (map[chess.Piece]*stdimage.RGBA, error) {
	return loadPieceSpritesFrom(internal.MustSVG)
}

func loadPieceSpritesFrom(readSVG func(string) string) (map[chess.Piece]*stdimage.RGBA, error) {
	sprites := make(map[chess.Piece]*stdimage.RGBA, 12)
	for pieceType, name := range pieceTypeMap {
		if pieceType == chess.NoPieceType {
			continue
		}
		for _, color := range []chess.Color{chess.White, chess.Black} {
			filename := fmt.Sprintf("pieces/%s%s.svg", color.String(), name)
			svgData := readSVG(filename)
			sprite, err := parsePieceSprite(filename, svgData)
			if err != nil {
				return nil, err
			}
			sprites[chess.NewPiece(pieceType, color)] = sprite
		}
	}
	return sprites, nil
}

func parsePieceSprite(filename, svgData string) (*stdimage.RGBA, error) {
	icon, err := oksvg.ReadIconStream(strings.NewReader(svgData), oksvg.StrictErrorMode)
	if err != nil {
		return nil, fmt.Errorf("image: parse piece SVG %q: %w", filename, err)
	}
	sprite := stdimage.NewRGBA(stdimage.Rect(0, 0, sqWidth, sqHeight))
	icon.SetTarget(0, 0, sqWidth, sqHeight)
	scanner := rasterx.NewScannerGV(sqWidth, sqHeight, sprite, sprite.Bounds())
	icon.Draw(rasterx.NewDasher(sqWidth, sqHeight, scanner), 1)
	return sprite, nil
}

func boardOrder(perspective chess.Color) ([]chess.Rank, []chess.File) {
	ranks := orderOfRanks
	files := orderOfFiles
	if perspective == chess.Black {
		ranks = orderOfRanksBlack
		files = orderOfFilesBlack
	}
	return ranks, files
}

func drawBoardBackground(e *encoder) *stdimage.RGBA {
	frame := stdimage.NewRGBA(stdimage.Rect(0, 0, boardWidth, boardHeight))
	ranks, files := boardOrder(e.perspective)
	for row, rank := range ranks {
		for column, file := range files {
			square := chess.NewSquare(file, rank)
			rect := stdimage.Rect(column*sqWidth, row*sqHeight, (column+1)*sqWidth, (row+1)*sqHeight)
			draw.Draw(frame, rect, stdimage.NewUniform(opaqueColor(e.colorForSquare(square))), stdimage.Point{}, draw.Src)
			if mark, ok := e.marks[square]; ok {
				draw.Draw(frame, rect, stdimage.NewUniform(markColor(mark)), stdimage.Point{}, draw.Over)
			}
		}
	}
	return frame
}

func redrawGIFSquare(canvas, background *stdimage.RGBA, bounds stdimage.Rectangle, square chess.Square, row, column int, piece chess.Piece, e *encoder, sprites map[chess.Piece]*stdimage.RGBA) {
	draw.Draw(canvas, bounds, background, bounds.Min, draw.Src)
	if piece != chess.NoPiece {
		if sprite := sprites[piece]; sprite != nil {
			draw.Draw(canvas, bounds, sprite, stdimage.Point{}, draw.Over)
		}
	}
	drawCoordinates(canvas, square, row, column, e)
}

func drawCoordinates(dst draw.Image, square chess.Square, row, column int, e *encoder) {
	if column == 0 {
		drawLabel(dst, e.colorForText(square), square.Rank().String(), column*sqWidth+sqWidth/20, row*sqHeight+sqHeight*5/20)
	}
	if row == len(orderOfRanks)-1 {
		labelWidth := font.MeasureString(basicfont.Face7x13, square.File().String()).Ceil()
		x := column*sqWidth + sqWidth*19/20 - labelWidth
		y := row*sqHeight + sqHeight - sqHeight/15
		drawLabel(dst, e.colorForText(square), square.File().String(), x, y)
	}
}

func drawLabel(dst draw.Image, c color.Color, label string, x, baseline int) {
	drawer := font.Drawer{
		Dst:  dst,
		Src:  stdimage.NewUniform(opaqueColor(c)),
		Face: basicfont.Face7x13,
		Dot:  fixed.P(x, baseline),
	}
	drawer.DrawString(label)
}

func markColor(c color.Color) color.Color {
	r, g, b, _ := opaqueColor(c).RGBA()
	return color.NRGBA{R: uint8(r >> 8), G: uint8(g >> 8), B: uint8(b >> 8), A: 51}
}

func quantizeFrame(src stdimage.Image, bounds stdimage.Rectangle, framePalette color.Palette) *stdimage.Paletted {
	dst := stdimage.NewPaletted(bounds, framePalette)
	draw.Draw(dst, bounds, src, bounds.Min, draw.Src)
	return dst
}

func gifPalette(e *encoder) color.Palette {
	pal := make(color.Palette, 0, 256)
	for _, c := range []color.Color{
		e.light,
		e.dark,
		color.Black,
		color.White,
		e.colorForText(chess.A1),
	} {
		pal = appendUniqueColor(pal, opaqueColor(c))
	}
	for square, mark := range e.marks {
		pal = appendUniqueColor(pal, blendedColor(e.colorForSquare(square), mark))
	}
	for _, c := range palette.Plan9 {
		pal = appendUniqueColor(pal, opaqueColor(c))
		if len(pal) == 256 {
			break
		}
	}
	return pal
}

func appendUniqueColor(p color.Palette, c color.Color) color.Palette {
	c = color.RGBAModel.Convert(c)
	for _, existing := range p {
		if colorsEqual(existing, c) {
			return p
		}
	}
	if len(p) < 256 {
		return append(p, c)
	}
	return p
}

func opaqueColor(c color.Color) color.Color {
	r, g, b, a := c.RGBA()
	if a == 0 {
		return color.RGBA{A: 255}
	}
	return color.RGBA{R: uint8(r >> 8), G: uint8(g >> 8), B: uint8(b >> 8), A: 255}
}

func blendedColor(base, overlay color.Color) color.Color {
	br, bg, bb, _ := opaqueColor(base).RGBA()
	or, og, ob, _ := opaqueColor(overlay).RGBA()
	return color.RGBA{
		R: uint8((br*4 + or) / 5 >> 8),
		G: uint8((bg*4 + og) / 5 >> 8),
		B: uint8((bb*4 + ob) / 5 >> 8),
		A: 255,
	}
}

func colorsEqual(a, b color.Color) bool {
	ar, ag, ab, aa := a.RGBA()
	br, bg, bb, ba := b.RGBA()
	return ar == br && ag == bg && ab == bb && aa == ba
}
