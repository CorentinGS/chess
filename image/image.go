// Package image renders chess positions as images.
package image

import (
	"embed"
	"errors"
	"fmt"
	"image/color"
	"io"
	"strings"

	"github.com/corentings/chess/v3"
)

const (
	defaultSquareSize = 45
	markOpacity       = "0.2"
	pieceBaseSize     = 45.0
)

var (
	defaultLightSquare = color.RGBA{R: 235, G: 209, B: 166, A: 255}
	defaultDarkSquare  = color.RGBA{R: 165, G: 117, B: 81, A: 255}

	orderOfRanks      = []chess.Rank{chess.Rank8, chess.Rank7, chess.Rank6, chess.Rank5, chess.Rank4, chess.Rank3, chess.Rank2, chess.Rank1}
	orderOfRanksBlack = []chess.Rank{chess.Rank1, chess.Rank2, chess.Rank3, chess.Rank4, chess.Rank5, chess.Rank6, chess.Rank7, chess.Rank8}
	orderOfFiles      = []chess.File{chess.FileA, chess.FileB, chess.FileC, chess.FileD, chess.FileE, chess.FileF, chess.FileG, chess.FileH}
	orderOfFilesBlack = []chess.File{chess.FileH, chess.FileG, chess.FileF, chess.FileE, chess.FileD, chess.FileC, chess.FileB, chess.FileA}
)

//go:embed internal/pieces/*.svg
var piecesFS embed.FS

var pieceSVGs = loadPieceSVGs()

// SVGOptions customizes SVG rendering.
// Zero-valued fields use defaults.
type SVGOptions struct {
	LightSquare     color.Color
	DarkSquare      color.Color
	Marks           map[chess.Square]color.Color
	Perspective     chess.Color
	SquareSize      int
	HideCoordinates bool
}

type svgOptions struct {
	lightSquare     color.Color
	darkSquare      color.Color
	marks           map[chess.Square]color.Color
	perspective     chess.Color
	squareSize      int
	hideCoordinates bool
}

// SVG writes the position SVG representation into the writer.
func SVG(w io.Writer, pos *chess.Position, opts *SVGOptions) error {
	if w == nil {
		return errors.New("image: nil writer")
	}
	if pos == nil || pos.Board() == nil {
		return errors.New("image: nil position")
	}

	o := normalizeSVGOptions(opts)
	r := &svgRenderer{w: w, opts: o}
	r.render(pos)
	return r.err
}

type svgRenderer struct {
	w    io.Writer
	opts svgOptions
	err  error
}

func (r *svgRenderer) render(pos *chess.Position) {
	boardSize := r.opts.squareSize * 8
	r.write(`<?xml version="1.0" encoding="UTF-8"?>` + "\n")
	r.writef(`<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">`+"\n", boardSize, boardSize, boardSize, boardSize)

	boardMap := pos.Board().SquareMap()
	ranks, files := rankAndFileOrder(r.opts.perspective)
	lastRank := len(ranks) - 1
	for i, rank := range ranks {
		for j, file := range files {
			x := j * r.opts.squareSize
			y := i * r.opts.squareSize
			sq := chess.NewSquare(file, rank)
			r.renderSquare(x, y, sq)
			r.renderMark(x, y, sq)
			r.renderPiece(x, y, sq, boardMap[sq])
			if !r.opts.hideCoordinates {
				r.renderCoordinates(x, y, i, j, lastRank, sq)
			}
		}
	}
	r.write("</svg>\n")
}

func (r *svgRenderer) renderSquare(x, y int, sq chess.Square) {
	squareClass := "light"
	fill := r.opts.lightSquare
	if isDarkSquare(sq) {
		squareClass = "dark"
		fill = r.opts.darkSquare
	}
	r.writef(`<rect class="square %s" data-square="%s" x="%d" y="%d" width="%d" height="%d" fill="%s"/>`+"\n",
		squareClass, sq.String(), x, y, r.opts.squareSize, r.opts.squareSize, colorToHex(fill))
}

func (r *svgRenderer) renderMark(x, y int, sq chess.Square) {
	markColor, ok := r.opts.marks[sq]
	if !ok {
		return
	}
	r.writef(`<rect class="mark" data-square="%s" x="%d" y="%d" width="%d" height="%d" fill="%s" fill-opacity="%s"/>`+"\n",
		sq.String(), x, y, r.opts.squareSize, r.opts.squareSize, colorToHex(markColor), markOpacity)
}

func (r *svgRenderer) renderPiece(x, y int, sq chess.Square, p chess.Piece) {
	if p == chess.NoPiece {
		return
	}
	inner, ok := pieceSVGs[pieceFileName(p)]
	if !ok {
		return
	}
	r.writef(`<g class="piece %s %s" data-square="%s" transform="translate(%d %d) scale(%s)">`+"\n",
		pieceColorClass(p.Color()), pieceTypeClass(p.Type()), sq.String(), x, y, scaleString(r.opts.squareSize))
	r.write(inner)
	if !strings.HasSuffix(inner, "\n") {
		r.write("\n")
	}
	r.write("</g>\n")
}

func (r *svgRenderer) renderCoordinates(x, y, rankIndex, fileIndex, lastRank int, sq chess.Square) {
	textColor := r.opts.darkSquare
	if isDarkSquare(sq) {
		textColor = r.opts.lightSquare
	}
	fontSize := r.opts.squareSize * 11 / defaultSquareSize
	if fontSize < 1 {
		fontSize = 1
	}
	if fileIndex == 0 {
		r.writef(`<text class="coordinate rank" x="%d" y="%d" font-size="%d" fill="%s">%s</text>`+"\n",
			x+r.opts.squareSize/20, y+r.opts.squareSize*5/20, fontSize, colorToHex(textColor), sq.Rank().String())
	}
	if rankIndex == lastRank {
		r.writef(`<text class="coordinate file" x="%d" y="%d" text-anchor="end" font-size="%d" fill="%s">%s</text>`+"\n",
			x+r.opts.squareSize*19/20, y+r.opts.squareSize-r.opts.squareSize/15, fontSize, colorToHex(textColor), sq.File().String())
	}
}

func (r *svgRenderer) write(s string) {
	if r.err != nil {
		return
	}
	_, r.err = io.WriteString(r.w, s)
}

func (r *svgRenderer) writef(format string, args ...any) {
	if r.err != nil {
		return
	}
	_, r.err = fmt.Fprintf(r.w, format, args...)
}

func normalizeSVGOptions(opts *SVGOptions) svgOptions {
	o := svgOptions{
		lightSquare: defaultLightSquare,
		darkSquare:  defaultDarkSquare,
		marks:       map[chess.Square]color.Color{},
		perspective: chess.White,
		squareSize:  defaultSquareSize,
	}
	if opts == nil {
		return o
	}
	if opts.LightSquare != nil {
		o.lightSquare = opts.LightSquare
	}
	if opts.DarkSquare != nil {
		o.darkSquare = opts.DarkSquare
	}
	if opts.Perspective == chess.Black {
		o.perspective = chess.Black
	}
	if opts.SquareSize > 0 {
		o.squareSize = opts.SquareSize
	}
	o.hideCoordinates = opts.HideCoordinates
	for sq, c := range opts.Marks {
		if isValidSquare(sq) && c != nil {
			o.marks[sq] = c
		}
	}
	return o
}

func rankAndFileOrder(perspective chess.Color) ([]chess.Rank, []chess.File) {
	if perspective == chess.Black {
		return orderOfRanksBlack, orderOfFilesBlack
	}
	return orderOfRanks, orderOfFiles
}

func isValidSquare(sq chess.Square) bool {
	return sq >= chess.A1 && sq <= chess.H8
}

func isDarkSquare(sq chess.Square) bool {
	return (int(sq.File())+int(sq.Rank()))%2 == 0
}

func colorToHex(c color.Color) string {
	r, g, b, _ := c.RGBA()
	return fmt.Sprintf("#%02x%02x%02x", uint8(r>>8), uint8(g>>8), uint8(b>>8))
}

func loadPieceSVGs() map[string]string {
	pieceFiles := []string{
		"internal/pieces/bB.svg",
		"internal/pieces/bK.svg",
		"internal/pieces/bN.svg",
		"internal/pieces/bP.svg",
		"internal/pieces/bQ.svg",
		"internal/pieces/bR.svg",
		"internal/pieces/wB.svg",
		"internal/pieces/wK.svg",
		"internal/pieces/wN.svg",
		"internal/pieces/wP.svg",
		"internal/pieces/wQ.svg",
		"internal/pieces/wR.svg",
	}
	pieceSVGs := make(map[string]string, len(pieceFiles))
	for _, f := range pieceFiles {
		b, err := piecesFS.ReadFile(f)
		if err != nil {
			panic(fmt.Sprintf("image: failed to read embedded asset %s: %v", f, err))
		}
		pieceSVGs[f] = innerSVG(f, string(b))
	}
	return pieceSVGs
}

func innerSVG(name, svg string) string {
	svg = strings.TrimSpace(svg)
	if !strings.HasPrefix(svg, "<svg") {
		panic(fmt.Sprintf("image: malformed embedded asset %s", name))
	}
	start := strings.Index(svg, ">")
	end := strings.LastIndex(svg, "</svg>")
	if start == -1 || end == -1 || end <= start {
		panic(fmt.Sprintf("image: malformed embedded asset %s", name))
	}
	return strings.TrimSpace(svg[start+1 : end])
}

func pieceFileName(p chess.Piece) string {
	return fmt.Sprintf("internal/pieces/%s%s.svg", p.Color().String(), pieceTypeMap[p.Type()])
}

func pieceColorClass(c chess.Color) string {
	if c == chess.Black {
		return "black"
	}
	return "white"
}

func pieceTypeClass(t chess.PieceType) string {
	switch t {
	case chess.King:
		return "king"
	case chess.Queen:
		return "queen"
	case chess.Rook:
		return "rook"
	case chess.Bishop:
		return "bishop"
	case chess.Knight:
		return "knight"
	case chess.Pawn:
		return "pawn"
	}
	return "piece"
}

func scaleString(squareSize int) string {
	return fmt.Sprintf("%.6g", float64(squareSize)/pieceBaseSize)
}

var pieceTypeMap = map[chess.PieceType]string{
	chess.King:        "K",
	chess.Queen:       "Q",
	chess.Rook:        "R",
	chess.Bishop:      "B",
	chess.Knight:      "N",
	chess.Pawn:        "P",
	chess.NoPieceType: "",
}
