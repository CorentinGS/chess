# image

## Introduction

**image** is a chess image utility that converts board positions into [SVG](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics), or Scalable Vector Graphics, images. The package has no external dependencies; SVG is written directly to an `io.Writer`.

## Usage

### SVG

The `SVG` function is the primary exported function of the package. It writes an SVG document to the `io.Writer` given. Rendering options are configured through an `SVGOptions` value; `nil` or a zero-valued `SVGOptions` uses defaults (white perspective, default square colors, 45px squares, with rank/file coordinates visible).

```go
f, err := os.Create("output.svg")
if err != nil {
    log.Fatal(err)
}
defer f.Close()

fenStr := "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1"
pos := &chess.Position{}
if err := pos.UnmarshalText([]byte(fenStr)); err != nil {
    log.Fatal(err)
}

if err := image.SVG(f, pos, nil); err != nil {
    log.Fatal(err)
}
```

### Square Colors

The default colors are `color.RGBA{235, 209, 166, 255}` for light squares and `color.RGBA{165, 117, 81, 255}` for dark squares. Both colors are opaque. Set `LightSquare` and `DarkSquare` on `SVGOptions` to customize them. Nil colors fall back to the defaults.

```go
white := color.RGBA{255, 255, 255, 255}
gray := color.RGBA{120, 120, 120, 255}

if err := image.SVG(f, pos, &image.SVGOptions{
    LightSquare: white,
    DarkSquare:  gray,
}); err != nil {
    log.Fatal(err)
}
```

### Marked Squares

`SVGOptions.Marks` highlights individual squares with a translucent overlay (fixed opacity 0.2; mark color controls hue only). A typical use is marking the squares involved in the previous move. Mark keys outside `A1`..`H8` and nil mark colors are ignored.

```go
yellow := color.RGBA{255, 255, 0, 255}
if err := image.SVG(f, pos, &image.SVGOptions{
    Marks: map[chess.Square]color.Color{
        chess.D2: yellow,
        chess.D4: yellow,
    },
}); err != nil {
    log.Fatal(err)
}
```

### Perspective

`SVGOptions.Perspective` selects the orientation of the board. White is the default. Any value other than `chess.Black` falls back to white without error.

```go
if err := image.SVG(f, pos, &image.SVGOptions{
    Perspective: chess.Black,
}); err != nil {
    log.Fatal(err)
}
```

### Square Size and Coordinates

`SquareSize` (in CSS pixels) scales the entire board. The default is 45. The board is always 8x8 squares, so the total SVG width and height are `8 * SquareSize`. A `SquareSize <= 0` falls back to the default. Set `HideCoordinates` to true to omit the rank and file labels.

```go
if err := image.SVG(f, pos, &image.SVGOptions{
    SquareSize:      90,
    HideCoordinates: true,
}); err != nil {
    log.Fatal(err)
}
```

### Errors

`SVG` returns a non-nil error when the writer is nil (`image: nil writer`), when the position is nil or has no board (`image: nil position`), or when an underlying write fails. The first write error is returned unchanged.

## Example Program

```go
package main

import (
    "image/color"
    "log"
    "os"

    "github.com/corentings/chess"
    "github.com/corentings/chess/image"
)

func main() {
    f, err := os.Create("output.svg")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()

    fenStr := "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1"
    pos := &chess.Position{}
    if err := pos.UnmarshalText([]byte(fenStr)); err != nil {
        log.Fatal(err)
    }

    yellow := color.RGBA{255, 255, 0, 255}
    if err := image.SVG(f, pos, &image.SVGOptions{
        Marks: map[chess.Square]color.Color{
            chess.D2: yellow,
            chess.D4: yellow,
        },
    }); err != nil {
        log.Fatal(err)
    }
}
```
