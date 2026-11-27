package image

import (
	"image/color"
	"testing"
)

func TestLoadPieceSpritesReturnsParseError(t *testing.T) {
	_, err := loadPieceSpritesFrom(func(string) string {
		return `<svg><g transform="invalid"/></svg>`
	})
	if err == nil {
		t.Fatal("loadPieceSpritesFrom returned nil error for invalid SVG")
	}
}

func TestAppendUniqueColorKeepsFullPalette(t *testing.T) {
	palette := make(color.Palette, 256)
	for i := range palette {
		palette[i] = color.RGBA{R: uint8(i), A: 255}
	}

	got := appendUniqueColor(palette, color.RGBA{G: 1, A: 255})
	if len(got) != len(palette) {
		t.Fatalf("palette length = %d, want %d", len(got), len(palette))
	}
}

func TestOpaqueColorMakesTransparentColorOpaque(t *testing.T) {
	got := opaqueColor(color.RGBA{})
	want := color.RGBA{A: 255}
	if got != want {
		t.Errorf("opaqueColor(transparent) = %v, want %v", got, want)
	}
}
