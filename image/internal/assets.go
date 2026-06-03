package internal

import (
	"embed"
	"fmt"
)

//go:embed pieces/*.svg
var piecesFS embed.FS

var pieceSVGs = map[string]string{}

func init() {
	pieceFiles := []string{
		"pieces/bB.svg",
		"pieces/bK.svg",
		"pieces/bN.svg",
		"pieces/bP.svg",
		"pieces/bQ.svg",
		"pieces/bR.svg",
		"pieces/wB.svg",
		"pieces/wK.svg",
		"pieces/wN.svg",
		"pieces/wP.svg",
		"pieces/wQ.svg",
		"pieces/wR.svg",
	}
	for _, f := range pieceFiles {
		b, err := piecesFS.ReadFile(f)
		if err != nil {
			panic(fmt.Sprintf("failed to read embedded asset %s: %v", f, err))
		}
		pieceSVGs[f] = string(b)
	}
}

// MustSVG returns the raw SVG string for the named piece asset.
func MustSVG(name string) string {
	s, ok := pieceSVGs[name]
	if !ok {
		panic(fmt.Sprintf("asset: Asset(%s): asset not found", name))
	}
	return s
}
