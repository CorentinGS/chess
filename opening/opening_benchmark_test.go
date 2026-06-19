package opening

import (
	"bytes"
	"testing"

	"github.com/corentings/chess/v3"
)

func BenchmarkNewBookECOData(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_, err := NewBook(bytes.NewReader(ecoData))
		if err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkDefaultBookCached measures the warm path promised by ADR-005:
// repeated DefaultBook() lookups must be ~0ms / 0 allocs after the first call
// initialises the singleton via sync.Once.
func BenchmarkDefaultBookCached(b *testing.B) {
	// Warm up the cache so we measure the steady-state lookup, not the parse.
	_, _ = DefaultBook()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = DefaultBook()
	}
}

func BenchmarkFind(b *testing.B) {
	book, err := NewBook(bytes.NewReader(ecoData))
	if err != nil {
		b.Fatal(err)
	}
	g := chess.NewGame()
	for _, move := range []string{"e4", "e5", "Nf3", "Nc6", "Bb5"} {
		if err := g.PushMove(move, nil); err != nil {
			b.Fatal(err)
		}
	}
	moves := g.Moves()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if book.Find(moves) == nil {
			b.Fatal("expected opening")
		}
	}
}

func BenchmarkPossible(b *testing.B) {
	book, err := NewBook(bytes.NewReader(ecoData))
	if err != nil {
		b.Fatal(err)
	}
	g := chess.NewGame()
	if err := g.PushMove("g3", nil); err != nil {
		b.Fatal(err)
	}
	moves := g.Moves()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if len(book.Possible(moves)) == 0 {
			b.Fatal("expected possible openings")
		}
	}
}

func BenchmarkOpeningGame(b *testing.B) {
	book, err := NewBook(bytes.NewReader(ecoData))
	if err != nil {
		b.Fatal(err)
	}
	g := chess.NewGame()
	for _, move := range []string{"e4", "e5"} {
		if err := g.PushMove(move, nil); err != nil {
			b.Fatal(err)
		}
	}
	o := book.Find(g.Moves())
	if o == nil {
		b.Fatal("expected opening")
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if o.Game() == nil {
			b.Fatal("expected game")
		}
	}
}
