package opening

import (
	"bytes"
	"testing"

	"github.com/corentings/chess/v2"
)

func BenchmarkNewBookECOData(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_, err := NewBook(bytes.NewReader(ecoData))
		if err != nil {
			b.Fatal(err)
		}
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
