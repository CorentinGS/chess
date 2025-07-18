package opening_test

import (
	"fmt"
	"testing"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/opening"
)

func Find() {
	g := chess.NewGame()
	_ = g.PushMove("e4", nil)
	_ = g.PushMove("e6", nil)

	// print French Defense
	book := opening.NewBookECO()
	o := book.Find(g.Moves())
	fmt.Println(o.Title())

	// Output: French Defense
}

func Possible() {
	g := chess.NewGame()
	_ = g.PushMove("e4", nil)
	_ = g.PushMove("d5", nil)

	// print all variantions of the Scandinavian Defense
	book := opening.NewBookECO()
	for _, o := range book.Possible(g.Moves()) {
		if o.Title() == "Scandinavian Defense" {
			fmt.Println(o.Title())
		}
	}

	// Output:
	// Scandinavian Defense
	// Scandinavian Defense
}

func TestFind(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("d5", nil); err != nil {
		t.Fatal(err)
	}
	book := opening.NewBookECO()
	o := book.Find(g.Moves())
	expected := "Scandinavian Defense"
	if o == nil || o.Title() != expected {
		t.Fatalf("expected to find opening %s but got %s", expected, o.Title())
	}
}

func TestPossible(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("g3", nil); err != nil {
		t.Fatal(err)
	}
	book := opening.NewBookECO()
	openings := book.Possible(g.Moves())
	actual := len(openings)
	if actual != 22 {
		t.Fatalf("expected %d possible openings but got %d", 22, actual)
	}
}

func BenchmarkNewBookECO(b *testing.B) {
	for i := 0; i < b.N; i++ {
		opening.NewBookECO()
	}
}
