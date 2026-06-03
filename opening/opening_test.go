package opening_test

import (
	"bytes"
	"fmt"
	"sync"
	"testing"

	"github.com/corentings/chess/v2"
	"github.com/corentings/chess/v2/opening"
)

func ExampleDefaultBook_find() {
	g := chess.NewGame()
	_ = g.PushMove("e4", nil)
	_ = g.PushMove("e6", nil)

	// print French Defense
	book, err := opening.DefaultBook()
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	o := book.Find(g.Moves())
	fmt.Println(o.Title())

	// Output: French Defense
}

func ExampleDefaultBook_possible() {
	g := chess.NewGame()
	_ = g.PushMove("e4", nil)
	_ = g.PushMove("d5", nil)

	// print all variantions of the Scandinavian Defense
	book, err := opening.DefaultBook()
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	for _, o := range book.Possible(g.Moves()) {
		if o.Title() == "Scandinavian Defense" {
			fmt.Println(o.Title())
		}
	}

	// Output:
	// Scandinavian Defense
	// Scandinavian Defense
}

func TestDefaultBook(t *testing.T) {
	book, err := opening.DefaultBook()
	if err != nil {
		t.Fatalf("DefaultBook() failed: %v", err)
	}
	if book == nil {
		t.Fatal("DefaultBook() returned nil")
	}
	// Second call should return the same instance
	book2, err := opening.DefaultBook()
	if err != nil {
		t.Fatalf("DefaultBook() second call failed: %v", err)
	}
	if book != book2 {
		t.Fatal("DefaultBook() returned different instances")
	}
}

func TestFind(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("d5", nil); err != nil {
		t.Fatal(err)
	}
	book, err := opening.DefaultBook()
	if err != nil {
		t.Fatalf("DefaultBook() failed: %v", err)
	}
	o := book.Find(g.Moves())
	expected := "Scandinavian Defense"
	if o == nil || o.Title() != expected {
		if o == nil {
			t.Fatalf("expected to find opening %s but got nil", expected)
		}
		t.Fatalf("expected to find opening %s but got %s", expected, o.Title())
	}
}

func TestPossible(t *testing.T) {
	g := chess.NewGame()
	if err := g.PushMove("g3", nil); err != nil {
		t.Fatal(err)
	}
	book, err := opening.DefaultBook()
	if err != nil {
		t.Fatalf("DefaultBook() failed: %v", err)
	}
	openings := book.Possible(g.Moves())
	actual := len(openings)
	if actual != 22 {
		t.Fatalf("expected %d possible openings but got %d", 22, actual)
	}
}

func TestOpeningGameRace(t *testing.T) {
	book, err := opening.DefaultBook()
	if err != nil {
		t.Fatalf("DefaultBook() failed: %v", err)
	}
	g := chess.NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	o := book.Find(g.Moves())
	if o == nil {
		t.Fatal("expected to find an opening")
	}

	// Call Game() from multiple goroutines concurrently
	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			game := o.Game()
			if game == nil {
				t.Error("Game() returned nil")
			}
		}()
	}
	wg.Wait()
}

func BenchmarkDefaultBook(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_, err := opening.DefaultBook()
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkDefaultBookCached(b *testing.B) {
	// Warm up the cache
	_, _ = opening.DefaultBook()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = opening.DefaultBook()
	}
}

func BenchmarkNewBookParse(b *testing.B) {
	// This benchmark measures the full parse cost
	for i := 0; i < b.N; i++ {
		_, _ = opening.NewBook(bytes.NewReader(nil))
	}
}
