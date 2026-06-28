package opening_test

import (
	"bytes"
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/corentings/chess/v3"
	"github.com/corentings/chess/v3/opening"
)

func ExampleDefaultBook_find() {
	g := chess.NewGame()
	_, _ = g.PushMove("e4", nil)
	_, _ = g.PushMove("e6", nil)

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
	_, _ = g.PushMove("e4", nil)
	_, _ = g.PushMove("d5", nil)

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
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("d5", nil); err != nil {
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
	if _, err := g.PushMove("g3", nil); err != nil {
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
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
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

func TestNewBookFailsFastOnMalformedRow(t *testing.T) {
	data := "eco\tname\tfyn\tmoves\nA00\tBroken\tonly-three-columns\n"

	_, err := opening.NewBook(strings.NewReader(data))
	if err == nil {
		t.Fatal("NewBook() succeeded for malformed row")
	}
	if !strings.Contains(err.Error(), "ECO row 2") {
		t.Fatalf("expected row-numbered error, got %v", err)
	}
	if !strings.Contains(err.Error(), "expected at least 4 columns") {
		t.Fatalf("expected malformed column error, got %v", err)
	}
}

func TestNewBookFailsFastOnInvalidOpeningMove(t *testing.T) {
	data := "eco\tname\tfyn\tmoves\nA00\tBroken Opening\t\t1.e2e5\n"

	_, err := opening.NewBook(strings.NewReader(data))
	if err == nil {
		t.Fatal("NewBook() succeeded for invalid opening move")
	}
	if !strings.Contains(err.Error(), "ECO row 2 (A00 Broken Opening)") {
		t.Fatalf("expected row and opening context, got %v", err)
	}
	if !strings.Contains(err.Error(), "apply move e2e5") {
		t.Fatalf("expected invalid move context, got %v", err)
	}
}

func TestOpeningGameReturnsCallerOwnedGame(t *testing.T) {
	data := "eco\tname\tfyn\tmoves\nC20\tKing Pawn Game\t\t1.e2e4 e7e5\n"
	book, err := opening.NewBook(strings.NewReader(data))
	if err != nil {
		t.Fatalf("NewBook() failed: %v", err)
	}

	g := chess.NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	o := book.Find(g.Moves())
	if o == nil {
		t.Fatal("expected to find opening")
	}

	game1 := o.Game()
	if game1 == nil {
		t.Fatal("Game() returned nil")
	}
	if _, err := game1.PushMove("Nf3", nil); err != nil {
		t.Fatal(err)
	}

	game2 := o.Game()
	if game2 == nil {
		t.Fatal("Game() returned nil on second call")
	}
	if got := len(game2.Moves()); got != 2 {
		t.Fatalf("expected second Game() result to have 2 moves, got %d", got)
	}
	if game1 == game2 {
		t.Fatal("Game() returned the same pointer twice")
	}
}

func BenchmarkDefaultBook(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_, err := opening.DefaultBook()
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkNewBookParse(b *testing.B) {
	// This benchmark measures the full parse cost
	for i := 0; i < b.N; i++ {
		_, _ = opening.NewBook(bytes.NewReader(nil))
	}
}
