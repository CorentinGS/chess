package opening

import (
	"strings"
	"testing"
)

func TestMustBookPanicsOnInvalidEmbeddedData(t *testing.T) {
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("mustBook() did not panic")
		}
		err, ok := r.(error)
		if !ok {
			t.Fatalf("mustBook() panic = %T(%v), want error", r, r)
		}
		if !strings.Contains(err.Error(), "opening: invalid embedded ECO data") {
			t.Fatalf("expected embedded data context, got %v", err)
		}
		if !strings.Contains(err.Error(), "decode move e2e5") {
			t.Fatalf("expected invalid move context, got %v", err)
		}
	}()

	mustBook(strings.NewReader("eco\tname\tfyn\tmoves\nA00\tBroken Opening\t\t1.e2e5\n"))
}
