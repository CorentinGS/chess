package chess

import "testing"

func BenchmarkCheckForResult(b *testing.B) {
	data := []byte("1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 1-0\n[Event ")
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		for j := range data {
			checkForResult(data, j)
		}
	}
}

func TestTokenizeIntoReusesProvidedSlice(t *testing.T) {
	game := &GameScanned{Raw: "1. e4 e5 1-0"}
	dst := make([]Token, 0, 32)
	tokens, err := tokenizeInto(game, dst)
	if err != nil {
		t.Fatal(err)
	}
	if len(tokens) == 0 {
		t.Fatal("expected tokens")
	}
	if cap(tokens) != cap(dst) {
		t.Fatalf("tokenizeInto did not reuse destination capacity: got %d, want %d", cap(tokens), cap(dst))
	}
}
