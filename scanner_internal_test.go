package chess

import "testing"

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
