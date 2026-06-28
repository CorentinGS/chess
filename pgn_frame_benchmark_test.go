package chess

import (
	"bytes"
	"context"
	"testing"
)

func BenchmarkPGN_Frame_Big(b *testing.B) {
	data := readPGNFixture("big.pgn")
	meta := pgnMeta("big.pgn")
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		games := 0
		for _, err := range PGNRecords(context.Background(), bytes.NewReader(data)) {
			if err != nil {
				b.Fatalf("frame error: %v", err)
			}
			games++
		}
		if games != meta.games {
			b.Fatalf("expected %d games, framed %d", meta.games, games)
		}
	}
}
