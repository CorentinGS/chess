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

	for range b.N {
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

func BenchmarkPGNRecord_Tags_Big(b *testing.B) {
	data := readPGNFixture("big.pgn")
	meta := pgnMeta("big.pgn")
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		for record, err := range PGNRecords(context.Background(), bytes.NewReader(data)) {
			if err != nil {
				b.Fatalf("record error: %v", err)
			}
			if _, err := record.Tags(); err != nil {
				b.Fatalf("Tags error: %v", err)
			}
		}
	}
}

func BenchmarkPGNEvents_Big(b *testing.B) {
	data := readPGNFixture("big.pgn")
	meta := pgnMeta("big.pgn")
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		for _, err := range PGNEvents(bytes.NewReader(data)) {
			if err != nil {
				b.Fatalf("event error: %v", err)
			}
		}
	}
}

func BenchmarkPGNRecord_TagsThenDecode_Big(b *testing.B) {
	data := readPGNFixture("big.pgn")
	meta := pgnMeta("big.pgn")
	b.SetBytes(meta.size)
	b.ReportAllocs()
	b.ResetTimer()

	for range b.N {
		for record, err := range PGNRecords(context.Background(), bytes.NewReader(data)) {
			if err != nil {
				b.Fatalf("record error: %v", err)
			}
			if _, err := record.Tags(); err != nil {
				b.Fatalf("Tags error: %v", err)
			}
			if _, err := record.Decode(); err != nil {
				b.Fatalf("Decode error: %v", err)
			}
		}
	}
}
