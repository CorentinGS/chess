package chess

import (
	"bytes"
	"context"
	"strings"
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

func BenchmarkPGN_FrameSingleLargeGame(b *testing.B) {
	data := []byte("[Event \"Large\"]\n\n1. " + strings.Repeat("e4 e5 ", 30_000) + "*\n")
	for _, size := range []struct {
		name       string
		bufferSize int
	}{
		{"Buffer1KiB", 1024},
		{"Buffer32KiB", 32 * 1024},
		{"Buffer256KiB", 256 * 1024},
	} {
		b.Run(size.name, func(b *testing.B) {
			b.SetBytes(int64(len(data)))
			b.ReportAllocs()
			b.ResetTimer()
			for range b.N {
				games := 0
				for _, err := range PGNRecords(context.Background(), bytes.NewReader(data), WithPGNBufferSize(size.bufferSize)) {
					if err != nil {
						b.Fatalf("frame error: %v", err)
					}
					games++
				}
				if games != 1 {
					b.Fatalf("expected one game, framed %d", games)
				}
			}
		})
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
