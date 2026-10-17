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
