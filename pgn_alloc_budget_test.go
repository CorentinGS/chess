package chess

import (
	"bytes"
	"errors"
	"io"
	"testing"
)

// Alloc-budget ratchet: ceilings sit ~3% above the post-fix baseline. Lower
// as fixes land; never raise without confirming the alloc growth is real.
const (
	pgnAllocBudgetBigBig = 2_850_000
	pgnAllocBudgetBig    = 685_000
)

func TestPGNDecode_AllocBudget_BigBig(t *testing.T) {
	assertPGNAllocBudget(t, "big_big.pgn", pgnAllocBudgetBigBig)
}

func TestPGNDecode_AllocBudget_Big(t *testing.T) {
	assertPGNAllocBudget(t, "big.pgn", pgnAllocBudgetBig)
}

func assertPGNAllocBudget(t *testing.T, fixture string, budget float64) {
	t.Helper()
	data := readPGNFixture(fixture)
	allocs := testing.AllocsPerRun(10, func() {
		dec := NewPGNDecoder(bytes.NewReader(data))
		for {
			g, err := dec.Decode()
			if errors.Is(err, io.EOF) {
				return
			}
			if err != nil {
				if isKnownInconsistentPgn(err) {
					continue
				}
				t.Fatalf("decode error: %v", err)
			}
			_ = g.Outcome()
		}
	})
	if allocs > budget {
		t.Fatalf("decode %s: %.0f allocs/run, budget %.0f", fixture, allocs, budget)
	}
}
