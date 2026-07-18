package chess

import (
	"bytes"
	"errors"
	"io"
	"testing"
)

// Alloc-budget ratchet: ceilings sit ~5% above the post-ticket-06 baseline.
// Baseline observed (AllocsPerRun, 10 iterations):
//   - big.pgn:     ~544,000 allocs/run
//   - big_big.pgn: ~1,925,000 allocs/run
//
// Compared to the pre-cursor baseline (~690k / ~2.86M) this is a 21-33%
// reduction from dropping per-node *Position and per-MoveHistory position
// caches; positions are now resolved lazily via the MoveTree cursor.
// Lower as fixes land; never raise without confirming the alloc growth is real.
const (
	pgnAllocBudgetBigBig = 2_050_000
	pgnAllocBudgetBig    = 575_000
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
