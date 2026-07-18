package chess

import (
	"bytes"
	"errors"
	"io"
	"testing"
)

// Alloc-budget ratchet: ceilings sit ~1% above the post-ticket-02 baseline
// (cursor infrastructure: t.rootPos / t.pos / t.undos on MoveTree). Lower as
// fixes land; never raise without confirming the alloc growth is real.
// Ticket 04 (drop MoveNode.position) is expected to ratchet these back below
// the pre-cursor ceilings.
const (
	pgnAllocBudgetBigBig = 2_900_000
	pgnAllocBudgetBig    = 700_000
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
