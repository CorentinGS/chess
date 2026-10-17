package chess

import (
	"testing"
)

func TestOutcomeStringAndParse(t *testing.T) {
	cases := []struct {
		o     Outcome
		token string
		valid bool
	}{
		{NoOutcome, "*", true},
		{WhiteWon, "1-0", true},
		{BlackWon, "0-1", true},
		{Draw, "1/2-1/2", true},
	}
	for _, c := range cases {
		if got := c.o.String(); got != c.token {
			t.Errorf("%v.String() = %q, want %q", c.o, got, c.token)
		}
		got, err := ParseOutcome(c.token)
		if err != nil {
			t.Errorf("ParseOutcome(%q) error = %v", c.token, err)
		}
		if got != c.o {
			t.Errorf("ParseOutcome(%q) = %v, want %v", c.token, got, c.o)
		}
	}

	if _, err := ParseOutcome("banana"); err == nil {
		t.Fatal("expected error for invalid outcome")
	}
}
