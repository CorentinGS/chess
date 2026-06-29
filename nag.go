package chess

import (
	"errors"
	"fmt"
)

// nagSymbolToNumeric maps the six move-quality symbolic spellings to their
// canonical numeric NAG codes. These are the only symbolic forms accepted on
// import; the canonical, stored, and written form is always the numeric "$N".
var nagSymbolToNumeric = map[string]string{
	"!":  "$1",
	"?":  "$2",
	"!!": "$3",
	"??": "$4",
	"!?": "$5",
	"?!": "$6",
}

// ParseNAG normalises a NAG spelling to its canonical numeric "$N" form.
//
// It accepts:
//   - any numeric NAG written as "$" followed by one or more digits (the six
//     standard codes as well as ChessBase-style extended codes such as "$1406"),
//   - the six move-quality symbolic spellings (! ? !! ?? !? ?!), which are
//     mapped to their canonical numeric codes.
//
// Leading zeros in a numeric NAG are stripped ("$01" becomes "$1").
//
// It rejects malformed input such as "", "$", "$x", "$ 1", "!!!", or any other
// unrecognised spelling.
//
// The returned value is always the canonical numeric form and never a symbol.
func ParseNAG(s string) (string, error) {
	if s == "" {
		return "", errors.New("chess: empty NAG")
	}
	if v, ok := nagSymbolToNumeric[s]; ok {
		return v, nil
	}
	if s[0] != '$' {
		return "", fmt.Errorf("chess: invalid NAG %q", s)
	}
	digits := s[1:]
	if digits == "" {
		return "", fmt.Errorf("chess: invalid NAG %q: missing digits", s)
	}
	n := 0
	for _, r := range digits {
		if r < '0' || r > '9' {
			return "", fmt.Errorf("chess: invalid NAG %q", s)
		}
		n = n*10 + int(r-'0')
	}
	return fmt.Sprintf("$%d", n), nil
}
