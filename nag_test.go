package chess

import "testing"

func TestParseNAG(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		want    string
		wantErr bool
	}{
		// Symbolic spellings → canonical numeric.
		{"bang", "!", "$1", false},
		{"qmark", "?", "$2", false},
		{"double_bang", "!!", "$3", false},
		{"double_qmark", "??", "$4", false},
		{"bang_qmark", "!?", "$5", false},
		{"qmark_bang", "?!", "$6", false},

		// Numeric forms, including extended ChessBase codes.
		{"numeric_one", "$1", "$1", false},
		{"numeric_zero", "$0", "$0", false},
		{"leading_zeros", "$01", "$1", false},
		{"extended", "$1406", "$1406", false},
		{"double_digit", "$13", "$13", false},

		// Malformed inputs are rejected.
		{"empty", "", "", true},
		{"bare_dollar", "$", "", true},
		{"dollar_letter", "$x", "", true},
		{"dollar_space_digit", "$ 1", "", true},
		{"triple_bang", "!!!", "", true},
		{"unknown_symbol", "@", "", true},
		{"letter", "N", "", true},
		{"dollar_minus", "$-1", "", true},
		{"dollar_plus", "$+1", "", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseNAG(tt.input)
			if (err != nil) != tt.wantErr {
				t.Fatalf("ParseNAG(%q) error = %v, wantErr %v", tt.input, err, tt.wantErr)
			}
			if !tt.wantErr && got != tt.want {
				t.Errorf("ParseNAG(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}
