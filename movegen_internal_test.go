package chess

import "testing"

func TestMoveMatchesMode(t *testing.T) {
	tests := []struct {
		name           string
		ownKingInCheck bool
		mode           moveGenerationMode
		want           bool
	}{
		{
			name:           "legal move in legal-only mode is kept",
			ownKingInCheck: false,
			mode:           generateLegalOnly,
			want:           true,
		},
		{
			name:           "legal move in unsafe-only mode is dropped",
			ownKingInCheck: false,
			mode:           generateUnsafeOnly,
			want:           false,
		},
		{
			name:           "illegal move in legal-only mode is dropped",
			ownKingInCheck: true,
			mode:           generateLegalOnly,
			want:           false,
		},
		{
			name:           "illegal move in unsafe-only mode is kept",
			ownKingInCheck: true,
			mode:           generateUnsafeOnly,
			want:           true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := moveMatchesMode(tc.ownKingInCheck, tc.mode)
			if got != tc.want {
				t.Errorf("moveMatchesMode(ownKingInCheck=%v, mode=%v) = %v; want %v",
					tc.ownKingInCheck, tc.mode, got, tc.want)
			}
		})
	}
}
