package chess

import (
	"strings"
	"testing"
)

func TestNewPositionStartingSetup(t *testing.T) {
	got, err := NewPosition(startingSetup)
	if err != nil {
		t.Fatalf("NewPosition(startingSetup): %v", err)
	}
	want := StartingPosition()
	if got.String() != want.String() {
		t.Errorf("FEN mismatch:\n  got  %s\n  want %s", got.String(), want.String())
	}
	if got.ZobristHash() != want.ZobristHash() {
		t.Errorf("Zobrist hash mismatch: got %x, want %x", got.ZobristHash(), want.ZobristHash())
	}
}

func TestNewPositionRejectsInvalidSetups(t *testing.T) {
	cases := []struct {
		name    string
		setup   Setup
		wantErr string
	}{
		{
			name: "missing white king",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/8")
				return Setup{Board: *b, Turn: White, FullMoveNo: 1}
			}(),
			wantErr: "white king",
		},
		{
			name: "missing black king",
			setup: func() Setup {
				b, _ := fenBoard("8/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: White, FullMoveNo: 1}
			}(),
			wantErr: "black king",
		},
		{
			name: "pawn on first rank",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/P3K3")
				return Setup{Board: *b, Turn: White, FullMoveNo: 1}
			}(),
			wantErr: "pawn on a1",
		},
		{
			name: "pawn on eighth rank",
			setup: func() Setup {
				b, _ := fenBoard("4k2p/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: White, FullMoveNo: 1}
			}(),
			wantErr: "pawn on h8",
		},
		{
			name: "castling rights without rook",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: White, CastleRights: NewCastleRights(true, false, false, false), FullMoveNo: 1}
			}(),
			wantErr: "white kingside castling rights",
		},
		{
			name: "en passant square without pawn",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: Black, EnPassant: D3, FullMoveNo: 1}
			}(),
			wantErr: "white pawn on d4",
		},
		{
			name: "negative halfmove clock",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: White, HalfMoveClock: -1, FullMoveNo: 1}
			}(),
			wantErr: "halfmove clock",
		},
		{
			name: "zero fullmove number",
			setup: func() Setup {
				b, _ := fenBoard("4k3/8/8/8/8/8/8/4K3")
				return Setup{Board: *b, Turn: White, FullMoveNo: 0}
			}(),
			wantErr: "fullmove number",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := NewPosition(tc.setup)
			if err == nil {
				t.Fatal("expected error, got nil")
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("error %q does not contain %q", err.Error(), tc.wantErr)
			}
		})
	}
}

func TestDecodeFENRoundsThroughNewPosition(t *testing.T) {
	_, err := decodeFEN("4k3/8/8/8/8/8/8/4K2R w KQ - 0 1")
	if err == nil {
		t.Fatal("expected decodeFEN to reject inconsistent white queenside rights")
	}
	if !strings.Contains(err.Error(), "white queenside castling rights") {
		t.Errorf("error %q does not mention white queenside castling rights", err.Error())
	}
}
