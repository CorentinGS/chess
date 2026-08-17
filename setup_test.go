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

func TestEmptySetup(t *testing.T) {
	s := EmptySetup()
	// EmptySetup is a construction starting point, not a playable position:
	// it has no kings, so NewPosition must reject it.
	if _, err := NewPosition(s); err == nil {
		t.Fatal("NewPosition(EmptySetup()) = nil, want error (no kings)")
	}
	if got := s.Board.Piece(A1); got != NoPiece {
		t.Errorf("EmptySetup().Board.Piece(A1) = %v, want NoPiece", got)
	}
	if s.Turn != White {
		t.Errorf("EmptySetup().Turn = %v, want White", s.Turn)
	}
	if s.EnPassant != NoSquare {
		t.Errorf("EmptySetup().EnPassant = %v, want NoSquare", s.EnPassant)
	}
	if s.HalfMoveClock != 0 {
		t.Errorf("EmptySetup().HalfMoveClock = %d, want 0", s.HalfMoveClock)
	}
	if s.FullMoveNo != 1 {
		t.Errorf("EmptySetup().FullMoveNo = %d, want 1", s.FullMoveNo)
	}
}

func TestInitialSetup(t *testing.T) {
	s := InitialSetup()
	pos, err := NewPosition(s)
	if err != nil {
		t.Fatalf("NewPosition(InitialSetup()): %v", err)
	}
	if got, want := pos.String(), StartingPosition().String(); got != want {
		t.Errorf("InitialSetup() FEN = %q, want %q", got, want)
	}
}

func TestSetupSwapTurn(t *testing.T) {
	s := InitialSetup()
	swapped := s.SwapTurn()
	if swapped.Turn != Black {
		t.Errorf("SwapTurn().Turn = %v, want Black", swapped.Turn)
	}
	if swapped.EnPassant != NoSquare {
		t.Errorf("SwapTurn().EnPassant = %v, want NoSquare", swapped.EnPassant)
	}
	// Original is not mutated.
	if s.Turn != White {
		t.Errorf("original Setup.Turn mutated: %v", s.Turn)
	}
	// SwapTurn is its own inverse for the turn field.
	if back := swapped.SwapTurn(); back.Turn != White {
		t.Errorf("SwapTurn().SwapTurn().Turn = %v, want White", back.Turn)
	}
}

func TestSetupMirror(t *testing.T) {
	s := InitialSetup()
	m := s.Mirror()
	// The mirrored starting position is still a legal starting position:
	// pieces are on the same squares but swapped in color, and since the
	// starting position is color-symmetric, the FEN must round-trip.
	if m.Turn != Black {
		t.Errorf("Mirror().Turn = %v, want Black", m.Turn)
	}
	pos, err := NewPosition(m)
	if err != nil {
		t.Fatalf("NewPosition(Mirror()): %v", err)
	}
	// White king on e1 becomes black king on e8 (and vice versa).
	if got := m.Board.Piece(E8); got.Type() != King || got.Color() != Black {
		t.Errorf("Mirror().Board.Piece(E8) = %v, want Black King", got)
	}
	if got := m.Board.Piece(E1); got.Type() != King || got.Color() != White {
		t.Errorf("Mirror().Board.Piece(E1) = %v, want White King", got)
	}
	_ = pos
}

func TestSetupMirrorEnPassant(t *testing.T) {
	// Black just played e7-e5; en passant target is e6.
	s := Setup{
		Board:        mustBoard(t, "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR"),
		Turn:         White,
		EnPassant:    E6,
		CastleRights: NewCastleRights(true, true, true, true),
		FullMoveNo:   1,
	}
	m := s.Mirror()
	if m.EnPassant != E3 {
		t.Errorf("Mirror().EnPassant = %v, want E3 (e6 mirrored to e3)", m.EnPassant)
	}
	if m.Turn != Black {
		t.Errorf("Mirror().Turn = %v, want Black", m.Turn)
	}
}

func mustBoard(t *testing.T, boardFEN string) Board {
	t.Helper()
	b, err := fenBoard(boardFEN)
	if err != nil {
		t.Fatalf("fenBoard(%q): %v", boardFEN, err)
	}
	return *b
}
