package chess_test

import (
	"sort"
	"testing"

	"github.com/corentings/chess/v3"
)

func positionFromFEN(t *testing.T, fen string) *chess.Position {
	t.Helper()
	opt, err := chess.FEN(fen)
	if err != nil {
		t.Fatalf("FEN decode: %v", err)
	}
	return chess.NewGame(opt).Position()
}

func findMoveByString(moves []chess.Move, want string) (chess.Move, bool) {
	for _, move := range moves {
		if move.String() == want {
			return move, true
		}
	}
	return chess.Move{}, false
}

func requireMove(t *testing.T, moves []chess.Move, want string) chess.Move {
	t.Helper()
	move, ok := findMoveByString(moves, want)
	if !ok {
		t.Fatalf("expected move %s", want)
	}
	return move
}

func TestPositionStatus(t *testing.T) {
	tests := []struct {
		name string
		fen  string
		want chess.Method
	}{
		{"starting_position", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", chess.NoMethod},
		{"stalemate", "7k/5K2/6Q1/8/8/8/8/8 b - - 0 1", chess.Stalemate},
		{"checkmate", "7k/5K2/7Q/8/8/8/8/8 b - - 0 1", chess.Checkmate},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := positionFromFEN(t, tt.fen)
			if got := pos.Status(); got != tt.want {
				t.Fatalf("Status() = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestPositionStatusIsStableAcrossRepeatedCallsAndUpdates(t *testing.T) {
	checkmate := positionFromFEN(t, "7k/5K2/7Q/8/8/8/8/8 b - - 0 1")
	if got := checkmate.Status(); got != chess.Checkmate {
		t.Fatalf("first Status() = %s, want %s", got, chess.Checkmate)
	}
	if got := checkmate.Status(); got != chess.Checkmate {
		t.Fatalf("second Status() = %s, want %s", got, chess.Checkmate)
	}

	start := chess.NewGame().Position()
	move := requireMove(t, start.ValidMoves(), "e2e4")
	next := start.Update(move)
	if got := start.Status(); got != chess.NoMethod {
		t.Fatalf("original Status() after Update = %s, want %s", got, chess.NoMethod)
	}
	if got := next.Status(); got != chess.NoMethod {
		t.Fatalf("updated Status() = %s, want %s", got, chess.NoMethod)
	}
}

func TestMoveGenerationTags(t *testing.T) {
	tests := []struct {
		name     string
		fen      string
		move     string
		wantTags []chess.MoveTag
		noTags   []chess.MoveTag
	}{
		{
			name:     "queen_side_castle",
			fen:      "r3kb1r/p2nqppp/5n2/1B2p1B1/4P3/1Q6/PPP2PPP/R3K2R b KQkq - 1 12",
			move:     "e8c8",
			wantTags: []chess.MoveTag{chess.QueenSideCastle},
		},
		{
			name:     "king_side_castle_with_check",
			fen:      "r4b1r/ppp3pp/8/4p3/2Pq4/3P4/PP2QPPP/2k1K2R w K - 0 18",
			move:     "e1g1",
			wantTags: []chess.MoveTag{chess.KingSideCastle, chess.Check},
		},
		{
			name:     "capture",
			fen:      "8/7p/3k2p1/8/2p2P2/R5bP/6K1/4r3 w - - 0 44",
			move:     "g2g3",
			wantTags: []chess.MoveTag{chess.Capture},
		},
		{
			name:     "en_passant_with_check",
			fen:      "r3k2r/pbppqpb1/1pn3p1/7p/1N2pPn1/1PP4N/PB1P2PP/2QRKR2 b kq f3 0 1",
			move:     "e4f3",
			wantTags: []chess.MoveTag{chess.EnPassant, chess.Check},
		},
		{
			name:   "quiet_move",
			fen:    "8/7p/3k2p1/8/2p2P2/R5KP/8/4r3 b - - 0 44",
			move:   "d6d5",
			noTags: []chess.MoveTag{chess.Capture, chess.EnPassant, chess.Check, chess.KingSideCastle, chess.QueenSideCastle},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := positionFromFEN(t, tt.fen)
			move := requireMove(t, pos.ValidMoves(), tt.move)
			for _, tag := range tt.wantTags {
				if !move.HasTag(tag) {
					t.Errorf("%s missing tag %v", tt.move, tag)
				}
			}
			for _, tag := range tt.noTags {
				if move.HasTag(tag) {
					t.Errorf("%s unexpectedly has tag %v", tt.move, tag)
				}
			}
		})
	}
}

func TestPromotionCheckTags(t *testing.T) {
	pos := positionFromFEN(t, "k7/4P3/8/8/8/8/8/K7 w - - 0 1")
	moves := pos.ValidMoves()

	tests := []struct {
		move      string
		wantCheck bool
	}{
		{"e7e8q", true},
		{"e7e8r", true},
		{"e7e8b", false},
		{"e7e8n", false},
	}
	for _, tt := range tests {
		t.Run(tt.move, func(t *testing.T) {
			move := requireMove(t, moves, tt.move)
			if got := move.HasTag(chess.Check); got != tt.wantCheck {
				t.Fatalf("%s Check = %v, want %v", tt.move, got, tt.wantCheck)
			}
		})
	}
}

func TestUnsafeMoves(t *testing.T) {
	tests := []struct {
		name  string
		fen   string
		want  []string
		count int
	}{
		{
			name:  "starting_position",
			fen:   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
			count: 0,
		},
		{
			name:  "pinned_knight",
			fen:   "4k3/8/8/8/1b6/2N5/8/4K3 w - - 0 1",
			count: 8,
		},
		{
			name:  "king_walks_into_check",
			fen:   "4r3/8/8/8/8/8/8/4K3 w - - 0 1",
			want:  []string{"e1e2"},
			count: 1,
		},
		{
			name:  "king_adjacent_rook_attacks",
			fen:   "8/8/8/8/8/3r4/8/4K3 w - - 0 1",
			want:  []string{"e1d1", "e1d2"},
			count: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := positionFromFEN(t, tt.fen)
			moves := pos.UnsafeMoves()
			if len(moves) != tt.count {
				t.Fatalf("UnsafeMoves() returned %d moves, want %d", len(moves), tt.count)
			}
			for _, want := range tt.want {
				requireMove(t, moves, want)
			}
		})
	}
}

func TestLegalMovesInCheckScenarios(t *testing.T) {
	t.Run("pinned_rook_stays_on_pin_ray", func(t *testing.T) {
		pos := positionFromFEN(t, "k3r3/8/8/8/8/8/4R3/4K3 w - - 0 1")
		got := moveStringsFrom(pos.ValidMoves(), chess.E2)
		want := []string{"e2e3", "e2e4", "e2e5", "e2e6", "e2e7", "e2e8"}
		sort.Strings(want)
		if !equalStrings(got, want) {
			t.Fatalf("pinned rook moves = %v, want %v", got, want)
		}
	})

	t.Run("single_check_allows_capture_block_or_king_move", func(t *testing.T) {
		pos := positionFromFEN(t, "k3r3/8/8/8/8/8/R7/4K3 w - - 0 1")
		got := moveStrings(pos.ValidMoves())
		want := []string{"a2e2", "e1d1", "e1d2", "e1f1", "e1f2"}
		sort.Strings(want)
		if !equalStrings(got, want) {
			t.Fatalf("single-check legal moves = %v, want %v", got, want)
		}
	})

	t.Run("double_check_only_king_moves", func(t *testing.T) {
		pos := positionFromFEN(t, "rnb1kbnr/pppp2pp/5N2/6pQ/8/8/PPPPPPPP/RNB1KB1R b KQkq - 0 1")
		for _, move := range pos.ValidMoves() {
			if move.S1() != chess.E8 {
				t.Fatalf("double check generated non-king move %s", move)
			}
		}
	})

	t.Run("king_move_escapes_check", func(t *testing.T) {
		pos := positionFromFEN(t, "4k3/8/8/8/8/8/8/4R3 b - - 0 1")
		requireMove(t, pos.ValidMoves(), "e8d8")
	})

	t.Run("discovered_check_tag", func(t *testing.T) {
		pos := positionFromFEN(t, "4k3/8/8/8/4N3/8/8/4R1K1 w - - 0 1")
		for _, move := range pos.ValidMoves() {
			if move.S1() == chess.E4 && move.HasTag(chess.Check) {
				return
			}
		}
		t.Fatal("expected at least one knight move from e4 with a discovered check tag")
	})

	t.Run("en_passant_discovered_check", func(t *testing.T) {
		pos := positionFromFEN(t, "4r3/8/8/8/4p3/8/3P4/4K1k1 w - - 0 1")
		d2d4 := requireMove(t, pos.ValidMoves(), "d2d4")
		next := pos.Update(d2d4)
		for _, move := range next.ValidMoves() {
			if move.HasTag(chess.EnPassant) && move.HasTag(chess.Check) {
				return
			}
		}
		t.Fatal("expected en passant move with discovered check tag")
	})

	t.Run("en_passant_cannot_expose_king_to_rook", func(t *testing.T) {
		pos := positionFromFEN(t, "4k3/8/8/r2pP2K/8/8/8/8 w - d6 0 1")
		if _, ok := findMoveByString(pos.ValidMoves(), "e5d6"); ok {
			t.Fatal("en passant exposing the king to a rook must not be legal")
		}
		if _, ok := findMoveByString(pos.UnsafeMoves(), "e5d6"); !ok {
			t.Fatal("UnsafeMoves should report the illegal en passant candidate")
		}
	})
}

func TestSlidingPieceLegalMovesWithBlockers(t *testing.T) {
	tests := []struct {
		name string
		fen  string
		from chess.Square
		want []string
	}{
		{
			name: "rook_rank_and_file_blockers",
			fen:  "4k3/8/3p4/8/1P1R1p2/8/3P4/4K3 w - - 0 1",
			from: chess.D4,
			want: []string{"d4c4", "d4e4", "d4f4", "d4d3", "d4d5", "d4d6"},
		},
		{
			name: "bishop_diagonal_blockers",
			fen:  "4k3/8/1P3p2/8/3B4/8/1p3P2/4K3 w - - 0 1",
			from: chess.D4,
			want: []string{"d4e5", "d4f6", "d4c5", "d4e3", "d4c3", "d4b2"},
		},
		{
			name: "queen_combined_blockers",
			fen:  "4k3/8/1P1p1p2/8/1P1Q1p2/8/1p1P1P2/4K3 w - - 0 1",
			from: chess.D4,
			want: []string{
				"d4c4", "d4e4", "d4f4", "d4d3", "d4d5", "d4d6",
				"d4e5", "d4f6", "d4c5", "d4e3", "d4c3", "d4b2",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pos := positionFromFEN(t, tt.fen)
			got := moveStringsFrom(pos.ValidMoves(), tt.from)
			want := append([]string(nil), tt.want...)
			sort.Strings(want)
			if !equalStrings(got, want) {
				t.Fatalf("moves from %s = %v, want %v", tt.from, got, want)
			}
		})
	}
}

func moveStringsFrom(moves []chess.Move, from chess.Square) []string {
	out := make([]string, 0, len(moves))
	for _, move := range moves {
		if move.S1() == from {
			out = append(out, move.String())
		}
	}
	sort.Strings(out)
	return out
}
