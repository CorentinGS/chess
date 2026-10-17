package uci_test

import (
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/corentings/chess/v3"
	"github.com/corentings/chess/v3/uci"
)

// recordingAdapter is a test Adapter that both returns canned responses (keyed
// by the command's first word, like FakeAdapter) and records the exact command
// strings sent to the engine, so tests can assert on protocol ordering.
type recordingAdapter struct {
	mu        sync.Mutex
	sent      []string
	responses map[string][]string
}

func newRecordingAdapter(responses map[string][]string) *recordingAdapter {
	return &recordingAdapter{responses: responses}
}

func (r *recordingAdapter) Exchange(cmd uci.Cmd) ([]string, error) {
	r.mu.Lock()
	r.sent = append(r.sent, cmd.String())
	r.mu.Unlock()
	if cmd.IsDone("") {
		return nil, nil
	}
	key := strings.SplitN(cmd.String(), " ", 2)[0]
	var lines []string
	for _, line := range r.responses[key] {
		lines = append(lines, line)
		if cmd.IsDone(line) {
			break
		}
	}
	return lines, nil
}

func (r *recordingAdapter) Close() error { return nil }

func (r *recordingAdapter) commands() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]string, len(r.sent))
	copy(out, r.sent)
	return out
}

// chess960CastlePosition returns a Chess960-variant position where White's
// king-side castle is legal: king e1, rook h1, an empty path, and the black
// king far from rank 1. The Shredder-FEN rights field ("H") infers the variant.
func chess960CastlePosition(t *testing.T) *chess.Position {
	t.Helper()
	fenOpt, err := chess.FEN("4k3/8/8/8/8/8/8/4K2R w H - 0 1")
	if err != nil {
		t.Fatalf("FEN: %v", err)
	}
	game := chess.NewGame(fenOpt)
	if game.Position().Variant() != chess.Chess960 {
		t.Fatalf("position variant = %v, want Chess960", game.Position().Variant())
	}
	return game.Position()
}

func containsCommand(sent []string, want string) bool {
	for _, s := range sent {
		if s == want {
			return true
		}
	}
	return false
}

func firstIndexOfPrefix(sent []string, prefix string) int {
	for i, s := range sent {
		if strings.HasPrefix(s, prefix) {
			return i
		}
	}
	return -1
}

// TestEngineNegotiatesUCIChess960 asserts that sending a Chess960 position
// makes the engine enable the UCI_Chess960 option before the position command,
// but only because the engine advertises the option.
func TestEngineNegotiatesUCIChess960(t *testing.T) {
	fake := newRecordingAdapter(map[string][]string{
		"uci": {
			"id name test-engine",
			"option name UCI_Chess960 type check default false",
			"uciok",
		},
	})
	eng := uci.NewWithAdapter(fake)
	if err := eng.Run(uci.CmdUCI{}); err != nil {
		t.Fatalf("CmdUCI: %v", err)
	}
	if err := eng.Run(uci.CmdPosition{Position: chess960CastlePosition(t)}); err != nil {
		t.Fatalf("CmdPosition: %v", err)
	}

	sent := fake.commands()
	if !containsCommand(sent, "setoption name UCI_Chess960 value true") {
		t.Fatalf("expected UCI_Chess960=true to be sent, got %v", sent)
	}
	setIdx := firstIndexOfPrefix(sent, "setoption name UCI_Chess960 value true")
	posIdx := firstIndexOfPrefix(sent, "position fen ")
	if setIdx < 0 || posIdx < 0 || setIdx > posIdx {
		t.Errorf("setoption must precede position: %v", sent)
	}
}

// TestEngineDoesNotNegotiateWithoutOption asserts that when the engine does not
// advertise UCI_Chess960, no setoption is sent (the codec still encodes
// king-to-rook, but the option is left untouched).
func TestEngineDoesNotNegotiateWithoutOption(t *testing.T) {
	fake := newRecordingAdapter(map[string][]string{
		"uci": {"id name no-chess960-engine", "uciok"},
	})
	eng := uci.NewWithAdapter(fake)
	if err := eng.Run(uci.CmdUCI{}); err != nil {
		t.Fatalf("CmdUCI: %v", err)
	}
	if err := eng.Run(uci.CmdPosition{Position: chess960CastlePosition(t)}); err != nil {
		t.Fatalf("CmdPosition: %v", err)
	}
	for _, s := range fake.commands() {
		if strings.HasPrefix(s, "setoption") {
			t.Errorf("unexpected setoption sent to engine without the option: %q", s)
		}
	}
}

// TestEngineTogglesUCIChess960OffForStandard asserts the option is disabled
// again when a later standard position is sent, so a mixed session keeps the
// engine's castling interpretation in sync.
func TestEngineTogglesUCIChess960OffForStandard(t *testing.T) {
	fake := newRecordingAdapter(map[string][]string{
		"uci": {
			"option name UCI_Chess960 type check default false",
			"uciok",
		},
	})
	eng := uci.NewWithAdapter(fake)
	if err := eng.Run(uci.CmdUCI{}); err != nil {
		t.Fatalf("CmdUCI: %v", err)
	}
	if err := eng.Run(uci.CmdPosition{Position: chess960CastlePosition(t)}); err != nil {
		t.Fatalf("Chess960 CmdPosition: %v", err)
	}
	if err := eng.Run(uci.CmdPosition{Position: chess.StartingPosition()}); err != nil {
		t.Fatalf("standard CmdPosition: %v", err)
	}
	sent := fake.commands()
	if !containsCommand(sent, "setoption name UCI_Chess960 value true") {
		t.Errorf("expected UCI_Chess960=true sent for Chess960 position: %v", sent)
	}
	if !containsCommand(sent, "setoption name UCI_Chess960 value false") {
		t.Errorf("expected UCI_Chess960=false sent for standard position: %v", sent)
	}
}

// TestEngineDecodesBestMoveKingToRook asserts that a best move reported in the
// Chess960 king-to-rook form ("e1h1") decodes to the canonical castle whose
// king destination is g1.
func TestEngineDecodesBestMoveKingToRook(t *testing.T) {
	fake := newRecordingAdapter(map[string][]string{
		"position": nil,
		"go":       {"bestmove e1h1"},
	})
	eng := uci.NewWithAdapter(fake)
	pos := chess960CastlePosition(t)
	if err := eng.Run(uci.CmdPosition{Position: pos}); err != nil {
		t.Fatalf("CmdPosition: %v", err)
	}
	if err := eng.Run(uci.CmdGo{MoveTime: 10 * time.Millisecond}); err != nil {
		t.Fatalf("CmdGo: %v", err)
	}
	best := eng.SearchResults().BestMove
	if best.S1() != chess.E1 || best.S2() != chess.G1 {
		t.Fatalf("bestmove = %v-%v, want e1-g1 (canonical castle)", best.S1(), best.S2())
	}
	if !best.HasTag(chess.KingSideCastle) {
		t.Errorf("bestmove %v: missing KingSideCastle tag", best)
	}
}

// TestCmdPositionEncodesChess960CastleKingToRook asserts the position command
// string emits king-to-rook for a Chess960 castle supplied in the move list,
// replaying through the position so the rook origin is correct.
func TestCmdPositionEncodesChess960CastleKingToRook(t *testing.T) {
	pos := chess960CastlePosition(t)
	// The king-side castle from this position: king e1 -> g1, rook h1 -> f1.
	var castle chess.Move
	for _, m := range pos.ValidMoves() {
		if m.HasTag(chess.KingSideCastle) {
			castle = m
			break
		}
	}
	if castle.S1() != chess.E1 { // sanity: a castle exists
		t.Fatalf("no king-side castle found in position")
	}
	got := uci.CmdPosition{Position: pos, Moves: []chess.Move{castle}}.String()
	if !strings.Contains(got, "moves e1h1") {
		t.Errorf("CmdPosition = %q, want moves to contain king-to-rook \"e1h1\"", got)
	}
}

// TestCmdPositionReplaysChess960MovesBeforeCastle verifies Chess960 move-list
// serialization advances through prior plies before encoding a later castle.
func TestCmdPositionReplaysChess960MovesBeforeCastle(t *testing.T) {
	fenOpt, err := chess.FEN("1n2k3/8/8/8/8/8/8/1N2K2R w H - 0 1")
	if err != nil {
		t.Fatalf("FEN: %v", err)
	}
	initial := chess.NewGame(fenOpt).Position()

	whiteKnight, err := chess.UCI().Decode(initial, "b1c3")
	if err != nil {
		t.Fatalf("decode white knight move: %v", err)
	}
	afterWhite := initial.Update(whiteKnight)
	blackKnight, err := chess.UCI().Decode(afterWhite, "b8c6")
	if err != nil {
		t.Fatalf("decode black knight move: %v", err)
	}
	beforeCastle := afterWhite.Update(blackKnight)

	var castle chess.Move
	for _, m := range beforeCastle.ValidMoves() {
		if m.HasTag(chess.KingSideCastle) {
			castle = m
			break
		}
	}
	if castle.S1() != chess.E1 {
		t.Fatal("no king-side castle found after the preceding moves")
	}

	got := uci.CmdPosition{
		Position: initial,
		Moves:    []chess.Move{whiteKnight, blackKnight, castle},
	}.String()
	const wantMoves = "moves b1c3 b8c6 e1h1"
	if !strings.HasSuffix(got, wantMoves) {
		t.Errorf("CmdPosition = %q, want suffix %q", got, wantMoves)
	}
}
