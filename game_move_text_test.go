package chess_test

import (
	"testing"

	"github.com/corentings/chess/v3"
)

func TestGamePushMoveTextWithCodecs(t *testing.T) {
	tests := []struct {
		name  string
		codec chess.MoveTextCodec
		text  string
		want  string
	}{
		{
			name:  "SAN",
			codec: chess.SAN(),
			text:  "e4",
			want:  "1. e4 *",
		},
		{
			name:  "UCI",
			codec: chess.UCI(),
			text:  "e2e4",
			want:  "1. e4 *",
		},
		{
			name:  "long algebraic",
			codec: chess.LongAlgebraic(),
			text:  "e2e4",
			want:  "1. e4 *",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			game := chess.NewGame()
			if _, err := game.PushMoveText(tt.text, tt.codec, nil); err != nil {
				t.Fatalf("PushMoveText() error = %v", err)
			}
			if got := game.String(); got != tt.want {
				t.Fatalf("game.String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestGamePushMoveTextUsesGeneratedMoveTags(t *testing.T) {
	game := chess.NewGame()
	if _, err := game.PushMoveText("e2e4", chess.UCI(), nil); err != nil {
		t.Fatalf("PushMoveText(e2e4) error = %v", err)
	}
	if _, err := game.PushMoveText("d7d5", chess.UCI(), nil); err != nil {
		t.Fatalf("PushMoveText(d7d5) error = %v", err)
	}
	node, err := game.PushMoveText("e4d5", chess.UCI(), nil)
	if err != nil {
		t.Fatalf("PushMoveText(e4d5) error = %v", err)
	}
	if !node.Move().HasTag(chess.Capture) {
		t.Fatalf("inserted move missing Capture tag: %v", node.Move())
	}
	if got := game.String(); got != "1. e4 d5 2. exd5 *" {
		t.Fatalf("game.String() = %q, want capture SAN", got)
	}
}

func TestGamePushMoveIsStrictSANShorthand(t *testing.T) {
	opt, err := chess.FEN("6k1/4P3/8/8/8/8/8/4K3 w - - 0 1")
	if err != nil {
		t.Fatalf("FEN() error = %v", err)
	}
	game := chess.NewGame(opt)
	if _, err := game.PushMove("e8Q", nil); err == nil {
		t.Fatalf("PushMove accepted import-only SAN")
	}
}
