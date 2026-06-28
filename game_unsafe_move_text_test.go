package chess_test

import (
	"errors"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestGameUnsafePushMoveTextRejectsSAN(t *testing.T) {
	_, err := chess.NewGame().UnsafePushMoveText("e4", chess.SAN(), nil)
	if !errors.Is(err, chess.ErrUnsafeMoveTextUnsupported) {
		t.Fatalf("UnsafePushMoveText(SAN) error = %v, want ErrUnsafeMoveTextUnsupported", err)
	}
}

func TestGameUnsafePushMoveTextAcceptsIllegalCoordinateMove(t *testing.T) {
	game := chess.NewGame()
	if _, err := game.UnsafePushMoveText("e2e5", chess.UCI(), nil); err != nil {
		t.Fatalf("UnsafePushMoveText() error = %v", err)
	}
	if got := game.Position().Board().Piece(chess.E5); got != chess.WhitePawn {
		t.Fatalf("piece on e5 = %v, want white pawn", got)
	}
}

func TestGameUnsafePushMoveTextClassifiesSpecialMoves(t *testing.T) {
	t.Run("capture from UCI", func(t *testing.T) {
		game := chess.NewGame()
		mustUnsafePushMoveText(t, game, "e2e4", chess.UCI())
		mustUnsafePushMoveText(t, game, "d7d5", chess.UCI())
		node := mustUnsafePushMoveText(t, game, "e4d5", chess.UCI())
		if !node.Move().HasTag(chess.Capture) {
			t.Fatalf("capture move missing Capture tag: %v", node.Move())
		}
	})

	t.Run("castling from UCI", func(t *testing.T) {
		game := mustGameFromFEN(t, "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
		node := mustUnsafePushMoveText(t, game, "e1g1", chess.UCI())
		if !node.Move().HasTag(chess.KingSideCastle) {
			t.Fatalf("castle move missing KingSideCastle tag: %v", node.Move())
		}
		if got := game.Position().Board().Piece(chess.F1); got != chess.WhiteRook {
			t.Fatalf("piece on f1 = %v, want white rook", got)
		}
	})

	t.Run("en passant from long algebraic", func(t *testing.T) {
		game := mustGameFromFEN(t, "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
		node := mustUnsafePushMoveText(t, game, "e5xd6", chess.LongAlgebraic())
		if !node.Move().HasTag(chess.EnPassant) {
			t.Fatalf("en passant move missing EnPassant tag: %v", node.Move())
		}
		if got := game.Position().Board().Piece(chess.D5); got != chess.NoPiece {
			t.Fatalf("piece on d5 = %v, want empty square", got)
		}
	})

	t.Run("null move from UCI", func(t *testing.T) {
		game := chess.NewGame()
		node := mustUnsafePushMoveText(t, game, "0000", chess.UCI())
		if !node.Move().HasTag(chess.Null) {
			t.Fatalf("null move missing Null tag: %v", node.Move())
		}
		if game.Position().Turn() != chess.Black {
			t.Fatalf("turn = %v, want black", game.Position().Turn())
		}
	})
}

func mustUnsafePushMoveText(t *testing.T, game *chess.Game, moveText string, codec chess.MoveTextCodec) *chess.MoveNode {
	t.Helper()
	node, err := game.UnsafePushMoveText(moveText, codec, nil)
	if err != nil {
		t.Fatalf("UnsafePushMoveText(%q, %s) error = %v", moveText, codec, err)
	}
	return node
}

func mustGameFromFEN(t *testing.T, fen string) *chess.Game {
	t.Helper()
	opt, err := chess.FEN(fen)
	if err != nil {
		t.Fatalf("FEN() error = %v", err)
	}
	return chess.NewGame(opt)
}
