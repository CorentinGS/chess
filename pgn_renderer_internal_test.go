package chess

import (
	"strings"
	"testing"
)

func TestWriteAnnotations(t *testing.T) {
	t.Run("NilMove", func(t *testing.T) {
		var sb strings.Builder
		(&pgnRender{sb: &sb}).writeAnnotations(nil)
		if sb.String() != "" {
			t.Fatalf("expected empty annotation output, got %q", sb.String())
		}
	})

	t.Run("StructuredCommentAndCommand", func(t *testing.T) {
		move := &MoveNode{}
		move.SetComment("Good move")
		move.SetCommand("clk", "0:05:00")
		var sb strings.Builder
		(&pgnRender{sb: &sb}).writeAnnotations(move)
		if sb.String() != " {Good move [%clk 0:05:00]}" {
			t.Fatalf("expected merged annotation, got %q", sb.String())
		}
	})

	t.Run("EmptyStructuredBlock", func(t *testing.T) {
		var sb strings.Builder
		writeCommentBlocks([]CommentBlock{{}}, &sb)
		if sb.String() != "" {
			t.Fatalf("expected empty structured block to be skipped, got %q", sb.String())
		}
	})
}
