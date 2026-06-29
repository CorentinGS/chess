package chess

import (
	"io"
	"slices"
	"strconv"
	"strings"
)

// PGNRenderer renders a Game into PGN text. It is a stateless
// function object that reads Game state and writes formatted PGN
// to a string or io.Writer without mutating the Game.
//
// The renderer takes a pointer so callers may extend it in the
// future (custom tag ordering, value escaping toggles, optional
// result token, etc.). All such configuration fields are deferred
// to v3.1; this initial type is a pure extraction with no knobs.
type PGNRenderer struct{}

// DefaultPGNRenderer is the package-level renderer used by
// Game.String and Game.WritePGN.
var DefaultPGNRenderer = &PGNRenderer{}

// Render returns the PGN text for g.
func (r *PGNRenderer) Render(g *Game) string {
	var sb strings.Builder
	if err := r.renderTo(g, &sb); err != nil {
		return ""
	}
	return sb.String()
}

// RenderGameTo writes the PGN text for g to w. It returns any
// write error from w.
func (r *PGNRenderer) RenderGameTo(g *Game, w io.Writer) error {
	return r.renderTo(g, w)
}

func (r *PGNRenderer) renderTo(g *Game, w io.Writer) error {
	var sb strings.Builder

	tagPairList := make([]sortableTagPair, len(g.tagPairs))

	var idx uint
	for tag, value := range g.tagPairs {
		tagPairList[idx] = sortableTagPair{
			Key:   tag,
			Value: value,
		}
		idx++
	}

	slices.SortFunc(tagPairList, cmpTags)

	for _, tagPair := range tagPairList {
		sb.WriteByte('[')
		sb.WriteString(tagPair.Key)
		sb.WriteString(" \"")
		sb.WriteString(escapeTagValue(tagPair.Value))
		sb.WriteString("\"]\n")
	}

	if len(g.tagPairs) > 0 {
		sb.WriteString("\n")
	}

	needTrailingSpace := false
	if g.tree != nil && g.tree.Root() != nil {
		root := g.tree.Root()
		if len(root.children) > 0 {
			writeMoves(root,
				root.position.moveCount,
				root.position.turn == White, &sb, false, false, true)
			needTrailingSpace = true
		} else if root.hasAnnotations() {
			writeAnnotations(root, &sb)
		}
	}

	if needTrailingSpace {
		sb.WriteString(" ")
	}
	sb.WriteString(g.Outcome().String())

	_, err := io.WriteString(w, sb.String())
	return err
}

// sortableTagPair is the key/value row used to sort a Game's tag pairs into
// PGN output order.
type sortableTagPair struct {
	Key   string
	Value string
}

// cmpTags orders two tag pairs. The Seven Tag Roster (Event, Site, Date,
// Round, White, Black, Result) takes priority; everything else is sorted
// ascending by key. Duplicate keys are treated as equal so the sort is
// stable.
func cmpTags(a, b sortableTagPair) int {
	// Don't re-order duplicate keys
	if a.Key == b.Key {
		return 0
	}

	// PGN defined tags take priority
	for _, req := range []string{
		"Event",
		"Site",
		"Date",
		"Round",
		"White",
		"Black",
		"Result",
	} {
		if a.Key == req {
			return -1
		}
		if b.Key == req {
			return +1
		}
	}

	// Finally compare the keys directly and sort by ascending
	if a.Key < b.Key {
		return -1
	} else if b.Key < a.Key {
		return +1
	}
	return 0
}

// escapeTagValue escapes backslash and double-quote characters so that the
// resulting string is safe to embed inside a PGN tag value.
func escapeTagValue(v string) string {
	var sb strings.Builder
	for i := range len(v) {
		c := v[i]
		if c == '\\' || c == '"' {
			sb.WriteByte('\\')
		}
		sb.WriteByte(c)
	}
	return sb.String()
}

// writeMoves recursively writes the PGN-formatted move sequence starting from the given move node into the provided strings.Builder.
// It handles move numbering for white and black moves, encodes moves using algebraic notation based on the appropriate position,
// and appends comments and command annotations if present. The function distinguishes between main line moves and sub-variations;
// when processing a sub-variation, moves are enclosed in parentheses.
//
// Parameters:
//
//	node - pointer to the current move node from which to write moves.
//	moveNum - the current move number corresponding to white’s moves.
//	isWhite - true if it is white’s move, false if it is black’s move.
//	sb - pointer to a strings.Builder where the formatted move notation is appended.
//	subVariation - true if the current call is within a sub-variation, affecting formatting details.
//	closedVariation - true if the prior call closed a sub-variation, affecting formatting details.
//	isRoot - true if the current move is the root move of a game, affecting formatting details.
//
// The function recurses through the move tree, writing the main line first and then processing any additional variations,
// ensuring that the output adheres to standard PGN conventions. Future enhancements may include support for all NAG values.
func writeMoves(node *MoveNode, moveNum int, isWhite bool, sb *strings.Builder,
	subVariation, closedVariation, isRoot bool,
) {
	// If no moves remain, stop.
	if node == nil {
		return
	}

	// Handle root move comments before processing children
	if isRoot && node.hasAnnotations() {
		writeAnnotations(node, sb)
	}

	var currentMove *MoveNode

	// The main line is the first child.
	if subVariation {
		currentMove = node
	} else {
		if len(node.children) == 0 {
			return // nothing to print if no child exists (should not happen for a proper game)
		}
		currentMove = node.children[0]
	}

	writeMoveNumber(moveNum, isWhite, subVariation, closedVariation, isRoot, sb)

	// Encode the move using your algebraicNotation.
	writeMoveEncoding(node, currentMove, subVariation, sb)

	writeAnnotations(currentMove, sb)

	if len(node.children) > 1 || len(currentMove.children) > 0 {
		sb.WriteString(" ")
	}
	// Process any variations (children beyond the first).
	// In PGN, variations are enclosed in parentheses.
	closedVar := writeVariations(node, moveNum, isWhite, sb)

	if len(currentMove.children) > 0 {
		var nextMoveNum int
		var nextIsWhite bool
		if isWhite {
			// After white's move, black plays using the same move number.
			nextMoveNum = moveNum
			nextIsWhite = false
		} else {
			// After black's move, increment move number.
			nextMoveNum = moveNum + 1
			nextIsWhite = true
		}
		writeMoves(currentMove, nextMoveNum, nextIsWhite, sb, false, closedVar,
			false)
	}
}

func writeMoveNumber(moveNum int, isWhite bool, subVariation, closedVariation,
	isRoot bool, sb *strings.Builder,
) {
	if closedVariation {
		sb.WriteString(" ")
	}
	if isWhite {
		sb.WriteString(strconv.Itoa(moveNum))
		sb.WriteString(". ")
	} else if subVariation || closedVariation || isRoot {
		sb.WriteString(strconv.Itoa(moveNum))
		sb.WriteString("... ")
	}
}

func writeMoveEncoding(node *MoveNode, currentMove *MoveNode, subVariation bool, sb *strings.Builder) {
	var (
		moveStr string
		err     error
	)
	if subVariation && node.parent != nil {
		moveStr, err = SAN().Encode(node.parent.position, currentMove.move)
	} else {
		moveStr, err = SAN().Encode(node.position, currentMove.move)
	}
	if err == nil {
		sb.WriteString(moveStr)
	}
}

func sortedCommandKeys(commands map[string]string) []string {
	keys := make([]string, 0, len(commands))
	for key := range commands {
		keys = append(keys, key)
	}
	slices.Sort(keys)
	return keys
}

func writeAnnotations(move *MoveNode, sb *strings.Builder) {
	if move == nil {
		return
	}

	for _, nag := range move.nags {
		sb.WriteByte(' ')
		sb.WriteString(nag)
	}

	if len(move.commentBlocks) > 0 {
		writeCommentBlocks(move.commentBlocks, sb)
	}
}

func writeCommentBlocks(blocks []CommentBlock, sb *strings.Builder) {
	for _, block := range blocks {
		if len(block.Items) == 0 {
			continue
		}

		sb.WriteString(" {")
		lastByte := byte('{')
		for _, item := range block.Items {
			switch item.Kind {
			case CommentText:
				lastByte = writeEscapedCommentText(sb, item.Text, lastByte)
			case CommentCommand:
				if needsCommandSeparator(lastByte) {
					sb.WriteString(" ")
				}
				sb.WriteString("[%")
				sb.WriteString(item.Key)
				sb.WriteString(" ")
				sb.WriteString(item.Value)
				sb.WriteString("]")
				lastByte = ']'
			}
		}
		sb.WriteString("}")
	}
}

func writeEscapedCommentText(sb *strings.Builder, text string, lastByte byte) byte {
	for i := range len(text) {
		if text[i] == '}' {
			sb.WriteByte('\\')
		}
		sb.WriteByte(text[i])
		lastByte = text[i]
	}
	return lastByte
}

func needsCommandSeparator(lastByte byte) bool {
	return lastByte != ' '
}

func writeVariations(node *MoveNode, moveNum int, isWhite bool, sb *strings.Builder) bool {
	wroteAtLeastOneVar := false

	if len(node.children) > 1 {
		for i := 1; i < len(node.children); i++ {
			if wroteAtLeastOneVar {
				sb.WriteString(" ")
			}
			wroteAtLeastOneVar = true

			variation := node.children[i]
			sb.WriteString("(")
			writeMoves(variation, moveNum, isWhite, sb, true, false, false)
			sb.WriteString(")")
		}
	}

	return wroteAtLeastOneVar
}
