package chess

import (
	"errors"
	"log"
	"strings"
	"testing"
	"time"
)

func TestGameAccessorsReturnDefensiveCopies(t *testing.T) {
	game := NewGame()
	if _, err := game.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	game.MoveTree().AddVariation(nil, Move{s1: D2, s2: D4})

	children := game.MoveTree().Root().Children()
	children[0] = nil
	if game.MoveTree().Root().children[0] == nil {
		t.Fatal("Children returned mutable backing slice")
	}

	variations := game.MoveTree().Variations(game.MoveTree().Root())
	variations[0] = nil
	if game.MoveTree().Root().children[1] == nil {
		t.Fatal("Variations returned mutable backing slice")
	}

	pos := game.Position()
	pos.board.mailbox[E4] = NoPiece
	if got := game.Position().Board().Piece(E4); got != WhitePawn {
		t.Fatalf("Position returned mutable game position, E4 = %v, want %v", got, WhitePawn)
	}
}

func TestCheckmate(t *testing.T) {
	fenStr := "rn1qkbnr/pbpp1ppp/1p6/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if _, err := g.PushMove("Qxf7#", nil); err != nil {
		t.Fatal(err)
	}
	if g.Method() != Checkmate {
		t.Fatalf("expected method %s but got %s", Checkmate, g.Method())
	}
	if g.Outcome() != WhiteWon {
		t.Fatalf("expected outcome %s but got %s", WhiteWon, g.Outcome())
	}

	// Checkmate on castle
	fenStr = "Q7/5Qp1/3k2N1/7p/8/4B3/PP3PPP/R3K2R w KQ - 0 31"
	fen, err = FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g = NewGame(fen)
	if _, err := g.PushMove("O-O-O", nil); err != nil {
		t.Fatal(err)
	}
	t.Log(g.Position().String())
	if g.Method() != Checkmate {
		t.Fatalf("expected method %s but got %s", Checkmate, g.Method())
	}
	if g.Outcome() != WhiteWon {
		t.Fatalf("expected outcome %s but got %s", WhiteWon, g.Outcome())
	}
}

func TestCheckmateFromFen(t *testing.T) {
	fenStr := "rn1qkbnr/pbpp1Qpp/1p6/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if g.Method() != Checkmate {
		t.Error(g.Position().Board().Draw())
		t.Fatalf("expected method %s but got %s", Checkmate, g.Method())
	}
	if g.Outcome() != WhiteWon {
		t.Fatalf("expected outcome %s but got %s", WhiteWon, g.Outcome())
	}
}

func TestStalemate(t *testing.T) {
	fenStr := "k1K5/8/8/8/8/8/8/1Q6 w - - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if _, err := g.PushMove("Qb6", nil); err != nil {
		t.Fatal(err)
	}
	if g.Method() != Stalemate {
		t.Fatalf("expected method %s but got %s", Stalemate, g.Method())
	}
	if g.Outcome() != Draw {
		t.Fatalf("expected outcome %s but got %s", Draw, g.Outcome())
	}
}

// position shouldn't result in stalemate because pawn can move http://en.lichess.org/Pc6mJDZN#138
func TestInvalidStalemate(t *testing.T) {
	fenStr := "8/3P4/8/8/8/7k/7p/7K w - - 2 70"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if _, err := g.PushMove("d8=Q", nil); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() != NoOutcome {
		t.Fatalf("expected outcome %s but got %s", NoOutcome, g.Outcome())
	}
}

func TestThreeFoldRepetition(t *testing.T) {
	g := NewGame()
	moves := []string{
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
	}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	pos := g.Positions()
	if err := g.Draw(ThreefoldRepetition); err != nil {
		for _, pos := range pos {
			log.Println(pos.String())
		}
		t.Fatalf("%s - %d reps", err.Error(), g.numOfRepetitions())
	}
}

func TestInvalidThreeFoldRepetition(t *testing.T) {
	g := NewGame()
	moves := []string{
		"Nf3", "Nf6", "Ng1", "Ng8",
	}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if err := g.Draw(ThreefoldRepetition); err == nil {
		t.Fatal("should require three repeated board states")
	}
}

func TestFiveFoldRepetition(t *testing.T) {
	g := NewGame()
	moves := []string{
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
	}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if g.Outcome() != Draw || g.Method() != FivefoldRepetition {
		t.Fatal("should automatically draw after five repetitions")
	}
}

func TestFiveFoldRepetitionIgnored(t *testing.T) {
	g := NewGame(IgnoreFivefoldRepetitionDraw())
	moves := []string{
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
		"Nf3", "Nf6", "Ng1", "Ng8",
	}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if g.Outcome() == Draw && g.Method() == FivefoldRepetition {
		t.Fatal("automatically draw after five repetitions should be ignored")
	}
}

func TestFiftyMoveRule(t *testing.T) {
	fen, _ := FEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 100 60")
	g := NewGame(fen)
	if err := g.Draw(FiftyMoveRule); err != nil {
		t.Fatal(err)
	}
}

func TestInvalidFiftyMoveRule(t *testing.T) {
	fen, _ := FEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 99 60")
	g := NewGame(fen)
	if err := g.Draw(FiftyMoveRule); err == nil {
		t.Fatal("should require fifty moves")
	}
}

func TestSeventyFiveMoveRule(t *testing.T) {
	fen, _ := FEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 149 80")
	g := NewGame(fen)
	if _, err := g.PushMove("Kf8", nil); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() != Draw || g.Method() != SeventyFiveMoveRule {
		t.Fatal("should automatically draw after seventy five moves w/ no pawn move or capture")
	}
}

func TestSeventyFiveMoveRuleIgnored(t *testing.T) {
	fen, _ := FEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 149 80")
	g := NewGame(fen, IgnoreSeventyFiveMoveRuleDraw())
	if _, err := g.PushMove("Kf8", nil); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() == Draw && g.Method() == SeventyFiveMoveRule {
		t.Fatal("automatically draw after seventy five moves w/ no pawn move or capture should be ignored")
	}
}

func TestInsufficientMaterial(t *testing.T) {
	fens := []string{
		"8/2k5/8/8/8/3K4/8/8 w - - 1 1",
		"8/2k5/8/8/8/3K1N2/8/8 w - - 1 1",
		"8/2k5/8/8/8/3K1B2/8/8 w - - 1 1",
		"8/2k5/2b5/8/8/3K1B2/8/8 w - - 1 1",
		"4b3/2k5/2b5/8/8/3K1B2/8/8 w - - 1 1",
	}
	for _, f := range fens {
		fen, err := FEN(f)
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(fen)
		if g.Outcome() != Draw || g.Method() != InsufficientMaterial {
			log.Println(g.Position().Board().Draw())
			t.Fatalf("%s should automatically draw by insufficient material", f)
		}
	}
}

func TestInsufficientMaterialIgnored(t *testing.T) {
	fens := []string{
		"8/2k5/8/8/8/3K4/8/8 w - - 1 1",
		"8/2k5/8/8/8/3K1N2/8/8 w - - 1 1",
		"8/2k5/8/8/8/3K1B2/8/8 w - - 1 1",
		"8/2k5/2b5/8/8/3K1B2/8/8 w - - 1 1",
		"4b3/2k5/2b5/8/8/3K1B2/8/8 w - - 1 1",
	}
	for _, f := range fens {
		fen, err := FEN(f)
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(IgnoreInsufficientMaterialDraw(), fen)
		if g.Outcome() == Draw && g.Method() == InsufficientMaterial {
			log.Println(g.Position().Board().Draw())
			t.Fatalf("%s automatically draw by insufficient material should be ignored", f)
		}
	}
}

func TestSufficientMaterial(t *testing.T) {
	fens := []string{
		"8/2k5/8/8/8/3K1B2/4N3/8 w - - 1 1",
		"8/2k5/8/8/8/3KBB2/8/8 w - - 1 1",
		"8/2k1b3/8/8/8/3K1B2/8/8 w - - 1 1",
		"8/2k5/8/8/4P3/3K4/8/8 w - - 1 1",
		"8/2k5/8/8/8/3KQ3/8/8 w - - 1 1",
		"8/2k5/8/8/8/3KR3/8/8 w - - 1 1",
	}
	for _, f := range fens {
		fen, err := FEN(f)
		if err != nil {
			t.Fatal(err)
		}
		g := NewGame(fen)
		if g.Outcome() != NoOutcome {
			log.Println(g.Position().Board().Draw())
			t.Fatalf("%s should not find insufficient material", f)
		}
	}
}

func TestInitialNumOfValidMoves(t *testing.T) {
	g := NewGame()
	if len(g.ValidMoves()) != 20 {
		t.Fatal("should find 20 valid moves from the initial position")
	}
}

func TestPositionHash(t *testing.T) {
	g1 := NewGame()
	for _, s := range []string{"Nc3", "e5", "Nf3"} {
		g1.PushMove(s, nil)
	}
	g2 := NewGame()
	for _, s := range []string{"Nf3", "e5", "Nc3"} {
		g2.PushMove(s, nil)
	}
	if g1.Position().ZobristHash() != g2.Position().ZobristHash() {
		t.Fatalf("expected position hashes to be equal but got %x and %x", g1.Position().ZobristHash(), g2.Position().ZobristHash())
	}
}

func BenchmarkStalemateStatus(b *testing.B) {
	fenStr := "k1K5/8/8/8/8/8/8/1Q6 w - - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		b.Fatal(err)
	}
	g := NewGame(fen)
	if _, err := g.PushMove("Qb6", nil); err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for range b.N {
		g.Position().Status()
	}
}

func BenchmarkInvalidStalemateStatus(b *testing.B) {
	fenStr := "8/3P4/8/8/8/7k/7p/7K w - - 2 70"
	fen, err := FEN(fenStr)
	if err != nil {
		b.Fatal(err)
	}
	g := NewGame(fen)
	if _, err := g.PushMove("d8=Q", nil); err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for range b.N {
		g.Position().Status()
	}
}

func BenchmarkPositionHash(b *testing.B) {
	fenStr := "8/3P4/8/8/8/7k/7p/7K w - - 2 70"
	fen, err := FEN(fenStr)
	if err != nil {
		b.Fatal(err)
	}
	g := NewGame(fen)
	b.ResetTimer()
	for range b.N {
		g.Position().ZobristHash()
	}
}

func TestAddVariationToEmptyParent(t *testing.T) {
	g := NewGame()
	parent := g.MoveTree().Root()
	newMove := Move{s1: E2, s2: E4}
	node, err := g.MoveTree().AddVariation(parent, newMove)
	if err != nil {
		t.Fatal(err)
	}
	if len(parent.children) != 1 || parent.children[0] != node || parent.children[0].move != newMove {
		t.Fatalf("expected newMove to be added to parent's children")
	}
	if parent.children[0].parent != parent {
		t.Fatalf("expected newMove's parent to be set to parent")
	}
}

func TestAddVariationToNonEmptyParent(t *testing.T) {
	g := NewGame()
	parent := g.MoveTree().Root()
	if _, err := g.MoveTree().AddVariation(parent, Move{s1: E2, s2: E4}); err != nil {
		t.Fatal(err)
	}
	newMove := Move{s1: D2, s2: D4}
	node, err := g.MoveTree().AddVariation(parent, newMove)
	if err != nil {
		t.Fatal(err)
	}
	if len(parent.children) != 2 || parent.children[1] != node || parent.children[1].move != newMove {
		t.Fatalf("expected newMove to be added to parent's children")
	}
	if parent.children[1].parent != parent {
		t.Fatalf("expected newMove's parent to be set to parent")
	}
}

func TestAddVariationWithNilParent(t *testing.T) {
	g := NewGame()
	newMove := Move{s1: E2, s2: E4}
	if _, err := g.MoveTree().AddVariation(nil, newMove); err != nil {
		t.Fatal(err)
	}
	if len(g.MoveTree().Root().children) != 1 {
		t.Fatalf("expected variation attached to root, got %d children", len(g.MoveTree().Root().children))
	}
	if g.MoveTree().Root().children[0].move != newMove {
		t.Fatalf("expected variation move %v, got %v", newMove, g.MoveTree().Root().children[0].move)
	}
	if g.MoveTree().Root().children[0].parent != g.MoveTree().Root() {
		t.Fatal("expected variation parent pointer to be the root move")
	}
}

func TestNavigateToMainLineFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	g.MoveTree().NavigateToMainLine()
	if g.MoveTree().Current() != g.MoveTree().Root().children[0] {
		t.Fatalf("expected to navigate to main line root move")
	}
}

func TestNavigateToMainLineFromVariation(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	variationMove := Move{s1: A7, s2: A6}
	variationNode, err := g.MoveTree().AddVariation(g.MoveTree().Current(), variationMove)
	if err != nil {
		t.Fatal(err)
	}
	g.tree.setCurrent(variationNode)
	g.MoveTree().NavigateToMainLine()
	if g.MoveTree().Current() != g.MoveTree().Root().children[0] {
		t.Fatalf("expected to navigate to main line root move")
	}
}

func TestNavigateToMainLineFromRoot(t *testing.T) {
	g := NewGame()
	g.MoveTree().NavigateToMainLine()
	if g.MoveTree().Current() != g.MoveTree().Root() {
		t.Fatalf("expected to stay at root move")
	}
}

func TestGoBackFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if !g.MoveTree().GoBack() {
		t.Fatalf("expected to go back from leaf move")
	}
	if g.MoveTree().Current() != g.MoveTree().Root().children[0].children[0].children[0].children[0] {
		t.Fatalf("expected current move to be Bb5's parent")
	}
}

func TestGoBackFromRoot(t *testing.T) {
	g := NewGame()
	if g.MoveTree().GoBack() {
		t.Fatalf("expected not to go back from root move")
	}
	if g.MoveTree().Current() != g.MoveTree().Root() {
		t.Fatalf("expected to stay at root move")
	}
}

func TestGoBackFromMainLine(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if !g.MoveTree().GoBack() {
		t.Fatalf("expected to go back from main line move")
	}
	if g.MoveTree().Current() != g.MoveTree().Root().children[0].children[0] {
		t.Fatalf("expected current move to be e5's parent")
	}
}

func TestGoForwardFromRoot(t *testing.T) {
	g := NewGame()
	_, _ = g.PushMove("e4", nil)
	_, _ = g.PushMove("e5", nil)
	g.tree.setCurrent(g.MoveTree().Root()) // Reset to root
	if !g.MoveTree().GoForward() {
		t.Fatalf("expected to go forward from root move")
	}
	if g.MoveTree().Current() != g.MoveTree().Root().children[0] {
		t.Fatalf("expected current move to be the first child of root move")
	}
}

func TestGoForwardFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	if g.MoveTree().GoForward() {
		t.Fatalf("expected not to go forward from leaf move")
	}
	if g.MoveTree().Current() != g.MoveTree().Root().children[0].children[0].children[0].children[0].children[0] {
		t.Fatalf("expected current move to stay at leaf move")
	}
}

func TestGoForwardFromVariation(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	variationMove := Move{s1: F1, s2: B5}
	variationNode, err := g.MoveTree().AddVariation(g.MoveTree().Current(), variationMove)
	if err != nil {
		t.Fatal(err)
	}
	childMove := Move{s1: A7, s2: A6}
	if _, err := g.MoveTree().AddVariation(variationNode, childMove); err != nil {
		t.Fatal(err)
	}
	g.tree.setCurrent(variationNode)
	if !g.MoveTree().GoForward() {
		t.Fatalf("expected to go forward from variation move")
	}
	if g.MoveTree().Current().move != childMove {
		t.Fatalf("expected current move to be the child of the variation move")
	}
}

func TestIsAtStartWhenAtRoot(t *testing.T) {
	g := NewGame()
	if !g.IsAtStart() {
		t.Fatalf("expected to be at start when at root move")
	}
}

func TestIsAtStartWhenNotAtRoot(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if g.IsAtStart() {
		t.Fatalf("expected not to be at start when not at root move")
	}
}

func TestIsAtEndWhenAtLeaf(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if !g.IsAtEnd() {
		t.Fatalf("expected to be at end when at leaf move")
	}
}

func TestIsAtEndWhenNotAtLeaf(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	// Add this line to move back to a non-leaf position
	g.MoveTree().GoBack()
	if g.IsAtEnd() {
		t.Fatalf("expected not to be at end when not at leaf move")
	}
}

func TestVariationsWithNoChildren(t *testing.T) {
	g := NewGame()
	move := &MoveNode{}
	variations := g.MoveTree().Variations(move)
	if variations != nil {
		t.Fatalf("expected no variations for move with no children")
	}
}

func TestVariationsWithOneChild(t *testing.T) {
	g := NewGame()
	move := &MoveNode{children: []*MoveNode{{}}}
	variations := g.MoveTree().Variations(move)
	if variations != nil {
		t.Fatalf("expected no variations for move with one child")
	}
}

func TestVariationsWithMultipleChildren(t *testing.T) {
	g := NewGame()
	move := &MoveNode{children: []*MoveNode{{}, {}}}
	variations := g.MoveTree().Variations(move)
	if len(variations) != 1 {
		t.Fatalf("expected one variation for move with multiple children")
	}
}

func TestVariationsWithNilMove(t *testing.T) {
	g := NewGame()
	variations := g.MoveTree().Variations(nil)
	if variations != nil {
		t.Fatalf("expected no variations for nil move")
	}
}

func TestCommentsWithNoComments(t *testing.T) {
	g := NewGame()
	comments := g.Comments()
	if len(comments) != 0 {
		t.Fatalf("expected no comments but got %d", len(comments))
	}
}

func TestCommentsWithSingleComment(t *testing.T) {
	g := NewGame()
	g.comments = [][]string{{"First comment"}}
	comments := g.Comments()
	if len(comments) != 1 || comments[0][0] != "First comment" {
		t.Fatalf("expected one comment 'First comment' but got %v", comments)
	}
}

func TestCommentsWithMultipleComments(t *testing.T) {
	g := NewGame()
	g.comments = [][]string{{"First comment"}, {"Second comment"}}
	comments := g.Comments()
	if len(comments) != 2 || comments[0][0] != "First comment" || comments[1][0] != "Second comment" {
		t.Fatalf("expected comments 'First comment' and 'Second comment' but got %v", comments)
	}
}

func TestCommentsWithNilComments(t *testing.T) {
	g := NewGame()
	g.comments = nil
	comments := g.Comments()
	if comments == nil || len(comments) != 0 {
		t.Fatalf("expected no comments but got %v", comments)
	}
}

func TestPushMove(t *testing.T) {
	tests := []struct {
		name          string
		setupMoves    []string           // Moves to set up the position
		move          string             // Move to push
		goBack        bool               // Whether to go back one move before pushing
		options       *MoveInsertOptions // Options for the push
		wantErr       bool               // Whether we expect an error
		wantPosition  string             // Expected FEN after the move
		checkMainline []string           // Expected mainline moves in UCI notation
	}{
		{
			name:          "basic pawn move",
			move:          "e4",
			wantErr:       false,
			wantPosition:  "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
			checkMainline: []string{"e2e4"},
		},
		{
			name:    "invalid move should fail",
			move:    "e9",
			wantErr: true,
		},
		{
			name:          "piece move",
			setupMoves:    []string{"e4", "e5"},
			move:          "Nf3",
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "g1f3"},
		},
		{
			name:          "create variation without force mainline",
			setupMoves:    []string{"e4", "e5", "Nf3"},
			move:          "Nc3",
			goBack:        true,
			options:       &MoveInsertOptions{},
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "g1f3"}, // Original mainline remains
		},
		{
			name:          "create variation with force mainline",
			setupMoves:    []string{"e4", "e5", "Nf3"},
			move:          "Nc3",
			goBack:        true,
			options:       &MoveInsertOptions{PromoteToMainLine: true},
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "b1c3"}, // New mainline
		},
		{
			name:          "push existing move without override",
			setupMoves:    []string{"e4", "e5", "Nf3"},
			move:          "Nf3",
			goBack:        true,
			options:       &MoveInsertOptions{},
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "g1f3"},
		},
		{
			name:          "castling move",
			setupMoves:    []string{"e4", "e5", "Nf3", "Nc6", "Bc4", "Bc5", "d3", "Nf6"},
			move:          "O-O",
			wantPosition:  "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 2 5",
			checkMainline: []string{"e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "f8c5", "d2d3", "g8f6", "O-O"},
		},
		{
			name:          "en passant capture",
			setupMoves:    []string{"e4", "Nf6", "e5", "d5"},
			move:          "exd6",
			wantPosition:  "rnbqkb1r/ppp1pppp/3P1n2/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3",
			checkMainline: []string{"e2e4", "g8f6", "e4e5", "d7d5", "e5d6"},
		},
		{
			name:          "pawn promotion",
			setupMoves:    []string{"e4", "d5", "exd5", "c6", "dxc6", "Nf6", "cxb7", "Nbd7"},
			move:          "bxa8=Q",
			wantPosition:  "Q1bqkb1r/p2npppp/5n2/8/8/8/PPPP1PPP/RNBQKBNR b KQk - 0 5",
			checkMainline: []string{"e2e4", "d7d5", "e4d5", "c7c6", "d5c6", "g8f6", "c6b7", "b8d7", "b7a8=q"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a new game for each test
			game := NewGame()

			// Setup moves
			for _, move := range tt.setupMoves {
				_, err := game.PushMove(move, nil)
				if err != nil {
					t.Fatalf("setup failed: %v", err)
				}
			}

			// Go back one move if needed for the test
			if tt.goBack && game.MoveTree().Current() != nil && game.MoveTree().Current().parent != nil {
				game.MoveTree().GoBack()
			}

			// Test the move
			_, err := game.PushMove(tt.move, tt.options)

			// Check error expectation
			if (err != nil) != tt.wantErr {
				t.Errorf("PushMove() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			// Check position
			if tt.wantPosition != "" {
				gotFEN := game.currentPosition().String()
				if gotFEN != tt.wantPosition {
					t.Errorf("Position after move = %v, want %v", gotFEN, tt.wantPosition)
				}
			}

			// Check mainline
			if tt.checkMainline != nil {
				mainline := getMainline(game)
				if !moveSlicesEqual(mainline, tt.checkMainline) {
					t.Errorf("Mainline = %v, want %v", mainline, tt.checkMainline)
				}
			}
		})
	}
}

// Helper function to get the mainline moves from a game
func getMainline(game *Game) []string {
	var moves []string
	current := game.MoveTree().Root()

	for len(current.children) > 0 {
		current = current.children[0] // Follow main line (first variation)
		moves = append(moves, algebraicMove(current))
	}

	return moves
}

// Helper function to convert a move to algebraic notation
func algebraicMove(move *MoveNode) string {
	mv := move.Move()
	// This is a simplified version - you might want to implement proper algebraic notation
	if mv.HasTag(KingSideCastle) {
		return "O-O"
	}
	if mv.HasTag(QueenSideCastle) {
		return "O-O-O"
	}

	s1 := mv.s1.String()
	s2 := mv.s2.String()

	if mv.promo != NoPieceType {
		return s1 + s2 + "=" + mv.promo.String()
	}

	return s1 + s2
}

// Helper function to compare move slices
func moveSlicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestCopyGameState(t *testing.T) {
	original := NewGame()
	_, _ = original.PushMove("e4", nil)
	_, _ = original.PushMove("e5", nil)
	_, _ = original.PushMove("Nf3", nil)

	newGame := NewGame()
	newGame.copy(original)

	if newGame.currentPosition().String() != original.currentPosition().String() {
		t.Fatalf("expected position %s but got %s", original.currentPosition().String(), newGame.currentPosition().String())
	}
	if newGame.MoveTree().Current() != original.MoveTree().Current() {
		t.Fatalf("expected current move to be %v but got %v", original.MoveTree().Current(), newGame.MoveTree().Current())
	}
	if newGame.outcome != original.outcome {
		t.Fatalf("expected outcome %s but got %s", original.outcome, newGame.outcome)
	}
	if newGame.method != original.method {
		t.Fatalf("expected method %d but got %d", original.method, newGame.method)
	}
	if len(newGame.Comments()) != len(original.Comments()) {
		t.Fatalf("expected comments %v but got %v", original.Comments(), newGame.Comments())
	}
}

func TestCopyGameStateWithNilComments(t *testing.T) {
	original := NewGame()
	original.comments = nil

	newGame := NewGame()
	newGame.copy(original)

	if newGame.comments == nil {
		t.Fatalf("expected comments to be initialized")
	}
}

func TestCopyGameStateWithTagPairs(t *testing.T) {
	original := NewGame()
	original.AddTagPair("Event", "Test Event")

	newGame := NewGame()
	newGame.copy(original)

	if newGame.GetTagPair("Event") != "Test Event" {
		t.Fatalf("expected tag pair 'Test Event' but got %s", newGame.GetTagPair("Event"))
	}
}

func TestCloneGameState(t *testing.T) {
	original := NewGame()
	_, _ = original.PushMove("e4", nil)
	_, _ = original.PushMove("e5", nil)
	_, _ = original.PushMove("Nf3", nil)

	clone := original.Clone()

	if clone.currentPosition().String() != original.currentPosition().String() {
		t.Fatalf("expected position %s but got %s", original.currentPosition().String(), clone.currentPosition().String())
	}
	if clone.MoveTree().Current().Move().String() != original.MoveTree().Current().Move().String() {
		t.Fatalf("expected current move to be %v but got %v", original.MoveTree().Current(), clone.MoveTree().Current())
	}
	if clone.MoveTree().Current() == original.MoveTree().Current() {
		t.Errorf("clone failed to deep copy currentMove")
	}
	if clone.MoveTree().Root() == original.MoveTree().Root() {
		t.Errorf("clone failed to deep copy rootMove")
	}
	if clone.outcome != original.outcome {
		t.Fatalf("expected outcome %s but got %s", original.outcome, clone.outcome)
	}
	if clone.method != original.method {
		t.Fatalf("expected method %d but got %d", original.method, clone.method)
	}
	if len(clone.Comments()) != len(original.Comments()) {
		t.Fatalf("expected comments %v but got %v", original.Comments(), clone.Comments())
	}

	// make sure we can modify the clone without impact on the original
	_, err := clone.PushMove("Nf6", nil)
	if err != nil {
		t.Fatalf("failed to push Nf6")
	}
	if clone.currentPosition().String() == original.currentPosition().String() {
		t.Error("modifying the clone incorrectly mutates the original position")
	}
	if len(clone.Moves()) == len(original.Moves()) {
		t.Errorf("modifying the clone incorrectly mutates the original moves")
	}
	if len(clone.Positions()) == len(original.Positions()) {
		t.Errorf("modifying the clone incorrectly mutates the original positions")
	}
}

func TestCloneGameStateWithNilComments(t *testing.T) {
	original := NewGame()
	original.comments = nil

	clone := original.Clone()

	if clone.comments == nil {
		t.Fatalf("expected comments to be initialized")
	}
}

func TestCloneGameStateWithTagPairs(t *testing.T) {
	original := NewGame()
	original.AddTagPair("Event", "Test Event")

	clone := original.Clone()

	if clone.GetTagPair("Event") != "Test Event" {
		t.Fatalf("expected tag pair 'Test Event' but got %s", clone.GetTagPair("Event"))
	}

	// modify original to ensure the clone is a true deep copy
	original.AddTagPair("Event", "Test Event Modified")

	if clone.GetTagPair("Event") != "Test Event" {
		t.Fatalf("expected tag pair 'Test Event' but got %s", clone.GetTagPair("Event"))
	}
}

func TestResignWhenGameInProgress(t *testing.T) {
	g := NewGame()
	g.Resign(White)
	if g.Outcome() != BlackWon {
		t.Fatalf("expected outcome %s but got %s", BlackWon, g.Outcome())
	}
	if g.Method() != Resignation {
		t.Fatalf("expected method %s but got %s", Resignation, g.Method())
	}
}

func TestResignWhenGameAlreadyCompleted(t *testing.T) {
	g := NewGame()
	g.Resign(White)
	g.Resign(Black)
	if g.Outcome() != BlackWon {
		t.Fatalf("expected outcome %s but got %s", BlackWon, g.Outcome())
	}
	if g.Method() != Resignation {
		t.Fatalf("expected method %s but got %s", Resignation, g.Method())
	}
}

func TestResignWithInvalidColor(t *testing.T) {
	g := NewGame()
	g.Resign(NoColor)
	if g.Outcome() != NoOutcome {
		t.Fatalf("expected outcome %s but got %s", NoOutcome, g.Outcome())
	}
	if g.Method() != NoMethod {
		t.Fatalf("expected method %s but got %s", NoMethod, g.Method())
	}
}

func TestResignWhenBlackResigns(t *testing.T) {
	g := NewGame()
	g.Resign(Black)
	if g.Outcome() != WhiteWon {
		t.Fatalf("expected outcome %s but got %s", WhiteWon, g.Outcome())
	}
	if g.Method() != Resignation {
		t.Fatalf("expected method %s but got %s", Resignation, g.Method())
	}
}

func TestEligibleDrawsWithNoRepetitionsAndLowHalfMoveClock(t *testing.T) {
	g := NewGame()
	draws := g.EligibleDraws()
	if len(draws) != 1 || draws[0] != DrawOffer {
		t.Fatalf("expected only DrawOffer but got %v", draws)
	}
}

func TestEligibleDrawsWithThreeRepetitions(t *testing.T) {
	g := NewGame()
	moves := []string{"Nf3", "Nf6", "Ng1", "Ng8", "Nf3", "Nf6", "Ng1", "Ng8", "Nf3", "Nf6"}
	for _, m := range moves {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	draws := g.EligibleDraws()
	if len(draws) != 2 || draws[1] != ThreefoldRepetition {
		t.Fatalf("expected DrawOffer and ThreefoldRepetition but got %v", draws)
	}
}

func TestEligibleDrawsWithFiftyMoveRule(t *testing.T) {
	fen, _ := FEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 100 60")
	g := NewGame(fen)
	draws := g.EligibleDraws()
	if len(draws) != 2 || draws[1] != FiftyMoveRule {
		t.Fatalf("expected DrawOffer and FiftyMoveRule but got %v", draws)
	}
}

func TestRemoveTagPairWhenKeyExists(t *testing.T) {
	g := NewGame()
	g.AddTagPair("Event", "Test Event")
	removed := g.RemoveTagPair("Event")
	if !removed {
		t.Fatalf("expected tag pair to be removed")
	}
	if g.GetTagPair("Event") != "" {
		t.Fatalf("expected tag pair to be empty but got %s", g.GetTagPair("Event"))
	}
}

func TestRemoveTagPairWhenKeyDoesNotExist(t *testing.T) {
	g := NewGame()
	removed := g.RemoveTagPair("NonExistentKey")
	if removed {
		t.Fatalf("expected tag pair not to be removed")
	}
}

func TestRemoveTagPairFromEmptyTagPairs(t *testing.T) {
	g := NewGame()
	g.tagPairs = make(map[string]string)
	removed := g.RemoveTagPair("Event")
	if removed {
		t.Fatalf("expected tag pair not to be removed")
	}
}

func TestAddTagPairWhenKeyExists(t *testing.T) {
	g := NewGame()
	g.AddTagPair("Event", "Test Event")
	overwritten := g.AddTagPair("Event", "Updated Event")
	if !overwritten {
		t.Fatalf("expected tag pair to be overwritten")
	}
	if g.GetTagPair("Event") != "Updated Event" {
		t.Fatalf("expected tag pair to be 'Updated Event' but got %s", g.GetTagPair("Event"))
	}
}

func TestAddTagPairWhenKeyDoesNotExist(t *testing.T) {
	g := NewGame()
	overwritten := g.AddTagPair("Event", "Test Event")
	if overwritten {
		t.Fatalf("expected tag pair not to be overwritten")
	}
	if g.GetTagPair("Event") != "Test Event" {
		t.Fatalf("expected tag pair to be 'Test Event' but got %s", g.GetTagPair("Event"))
	}
}

func TestAddTagPairWithNilTagPairs(t *testing.T) {
	g := NewGame()
	g.tagPairs = nil
	overwritten := g.AddTagPair("Event", "Test Event")
	if overwritten {
		t.Fatalf("expected tag pair not to be overwritten")
	}
	if g.GetTagPair("Event") != "Test Event" {
		t.Fatalf("expected tag pair to be 'Test Event' but got %s", g.GetTagPair("Event"))
	}
	if g.tagPairs == nil {
		t.Fatalf("expected tagPairs to be initialized")
	}
}

func TestPGNWithValidData(t *testing.T) {
	pgnData := mustParsePGN("fixtures/pgns/single_game.pgn")
	r := strings.NewReader(pgnData)
	updateFunc, err := PGN(r)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame()
	updateFunc(g)
	if g.Outcome() != WhiteWon {
		t.Fatalf("expected outcome %s but got %s", WhiteWon, g.Outcome())
	}
	if g.Method() != NoMethod {
		t.Fatalf("expected method %s but got %s", NoMethod, g.Method())
	}
	if len(g.Moves()) != 6 {
		t.Fatalf("expected 6 moves got %v", len(g.Moves()))
	}
	if len(g.Positions()) != 7 {
		t.Fatalf("expected 7 positions got %v", len(g.Positions()))
	}
	if g.MoveTree().Current().Move().String() != "a7a6" {
		t.Fatalf("expected current move a7a6 but got %v", g.MoveTree().Current().Move().String())
	}
}

func TestTaglessPGN(t *testing.T) {
	pgnData := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 h6 15. Bh4 Re8 16. a3 Bf8 17. Rc1 Qb6 18. dxe5 dxe5 19. Qe2 Nh5 20. Qd2 Nc5 21. Bc2 Nf4 22. Bg3 Rad8 23. Qe3 Qc7 24. Rcd1 Rxd1 25. Rxd1 Nce6 26. Bb3 Bc5 27. Qe1 Nd4 28. Nxd4 Bxd4 29. Bxf4 exf4 30. Rxd4 c5 31. Rd1 c4 32. Bc2 Qe5 33. f3 Qc5+ 34. Qf2 Qe5 35. Qd4 Qg5 36. Qd7 Re7 37. Qd8+ Kh7 38. e5+ g6 39. Qd6 Bxf3 40. Rd2 Rxe5 41. Qd4 Re1+ 42. Kf2 Qg3# 0-1"

	r := strings.NewReader("#!)(*#@$" + pgnData)
	_, err := PGN(r)
	if err == nil {
		t.Fatal("expected error for invalid PGN data")
	}

	r = strings.NewReader(pgnData)
	_, err = PGN(r)
	if err != nil {
		t.Fatal("expected non-nil error for tagless PGN data")
	}
}

func TestPGNWithEmptyData(t *testing.T) {
	r := strings.NewReader("")
	_, err := PGN(r)
	if !errors.Is(err, ErrNoGameFound) {
		t.Fatalf("expected error %v but got %v", ErrNoGameFound, err)
	}
}

func TestGameString(t *testing.T) {
	tests := []struct {
		name     string
		setup    func() *Game
		expected string
	}{
		{
			name: "GameStringWithNoMoves",
			setup: func() *Game {
				return NewGame()
			},
			expected: "*",
		},
		{
			name: "GameStringWithSingleMove",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				return g
			},
			expected: "1. e4 *",
		},
		{
			name: "GameStringWithMultipleMoves",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				_, _ = g.PushMove("e5", nil)
				_, _ = g.PushMove("Nf3", nil)
				return g
			},
			expected: "1. e4 e5 2. Nf3 *",
		},
		{
			name: "GameStringWithLongerGame",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("Nf3", nil)
				_, _ = g.PushMove("Nc6", nil)
				_, _ = g.PushMove("Nc3", nil)
				_, _ = g.PushMove("e6", nil)
				_, _ = g.PushMove("e4", nil)
				_, _ = g.PushMove("a6", nil)
				_, _ = g.PushMove("Ne2", nil)
				_, _ = g.PushMove("Nf6", nil)
				_, _ = g.PushMove("Ned4", nil)
				return g
			},
			expected: "1. Nf3 Nc6 2. Nc3 e6 3. e4 a6 4. Ne2 Nf6 5. Ned4 *",
		},
		{
			name: "GameStringWithComments",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				g.MoveTree().Current().SetComment("Good move")
				return g
			},
			expected: "1. e4 {Good move} *",
		},
		{
			name: "GameStringWithVariations",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				_, _ = g.PushMove("e5", nil)
				_, _ = g.PushMove("Nf3", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("Nc3", nil)
				return g
			},
			expected: "1. e4 e5 2. Nf3 (2. Nc3) *",
		},
		{
			name: "GameStringWithTags",
			setup: func() *Game {
				g := NewGame()
				g.AddTagPair("Event", "Test Event")
				g.AddTagPair("Site", "Test Site")
				return g
			},
			expected: "[Event \"Test Event\"]\n[Site \"Test Site\"]\n\n*",
		},
		{
			name: "GameStringWithWhiteWinResult",
			setup: func() *Game {
				g := NewGame()
				g.outcome = WhiteWon
				return g
			},
			expected: "1-0",
		},
		{
			name: "GameStringWithBlackWinResult",
			setup: func() *Game {
				g := NewGame()
				g.outcome = BlackWon
				return g
			},
			expected: "0-1",
		},
		{
			name: "GameStringWithDrawResult",
			setup: func() *Game {
				g := NewGame()
				g.outcome = Draw
				return g
			},
			expected: "1/2-1/2",
		},
		{
			name: "GameStringWithCommentsAndClock",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				g.MoveTree().Current().SetComment("Good move")
				g.MoveTree().Current().SetCommand("clk", "10:00:00")
				return g
			},
			expected: "1. e4 {Good move [%clk 10:00:00]} *",
		},
		{
			name: "GameStringWithCommandsOnly",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				g.MoveTree().Current().SetCommand("eval", "0.24")
				g.MoveTree().Current().SetCommand("clk", "10:00:00")
				return g
			},
			expected: "1. e4 { [%eval 0.24] [%clk 10:00:00]} *",
		},
		{
			name: "GameStringWithCommentsAndMultipleCommands",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				g.MoveTree().Current().SetComment("Good move")
				g.MoveTree().Current().SetCommand("eval", "0.24")
				g.MoveTree().Current().SetCommand("clk", "10:00:00")
				return g
			},
			expected: "1. e4 {Good move [%eval 0.24] [%clk 10:00:00]} *",
		},
		{
			name: "GameStringWithMultipleNestedVariations",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				_, _ = g.PushMove("e5", nil)
				_, _ = g.PushMove("Nf3", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("Nc3", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("d4", nil)
				_, _ = g.PushMove("d5", nil)
				_, _ = g.PushMove("c4", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("c3", nil)
				g.MoveTree().GoBack()
				return g
			},
			expected: "1. e4 e5 2. Nf3 (2. Nc3) (2. d4 d5 3. c4 (3. c3)) *",
		},
		{
			name: "GameStringWithVariationsForBlack",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				_, _ = g.PushMove("e5", nil)
				_, _ = g.PushMove("Nf3", nil)
				_, _ = g.PushMove("Nc6", nil)
				_, _ = g.PushMove("Bb5", nil)
				_, _ = g.PushMove("a6", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("d6", nil)
				return g
			},
			expected: "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 (3... d6) *",
		},
		{
			name: "GameStringWithVariationsOnRoot",
			setup: func() *Game {
				g := NewGame()
				_, _ = g.PushMove("e4", nil)
				g.MoveTree().GoBack()
				_, _ = g.PushMove("d4", nil)
				return g
			},
			expected: "1. e4 (1. d4) *",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g := tt.setup()
			if g.String() != tt.expected {
				t.Fatalf("\n\tExpected:'%v'\n\tGot:     '%v'\n", tt.expected, g.String())
			}
		})
	}
}

func FuzzTestPushMoveText(f *testing.F) {
	f.Add("e2e4", 0)
	f.Add("e4", 1)
	f.Add("Nb1c3", 2)

	f.Fuzz(func(_ *testing.T, move string, codecType int) {
		game := NewGame()

		var codec MoveTextCodec
		switch codecType % 3 {
		case 0:
			codec = UCI()
		case 1:
			codec = SAN()
		case 2:
			codec = LongAlgebraic()
		}

		_, _ = game.PushMoveText(move, codec, nil)
	})
}

func TestInvalidPushMoveText(t *testing.T) {
	fen := "r1bqk1nr/pp1pppbp/6p1/1Bp1P3/P2n1P2/2N2N2/1PPP2PP/R1BQK2R w KQkq - 0 1"
	bogusMv := "Kxh1"
	opt, err := FEN(fen)
	if err != nil {
		t.Fatalf("FEN(fen) failed")
	}
	game := NewGame(opt)

	_, err = game.PushMoveText(bogusMv, UCI(), nil)
	if err == nil {
		t.Errorf("PushMoveText() (uci) succeeded in pushing bogus mv when it should have failed")
	}
	_, err = game.PushMoveText(bogusMv, SAN(), nil)
	if err == nil {
		t.Errorf("PushMoveText() (SAN) succeeded in pushing bogus mv when it should have failed")
	}
}

func TestValidPushMoveText(t *testing.T) {
	pgn := strings.NewReader("1. e4 (1. g4) 1... c5 2. Nc3 Nc6 3. f4 g6 4. Nf3 Bg7 5. a4 Nf6 6. e5 *")
	mv := "Ng4"
	opt, err := PGN(pgn)
	if err != nil {
		t.Fatalf("PGN(pgn) failed")
	}
	game := NewGame(opt)

	startMlen := len(game.Moves())
	startPlen := len(game.Positions())

	_, err = game.PushMoveText(mv, SAN(), &MoveInsertOptions{
		PromoteToMainLine: true,
	})
	if err != nil {
		t.Errorf("PushMoveText() failed but should have succeeded")
	}

	if len(game.Moves()) != startMlen+1 {
		t.Errorf("PushMoveText() failed to update game.Moves()")
	}
	if len(game.Positions()) != startPlen+1 {
		t.Errorf("PushMoveText() failed to update game.Positions()")
	}
}

func validateSplit(t *testing.T, origPgn string, expectedLastLines []string) {
	reader := strings.NewReader(origPgn)
	game, err := NewPGNDecoder(reader).Decode()
	if err != nil {
		t.Fatalf("fail to parse game: %s", err.Error())
	}

	if game == nil {
		t.Fatalf("game is nil")
	}

	splitGames := game.Split()
	if len(expectedLastLines) != len(splitGames) {
		t.Fatalf("expected %v split games but got %v", len(expectedLastLines),
			len(splitGames))
	}

	for idx, g := range game.Split() {
		lines := strings.Split(g.String(), "\n")
		if len(lines) == 0 {
			t.Fatalf("split game %v output blank", idx)
		}

		lastLine := lines[len(lines)-1]
		if lastLine != expectedLastLines[idx] {
			t.Errorf("game output not correct\n\tExpected:'%v'\n\tGot:     '%v'\n",
				expectedLastLines[idx], lastLine)
		}
	}
}

func TestGameSplitVar(t *testing.T) {
	expectedLastLines := []string{
		"1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Nxd4 *",
		"1. e4 e5 2. Nc3 Nf6 3. f4 *",
		"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 dxe5 5. Qxd8+ Kxd8 *",
		"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. Nf3 Nbd7 *",
		"1. e3 e5 *",
	}

	pgn := mustParsePGN("fixtures/pgns/variations.pgn")
	validateSplit(t, pgn, expectedLastLines)
}

func TestGameSplitNoVar(t *testing.T) {
	expectedLastLines := []string{
		"1. e4 e5 2. Nf3 Nc6 *",
	}

	pgn := "[Event \"SomeEvent\"]\n1. e4 e5 2. Nf3 Nc6\n\n"
	validateSplit(t, pgn, expectedLastLines)
}

func TestRootMoveComments(t *testing.T) {
	t.Run("BasicRootMoveComment", func(t *testing.T) {
		game := NewGame()

		// Add a comment to the root move
		root := game.MoveTree().Root()
		root.AddComment("This is a comment before the first move")

		// Add some moves
		game.PushMove("e4", nil)
		game.PushMove("e5", nil)

		// Generate PGN
		pgn := game.String()

		// Check that the comment appears in the PGN
		if !strings.Contains(pgn, "{This is a comment before the first move}") {
			t.Errorf("Root move comment not found in PGN output: %s", pgn)
		}

		// Verify the comment appears before the first move
		lines := strings.Split(pgn, "\n")
		foundComment := false
		foundFirstMove := false

		for _, line := range lines {
			if strings.Contains(line, "{This is a comment before the first move}") {
				foundComment = true
			}
			if strings.Contains(line, "1. e4") {
				foundFirstMove = true
			}
			// Comment should appear before the first move
			if foundFirstMove && !foundComment {
				t.Errorf("Comment should appear before the first move in PGN")
			}
		}

		if !foundComment {
			t.Errorf("Comment not found in PGN output")
		}
	})

	t.Run("RootMoveCommentWithNoMoves", func(t *testing.T) {
		game := NewGame()

		// Add a comment to the root move
		root := game.MoveTree().Root()
		root.AddComment("Comment on empty game")

		// Generate PGN
		pgn := game.String()

		// Check that the comment appears in the PGN
		if !strings.Contains(pgn, "{Comment on empty game}") {
			t.Errorf("Root move comment not found in PGN output for empty game: %s", pgn)
		}
	})

	t.Run("RootMoveCommentWithMultipleComments", func(t *testing.T) {
		game := NewGame()

		// Add multiple comments to the root move
		root := game.MoveTree().Root()
		root.AddComment("First comment. ")
		root.AddComment("Second comment.")

		// Add some moves
		game.PushMove("e4", nil)

		// Generate PGN
		pgn := game.String()

		// Check that both comments appear in the PGN
		if !strings.Contains(pgn, "{First comment. Second comment.}") {
			t.Errorf("Combined root move comments not found in PGN output: %s", pgn)
		}
	})

	t.Run("RootMoveCommentWithTags", func(t *testing.T) {
		game := NewGame()

		// Add tag pairs
		game.AddTagPair("Event", "Test Event")
		game.AddTagPair("Site", "Test Site")

		// Add a comment to the root move
		root := game.MoveTree().Root()
		root.AddComment("Comment with tags")

		// Add some moves
		game.PushMove("e4", nil)

		// Generate PGN
		pgn := game.String()

		// Check that the comment appears in the PGN
		if !strings.Contains(pgn, "{Comment with tags}") {
			t.Errorf("Root move comment not found in PGN output with tags: %s", pgn)
		}

		// Verify the structure: tags, empty line, comment, moves
		lines := strings.Split(pgn, "\n")
		foundTags := false
		foundEmptyLine := false
		foundComment := false
		foundMove := false

		for _, line := range lines {
			if strings.Contains(line, "[Event") || strings.Contains(line, "[Site") {
				foundTags = true
			}
			if line == "" && foundTags {
				foundEmptyLine = true
			}
			if strings.Contains(line, "{Comment with tags}") {
				foundComment = true
			}
			if strings.Contains(line, "1. e4") {
				foundMove = true
			}
		}

		if !foundTags || !foundEmptyLine || !foundComment || !foundMove {
			t.Errorf("PGN structure incorrect. Tags: %v, EmptyLine: %v, Comment: %v, Move: %v",
				foundTags, foundEmptyLine, foundComment, foundMove)
		}
	})

	t.Run("RootMoveCommentWithVariations", func(t *testing.T) {
		game := NewGame()

		// Add a comment to the root move
		root := game.MoveTree().Root()
		root.AddComment("Comment before variations")

		// Add moves and create variations
		game.PushMove("e4", nil)
		game.PushMove("e5", nil)
		game.MoveTree().GoBack()
		game.PushMove("d5", nil)

		// Generate PGN
		pgn := game.String()

		// Check that the comment appears in the PGN
		if !strings.Contains(pgn, "{Comment before variations}") {
			t.Errorf("Root move comment not found in PGN output with variations: %s", pgn)
		}

		// Verify the comment appears before any moves or variations
		lines := strings.Split(pgn, "\n")
		foundComment := false
		foundMoves := false

		for _, line := range lines {
			if strings.Contains(line, "{Comment before variations}") {
				foundComment = true
			}
			if strings.Contains(line, "1. e4") || strings.Contains(line, "(1... d5)") {
				foundMoves = true
			}
			// Comment should appear before moves
			if foundMoves && !foundComment {
				t.Errorf("Comment should appear before moves in PGN with variations")
			}
		}
	})
}

func TestWriteAnnotations(t *testing.T) {
	t.Run("NilMove", func(t *testing.T) {
		var sb strings.Builder
		writeAnnotations(nil, &sb)
		if sb.String() != "" {
			t.Fatalf("expected empty annotation output, got %q", sb.String())
		}
	})

	t.Run("StructuredCommentAndCommand", func(t *testing.T) {
		move := &MoveNode{}
		move.SetComment("Good move")
		move.SetCommand("clk", "0:05:00")
		var sb strings.Builder
		writeAnnotations(move, &sb)
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

func TestValidateSAN(t *testing.T) {
	tests := []struct {
		name    string
		san     string
		wantErr bool
	}{
		// Valid SAN notation tests
		{
			name:    "valid pawn move",
			san:     "e4",
			wantErr: false,
		},
		{
			name:    "valid piece move",
			san:     "Nf3",
			wantErr: false,
		},
		{
			name:    "valid piece move with check",
			san:     "Qd2+",
			wantErr: false,
		},
		{
			name:    "valid piece move with checkmate",
			san:     "Qd2#",
			wantErr: false,
		},
		{
			name:    "valid capture",
			san:     "Qxd2",
			wantErr: false,
		},
		{
			name:    "valid capture with check",
			san:     "Qxd2+",
			wantErr: false,
		},
		{
			name:    "valid pawn capture",
			san:     "exd5",
			wantErr: false,
		},
		{
			name:    "valid promotion",
			san:     "e8=Q",
			wantErr: false,
		},
		{
			name:    "valid promotion with check",
			san:     "e8=Q+",
			wantErr: false,
		},
		{
			name:    "valid promotion with checkmate",
			san:     "e8=Q#",
			wantErr: false,
		},
		{
			name:    "valid castling kingside",
			san:     "O-O",
			wantErr: false,
		},
		{
			name:    "valid castling queenside",
			san:     "O-O-O",
			wantErr: false,
		},
		{
			name:    "valid castling with check",
			san:     "O-O+",
			wantErr: false,
		},
		{
			name:    "valid castling with checkmate",
			san:     "O-O#",
			wantErr: false,
		},
		{
			name:    "valid move with disambiguation file",
			san:     "Nbd7",
			wantErr: false,
		},
		{
			name:    "valid move with disambiguation rank",
			san:     "N1d2",
			wantErr: false,
		},
		{
			name:    "valid move with disambiguation both",
			san:     "N1d2",
			wantErr: false,
		},
		{
			name:    "valid move with question mark",
			san:     "e4?",
			wantErr: false,
		},
		{
			name:    "valid move with exclamation mark",
			san:     "e4!",
			wantErr: false,
		},
		{
			name:    "valid move with double question mark",
			san:     "e4??",
			wantErr: false,
		},
		{
			name:    "valid move with double exclamation mark",
			san:     "e4!!",
			wantErr: false,
		},
		{
			name:    "valid move with question exclamation",
			san:     "e4?!",
			wantErr: false,
		},
		{
			name:    "valid move with exclamation question",
			san:     "e4!?",
			wantErr: false,
		},
		{
			name:    "valid en passant",
			san:     "exd6e.p.",
			wantErr: false,
		},
		{
			name:    "valid en passant with check",
			san:     "exd6e.p.+",
			wantErr: false,
		},
		{
			name:    "valid move with multiple files (disambiguation)",
			san:     "Nef3",
			wantErr: false,
		},
		{
			name:    "valid move with capture and disambiguation",
			san:     "Nxd7",
			wantErr: false,
		},
		{
			name:    "valid move with rank 9 (edge case - regex accepts it)",
			san:     "e9",
			wantErr: false,
		},

		// Invalid SAN notation tests
		{
			name:    "invalid piece",
			san:     "Xf3",
			wantErr: true,
		},
		{
			name:    "invalid file",
			san:     "ei4",
			wantErr: true,
		},
		{
			name:    "invalid capture without destination",
			san:     "Qx",
			wantErr: true,
		},
		{
			name:    "invalid promotion without piece",
			san:     "e8=",
			wantErr: true,
		},
		{
			name:    "invalid promotion piece",
			san:     "e8=P",
			wantErr: true,
		},
		{
			name:    "invalid castling",
			san:     "O-O-O-O",
			wantErr: true,
		},
		{
			name:    "invalid castling format",
			san:     "0-0",
			wantErr: true,
		},
		{
			name:    "empty string",
			san:     "",
			wantErr: true,
		},
		{
			name:    "just piece",
			san:     "Q",
			wantErr: true,
		},
		{
			name:    "just file",
			san:     "e",
			wantErr: true,
		},
		{
			name:    "just rank",
			san:     "4",
			wantErr: true,
		},
		{
			name:    "invalid move with multiple pieces",
			san:     "NNf3",
			wantErr: true,
		},
		{
			name:    "invalid move with multiple ranks",
			san:     "N13f3",
			wantErr: true,
		},
		{
			name:    "invalid move with invalid characters",
			san:     "N@f3",
			wantErr: true,
		},
		{
			name:    "invalid move with spaces",
			san:     "N f3",
			wantErr: true,
		},
		{
			name:    "invalid move with tabs",
			san:     "N\tf3",
			wantErr: true,
		},
		{
			name:    "invalid move with invalid promotion",
			san:     "e8=X",
			wantErr: true,
		},
		{
			name:    "invalid move with invalid check symbol",
			san:     "e4*",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateSAN(tt.san)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateSAN(%q) error = %v, wantErr %v", tt.san, err, tt.wantErr)
			}
		})
	}
}

func TestGameMoveValidation(t *testing.T) {
	tests := []struct {
		name        string
		setupMoves  []string // Moves to set up the position
		move        Move     // Move to test
		wantErr     bool     // Whether we expect an error
		errorString string   // Expected error string (if wantErr is true)
	}{
		{
			name: "valid move should succeed",
			move: Move{
				s1: E2,
				s2: E4,
			},
			wantErr: false,
		},
		{
			name: "invalid move should fail",
			move: Move{
				s1: E2,
				s2: E5, // Invalid move - pawn can't move three squares from e2 to e5
			},
			wantErr:     true,
			errorString: "move e2e5 is not valid for the current position",
		},
		{
			name:       "invalid move from valid position should fail",
			setupMoves: []string{"e4", "e5"},
			move: Move{
				s1: E4,
				s2: E6, // Invalid move - pawn can't move two squares from e4 to e6
			},
			wantErr:     true,
			errorString: "move e4e6 is not valid for the current position",
		},
		{
			name:       "valid move from valid position should succeed",
			setupMoves: []string{"e4", "e5"},
			move: Move{
				s1: G1,
				s2: F3,
			},
			wantErr: false,
		},
		{
			name:       "valid promotion move should succeed",
			setupMoves: []string{"e4", "d5", "exd5", "c6", "dxc6", "Nf6", "cxb7", "Nbd7"},
			move: Move{
				s1:    B7,
				s2:    A8,
				promo: Queen,
			},
			wantErr: false,
		},
		{
			name:       "invalid promotion move should fail",
			setupMoves: []string{"e4", "d5", "exd5", "c6", "dxc6", "Nf6", "cxb7", "Nbd7"},
			move: Move{
				s1:    B7,
				s2:    A8,
				promo: King, // Invalid promotion piece
			},
			wantErr:     true,
			errorString: "move b7a8k is not valid for the current position",
		},
		{
			name:       "valid castling move should succeed",
			setupMoves: []string{"e4", "e5", "Nf3", "Nc6", "Bc4", "Bc5", "d3", "Nf6"},
			move: Move{
				s1:   E1,
				s2:   G1,
				tags: KingSideCastle,
			},
			wantErr: false,
		},
		{
			name:       "invalid castling move should fail",
			setupMoves: []string{"e4", "e5", "Nf3", "Nc6", "Bc4", "Bc5", "d3", "Nf6"},
			move: Move{
				s1:   E1,
				s2:   H1, // Invalid castling destination
				tags: KingSideCastle,
			},
			wantErr:     true,
			errorString: "move e1h1 is not valid for the current position",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a new game for each test
			game := NewGame()

			// Setup moves
			for _, move := range tt.setupMoves {
				_, err := game.PushMove(move, nil)
				if err != nil {
					t.Fatalf("setup failed: %v", err)
				}
			}

			// Test the move
			_, err := game.Move(tt.move, nil)

			// Check error expectation
			if (err != nil) != tt.wantErr {
				t.Errorf("Move() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				if tt.errorString != "" && err.Error() != tt.errorString {
					t.Errorf("Move() error = %v, want error string %v", err.Error(), tt.errorString)
				}
				return
			}

			// Check that the current move matches our move
			if game.MoveTree().Current() == nil {
				t.Errorf("Move() succeeded but currentMove is nil")
				return
			}

			if game.MoveTree().Current().Move() != tt.move {
				t.Errorf("Move() succeeded but currentMove doesn't match: got %v, want %v",
					game.MoveTree().Current().Move(), tt.move)
			}
		})
	}
}

func TestGameUnsafeMove(t *testing.T) {
	tests := []struct {
		name       string
		setupMoves []string // Moves to set up the position
		move       Move     // Move to test
		wantErr    bool     // Whether we expect an error
	}{
		{
			name: "valid move should succeed without validation",
			move: Move{
				s1: E2,
				s2: E4,
			},
			wantErr: false,
		},
		{
			name: "invalid move should still succeed (no validation)",
			move: Move{
				s1: E2,
				s2: E5, // Invalid move but UnsafeMove doesn't validate
			},
			wantErr: false, // UnsafeMove doesn't validate, so no error expected
		},
		{
			name:       "complex valid move should succeed",
			setupMoves: []string{"e4", "e5"},
			move: Move{
				s1: G1,
				s2: F3,
			},
			wantErr: false,
		},
		{
			name:       "promotion move should succeed without validation",
			setupMoves: []string{"e4", "d5", "exd5", "c6", "dxc6", "Nf6", "cxb7", "Nbd7"},
			move: Move{
				s1:    B7,
				s2:    A8,
				promo: Queen,
			},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a new game for each test
			game := NewGame()

			// Setup moves
			for _, move := range tt.setupMoves {
				_, err := game.PushMove(move, nil)
				if err != nil {
					t.Fatalf("setup failed: %v", err)
				}
			}

			// Test the move
			_, err := game.UnsafeMove(tt.move, nil)

			// Check error expectation
			if (err != nil) != tt.wantErr {
				t.Errorf("UnsafeMove() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			// Check that the current move matches our move
			if game.MoveTree().Current() == nil {
				t.Errorf("UnsafeMove() succeeded but currentMove is nil")
				return
			}

			if game.MoveTree().Current().Move() != tt.move {
				t.Errorf("UnsafeMove() succeeded but currentMove doesn't match: got %v, want %v",
					game.MoveTree().Current().Move(), tt.move)
			}
		})
	}
}

// TestMoveVsUnsafeMovePerformance demonstrates the performance difference
func TestMoveVsUnsafeMovePerformance(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping performance test in short mode")
	}

	game := NewGame()
	validMoves := game.ValidMoves()
	if len(validMoves) == 0 {
		t.Fatal("no valid moves available")
	}

	move := validMoves[0]

	// Test Move (with validation)
	start := time.Now()
	for range 1000 {
		gameClone := game.Clone()
		_, err := gameClone.Move(move, nil)
		if err != nil {
			t.Fatalf("Move failed: %v", err)
		}
	}
	moveTime := time.Since(start)

	// Test UnsafeMove (without validation)
	start = time.Now()
	for range 1000 {
		gameClone := game.Clone()
		_, err := gameClone.UnsafeMove(move, nil)
		if err != nil {
			t.Fatalf("UnsafeMove failed: %v", err)
		}
	}
	unsafeMoveTime := time.Since(start)

	t.Logf("Move() (with validation): %v", moveTime)
	t.Logf("UnsafeMove() (no validation): %v", unsafeMoveTime)
	t.Logf("Performance improvement: %.2fx", float64(moveTime)/float64(unsafeMoveTime))

	// UnsafeMove should be faster (though the difference might be small for simple positions)
	if unsafeMoveTime >= moveTime {
		t.Logf("Warning: UnsafeMove wasn't faster than Move - this might be expected for simple positions")
	}
}

func TestUnsafePushMoveText(t *testing.T) {
	tests := []struct {
		name       string
		setupMoves []string // Moves to set up the position
		moveStr    string   // Move to test
		codec      MoveTextCodec
		wantErr    bool // Whether we expect an error
	}{
		{
			name:    "valid UCI move should succeed without validation",
			moveStr: "e2e4",
			codec:   UCI(),
			wantErr: false,
		},
		{
			name:    "valid long algebraic move should succeed without validation",
			moveStr: "e2e4",
			codec:   LongAlgebraic(),
			wantErr: false,
		},
		{
			name:    "invalid notation should fail during parsing",
			moveStr: "xyz",
			codec:   UCI(),
			wantErr: true, // This should fail at notation parsing, not validation
		},
		{
			name:       "invalid move should still succeed (no validation)",
			setupMoves: []string{"e4", "e5"},
			moveStr:    "e2e3", // This move is illegal but UnsafePushMoveText doesn't validate
			codec:      UCI(),
			wantErr:    false, // No validation, so no error expected
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a new game for each test
			game := NewGame()

			// Setup moves
			for _, move := range tt.setupMoves {
				_, err := game.PushMoveText(move, SAN(), nil)
				if err != nil {
					t.Fatalf("setup failed: %v", err)
				}
			}

			// Test the move
			_, err := game.UnsafePushMoveText(tt.moveStr, tt.codec, nil)

			// Check error expectation
			if (err != nil) != tt.wantErr {
				t.Errorf("UnsafePushMoveText() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			if tt.wantErr {
				return
			}

			// If the move was successful, verify it was added to the game
			if game.MoveTree().Current() == nil {
				t.Errorf("UnsafePushMoveText() succeeded but currentMove is nil")
				return
			}

			// For successful cases, just verify that some move was made
			moves := game.Moves()
			if len(moves) == 0 {
				t.Errorf("UnsafePushMoveText() succeeded but no moves in game")
			}
		})
	}
}

// TestPushMoveTextVsUnsafePushMoveTextPerformance demonstrates the performance difference.
func TestPushMoveTextVsUnsafePushMoveTextPerformance(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping performance test in short mode")
	}

	game := NewGame()

	// Test with a common opening move
	moveStr := "e4"
	unsafeMoveStr := "e2e4"

	// Test PushMoveText (with validation)
	start := time.Now()
	for range 1000 {
		gameClone := game.Clone()
		_, err := gameClone.PushMoveText(moveStr, SAN(), nil)
		if err != nil {
			t.Fatalf("PushMoveText failed: %v", err)
		}
	}
	pushNotationMoveTime := time.Since(start)

	// Test UnsafePushMoveText (without validation)
	start = time.Now()
	for range 1000 {
		gameClone := game.Clone()
		_, err := gameClone.UnsafePushMoveText(unsafeMoveStr, UCI(), nil)
		if err != nil {
			t.Fatalf("UnsafePushMoveText failed: %v", err)
		}
	}
	unsafePushNotationMoveTime := time.Since(start)

	t.Logf("PushMoveText() (with validation): %v", pushNotationMoveTime)
	t.Logf("UnsafePushMoveText() (no validation): %v", unsafePushNotationMoveTime)
	t.Logf("Performance improvement: %.2fx", float64(pushNotationMoveTime)/float64(unsafePushNotationMoveTime))

	// UnsafePushMoveText should be faster
	if unsafePushNotationMoveTime >= pushNotationMoveTime {
		t.Logf("Warning: UnsafePushMoveText wasn't faster than PushMoveText - this might be expected for simple positions")
	}
}

func TestIgnoreFivefoldRepetitionDraw(t *testing.T) {
	g := NewGame(IgnoreFivefoldRepetitionDraw())
	if !g.ignoreFivefoldRepetitionDraw {
		t.Fatal("ignoreFivefoldRepetitionDraw should be true after being ignored")
	}
}

func TestIgnoreSeventyFiveMoveRuleDraw(t *testing.T) {
	g := NewGame(IgnoreSeventyFiveMoveRuleDraw())
	if !g.ignoreSeventyFiveMoveRuleDraw {
		t.Fatal("ignoreSeventyFiveMoveRuleDraw should be true after being ignored")
	}
}

func TestIgnoreInsufficientMaterialDraw(t *testing.T) {
	g := NewGame(IgnoreInsufficientMaterialDraw())
	if !g.ignoreInsufficientMaterialDraw {
		t.Fatal("ignoreInsufficientMaterialDraw should be true after being ignored")
	}
}

func TestCastlingInteractions(t *testing.T) {
	tests := []struct {
		name        string
		fen         string
		firstMove   string
		secondMove  string
		shouldAllow bool
	}{
		// No Pawns (Blocked)
		{
			name:        "No Pawns: White O-O then Black O-O",
			fen:         "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1",
			firstMove:   "O-O",
			secondMove:  "O-O",
			shouldAllow: false,
		},
		{
			name:        "No Pawns: White O-O-O then Black O-O-O",
			fen:         "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1",
			firstMove:   "O-O-O",
			secondMove:  "O-O-O",
			shouldAllow: false,
		},
		{
			name:        "No Pawns: Black O-O then White O-O",
			fen:         "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1",
			firstMove:   "O-O",
			secondMove:  "O-O",
			shouldAllow: false,
		},
		{
			name:        "No Pawns: Black O-O-O then White O-O-O",
			fen:         "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1",
			firstMove:   "O-O-O",
			secondMove:  "O-O-O",
			shouldAllow: false,
		},
		// With Pawns (Allowed)
		{
			name:        "With Pawns: White O-O then Black O-O",
			fen:         "r3k2r/5p2/8/8/8/8/5P2/R3K2R w KQkq - 0 1", // Pawns at f2, f7
			firstMove:   "O-O",
			secondMove:  "O-O",
			shouldAllow: true,
		},
		{
			name:        "With Pawns: White O-O-O then Black O-O-O",
			fen:         "r3k2r/3p4/8/8/8/8/3P4/R3K2R w KQkq - 0 1", // Pawns at d2, d7
			firstMove:   "O-O-O",
			secondMove:  "O-O-O",
			shouldAllow: true,
		},
		{
			name:        "With Pawns: Black O-O then White O-O",
			fen:         "r3k2r/5p2/8/8/8/8/5P2/R3K2R b KQkq - 0 1", // Pawns at f2, f7
			firstMove:   "O-O",
			secondMove:  "O-O",
			shouldAllow: true,
		},
		{
			name:        "With Pawns: Black O-O-O then White O-O-O",
			fen:         "r3k2r/3p4/8/8/8/8/3P4/R3K2R b KQkq - 0 1", // Pawns at d2, d7
			firstMove:   "O-O-O",
			secondMove:  "O-O-O",
			shouldAllow: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fen, err := FEN(tt.fen)
			if err != nil {
				t.Fatalf("Invalid FEN: %v", err)
			}
			g := NewGame(fen)

			// Make first move
			pos := g.Position()
			m1, err := algebraicNotation{}.Decode(pos, tt.firstMove)
			if err != nil {
				t.Fatalf("Failed to decode first move %s: %v", tt.firstMove, err)
			}
			if _, err := g.Move(m1, nil); err != nil {
				t.Fatalf("Failed to make first move %s: %v", tt.firstMove, err)
			}

			// Check if second move is valid
			validMoves := g.ValidMoves()
			isAllowed := false

			// Determine expected tag based on second move string
			var expectedTag MoveTag
			if tt.secondMove == "O-O" {
				expectedTag = KingSideCastle
			} else {
				expectedTag = QueenSideCastle
			}

			for _, m := range validMoves {
				if m.HasTag(expectedTag) {
					isAllowed = true
					break
				}
			}

			if isAllowed != tt.shouldAllow {
				t.Errorf("Castling allowed: %v, want: %v", isAllowed, tt.shouldAllow)
			}
		})
	}
}

func TestMoveHistoryEmptyGame(t *testing.T) {
	g := NewGame()
	history := g.MoveHistory()
	if len(history) != 0 {
		t.Fatalf("expected empty move history, got %d", len(history))
	}
}

func TestMoveHistoryMainLine(t *testing.T) {
	g := NewGame()
	for _, m := range []string{"e4", "e5", "Nf3"} {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	history := g.MoveHistory()
	if len(history) != 3 {
		t.Fatalf("expected 3 move history entries, got %d", len(history))
	}
	if history[0].Move.String() != "e2e4" {
		t.Fatalf("expected first move e2e4, got %s", history[0].Move)
	}
	if history[0].PrePosition != g.MoveTree().Root().position {
		t.Fatalf("expected first pre-position to be root position")
	}
	if history[0].PostPosition != g.MoveTree().MainLine()[0].position {
		t.Fatalf("expected post-position to match move position")
	}
	if history[1].PrePosition != history[0].PostPosition {
		t.Fatalf("expected second pre-position to match first post-position")
	}
}

func TestMoveHistoryComments(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	g.MoveTree().Current().SetComment("good move")
	history := g.MoveHistory()
	if len(history) != 1 {
		t.Fatalf("expected 1 move history entry, got %d", len(history))
	}
	if len(history[0].Comments) != 1 || history[0].Comments[0] != "good move" {
		t.Fatalf("expected comment %q, got %v", "good move", history[0].Comments)
	}
}

func TestMoveHistoryNoComments(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	history := g.MoveHistory()
	if len(history[0].Comments) != 0 {
		t.Fatalf("expected no comments, got %v", history[0].Comments)
	}
}

func TestMoveHistoryWithVariations(t *testing.T) {
	g := NewGame()
	if _, err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if _, err := g.PushMove("e5", nil); err != nil {
		t.Fatal(err)
	}
	variationMove := Move{}
	g.MoveTree().AddVariation(g.MoveTree().Root().children[0], variationMove)
	history := g.MoveHistory()
	if len(history) != 2 {
		t.Fatalf("expected 2 main line entries (variations excluded), got %d", len(history))
	}
}

func TestMoveHistoryMatchesMovesLength(t *testing.T) {
	g := NewGame()
	for _, m := range []string{"e4", "e5", "Nf3", "Nc6", "Bb5"} {
		if _, err := g.PushMove(m, nil); err != nil {
			t.Fatal(err)
		}
	}
	moves := g.Moves()
	history := g.MoveHistory()
	if len(history) != len(moves) {
		t.Fatalf("expected history length %d to match moves length %d", len(history), len(moves))
	}
}

func TestMoveHistoryFromPGN(t *testing.T) {
	pgnData := mustParsePGN("fixtures/pgns/single_game.pgn")
	r := strings.NewReader(pgnData)
	opt, err := PGN(r)
	if err != nil {
		t.Fatal(err)
	}
	game := NewGame(opt)

	history := game.MoveHistory()
	if len(history) == 0 {
		t.Fatal("expected move history from PGN game")
	}

	for i, h := range history {
		if h.PrePosition == nil {
			t.Fatalf("entry %d: PrePosition is nil", i)
		}
		if h.PostPosition == nil {
			t.Fatalf("entry %d: PostPosition is nil", i)
		}
		if i > 0 && h.PrePosition != history[i-1].PostPosition {
			t.Fatalf("entry %d: PrePosition should match previous PostPosition", i)
		}
	}
}

func TestGameUnsafeMoves(t *testing.T) {
	game := NewGame()
	if len(game.UnsafeMoves()) != 0 {
		t.Errorf("expected 0 unsafe moves at start, got %d", len(game.UnsafeMoves()))
	}

	// Verify delegation to Position
	pos := game.Position()
	if len(game.UnsafeMoves()) != len(pos.UnsafeMoves()) {
		t.Errorf("Game.UnsafeMoves() length mismatch with Position.UnsafeMoves()")
	}
}

func TestEscapeTagValue(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"plain", "hello", "hello"},
		{"quote", `say "hi"`, `say \"hi\"`},
		{"backslash", `path\to`, `path\\to`},
		{"mixed", `a\"b`, `a\\\"b`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := escapeTagValue(tc.input); got != tc.want {
				t.Errorf("escapeTagValue(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}
