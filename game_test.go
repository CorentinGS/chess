package chess

import (
	"log"
	"testing"
)

func TestCheckmate(t *testing.T) {
	fenStr := "rn1qkbnr/pbpp1ppp/1p6/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if err := g.MoveStr("Qxf7#"); err != nil {
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
	if err := g.MoveStr("O-O-O"); err != nil {
		t.Fatal(err)
	}
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
	if err := g.MoveStr("Qb6"); err != nil {
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
	if err := g.MoveStr("d8=Q"); err != nil {
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
		if err := g.MoveStr(m); err != nil {
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
		if err := g.MoveStr(m); err != nil {
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
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	if g.Outcome() != Draw || g.Method() != FivefoldRepetition {
		t.Fatal("should automatically draw after five repetitions")
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
	if err := g.MoveStr("Kf8"); err != nil {
		t.Fatal(err)
	}
	if g.Outcome() != Draw || g.Method() != SeventyFiveMoveRule {
		t.Fatal("should automatically draw after seventy five moves w/ no pawn move or capture")
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
		g1.MoveStr(s)
	}
	g2 := NewGame()
	for _, s := range []string{"Nf3", "e5", "Nc3"} {
		g2.MoveStr(s)
	}
	if g1.Position().Hash() != g2.Position().Hash() {
		t.Fatalf("expected position hashes to be equal but got %s and %s", g1.Position().Hash(), g2.Position().Hash())
	}
}

func BenchmarkStalemateStatus(b *testing.B) {
	fenStr := "k1K5/8/8/8/8/8/8/1Q6 w - - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		b.Fatal(err)
	}
	g := NewGame(fen)
	if err := g.MoveStr("Qb6"); err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
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
	if err := g.MoveStr("d8=Q"); err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
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
	for n := 0; n < b.N; n++ {
		g.Position().Hash()
	}
}

func TestAddVariationToEmptyParent(t *testing.T) {
	g := NewGame()
	parent := &Move{}
	newMove := &Move{}
	g.AddVariation(parent, newMove)
	if len(parent.children) != 1 || parent.children[0] != newMove {
		t.Fatalf("expected newMove to be added to parent's children")
	}
	if newMove.parent != parent {
		t.Fatalf("expected newMove's parent to be set to parent")
	}
}

func TestAddVariationToNonEmptyParent(t *testing.T) {
	g := NewGame()
	parent := &Move{children: []*Move{{}}}
	newMove := &Move{}
	g.AddVariation(parent, newMove)
	if len(parent.children) != 2 || parent.children[1] != newMove {
		t.Fatalf("expected newMove to be added to parent's children")
	}
	if newMove.parent != parent {
		t.Fatalf("expected newMove's parent to be set to parent")
	}
}

func TestAddVariationWithNilParent(t *testing.T) {
	g := NewGame()
	newMove := &Move{}
	defer func() {
		if r := recover(); r == nil {
			t.Fatalf("expected panic when parent is nil")
		}
	}()
	g.AddVariation(nil, newMove)
}

func TestNavigateToMainLineFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	g.NavigateToMainLine()
	if g.currentMove != g.rootMove.children[0] {
		t.Fatalf("expected to navigate to main line root move")
	}
}

func TestNavigateToMainLineFromBranch(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	variationMove := &Move{}
	g.AddVariation(g.currentMove, variationMove)
	g.currentMove = variationMove
	g.NavigateToMainLine()
	if g.currentMove != g.rootMove.children[0] {
		t.Fatalf("expected to navigate to main line root move")
	}
}

func TestNavigateToMainLineFromRoot(t *testing.T) {
	g := NewGame()
	g.NavigateToMainLine()
	if g.currentMove != g.rootMove {
		t.Fatalf("expected to stay at root move")
	}
}

func TestGoBackFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	if !g.GoBack() {
		t.Fatalf("expected to go back from leaf move")
	}
	if g.currentMove != g.rootMove.children[0].children[0].children[0].children[0] {
		t.Fatalf("expected current move to be Bb5's parent")
	}
}

func TestGoBackFromRoot(t *testing.T) {
	g := NewGame()
	if g.GoBack() {
		t.Fatalf("expected not to go back from root move")
	}
	if g.currentMove != g.rootMove {
		t.Fatalf("expected to stay at root move")
	}
}

func TestGoBackFromMainLine(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	if !g.GoBack() {
		t.Fatalf("expected to go back from main line move")
	}
	if g.currentMove != g.rootMove.children[0].children[0] {
		t.Fatalf("expected current move to be e5's parent")
	}
}

func TestGoForwardFromRoot(t *testing.T) {
	g := NewGame()
	g.MoveStr("e4")
	g.MoveStr("e5")
	g.currentMove = g.rootMove // Reset to root
	if !g.GoForward() {
		t.Fatalf("expected to go forward from root move")
	}
	if g.currentMove != g.rootMove.children[0] {
		t.Fatalf("expected current move to be the first child of root move")
	}
}

func TestGoForwardFromLeaf(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6", "Bb5"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	if g.GoForward() {
		t.Fatalf("expected not to go forward from leaf move")
	}
	if g.currentMove != g.rootMove.children[0].children[0].children[0].children[0].children[0] {
		t.Fatalf("expected current move to stay at leaf move")
	}
}

func TestGoForwardFromBranch(t *testing.T) {
	g := NewGame()
	moves := []string{"e4", "e5", "Nf3", "Nc6"}
	for _, m := range moves {
		if err := g.MoveStr(m); err != nil {
			t.Fatal(err)
		}
	}
	variationMove := &Move{}
	g.AddVariation(g.currentMove, variationMove)
	childMove := &Move{}                     // Add this line
	g.AddVariation(variationMove, childMove) // Add this line
	g.currentMove = variationMove
	if !g.GoForward() {
		t.Fatalf("expected to go forward from branch move")
	}
	if g.currentMove != childMove { // Change this line
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
	if err := g.MoveStr("e4"); err != nil {
		t.Fatal(err)
	}
	if g.IsAtStart() {
		t.Fatalf("expected not to be at start when not at root move")
	}
}

func TestIsAtEndWhenAtLeaf(t *testing.T) {
	g := NewGame()
	if err := g.MoveStr("e4"); err != nil {
		t.Fatal(err)
	}
	if !g.IsAtEnd() {
		t.Fatalf("expected to be at end when at leaf move")
	}
}

func TestIsAtEndWhenNotAtLeaf(t *testing.T) {
	g := NewGame()
	if err := g.MoveStr("e4"); err != nil {
		t.Fatal(err)
	}
	if err := g.MoveStr("e5"); err != nil {
		t.Fatal(err)
	}
	// Add this line to move back to a non-leaf position
	g.GoBack()
	if g.IsAtEnd() {
		t.Fatalf("expected not to be at end when not at leaf move")
	}
}

func TestVariationsWithNoChildren(t *testing.T) {
	g := NewGame()
	move := &Move{}
	variations := g.Variations(move)
	if variations != nil {
		t.Fatalf("expected no variations for move with no children")
	}
}

func TestVariationsWithOneChild(t *testing.T) {
	g := NewGame()
	move := &Move{children: []*Move{{}}}
	variations := g.Variations(move)
	if variations != nil {
		t.Fatalf("expected no variations for move with one child")
	}
}

func TestVariationsWithMultipleChildren(t *testing.T) {
	g := NewGame()
	move := &Move{children: []*Move{{}, {}}}
	variations := g.Variations(move)
	if len(variations) != 1 {
		t.Fatalf("expected one variation for move with multiple children")
	}
}

func TestVariationsWithNilMove(t *testing.T) {
	g := NewGame()
	variations := g.Variations(nil)
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
