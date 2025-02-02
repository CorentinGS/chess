package chess

import (
	"errors"
	"log"
	"strings"
	"testing"
)

func TestCheckmate(t *testing.T) {
	fenStr := "rn1qkbnr/pbpp1ppp/1p6/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1"
	fen, err := FEN(fenStr)
	if err != nil {
		t.Fatal(err)
	}
	g := NewGame(fen)
	if err := g.PushMove("Qxf7#", nil); err != nil {
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
	if err := g.PushMove("O-O-O", nil); err != nil {
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
	if err := g.PushMove("Qb6", nil); err != nil {
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
	if err := g.PushMove("d8=Q", nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
	if err := g.PushMove("Kf8", nil); err != nil {
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
		g1.PushMove(s, nil)
	}
	g2 := NewGame()
	for _, s := range []string{"Nf3", "e5", "Nc3"} {
		g2.PushMove(s, nil)
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
	if err := g.PushMove("Qb6", nil); err != nil {
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
	if err := g.PushMove("d8=Q", nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
	_ = g.PushMove("e4", nil)
	_ = g.PushMove("e5", nil)
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
		if err := g.PushMove(m, nil); err != nil {
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
		if err := g.PushMove(m, nil); err != nil {
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
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if g.IsAtStart() {
		t.Fatalf("expected not to be at start when not at root move")
	}
}

func TestIsAtEndWhenAtLeaf(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if !g.IsAtEnd() {
		t.Fatalf("expected to be at end when at leaf move")
	}
}

func TestIsAtEndWhenNotAtLeaf(t *testing.T) {
	g := NewGame()
	if err := g.PushMove("e4", nil); err != nil {
		t.Fatal(err)
	}
	if err := g.PushMove("e5", nil); err != nil {
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

func TestPushMove(t *testing.T) {
	tests := []struct {
		name          string
		setupMoves    []string         // Moves to set up the position
		move          string           // Move to push
		goBack        bool             // Whether to go back one move before pushing
		options       *PushMoveOptions // Options for the push
		wantErr       bool             // Whether we expect an error
		wantPosition  string           // Expected FEN after the move
		checkMainline []string         // Expected mainline moves in UCI notation
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
			options:       &PushMoveOptions{},
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "g1f3"}, // Original mainline remains
		},
		{
			name:          "create variation with force mainline",
			setupMoves:    []string{"e4", "e5", "Nf3"},
			move:          "Nc3",
			goBack:        true,
			options:       &PushMoveOptions{ForceMainline: true},
			wantPosition:  "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
			checkMainline: []string{"e2e4", "e7e5", "b1c3"}, // New mainline
		},
		{
			name:          "push existing move without override",
			setupMoves:    []string{"e4", "e5", "Nf3"},
			move:          "Nf3",
			goBack:        true,
			options:       &PushMoveOptions{},
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
				err := game.PushMove(move, nil)
				if err != nil {
					t.Fatalf("setup failed: %v", err)
				}
			}

			// Go back one move if needed for the test
			if tt.goBack && game.currentMove != nil && game.currentMove.parent != nil {
				game.GoBack()

			}

			// Test the move
			err := game.PushMove(tt.move, tt.options)

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
				gotFEN := game.pos.String()
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
	current := game.rootMove

	for len(current.children) > 0 {
		current = current.children[0] // Follow main line (first variation)
		moves = append(moves, algebraicMove(current))
	}

	return moves
}

// Helper function to convert a move to algebraic notation
func algebraicMove(move *Move) string {
	// This is a simplified version - you might want to implement proper algebraic notation
	if move.HasTag(KingSideCastle) {
		return "O-O"
	}
	if move.HasTag(QueenSideCastle) {
		return "O-O-O"
	}

	s1 := move.s1.String()
	s2 := move.s2.String()

	if move.promo != NoPieceType {
		return s1 + s2 + "=" + move.promo.String()
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
	_ = original.PushMove("e4", nil)
	_ = original.PushMove("e5", nil)
	_ = original.PushMove("Nf3", nil)

	newGame := NewGame()
	newGame.copy(original)

	if newGame.pos.String() != original.pos.String() {
		t.Fatalf("expected position %s but got %s", original.pos.String(), newGame.pos.String())
	}
	if newGame.currentMove != original.currentMove {
		t.Fatalf("expected current move to be %v but got %v", original.currentMove, newGame.currentMove)
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
	_ = original.PushMove("e4", nil)
	_ = original.PushMove("e5", nil)
	_ = original.PushMove("Nf3", nil)

	clone := original.Clone()

	if clone.pos.String() != original.pos.String() {
		t.Fatalf("expected position %s but got %s", original.pos.String(), clone.pos.String())
	}
	if clone.currentMove != original.currentMove {
		t.Fatalf("expected current move to be %v but got %v", original.currentMove, clone.currentMove)
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
		if err := g.PushMove(m, nil); err != nil {
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
}

func TestPGNWithInvalidData(t *testing.T) {
	pgnData := "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 h6 15. Bh4 Re8 16. a3 Bf8 17. Rc1 Qb6 18. dxe5 dxe5 19. Qe2 Nh5 20. Qd2 Nc5 21. Bc2 Nf4 22. Bg3 Rad8 23. Qe3 Qc7 24. Rcd1 Rxd1 25. Rxd1 Nce6 26. Bb3 Bc5 27. Qe1 Nd4 28. Nxd4 Bxd4 29. Bxf4 exf4 30. Rxd4 c5 31. Rd1 c4 32. Bc2 Qe5 33. f3 Qc5+ 34. Qf2 Qe5 35. Qd4 Qg5 36. Qd7 Re7 37. Qd8+ Kh7 38. e5+ g6 39. Qd6 Bxf3 40. Rd2 Rxe5 41. Qd4 Re1+ 42. Kf2 Qg3# 0-1"
	r := strings.NewReader(pgnData)
	_, err := PGN(r)
	if err == nil {
		t.Fatal("expected error for invalid PGN data")
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
				_ = g.PushMove("e4", nil)
				return g
			},
			expected: "1. e4 *",
		},
		{
			name: "GameStringWithMultipleMoves",
			setup: func() *Game {
				g := NewGame()
				_ = g.PushMove("e4", nil)
				_ = g.PushMove("e5", nil)
				_ = g.PushMove("Nf3", nil)
				return g
			},
			expected: "1. e4 e5 2. Nf3 *",
		},
		{
			name: "GameStringWithComments",
			setup: func() *Game {
				g := NewGame()
				_ = g.PushMove("e4", nil)
				g.currentMove.comments = "Good move"
				return g
			},
			expected: "1. e4 {Good move} *",
		},
		{
			name: "GameStringWithVariations",
			setup: func() *Game {
				g := NewGame()
				_ = g.PushMove("e4", nil)
				_ = g.PushMove("e5", nil)
				_ = g.PushMove("Nf3", nil)
				g.GoBack()
				_ = g.PushMove("Nc3", nil)
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
			name: "GameStringWithDifferentResults",
			setup: func() *Game {
				g := NewGame()
				g.outcome = WhiteWon
				return g
			},
			expected: "1-0",
		},
		{
			name: "GameStringWithDifferentResults",
			setup: func() *Game {
				g := NewGame()
				g.outcome = BlackWon
				return g
			},
			expected: "0-1",
		},
		{
			name: "GameStringWithDifferentResults",
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
				_ = g.PushMove("e4", nil)
				g.currentMove.comments = "Good move"
				g.currentMove.SetCommand("clk", "10:00:00")
				return g
			},
			expected: "1. e4 {Good move} { [%clk 10:00:00] } *",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			g := tt.setup()
			if g.String() != tt.expected {
				t.Fatalf("expected %v but got %v", tt.expected, g.String())
			}
		})
	}
}
