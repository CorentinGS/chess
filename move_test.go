package chess

import (
	"log"
	"testing"
)

type moveTest struct {
	pos     *Position
	m       *Move
	postPos *Position
}

var (
	validMoves = []moveTest{
		// pawn moves
		{m: &Move{s1: E2, s2: E4}, pos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")},
		{m: &Move{s1: A2, s2: A3}, pos: unsafeFEN("8/8/8/8/8/8/P7/8 w - - 0 1")},
		{m: &Move{s1: A7, s2: A6}, pos: unsafeFEN("8/p7/8/8/8/8/8/8 b - - 0 1")},
		{m: &Move{s1: A7, s2: A5}, pos: unsafeFEN("8/p7/8/8/8/8/8/8 b - - 0 1")},
		{m: &Move{s1: C4, s2: B5}, pos: unsafeFEN("8/8/8/1p1p4/2P5/8/8/8 w - - 0 1")},
		{m: &Move{s1: C4, s2: D5}, pos: unsafeFEN("8/8/8/1p1p4/2P5/8/8/8 w - - 0 1")},
		{m: &Move{s1: C4, s2: C5}, pos: unsafeFEN("8/8/8/1p1p4/2P5/8/8/8 w - - 0 1")},
		{m: &Move{s1: C5, s2: B4}, pos: unsafeFEN("8/8/8/2p5/1P1P4/8/8/8 b - - 0 1")},
		{m: &Move{s1: C5, s2: D4}, pos: unsafeFEN("8/8/8/2p5/1P1P4/8/8/8 b - - 0 1")},
		{m: &Move{s1: C5, s2: C4}, pos: unsafeFEN("8/8/8/2p5/1P1P4/8/8/8 b - - 0 1")},
		{m: &Move{s1: A4, s2: B3}, pos: unsafeFEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 0 23")},
		{m: &Move{s1: A2, s2: A1, promo: Queen}, pos: unsafeFEN("8/8/8/8/8/8/p7/8 b - - 0 1")},
		{m: &Move{s1: E7, s2: E6}, pos: unsafeFEN("r2qkbnr/pppnpppp/8/3p4/6b1/1P3NP1/PBPPPP1P/RN1QKB1R b KQkq - 2 4")},
		// knight moves
		{m: &Move{s1: E4, s2: F6}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D6}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: G5}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: G3}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D2}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: C3}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: C5}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: B8, s2: D7}, pos: unsafeFEN("rn1qkb1r/pp3ppp/2p1pn2/3p4/2PP4/2NQPN2/PP3PPP/R1B1K2R b KQkq - 0 7")},
		{m: &Move{s1: F6, s2: E4}, pos: unsafeFEN("r1b1k2r/ppp2ppp/2p2n2/4N3/4P3/2P5/PPP2PPP/R1BK3R b kq - 0 8")},
		// bishop moves
		{m: &Move{s1: E4, s2: H7}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D5}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: B1}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		// rook moves
		{m: &Move{s1: B2, s2: B4}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: B7}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: A2}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: H2}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: E1, s2: E8}, pos: unsafeFEN("r3r1k1/p4p1p/3p4/1p4p1/2pP4/2P2P2/PP3P1P/R3RK2 w - g6 0 22")},
		// queen moves
		{m: &Move{s1: B2, s2: E5}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: A1}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: A2}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: H2}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: D8, s2: D1}, pos: unsafeFEN("r1bqk2r/ppp2ppp/2p2n2/4N3/4P3/2P5/PPP2PPP/R1BQK2R b KQkq - 0 7")},
		// king moves
		{m: &Move{s1: E4, s2: E5}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: E3}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D3}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D4}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: D5}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: E5}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		// castleing
		{m: &Move{s1: E1, s2: G1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")},
		{m: &Move{s1: E1, s2: C1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")},
		{m: &Move{s1: E8, s2: G8}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")},
		{m: &Move{s1: E8, s2: C8}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")},
		// king moving in front of enemy pawn http://en.lichess.org/4HXJOtpN#75
		{m: &Move{s1: F8, s2: G7}, pos: unsafeFEN("3rrk2/8/2p3P1/1p2nP1p/pP2p3/P1B1NbPB/2P2K2/5R2 b - - 1 38")},
	}

	invalidMoves = []moveTest{
		// out of turn moves
		{m: &Move{s1: E7, s2: E5}, pos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")},
		{m: &Move{s1: E2, s2: E4}, pos: unsafeFEN("rnbqkbnr/1ppppppp/p7/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")},
		// pawn moves
		{m: &Move{s1: E2, s2: D3}, pos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")},
		{m: &Move{s1: E2, s2: F3}, pos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")},
		{m: &Move{s1: E2, s2: E5}, pos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")},
		{m: &Move{s1: A2, s2: A1}, pos: unsafeFEN("8/8/8/8/8/8/p7/8 b - - 0 1")},
		{m: &Move{s1: E6, s2: E5}, pos: unsafeFEN(`2b1r3/2k2p1B/p2np3/4B3/8/5N2/PP1K1PPP/3R4 b - - 2 1`)},
		{m: &Move{s1: H7, s2: H5}, pos: unsafeFEN(`2bqkbnr/rpppp2p/2n2p2/p5pB/5P2/4P3/PPPP2PP/RNBQK1NR b KQk - 4 6`)},
		// knight moves
		{m: &Move{s1: E4, s2: F2}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: F3}, pos: unsafeFEN("8/8/8/3pp3/4N3/8/5B2/8 w - - 0 1")},
		// bishop moves
		{m: &Move{s1: E4, s2: C6}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: E5}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: E4}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: F3}, pos: unsafeFEN("8/8/8/3pp3/4B3/5N2/8/8 w - - 0 1")},
		// rook moves
		{m: &Move{s1: B2, s2: B1}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: C3}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: B8}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: G7}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1R6/1B6 w - - 0 1")},
		// queen moves
		{m: &Move{s1: B2, s2: B1}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: C4}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: B8}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		{m: &Move{s1: B2, s2: G7}, pos: unsafeFEN("8/1p5b/4N3/4p3/8/8/1Q6/1B6 w - - 0 1")},
		// king moves
		{m: &Move{s1: E4, s2: F3}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: F4}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		{m: &Move{s1: E4, s2: F5}, pos: unsafeFEN("5r2/8/8/8/4K3/8/8/8 w - - 0 1")},
		// castleing
		{m: &Move{s1: E1, s2: B1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")},
		{m: &Move{s1: E8, s2: B8}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1")},
		{m: &Move{s1: E1, s2: C1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R2QK2R w KQkq - 0 1")},
		{m: &Move{s1: E1, s2: C1}, pos: unsafeFEN("2r1k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")},
		{m: &Move{s1: E1, s2: C1}, pos: unsafeFEN("3rk2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")},
		{m: &Move{s1: E1, s2: G1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w Qkq - 0 1")},
		{m: &Move{s1: E1, s2: C1}, pos: unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w Kkq - 0 1")},
		// invalid promotion for non-pawn move
		{m: &Move{s1: B8, s2: D7, promo: Pawn}, pos: unsafeFEN("rn1qkb1r/pp3ppp/2p1pn2/3p4/2PP4/2NQPN2/PP3PPP/R1B1K2R b KQkq - 0 7")},
		// en passant on doubled pawn file http://en.lichess.org/TnRtrHxf#24
		{m: &Move{s1: E3, s2: F6}, pos: unsafeFEN("r1b2rk1/pp2b1pp/1qn1p3/3pPp2/1P1P4/P2BPN2/6PP/RN1Q1RK1 w - f6 0 13")},
		// can't move piece out of pin (even if checking enemy king) http://en.lichess.org/JCRBhXH7#62
		{m: &Move{s1: E1, s2: E7}, pos: unsafeFEN("4R3/1r1k2pp/p1p5/1pP5/8/8/1PP3PP/2K1Rr2 w - - 5 32")},
		// invalid one up pawn capture
		{m: &Move{s1: E6, s2: E5}, pos: unsafeFEN(`2b1r3/2k2p1B/p2np3/4B3/8/5N2/PP1K1PPP/3R4 b - - 2 1`)},
		// invalid two up pawn capture
		{m: &Move{s1: H7, s2: H5}, pos: unsafeFEN(`2bqkbnr/rpppp2p/2n2p2/p5pB/5P2/4P3/PPPP2PP/RNBQK1NR b KQk - 4 6`)},
		// invalid pawn move d5e4
		{m: &Move{s1: D5, s2: E4}, pos: unsafeFEN(`rnbqkbnr/pp2pppp/8/2pp4/3P4/4PN2/PPP2PPP/RNBQKB1R b KQkq - 0 3`)},
	}

	positionUpdates = []moveTest{
		{
			m:       &Move{s1: E2, s2: E4},
			pos:     unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
			postPos: unsafeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"),
		},
		{
			m:       &Move{s1: E1, s2: G1, tags: KingSideCastle},
			pos:     unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"),
			postPos: unsafeFEN("r3k2r/8/8/8/8/8/8/R4RK1 b kq - 1 1"),
		},
		{
			m:       &Move{s1: A4, s2: B3, tags: EnPassant},
			pos:     unsafeFEN("2r3k1/1q1nbppp/r3p3/3pP3/pPpP4/P1Q2N2/2RN1PPP/2R4K b - b3 0 23"),
			postPos: unsafeFEN("2r3k1/1q1nbppp/r3p3/3pP3/11pP4/PpQ2N2/2RN1PPP/2R4K w - - 0 24"),
		},
		{
			m:       &Move{s1: E1, s2: G1, tags: KingSideCastle},
			pos:     unsafeFEN("r2qk2r/pp1n1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P4PPP/R1B1K2R w KQkq - 1 9"),
			postPos: unsafeFEN("r2qk2r/pp1n1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P4PPP/R1B2RK1 b kq - 2 9"),
		},
		// half move clock - knight move to f3 from starting position
		{
			m:       &Move{s1: G1, s2: F3},
			pos:     unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
			postPos: unsafeFEN("rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1"),
		},
		// half move clock - king side castle
		{
			m:       &Move{s1: E1, s2: G1, tags: KingSideCastle},
			pos:     unsafeFEN("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"),
			postPos: unsafeFEN("r3k2r/8/8/8/8/8/8/R4RK1 b kq - 1 1"),
		},
		// half move clock - queen side castle
		{
			m:       &Move{s1: E1, s2: C1, tags: QueenSideCastle},
			pos:     unsafeFEN("r3k2r/ppqn1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P2B1PPP/R3K2R w KQkq - 3 10"),
			postPos: unsafeFEN("r3k2r/ppqn1ppp/2pbpn2/3p4/2PP4/1PNQPN2/P2B1PPP/2KR3R b kq - 4 10"),
		},
		// half move clock - pawn push
		{
			m:       &Move{s1: E2, s2: E4},
			pos:     unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
			postPos: unsafeFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"),
		},
		// half move clock - pawn capture
		{
			m:       &Move{s1: E4, s2: D5, tags: Capture},
			pos:     unsafeFEN("r1bqkbnr/ppp1pppp/2n5/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3"),
			postPos: unsafeFEN("r1bqkbnr/ppp1pppp/2n5/3P4/8/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 3"),
		},
		// half move clock - en passant
		{
			m:       &Move{s1: E5, s2: F6, tags: EnPassant},
			pos:     unsafeFEN("r1bqkbnr/ppp1p1pp/2n5/3pPp2/8/5N2/PPPP1PPP/RNBQKB1R w KQkq f6 0 4"),
			postPos: unsafeFEN("r1bqkbnr/ppp1p1pp/2n2P2/3p4/8/5N2/PPPP1PPP/RNBQKB1R b KQkq - 0 4"),
		},
		// half move clock - piece captured by knight
		{
			m:       &Move{s1: C6, s2: D4, tags: Capture},
			pos:     unsafeFEN("r1bqkbnr/ppp1p1pp/2n5/3pPp2/3N4/8/PPPP1PPP/RNBQKB1R b KQkq - 1 4"),
			postPos: unsafeFEN("r1bqkbnr/ppp1p1pp/8/3pPp2/3n4/8/PPPP1PPP/RNBQKB1R w KQkq - 0 5"),
		},
	}
)

func unsafeFEN(s string) *Position {
	pos, err := decodeFEN(s)
	if err != nil {
		log.Fatal(err)
	}
	return pos
}

func TestValidMoves(t *testing.T) {
	for _, mt := range validMoves {
		if !moveIsValid(mt.pos, mt.m, false) {
			log.Println(mt.pos.String())
			log.Println(mt.pos.board.Draw())
			log.Println(mt.pos.ValidMoves())
			log.Println("In Check:", squaresAreAttacked(mt.pos, mt.pos.board.whiteKingSq))
			// log.Println("In Check:", mt.pos.inCheck())
			mt.pos.turn = mt.pos.turn.Other()
			t.Fatalf("expected move %s to be valid", mt.m)
		}
	}
}

func TestInvalidMoves(t *testing.T) {
	for _, mt := range invalidMoves {
		if moveIsValid(mt.pos, mt.m, false) {
			log.Println(mt.pos.String())
			log.Println(mt.pos.board.Draw())
			t.Fatalf("expected move %s to be invalid", mt.m)
		}
	}
}

func TestPositionUpdates(t *testing.T) {
	for _, mt := range positionUpdates {
		if !moveIsValid(mt.pos, mt.m, true) {
			log.Println(mt.pos.String())
			log.Println(mt.pos.board.Draw())
			log.Println(mt.pos.ValidMoves())
			t.Fatalf("expected move %s %v to be valid", mt.m, mt.m.tags)
		}

		postPos := mt.pos.Update(*mt.m)
		if postPos.String() != mt.postPos.String() {
			t.Fatalf("starting from board \n%s%s\n after move %s\n expected board to be %s\n%s\n but was %s\n%s\n",
				mt.pos.String(),
				mt.pos.board.Draw(),
				mt.m.String(),
				mt.postPos.String(),
				mt.postPos.board.Draw(),
				postPos.String(),
				postPos.board.Draw(),
			)
		}
	}
}

func Test_SetCommentUpdatesComment(t *testing.T) {
	move := &MoveNode{}
	move.SetComment("Initial comment")
	expected := "Initial comment"
	if move.Comments() != expected {
		t.Fatalf("expected %v but got %v", expected, move.Comments())
	}
}

func Test_SetCommentOverwritesExistingComment(t *testing.T) {
	move := &MoveNode{}
	move.SetComment("Old comment")
	move.SetComment("New comment")
	expected := "New comment"
	if move.Comments() != expected {
		t.Fatalf("expected %v but got %v", expected, move.Comments())
	}
}

func Test_SetCommentWithEmptyString(t *testing.T) {
	move := &MoveNode{}
	move.SetComment("Existing comment")
	move.SetComment("")
	expected := ""
	if move.Comments() != expected {
		t.Fatalf("expected %v but got %v", expected, move.Comments())
	}
}

func TestAddComment(t *testing.T) {
	t.Run("AddCommentAppendsToExistingComment", func(t *testing.T) {
		move := &MoveNode{}
		move.SetComment("Initial comment. ")
		move.AddComment("Additional comment.")
		expected := "Initial comment. Additional comment."
		if move.Comments() != expected {
			t.Fatalf("expected %v but got %v", expected, move.Comments())
		}
	})

	t.Run("AddCommentToEmptyComment", func(t *testing.T) {
		move := &MoveNode{}
		move.AddComment("First comment.")
		expected := "First comment."
		if move.Comments() != expected {
			t.Fatalf("expected %v but got %v", expected, move.Comments())
		}
	})
}

func TestAddCommentToStructuredComments(t *testing.T) {
	t.Run("AddsFirstTextBlock", func(t *testing.T) {
		move := &MoveNode{}
		move.AddComment("First comment.")

		blocks := move.CommentBlocks()
		if len(blocks) != 1 || len(blocks[0].Items) != 1 {
			t.Fatalf("expected one text block, got %#v", blocks)
		}
		assertCommentItem(t, blocks[0].Items[0], CommentText, "First comment.", "", "")
	})

	t.Run("AppendsToExistingTextItem", func(t *testing.T) {
		move := &MoveNode{}
		move.addCommentBlock(CommentBlock{Items: []CommentItem{{Kind: CommentText, Text: "First "}}})
		move.AddComment("second")

		blocks := move.CommentBlocks()
		assertCommentItem(t, blocks[0].Items[0], CommentText, "First second", "", "")
		if move.Comments() != "First second" {
			t.Fatalf("expected flattened comment to sync, got %q", move.Comments())
		}
	})

	t.Run("AppendsTextAfterCommandOnlyBlock", func(t *testing.T) {
		move := &MoveNode{}
		move.addCommentBlock(CommentBlock{Items: []CommentItem{{Kind: CommentCommand, Key: "clk", Value: "0:05:00"}}})
		move.AddComment("after command")

		blocks := move.CommentBlocks()
		if len(blocks[0].Items) != 2 {
			t.Fatalf("expected command and text item, got %#v", blocks)
		}
		assertCommentItem(t, blocks[0].Items[1], CommentText, "after command", "", "")
	})
}

func TestNAGReturnsCorrectValue(t *testing.T) {
	t.Run("NAGReturnsCorrectValue", func(t *testing.T) {
		move := &MoveNode{nag: "!!"}
		expected := "!!"
		if move.NAG() != expected {
			t.Fatalf("expected %v but got %v", expected, move.NAG())
		}
	})
}

func TestSetNAGUpdatesNAG(t *testing.T) {
	t.Run("SetNAGUpdatesNAG", func(t *testing.T) {
		move := &MoveNode{}
		move.SetNAG("??")
		expected := "??"
		if move.NAG() != expected {
			t.Fatalf("expected %v but got %v", expected, move.NAG())
		}
	})
}

func TestGetCommand(t *testing.T) {
	t.Run("GetCommandReturnsValueIfExists", func(t *testing.T) {
		move := &MoveNode{}
		move.SetCommand("key", "value")
		value, ok := move.GetCommand("key")
		if !ok || value != "value" {
			t.Fatalf("expected value to be 'value' and ok to be true, but got value: %v, ok: %v", value, ok)
		}
	})

	t.Run("GetCommandReturnsFalseIfKeyDoesNotExist", func(t *testing.T) {
		move := &MoveNode{}
		move.SetCommand("key", "value")
		_, ok := move.GetCommand("nonexistent")
		if ok {
			t.Fatalf("expected ok to be false, but got true")
		}
	})

	t.Run("GetCommandDoesNotCreateAnnotations", func(t *testing.T) {
		move := &MoveNode{}
		_, ok := move.GetCommand("key")
		if ok {
			t.Fatalf("expected ok to be false, but got true")
		}
		if move.hasAnnotations() {
			t.Fatalf("expected missing command lookup not to create annotations")
		}
	})
}

func TestStructuredCommentCommandBranches(t *testing.T) {
	t.Run("SetCommandAddsFirstStructuredBlock", func(t *testing.T) {
		move := &MoveNode{}
		move.SetCommand("eval", "0.25")

		blocks := move.CommentBlocks()
		if len(blocks) != 1 || len(blocks[0].Items) != 1 {
			t.Fatalf("expected one command block, got %#v", blocks)
		}
		assertCommentItem(t, blocks[0].Items[0], CommentCommand, "", "eval", "0.25")
	})

	t.Run("EmptyCommentBlockIsIgnored", func(t *testing.T) {
		move := &MoveNode{}
		move.addCommentBlock(CommentBlock{})
		if move.hasAnnotations() {
			t.Fatalf("empty comment block should not add annotations")
		}
	})
}

func TestPlyNilAndMissingPosition(t *testing.T) {
	var nilMove *MoveNode
	if nilMove.Ply() != 0 {
		t.Fatalf("nil move should have ply 0")
	}
	if (&MoveNode{}).Ply() != 0 {
		t.Fatalf("move without position should have ply 0")
	}
}

func BenchmarkValidMoves(b *testing.B) {
	pos := unsafeFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	b.ResetTimer()
	for n := 0; n < b.N; n++ {
		pos.ValidMoves()
		pos.validMoves = nil
	}
}

func moveIsValid(pos *Position, m *Move, useTags bool) bool {
	for _, move := range pos.ValidMoves() {
		if move.s1 == m.s1 && move.s2 == m.s2 && move.promo == m.promo {
			if useTags {
				if m.tags != move.tags {
					return false
				}
			}
			return true
		}
	}
	return false
}

func assertMoveNodesAreEqual(t *testing.T, m1, m2 *MoveNode) {
	if m1.parent != m2.parent {
		t.Fatalf("cloned mv %v parent is not the same", m1)
	}
	if m1.position.String() != m2.position.String() {
		t.Fatalf("cloned mv %v position is not the same", m1)
	}
	if m1.nag != m2.nag {
		t.Fatalf("cloned mv %v nag is not the same", m1)
	}
	if m1.Comments() != m2.Comments() {
		t.Fatalf("cloned mv %v comments is not the same", m1)
	}
	if m1.number != m2.number {
		t.Fatalf("cloned mv %v number is not the same", m1)
	}
	if m1.move.tags != m2.move.tags {
		t.Fatalf("cloned mv %v tags is not the same", m1)
	}
	if m1.move.s1 != m2.move.s1 {
		t.Fatalf("cloned mv %v s1 is not the same", m1)
	}
	if m1.move.s2 != m2.move.s2 {
		t.Fatalf("cloned mv %v s2 is not the same", m1)
	}
	if m1.move.promo != m2.move.promo {
		t.Fatalf("cloned mv %v s2 is not the same", m1)
	}

	if len(m1.commentBlocks) != len(m2.commentBlocks) {
		t.Fatalf("cloned mv %v len(commentBlocks) is not the same", m1)
	}
	if len(m1.children) != len(m2.children) {
		t.Fatalf("cloned mv %v len(command) is not the same", m1)
	} else {
		for idx, c1 := range m1.children {
			c2 := m2.children[idx]
			assertMoveNodesAreEqual(t, c1, c2)
		}
	}
}

func TestMoveNodeClone(t *testing.T) {
	for _, mt := range validMoves {
		node := &MoveNode{move: *mt.m, position: mt.pos, number: 1, nag: "!"}
		clonedM1 := node.clone()
		assertMoveNodesAreEqual(t, node, clonedM1)
		clonedM1.SetCommand("foo", "bar")
		clonedM2 := clonedM1.clone()
		assertMoveNodesAreEqual(t, clonedM1, clonedM2)
		clonedM1.SetCommand("foo", "bar modified")
		fooVal, ok := clonedM2.GetCommand("foo")
		if !ok || fooVal != "bar" {
			t.Fatalf("cloned mv %v is not a deep copy", clonedM2)
		}
	}
}
