package chess_test

import (
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGN_ParsesDisambiguationGame(t *testing.T) {
	tests := []struct {
		name       string
		pgn        string
		shouldFail bool
	}{
		{
			name:       "multiple_pieces_same_type_complex",
			pgn:        `1. h4 d6 2. g4 Kd7 3. f4 Kc6 4. f5 Kd7 5. g5 Ke8 6. h5 Kd7 7. h6 Kc6 8. g6 Kb6 9. f6 Kc6 10. hxg7 Kd7 11. gxf7 Kc6 12. fxe7 Kb6 13. gxf8=Q Kc6 14. fxg8=Q Kb6 15. e8=Q Ka6 16. Qfe7 Kb6 17. Qe8f7`,
			shouldFail: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			scanner := chess.NewScanner(strings.NewReader(tc.pgn))
			game, err := scanner.ParseNext()
			if err != nil {
				if !tc.shouldFail {
					t.Errorf("%s: expected PGN parsing to succeed but got error: %v", tc.name, err)
				}
				return
			}
			if tc.shouldFail {
				t.Errorf("%s: expected test case to fail but it succeeded", tc.name)
			}
			if game == nil {
				t.Errorf("%s: game should not be nil for successful parsing", tc.name)
			}
		})
	}
}

func TestLexer_FullSquareDisambiguation(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected []chess.Token
	}{
		{"queen_full_square", "Qe8f7", []chess.Token{
			{Type: chess.PIECE, Value: "Q"},
			{Type: chess.DeambiguationSquare, Value: "e8"},
			{Type: chess.SQUARE, Value: "f7"},
		}},
		{"rook_full_square", "Ra1d1", []chess.Token{
			{Type: chess.PIECE, Value: "R"},
			{Type: chess.DeambiguationSquare, Value: "a1"},
			{Type: chess.SQUARE, Value: "d1"},
		}},
		{"knight_full_square", "Nb1c3", []chess.Token{
			{Type: chess.PIECE, Value: "N"},
			{Type: chess.DeambiguationSquare, Value: "b1"},
			{Type: chess.SQUARE, Value: "c3"},
		}},
		{"queen_full_square_capture", "Qg8xg7", []chess.Token{
			{Type: chess.PIECE, Value: "Q"},
			{Type: chess.DeambiguationSquare, Value: "g8"},
			{Type: chess.CAPTURE, Value: "x"},
			{Type: chess.SQUARE, Value: "g7"},
		}},
		{"standard_piece_move", "Nf3", []chess.Token{
			{Type: chess.PIECE, Value: "N"},
			{Type: chess.SQUARE, Value: "f3"},
		}},
		{"file_disambiguation", "Nbd2", []chess.Token{
			{Type: chess.PIECE, Value: "N"},
			{Type: chess.FILE, Value: "b"},
			{Type: chess.SQUARE, Value: "d2"},
		}},
		{"rank_disambiguation", "R1d2", []chess.Token{
			{Type: chess.PIECE, Value: "R"},
			{Type: chess.RANK, Value: "1"},
			{Type: chess.SQUARE, Value: "d2"},
		}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lexer := chess.NewLexer(tc.input)
			for i, expected := range tc.expected {
				token := lexer.NextToken()
				if token.Type != expected.Type || token.Value != expected.Value {
					t.Errorf("Token %d: expected {%v, %q}, got {%v, %q}",
						i, expected.Type, expected.Value, token.Type, token.Value)
				}
			}
			if tok := lexer.NextToken(); tok.Type != chess.EOF {
				t.Errorf("expected EOF after tokens, got {%v, %q}", tok.Type, tok.Value)
			}
		})
	}
}

func TestPGN_ParsesFullSquareCapture(t *testing.T) {
	pgn := `[Event "rated blitz game"]
[Site "https://lichess.org/FaQvH6Iq"]
[Result "1-0"]

1. c4 Nf6 2. Nc3 Ng8 3. e4 Nf6 4. e5 Ng8 5. d4 e6 6. f4 Ne7 7. Nf3 Ng8 8. d5 Ne7 9. Be3 Nf5 10. Bf2 Ne7 11. Bd3 Ng8 12. O-O Nh6 13. h3 Ng8 14. g4 h5 15. g5 d6 16. Qc2 Qf6 17. exf6 Nc6 18. fxg7 Bd7 19. gxh8=Q O-O-O 20. g6 Bg7 21. gxf7 Kb8 22. fxg8=Q Ka8 23. f5 Rf8 24. f6 Be8 25. f7 Nd8 26. fxe8=Q Kb8 27. Qhxh5 Ka8 28. Qg4 Kb8 29. h4 Ka8 30. h5 Kb8 31. h6 Ka8 32. h7 Kb8 33. h8=Q Ka8 34. dxe6 Kb8 35. e7 Ka8 36. exf8=Q Kb8 37. Qg8xg7 Ka8 38. Qgg8 Kb8 39. Q4g7 Ka8 40. c5 Kb8 41. cxd6 Ka8 42. dxc7 b6 43. cxd8=Q# 1-0`

	scanner := chess.NewScanner(strings.NewReader(pgn))
	game, err := scanner.ParseNext()
	if err != nil {
		t.Fatalf("expected PGN with full-square disambiguation capture to parse: %v", err)
	}
	if game == nil {
		t.Fatal("expected game to be parsed")
	}

	moves := game.Moves()
	if len(moves) < 73 {
		t.Fatalf("expected at least 73 moves, got %d", len(moves))
	}

	move37 := moves[72]
	if move37.S1() != chess.G8 || move37.S2() != chess.G7 {
		t.Errorf("expected move 37 to be g8g7, got %s%s", move37.S1(), move37.S2())
	}
	if !move37.HasTag(chess.Capture) {
		t.Errorf("expected move 37 to have Capture tag")
	}
}

func TestAlgebraicNotation_EncodeDisambiguation(t *testing.T) {
	tests := []struct {
		name     string
		fen      string
		from, to chess.Square
		wantSAN  string
	}{
		{"none", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", chess.E2, chess.E4, "e4"},
		{"file_only", "rnbqkbnr/pppp1ppp/8/4p3/3P4/5N2/PPP1PPPP/RNBQKB1R w KQkq - 0 1", chess.F3, chess.D2, "Nfd2"},
		{"rank_only", "4k3/8/8/8/R7/8/8/R3K3 w - - 0 1", chess.A4, chess.A3, "R4a3"},
		{"file_and_rank", "1k6/8/8/8/Q7/8/8/Q2QK3 w - - 0 1", chess.A1, chess.D4, "Qa1d4"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			opt, err := chess.FEN(tc.fen)
			if err != nil {
				t.Fatalf("%s: invalid FEN: %v", tc.name, err)
			}
			pos := chess.NewGame(opt).Position()
			var target chess.Move
			found := false
			for _, m := range pos.ValidMoves() {
				if m.S1() == tc.from && m.S2() == tc.to {
					target = m
					found = true
					break
				}
			}
			if !found {
				t.Skipf("%s: move %s%s unavailable in position", tc.name, tc.from, tc.to)
			}
			got := chess.AlgebraicNotation{}.Encode(pos, target)
			if got != tc.wantSAN {
				t.Errorf("%s: Encode = %q, want %q", tc.name, got, tc.wantSAN)
			}
		})
	}
}
