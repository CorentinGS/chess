package chess_test

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestPGNDecoderPreservesEscapedQuoteInTagValue(t *testing.T) {
	const escapedQuoteGame = `[Event "Internet Section 08A g/8'+2\""]
[Site "Dos Hermanas"]
[Date "2004.03.08"]
[Round "6"]
[White "Di Berardino, Diego Rafael"]
[Black "Mar, Fernando"]
[Result "1/2-1/2"]
[ECO "B96"]

1. e4 c5 2. Nf3 1/2-1/2
`
	dec := chess.NewPGNDecoder(strings.NewReader(escapedQuoteGame))
	game, err := dec.Decode()
	if err != nil {
		t.Fatalf("parser rejects spec-legal escaped quote in tag value: %v", err)
	}
	if game.GetTagPair("Event") != `Internet Section 08A g/8'+2"` {
		t.Fatalf("expected unescaped quote in tag value, got %q", game.GetTagPair("Event"))
	}
	pgn := game.String()
	if !strings.Contains(pgn, `[Event "Internet Section 08A g/8'+2\""]`) {
		t.Fatalf("round-trip PGN does not contain escaped quote: %s", pgn)
	}
}

func TestPGNImportAcceptsPromotionWithoutEquals(t *testing.T) {
	const pgn = `[Event "Promotion import"]
[SetUp "1"]
[FEN "6k1/4P3/8/8/8/8/8/4K3 w - - 0 1"]
[Result "*"]

1. e8Q *
`

	game, err := chess.ParsePGN(strings.NewReader(pgn))
	if err != nil {
		t.Fatalf("ParsePGN rejected import promotion without equals: %v", err)
	}
	if got := game.String(); !strings.Contains(got, "1. e8=Q") {
		t.Fatalf("generated PGN should canonicalise promotion with equals, got:\n%s", got)
	}
}

func TestPGNSANPolicyOptionsLastOneWins(t *testing.T) {
	const pgn = `[Event "Promotion import"]
[SetUp "1"]
[FEN "6k1/4P3/8/8/8/8/8/4K3 w - - 0 1"]
[Result "*"]

1. e8Q *
`

	if _, err := chess.ParsePGN(strings.NewReader(pgn), chess.WithStrictSAN()); err == nil {
		t.Fatalf("ParsePGN WithStrictSAN accepted import-only promotion spelling")
	}
	if _, err := chess.ParsePGN(strings.NewReader(pgn), chess.WithStrictSAN(), chess.WithImportSAN()); err != nil {
		t.Fatalf("ParsePGN last import option error = %v", err)
	}
	if _, err := chess.ParsePGN(strings.NewReader(pgn), chess.WithImportSAN(), chess.WithStrictSAN()); err == nil {
		t.Fatalf("ParsePGN last strict option accepted import-only promotion spelling")
	}
}

func TestPGNDecoderGameBoundaries(t *testing.T) {
	tests := []struct {
		name        string
		input       string
		wantGames   int
		wantOutcome chess.Outcome
	}{
		{"empty", "", 0, chess.NoOutcome},
		{"whitespace_only", "   \n\t  ", 0, chess.NoOutcome},
		{"result_only", "1-0\n", 1, chess.WhiteWon},
		{"star_result_only_not_detected", "*\n", 0, chess.NoOutcome},
		{"tagless_game", "1. e4 e5 1-0\n", 1, chess.WhiteWon},
		{"tags_only", "[Event \"x\"]\n[Site \"y\"]\n\n", 1, chess.NoOutcome},
		{"crlf_no_tag_separator", "1. e4 e5 1-0\r\n1. d4 d5 0-1\r\n", 1, chess.WhiteWon},
		{"bom_prefix", "\xEF\xBB\xBF1. e4 e5 1-0\n", 0, chess.NoOutcome},
		{"no_blank_line_separator", "1. e4 e5 1-0\n[Event \"z\"]\n\n1. d4 d5 0-1\n", 1, chess.BlackWon},
		{"trailing_garbage", "1. e4 e5 1-0\nGARBAGE\n", 1, chess.WhiteWon},
		{"result_prefix_no_false_positive", "10-0\n", 1, chess.NoOutcome},
		{"result_embedded_in_movetext", "1-0e5\n", 1, chess.NoOutcome},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dec := chess.NewPGNDecoder(strings.NewReader(tt.input))
			count := 0
			var lastOutcome chess.Outcome
			for {
				g, err := dec.Decode()
				if errors.Is(err, io.EOF) {
					break
				}
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				lastOutcome = g.Outcome()
				count++
			}
			if count != tt.wantGames {
				t.Errorf("game count = %d, want %d", count, tt.wantGames)
			}
			if tt.wantGames > 0 && lastOutcome != tt.wantOutcome {
				t.Errorf("outcome = %s, want %s", lastOutcome, tt.wantOutcome)
			}
		})
	}
}

func TestPGNDecoderRoundTrip(t *testing.T) {
	pgn := mustReadFile("fixtures/pgns/variations.pgn")
	dec := chess.NewPGNDecoder(strings.NewReader(pgn))
	for {
		g, err := dec.Decode()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			t.Fatalf("parse error: %v", err)
		}
		rendered := g.String()
		reparsed := mustParseSingleGame(t, rendered)
		if reparsed.String() != rendered {
			t.Errorf("round-trip mismatch:\nfirst:\n%s\nsecond:\n%s", rendered, reparsed.String())
		}
	}
}

func TestPGNDecoderVariationExpansionFixtures(t *testing.T) {
	tests := []struct {
		name             string
		fixture          string
		expandVariations bool
		lastLines        []string
		finalPos         []string
	}{
		{"variations_expand", "fixtures/pgns/variations.pgn", true,
			[]string{
				"1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Nxd4 *",
				"1. e4 e5 2. Nc3 Nf6 3. f4 *",
				"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 dxe5 5. Qxd8+ Kxd8 *",
				"1. e4 d6 2. d4 Nf6 3. Nc3 e5 4. Nf3 Nbd7 *",
				"1. e3 e5 *",
			},
			[]string{
				"r1bqkbnr/pppp1ppp/2n5/8/3NP3/8/PPP2PPP/RNBQKB1R b KQkq - 0 4",
				"rnbqkb1r/pppp1ppp/5n2/4p3/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 0 3",
				"rnbk1b1r/ppp2ppp/5n2/4p3/4P3/2N5/PPP2PPP/R1B1KBNR w KQ - 0 6",
				"r1bqkb1r/pppn1ppp/3p1n2/4p3/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 2 5",
				"rnbqkbnr/pppp1ppp/8/4p3/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
			}},
		{"variations_no_expand", "fixtures/pgns/variations.pgn", false,
			[]string{
				"1. e4 (1. e3 e5) 1... e5 (1... d6 2. d4 Nf6 3. Nc3 e5 4. dxe5 (4. Nf3 Nbd7) 4... dxe5 5. Qxd8+ Kxd8) 2. Nf3 (2. Nc3 Nf6 3. f4) 2... Nc6 3. d4 exd4 4. Nxd4 1-0",
			},
			[]string{
				"r1bqkbnr/pppp1ppp/2n5/8/3NP3/8/PPP2PPP/RNBQKB1R b KQkq - 0 4",
			}},
		{"multi_frompos_no_expand", "fixtures/pgns/multi_frompos_games.pgn", false,
			[]string{
				"1. d4 d5 2. c4 c6 { [%eval 0.21]} *",
				"3. Nf3 (3. Nc3 Nf6 4. Nf3) 3... Nf6 4. Nc3 { [%eval 0.16]} *",
				"4... a6 { [%eval 0.19]} *",
				"5. cxd5 (5. e3 e6 6. cxd5 cxd5) 5... cxd5 6. e3 e6 { [%eval 0.11]} *",
			},
			[]string{
				"rnbqkbnr/pp2pppp/2p5/3p4/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3",
				"rnbqkb1r/pp2pppp/2p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R b KQkq - 3 4",
				"rnbqkb1r/1p2pppp/p1p2n2/3p4/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5",
				"rnbqkb1r/1p3ppp/p3pn2/3p4/3P4/2N1PN2/PP3PPP/R1BQKB1R w KQkq - 0 7",
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pgn := mustReadFile(tt.fixture)
			var dec *chess.PGNDecoder
			if tt.expandVariations {
				dec = chess.NewPGNDecoder(strings.NewReader(pgn), chess.WithPGNExpandVariations())
			} else {
				dec = chess.NewPGNDecoder(strings.NewReader(pgn))
			}
			count := 0
			for {
				game, err := dec.Decode()
				if errors.Is(err, io.EOF) {
					break
				}
				if err != nil {
					t.Fatalf("fail to parse game %d: %s", count+1, err.Error())
				}
				if game == nil {
					t.Fatalf("game is nil")
				}
				if count >= len(tt.lastLines) {
					t.Fatalf("expected %d games but found at least %d", len(tt.lastLines), count+1)
				}
				lines := strings.Split(game.String(), "\n")
				lastLine := lines[len(lines)-1]
				if lastLine != tt.lastLines[count] {
					t.Errorf("game output not correct\n\tExpected:'%v'\n\tGot:     '%v'\n", tt.lastLines[count], lastLine)
				}
				fen := game.Position().XFENString()
				if fen != tt.finalPos[count] {
					t.Errorf("game position not correct\n\tExpected:'%v'\n\tGot:     '%v'\n", tt.finalPos[count], fen)
				}
				count++
			}
			if count != len(tt.lastLines) {
				t.Fatalf("expected %d games but found only %d", len(tt.lastLines), count)
			}
		})
	}
}

func TestPGNGamesReadsMultiGameFixture(t *testing.T) {
	pgn := mustReadFile("fixtures/pgns/multi_game.pgn")
	var games int
	for _, err := range chess.PGNGames(strings.NewReader(pgn)) {
		if err != nil {
			t.Fatalf("unexpected error on game %d: %v", games+1, err)
		}
		games++
	}
	if games != 4 {
		t.Errorf("expected 4 games, got %d", games)
	}
}
