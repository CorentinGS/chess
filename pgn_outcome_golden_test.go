package chess_test

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/corentings/chess/v3"
)

// TestPGNOutcomeGolden is an end-to-end regression for the unified outcome
// policy. Each .pgn fixture in testdata/outcomes is parsed, then its PGN-level
// Outcome/Method and every Split game's Outcome/Method/Result-tag are rendered
// to a stable text form and compared against a sibling .golden file.
//
// Regenerate the golden files with:
//
//	UPDATE_GOLDEN=1 go test -run TestPGNOutcomeGolden
func TestPGNOutcomeGolden(t *testing.T) {
	files, err := filepath.Glob("testdata/outcomes/*.pgn")
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no .pgn fixtures found in testdata/outcomes")
	}

	for _, pgnPath := range files {
		pgnPath := pgnPath
		name := strings.TrimSuffix(filepath.Base(pgnPath), ".pgn")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(pgnPath)
			if err != nil {
				t.Fatal(err)
			}
			game, err := chess.NewScanner(strings.NewReader(string(data))).ParseNext()
			if err != nil {
				t.Fatalf("parse %s: %v", pgnPath, err)
			}

			var b strings.Builder
			fmt.Fprintf(&b, "pgn:  outcome=%s method=%s\n", game.Outcome(), game.Method())
			for i, sg := range game.Split() {
				result := sg.GetTagPair("Result")
				if result == "" {
					result = "none"
				}
				fmt.Fprintf(&b, "split[%d]: outcome=%s method=%s result=%s\n", i, sg.Outcome(), sg.Method(), result)
			}

			got := b.String()
			goldenPath := strings.TrimSuffix(pgnPath, ".pgn") + ".golden"
			if os.Getenv("UPDATE_GOLDEN") == "1" {
				if err := os.WriteFile(goldenPath, []byte(got), 0o644); err != nil {
					t.Fatal(err)
				}
				t.Logf("wrote %s", goldenPath)
				return
			}
			want, err := os.ReadFile(goldenPath)
			if err != nil {
				t.Fatalf("missing or unreadable golden file %s (run UPDATE_GOLDEN=1 go test -run TestPGNOutcomeGolden to create it): %v", goldenPath, err)
			}
			if got != string(want) {
				t.Errorf("golden mismatch for %s\n--- want ---\n%s\n--- got ---\n%s", goldenPath, want, got)
			}
		})
	}
}
