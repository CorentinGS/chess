package uci

import "strings"

// FakeAdapter is a test Adapter that returns canned responses keyed by the
// first word of each command string (e.g. "uci", "go", "isready").
type FakeAdapter struct {
	Responses map[string][]string
}

// Exchange returns the canned response lines for the given command, or nil if
// no response is configured for the command key.
func (f *FakeAdapter) Exchange(cmd Cmd) ([]string, error) {
	key := strings.SplitN(cmd.String(), " ", 2)[0]
	responses, ok := f.Responses[key]
	if !ok {
		return nil, nil
	}
	if cmd.IsDone("") {
		return nil, nil
	}
	var lines []string
	for _, line := range responses {
		lines = append(lines, line)
		if cmd.IsDone(line) {
			break
		}
	}
	return lines, nil
}

// Close is a no-op for FakeAdapter.
func (f *FakeAdapter) Close() error {
	return nil
}
