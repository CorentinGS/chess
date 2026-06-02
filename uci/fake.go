package uci

import "strings"

type FakeAdapter struct {
	Responses map[string][]string
}

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

func (f *FakeAdapter) Close() error {
	return nil
}
