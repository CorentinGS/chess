package uci

import (
	"bufio"
	"fmt"
	"io"
	"os/exec"
)

type Adapter interface {
	Exchange(cmd Cmd) ([]string, error)
	Close() error
}

type SubprocessAdapter struct {
	cmd     *exec.Cmd
	writer  *io.PipeWriter
	reader  *io.PipeReader
	scanner *bufio.Scanner
}

func NewSubprocessAdapter(path string) (*SubprocessAdapter, error) {
	path, err := exec.LookPath(path)
	if err != nil {
		return nil, fmt.Errorf("uci: executable not found at path %s %w", path, err)
	}
	rIn, wIn := io.Pipe()
	rOut, wOut := io.Pipe()
	cmd := exec.Command(path)
	cmd.Stdin = rIn
	cmd.Stdout = wOut
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("uci: failed to start executable %s: %w", path, err)
	}
	go cmd.Wait()
	return &SubprocessAdapter{
		cmd:     cmd,
		writer:  wIn,
		reader:  rOut,
		scanner: bufio.NewScanner(rOut),
	}, nil
}

func (s *SubprocessAdapter) Exchange(cmd Cmd) ([]string, error) {
	if _, err := fmt.Fprintln(s.writer, cmd.String()); err != nil {
		return nil, err
	}
	if cmd.IsDone("") {
		return nil, nil
	}
	var lines []string
	for s.scanner.Scan() {
		line := s.scanner.Text()
		lines = append(lines, line)
		if cmd.IsDone(line) {
			break
		}
	}
	if err := s.scanner.Err(); err != nil {
		return lines, err
	}
	return lines, nil
}

func (s *SubprocessAdapter) Close() error {
	if err := s.writer.Close(); err != nil {
		return err
	}
	if err := s.reader.Close(); err != nil {
		return err
	}
	return s.cmd.Process.Kill()
}

func (s *SubprocessAdapter) Pid() int {
	return s.cmd.Process.Pid
}
