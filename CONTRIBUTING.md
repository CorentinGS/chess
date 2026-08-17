# Contributing

## Development

```bash
git clone https://github.com/corentings/chess/v3
cd chess
go test ./...
```

## Linting

Run `golangci-lint` before submitting changes:

```bash
golangci-lint run
```

## Benchmarks

Benchmarks live alongside the code in `_test.go` files. Run them with:

```bash
go test -bench=.
```

### Comparative benchmarking with benchstat

To compare performance between two versions (e.g., before and after a change),
use [`benchstat`](https://pkg.go.dev/golang.org/x/perf/cmd/benchstat):

```bash
# Install benchstat (once)
go install golang.org/x/perf/cmd/benchstat@latest

# Capture baseline on the reference commit
git stash
git checkout <base-commit>
go test -bench=. -count=10 > base.txt
git checkout -
git stash pop

# Capture new measurements on your branch
go test -bench=. -count=10 > new.txt

# Compare
benchstat base.txt new.txt
```

`-count=10` runs each benchmark ten times so benchstat can compute statistical
significance. For noisy environments, increase the count.

### Perft regression testing

The `perft_test.go` file contains perft (performance test) cases that verify
move-generation correctness against known node counts. These run as part of
`go test ./...` and should always pass.

The `benchcmp/` directory contains tooling for comparing perft performance
against a C++ reference implementation. See `benchcmp/cpp/pgn_bench.cpp` and
`benchcmp/perft/bench_test.go`.
