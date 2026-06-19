---
title: Reduce Allocations in Opening Possible Lookup
status: ready-for-agent
labels:
  - enhancement
  - opening
  - performance
  - v3
---

## Problem Statement

`opening.BookECO.Possible` still allocates on the opening lookup hot path. After the ADR-005 opening refactor, `Find` is zero-allocation, but `Possible` remains at about **1464 B/op** and **10 allocs/op**.

Current benchmark from `go test ./opening -bench=. -benchmem` on an AMD Ryzen 9 5950X:

```text
BenchmarkFind-32        56.94 ns/op    0 B/op       0 allocs/op
BenchmarkPossible-32    2413 ns/op     1464 B/op    10 allocs/op
```

## Context

The opening trie now uses compact `uint32` move keys, so `Find` no longer pays for `move.String()` allocations. `Possible` still builds a result slice through `nodeList` / `collectNodes`, which likely allocates an intermediate `[]*node` before producing `[]*Opening`.

Relevant files:

- `opening/eco.go`
- `opening/opening_benchmark_test.go`

## Goal

Reduce allocations in `BookECO.Possible` while preserving the current public API:

```go
Possible(moves []*chess.Move) []*Opening
```

Ideal target:

- Avoid intermediate `[]*node` allocation.
- Reduce `Possible` allocations substantially, ideally to one allocation for the returned `[]*Opening` or zero where no openings are possible.
- Keep behavior unchanged.

## Suggested Direction

Traverse the trie and append openings directly to the result slice instead of first collecting nodes. Consider replacing `nodeList` / `collectNodes` with a helper that accepts `*[]*Opening` and appends only non-nil openings.

## Acceptance Criteria

- Keep benchmark coverage for `BenchmarkPossible`.
- `go test ./...` passes.
- `go test ./opening -bench=BenchmarkPossible -benchmem` shows a lower allocation count than the current `10 allocs/op`.
