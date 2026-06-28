# Perft Benchmark — corentings/chess v3 vs dylhunn/dragontoothmg

Machine: AMD Ryzen 9 5950X, 16 cores. Go 1.26.4, single-threaded bench,
`-benchtime=1s` per row. Correctness gated by `TestCorrectness` against the
six canonical positions from
[chessprogramming.org/Perft_Results](https://www.chessprogramming.org/Perft_Results).

`dragontoothmg` is included here as a permissive Go reference implementation
(GPL v3). It is kept in this separate Go module so the main module's license
is unaffected.

## Headline numbers (depth 5, startpos)

| Library | ns/op | MNPS | B/op | allocs/op | allocs/node |
|---|---:|---:|---:|---:|---:|
| `corentings/chess/v3` baseline | 636,120,324 | 7.65 | 121,134,864 | 1,032,983 | 0.21 |
| `corentings/chess/v3` current | 111,951,657 | 43.46 | 821,824 | 206,461 | 0.04 |
| `dylhunn/dragontoothmg` current | 59,075,698 | 82.36 | 59,917,398 | 619,852 | 0.13 |

After issue 027 (`Position.Perft`/`Divide` using the internal zero-copy move
cache), a focused one-iteration guard benchmark on the same machine reports:

| Slice | ns/op | MNPS | B/op | allocs/op | allocs/node |
|---|---:|---:|---:|---:|---:|
| startpos d=5 after issue 027 | 603,013,439 | 8.07 | 89,142,384 | 826,363 | 0.17 |
| startpos d=5 after issue 028 line-mask indexing | 552,698,010 | 8.80 | 89,138,192 | 826,360 | 0.17 |
| startpos d=5 after issue 029 legal-only Perft generation | 389,207,131 | 12.50 | 89,142,656 | 826,365 | 0.17 |
| startpos d=5 after issue 032 make/unmake Perft | 355,226,189 | 13.70 | 32,825,352 | 413,098 | 0.08 |
| startpos d=5 after issue 033 incremental occupancy | 358,300,224 | 13.58 | 32,833,568 | 413,105 | 0.08 |
| after issue 028 byte-index table | 280,334,237 | 17.36 | 32,816,293 | 413,096 | 0.08 |
| after visitor Perft traversal | 268,702,820 | 18.11 | 821,357 | 206,460 | 0.04 |
| after issue 028 word-index table | 234,289,620 | 20.77 | 821,357 | 206,460 | 0.04 |
| after issue 028 rank-specialized index | 229,973,674 | 21.16 | 821,357 | 206,460 | 0.04 |
| after attack-vector reuse | 217,674,433 | 22.35 | 821,357 | 206,460 | 0.04 |
| final guard run | 220,394,334 | 22.08 | 821,362 | 206,460 | 0.04 |
| after unaligned-move legality shortcut | 199,090,263 | 24.44 | 821,362 | 206,460 | 0.04 |
| after aligned-mask legality shortcut | 190,138,795 | 25.59 | 821,368 | 206,460 | 0.04 |
| after direct aligned-slider legality check | 147,566,878 | 32.97 | 821,474 | 206,462 | 0.04 |
| after file-specialized sliding index | 144,529,717 | 33.67 | 821,362 | 206,460 | 0.04 |
| final verified guard run | 147,504,263 | 32.99 | 821,362 | 206,460 | 0.04 |
| after directional slider exposure check | 133,375,224 | 36.48 | 821,400 | 206,460 | 0.04 |
| after duplicate alignment-check cleanup | 119,458,447 | 40.73 | 822,528 | 206,461 | 0.04 |
| after lazy pin/check context and perft hash elision | 111,951,657 | 43.46 | 821,824 | 206,461 | 0.04 |

Command:

```sh
cd benchcmp/perft
GOCACHE=/tmp/chess-go-build go test -bench '^BenchmarkChessPerft/startpos/d=5' -benchtime=1x -run '^$' -benchmem
```

The original startpos d=5 target was at least 40 MNPS. The current code reaches
43.46 MNPS on the focused guard run, so the target is met. The current gap to
`dragontoothmg` on the same row is about 2x.

## Per-position, per-depth wall-clock (ns/op)

| Position | Depth | Nodes | `chess` (ns/op) | `dragontoothmg` (ns/op) | chess/dt |
|---|---|---|---|---|---|
| startpos | 1 | 20 | 32,096 | 1,971 | 16× |
| startpos | 2 | 400 | 81,154 | 7,450 | 11× |
| startpos | 3 | 8,902 | 1,191,259 | 113,666 | 10× |
| startpos | 4 | 197,281 | 26,844,883 | 2,473,436 | 11× |
| startpos | 5 | 4,865,609 | 636,120,324 | 56,720,283 | 11× |
| startpos | 6 | 119,060,324 | 16,157,935,684 | 1,386,952,665 | 12× |
| kiwipete | 4 | 4,085,603 | 611,410,684 | 36,168,024 | 17× |
| kiwipete | 5 | 193,690,690 | 27,842,856,599 | 1,522,140,459 | 18× |
| pos3 | 5 | 674,624 | 173,293,956 | 12,305,646 | 14× |
| pos3 | 6 | 11,030,083 | 2,987,982,350 | 201,530,033 | 15× |
| pos4 | 5 | 15,833,292 | 2,850,106,770 | 142,987,332 | 20× |
| pos5 | 5 | 15,833,292 | 2,924,871,778 | 143,035,894 | 20× |
| pos6 | 4 | 2,103,487 | 437,208,898 | 19,878,674 | 22× |
| pos6 | 5 | 89,941,194 | 19,489,969,306 | 766,675,268 | 25× |

Range of slowdown: **11×–25×**, median ~17×.

## Hotspots (startpos d=5, `go tool pprof -top`)

### `corentings/chess` v3 baseline

```
      flat  flat%        cum   cum%
     1.34s 49.81%      1.34s 49.81%  chess.lineIndex (inline)
     0.14s  5.20%      1.95s 72.49%  chess.moveTags
     0.12s  4.46%      1.58s 58.74%  chess.isSquareAttackedBy
     0.11s  4.09%      0.96s 35.69%  chess.hvAttack
     0.10s  3.72%      2.31s 85.87%  chess.visitStandardMoves
     0.06s  2.23%      0.55s 20.45%  chess.diaAttack
     0.05s  1.86%      0.05s  1.86%  chess.(*Board).calcConvienceBBs
```

Half of CPU time is spent in `lineIndex` — the inner loop of
`diaAttack`/`hvAttack` that walks a sliding line until it hits a blocker or
the board edge. This is the textbook case for **magic bitboards** (single
multiply + shift + table lookup). The other large bucket is the per-move
legality check (`moveTags` + `isSquareAttackedBy`), which fires on every
pseudo-legal candidate. `calcConvienceBBs` is small but unnecessary work on
every ply — it can be invalidated lazily.

### `corentings/chess` v3 current

Captured profile: `benchcmp/perft/prof/chess_current_d5.prof`.

```text
      flat  flat%        cum   cum%
     0.12s 17.65%      0.68s   100%  chess.visitStandardMoves
     0.07s 10.29%      0.07s 10.29%  chess.lineIndex
     0.04s  5.88%      0.04s  5.88%  chess.pawnMoves
     0.04s  5.88%      0.04s  5.88%  chess.squareFromBit
     0.03s  4.41%      0.08s 11.76%  chess.(*Position).makeMove
     0.03s  4.41%      0.10s 14.71%  chess.diaAttack
     0.03s  4.41%      0.19s 27.94%  chess.moveTagsForPiece
     0.01s  1.47%      0.01s  1.47%  chess.pinnedRayForPiece
```

The completed slices reduced wall time and allocation pressure substantially.
`lineIndex`, `pawnMoves`, `squareFromBit`, and move enumeration are now the
largest flat costs in the profiled run. Issue 030 is handled by direct
single-check/double-check filtering and lazy pin-ray restriction for legal
generation; en passant and king moves still use the exact attack simulation
because their legality depends on changed occupancy or destination-square
attack checks.

### `dylhunn/dragontoothmg` current

Captured profile: `benchcmp/perft/prof/dragontooth_current_d5.prof`.

```
      flat  flat%        cum   cum%
      40ms  9.09%      120ms 27.27%  dragontoothmg.(*Board).Apply
      40ms  9.09%       40ms  9.09%  dragontoothmg.(*Board).countAttacks
      40ms  9.09%       50ms 11.36%  dragontoothmg.(*Board).pawnPushes
      20ms  4.55%       20ms  4.55%  dragontoothmg.CalculateBishopMoveBitboard
      20ms  4.55%       20ms  4.55%  dragontoothmg.CalculateRookMoveBitboard
      10ms  2.27%      240ms 54.55%  dragontoothmg.(*Board).GenerateLegalMoves
```

CPU is spread evenly across move generation, sliding attacks, and the
unapply closure — no single hot loop dominates. Compare to v3 where a single
`lineIndex` loop eats half the CPU.

## Memory

`v3` now allocates fewer bytes and fewer objects than `dragontoothmg` on the
startpos d=5 guard row after the make/unmake Perft path:

| Library | B/op | allocs/op | allocs/node |
|---|---:|---:|---:|
| `corentings/chess/v3` current | 821,824 | 206,461 | 0.04 |
| `dylhunn/dragontoothmg` current | 59,917,398 | 619,852 | 0.13 |

The remaining performance gap is CPU-bound, not allocation-bound.

## Reproducing

```
cd benchcmp/perft
go test -run TestCorrectness -v                 # correctness gate (~80s at full depth)
go test -bench=. -benchtime=1s -run=^$ -benchmem # benchmarks (~160s)
go test -bench '^BenchmarkChessPerft/startpos/d=5' -benchtime=1x -run '^$' -benchmem -cpuprofile prof/chess_current_d5.prof
go test -bench '^BenchmarkDragontoothPerft/startpos/d=5' -benchtime=1x -run '^$' -benchmem -cpuprofile prof/dragontooth_current_d5.prof
go tool pprof -top prof/chess_current_d5.prof
go tool pprof -top prof/dragontooth_current_d5.prof
```

Current CPU profiles are committed at `prof/chess_current_d5.prof` and
`prof/dragontooth_current_d5.prof` (startpos d=5).

## Where to next

The remaining gap is CPU-bound and currently explained by two hot areas:

1. **Replace the remaining sliding-attack indexes with magic bitboards.** The
   current 16-bit occupancy lookup plus rank/file specialization removed most
   of the original line-index cost, but sliding indexes remain the largest flat
   hotspots. Magic bitboards would reduce each remaining sliding lookup to one
   multiply, one shift, and one table access.

2. **Specialize the remaining legal-only king/en-passant checks.** Lazy
   pin/check context removes most ordinary temporary-board legality checks.
   King moves and en passant still need attack simulation; direct destination
   attack checks and a dedicated en-passant discovered-check test could trim
   more of `moveTagsForPiece` without changing public annotations.

Completed work so far: legal-only Perft generation, visitor-based Perft
traversal, in-place Perft make/unmake, incremental aggregate occupancy in
`Board.update`, and `Status()` caching.

See the follow-up issue for the concrete refactor plan.
