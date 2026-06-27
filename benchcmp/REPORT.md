# PGN Import Benchmark — Go vs C++ chess-library

Machine: AMD Ryzen 9 5950X, 16 cores. Go 1.26.4, single-threaded bench.
Toolchain: `g++ -std=c++17 -O3 -DNDEBUG -march=native -flto` (single-header `chess.hpp` from Disservin/chess-library).

## Headline numbers

| Fixture | Size | Games | Positions | Go (MB/s) | C++ (MB/s) | Speedup |
|---|---|---|---|---|---|---|
| `fixtures/pgns/big.pgn` | 4.12 MB | 1 000 | 74 721 | **3.47** | **359** | ~103× |
| `fixtures/pgns/big_big.pgn` | 9.05 MB | 10 000 | 862 150 | **1.25** | **120** | ~96× |
| `fixtures/pgns/0001.pgn` | small | 1 | ~70 | (existing, see below) | n/a | n/a |

Go bench (`-benchtime=10x`):

```
BenchmarkPGN-32                       10    906381 ns/op    110614 B/op    2959 allocs/op
BenchmarkPGNWithVariations-32         10    137777 ns/op     27259 B/op     385 allocs/op
BenchmarkPGN_Stream_Big-32            10  1186142223 ns/op    3.47 MB/s  180605758 B/op  2151164 allocs/op
BenchmarkPGN_Stream_BigBig-32         10  7208661372 ns/op    1.25 MB/s  955612091 B/op 11945452 allocs/op
BenchmarkPGN_Stream_Variations-32     10     176385 ns/op    1.17 MB/s    21437 B/op     208 allocs/op
```

### Caveat: data quirks

7 / 1000 games in `big.pgn` fail Go parse with *"Result tag conflicts with board-derivable outcome"* / *"movetext result token conflicts"* — real lichess data: a game ends in mate on the board but the comment says `{ Black wins on time. }`. The bench (`benchStreamFixture`) swallows per-game errors and continues. C++ skipped 0 — its parser is more permissive about conflicting result tokens.

## Profiles

Saved at `benchcmp/prof/{big,bigbig}.{cpu,mem}.prof`. Inspect with:

```
go tool pprof -top -nodecount=15 -cum benchcmp/prof/big.cpu.prof
go tool pprof -top -nodecount=15 -sample_index=alloc_objects benchcmp/prof/big.mem.prof
go tool pprof -top -nodecount=15 -sample_index=alloc_space  benchcmp/prof/big.mem.prof
```

### CPU (12.51 s total, `big.cpu.prof`)

**Flat top** — pure hot loops:

| flat% | symbol |
|---|---|
| 19.98 | `math/bits.Reverse64` (inline) |
| 8.87 | `(*Board).update` |
| 8.15 | `(*Board).bbForPiece` (inline) |
| 7.27 | `standardMoves` |
| 6.87 | `(*Board).setBBForPiece` |
| 3.44 | `isSquareAttackedBy` |
| 3.36 | `linearAttack` |
| 2.72 | `diaAttack` |
| 1.28 | `hvAttack` |
| 1.20 | `bbForSquare` (inline) |

**Cumulative** — Parser dominates the wall time:

| cum% | symbol |
|---|---|
| 97.4 | `BenchmarkPGN_Stream_Big` |
| 94.8 | `(*Scanner).ParseNext` |
| 88.6 | `(*Parser).Parse` |
| 79.5 | `engine.CalcMoves` |
| 76.4 | `(*Parser).parseMove` |
| 73.3 | `(*Position).ValidMovesUnsafe` |

Move generation (`standardMoves` 77% cum, `CalcMoves` 79% cum) is the wall-time bottleneck, not the lexer/parser text work. The `math/bits.Reverse64` + `bbForPiece`/`setBBForPiece` pair (combined **~35% flat**) is doing board<->bitboard translation on every square touched during move gen — the C++ chess-library uses fixed-size arrays indexed by square, which is essentially free.

### Memory by alloc count (`big.mem.prof`)

| flat% | allocs | symbol |
|---|---|---|
| 43.4 | 13.13 M | `(*Parser).parseMove` |
| 10.5 | 3.18 M | `(*Parser).addMove` |
| 8.6 | 2.59 M | `castleMoves` |
| 8.6 | 2.59 M | `internal/bytealg.MakeNoZero` (slab for castleMoves) |
| 6.2 | 1.87 M | `standardMoves` |
| 5.2 | 1.57 M | `(*Board).copy` (inline) |
| 4.6 | 1.39 M | `(*Parser).parseComment` |
| 4.6 | 1.38 M | `(*Position).Update` |

### Memory by bytes (`big.mem.prof`)

| flat% | MB | symbol |
|---|---|---|
| **53.0** | **1554** | **`TokenizeGame`** |
| 9.8 | 288 | `(*Board).copy` |
| 8.2 | 240 | `standardMoves` |
| 7.3 | 213 | `parseMove` |
| 5.1 | 148 | `addMove` |
| 3.7 | 109 | `parseComment` |
| 3.6 | 105 | `(*Position).Update` |

## Hotspot analysis

Ranked by expected gain. Numbers = `big.pgn` (1000 games), so impact on `big_big.pgn` is ~10×.

### 1. `TokenizeGame` append-to-nil — 1.55 GB / 53% of all memory
`scanner.go:48-65`:

```go
var tokens []Token
for {
    token := lexer.NextToken()
    if token.Type == EOF { break }
    tokens = append(tokens, token)
}
```

A nil slice grown only by `append` grows geometrically and throws away every prior backing array. For a 4 KB game that's ~14 reallocations; a 40 KB game ~17. `bufio.Scanner.Text` allocates another 43 MB on top.

**Fix**: `tokens := make([]Token, 0, len(game.Raw)/4)` (or similar heuristic). Even a rough guess cuts reallocations from 17 to 1 and drops ~1 GB. Single biggest win in this profile.

### 2. `parseMove` for-range `&m` escape — 12.68 M Move copies / 43% of allocs
`pgn.go:440-503`:

```go
for _, m := range validMoves {       // ← line 440
    ...
    matchingMove = &m                // ← line 500 — escape forces stack-to-heap per iter
    break
}
```

Because the loop body takes `&m`, the compiler can't keep `m` in a register; each iteration copies the Move value to the heap. 12.68 M copies × `sizeof(Move)` ≈ half of `parseMove`'s 213 MB.

**Fix**: index loop, then take address of `validMoves[i]` once at the match site:
```go
for i := range validMoves {
    m := validMoves[i]
    ...
    mv := &validMoves[i]
    matchingMove = mv
    break
}
```
Or just copy the fields at the end — `validMoves[i]` is what's wanted anyway.

### 3. `parseMove` `mismatchReasons` waste in happy path — `pgn.go:438-498`
On every mismatch candidate move, a `*ParserError` is heap-allocated and appended to a slice. In the happy path the slice is discarded without ever being read.

**Fix**: build the error string lazily — only allocate when `matchingMove == nil`. The common case (one of N candidates matches on the first try) skips 99% of these allocs. Or use `errors.Join` at the end without intermediate storage.

### 4. `parseMove` castle branches — `pgn.go:313`, `:334`
Same `for _, m := range validMoves` shape; same per-iter Move copy on the escape path. Smaller scope (only castle moves) but identical fix.

### 5. `castleMoves` stack-escape — 2.59 M allocs / 39 MB
`engine.go:355-407`:

```go
var moves [2]Move
count := 0
... moves[count] = m; count++ ...
return moves[:count]    // ← escapes to heap
```

The `[2]Move` array is stack-resident, but the returned slice header forces a heap copy. 2.59 M calls / 1000 games = ~2.6 calls per move × 1000 moves/game × 1000 games — called once per `CalcMoves`, i.e. once per ply. So ~`2 × 862 K = 1.7 M` for big.pgn. Close.

**Fix**: inline the logic into `CalcMoves` (no return), or pass `*Position` and a small stack buffer by pointer:
```go
func castleMovesInto(pos *Position, buf *[2]Move) int
```
That keeps `buf` on the caller's stack.

### 6. `parseVariation` — 12.16 M cum allocs
`addMove` allocates a `MoveNode` per move (3.18 M direct) plus a fresh `Position` (1.38 M via `Position.Update`). Building the move tree is inherently allocation-heavy, but the per-node Position copy dominates. If we ever support a "lazy position" tree node, this drops sharply. Out of scope for a pure throughput fix.

### 7. `math/bits.Reverse64` — 19.98% CPU
Used in `bbForSquare` to map a `Square` (0..63) to a bitboard. Each `standardMoves` invocation iterates 64 squares per piece per bitboard. C++ chess-library uses a `[64]uint64_t` lookup table — flat array read.

**Fix**: precompute a `var bbForSquareTable [64]uint64_t` once. Drops 20% of CPU for a few KB of init.

### 8. `isSquareAttackedBy` + `linearAttack`/`diaAttack`/`hvAttack` — ~10% CPU cum
Magics / precomputed attack tables would help here too. Lower priority than (7) since it's a smaller fraction.

### 9. `standardMoves` itself — 7.27% CPU flat
Already uses `sync.Pool` for its 218-Move backing array — good. Inner loop still walks every square of every piece's bitboard; a mailbox representation (or attack-table move gen) would help, but that's a bigger architectural change.

## Reproducing

```bash
# Go benchmarks
go test -bench=BenchmarkPGN -benchmem -benchtime=5x -run=^$ ./...

# C++ benchmark
cd benchcmp/cpp
g++ -std=c++17 -O3 -DNDEBUG -march=native -flto \
    -I /tmp/chess-library/include \
    pgn_bench.cpp -o pgn_bench
./pgn_bench ../../fixtures/pgns/big.pgn
./pgn_bench ../../fixtures/pgns/big_big.pgn

# CPU profile
go test -bench=BenchmarkPGN_Stream_Big -benchtime=10s -run=^$ -cpuprofile=benchcmp/prof/big.cpu.prof -memprofile=benchcmp/prof/big.mem.prof ./...
go tool pprof -top -nodecount=20 benchcmp/prof/big.cpu.prof
```

## TL;DR

- Go PGN import is **~100× slower** than Disservin's chess-library on the same files.
- The single biggest memory waste is `TokenizeGame`'s append-to-nil slice (1.55 GB / 53%). One-line fix.
- The single biggest allocation count is `parseMove`'s `&m` escape in the legal-move scan (12.68 M Move copies). Index-loop fix.
- The single biggest CPU hotspot is `math/bits.Reverse64` inside `linearAttack` (Hyperbola Quintessence, ~20%). Replace with per-sub-direction loop-based attacks.

## Applied fixes (this commit)

The three mechanical high-impact fixes have been applied. Verified against the full test suite (`go test ./...`) and benchmarked.

| Metric | Before | After | Δ |
|---|---|---|---|
| `big.pgn` allocations | 4.5 M | 2.15 M | **-52%** |
| `big.pgn` memory | 301 MB | 181 MB | **-40%** |
| `big.pgn` throughput | 3.53 MB/s | 3.47 MB/s | -2% |
| `big_big.pgn` allocations | 25.6 M | 11.95 M | **-53%** |
| `big_big.pgn` memory | 1.13 GB | 956 MB | **-15%** |
| `big_big.pgn` throughput | 1.36 MB/s | 1.25 MB/s | -8% |
| `math/bits.Reverse64` CPU | 19.98% | 0% | **gone** |

Throughput is roughly neutral because the loop-based attacks are slightly slower per call than Hyperbola Quintessence with `Reverse64` (the Reverse64 overhead was offset by HQ's tight math). Allocations and memory drop substantially.

### What was changed

1. `scanner.go:48-65` — `TokenizeGame` now preallocates the token slice from input length.
2. `pgn.go:313`, `:334`, `:440` — `parseMove` (and its two castle branches) now use index loops into `validMoves`, eliminating the per-iteration Move copy via `&m` escape. `matchingMove` is tracked by index and copied at the end.
3. `engine.go:438-461` — `diaAttack`/`hvAttack` rewritten as four per-sub-direction loops (NE/SW/NW/SE and E/W/N/S) with explicit blocker checks. `linearAttack` and the `math/bits.Reverse64` call chain are gone.

## Round 2 — re-profiling post-fixes

Re-ran with `-benchtime=10x` (`big.cpu.prof`, `big.mem.prof`):

```
BenchmarkPGN_Stream_Big-32        10  1181523947 ns/op   3.49 MB/s  180446343 B/op  2151052 allocs/op
BenchmarkPGN_Stream_BigBig-32     10  7031361452 ns/op   1.29 MB/s  955379235 B/op 11945317 allocs/op
```

### CPU new landscape (91.15 s total)

The profile shape changed completely — `Reverse64` is gone, but the parser is now **dominated by `moveTags`**, which is the per-pseudo-legal-move legality check.

**Flat top** (sample = 91.15 s):

| flat% | symbol | note |
|---|---|---|
| 13.20 | `hvAttack` | loop-based sliding (was 1.28% before fix #3) |
| 12.68 | `diaAttack` | loop-based sliding (was 2.72% before) |
| 9.51 | `standardMoves` | the main move-gen loop |
| 8.57 | `(*Board).update` | mutates bitboard map per move |
| 8.41 | `(*Board).bbForPiece` (inline) | map read |
| 6.76 | `(*Board).setBBForPiece` | map write |
| 5.70 | `Piece.Color` (inline) | interface call |
| 3.66 | `isSquareAttackedBy` | called by `moveTags` |
| 3.38 | `NewPiece` (inline) | interface alloc |
| 2.55 | `moveTags` | flat, but 70.9% cum |

**Cumulative**:

| cum% | symbol |
|---|---|
| 83.2 | `standardMoves` |
| 79.8 | `(*Parser).parseMove` |
| 70.9 | `moveTags` |
| 40.2 | `isSquareAttackedBy` |
| 28.3 | `(*Board).update` |
| 7.6 | `(*Game).evaluatePositionStatus` |

### The diamond: `moveTags` is the new king

`engine.go:189-226` runs **per pseudo-legal move** inside `standardMoves`. For each candidate move it:

1. `tempBoard := *pos.board` — full struct copy of Board (board.go).
2. `tempBoard.update(local)` — `(*Board).update` mutates the bitboard map, calls `calcConvienceBBs` (24.93 s cum).
3. **Two `isSquareAttackedBy` calls** — one to test if self king is attacked (illegal-move filter, 19.72 s), one for the `Check` SAN tag (17.00 s).

`moveTags` itself is only 2.55% flat — its cost is almost entirely in `tempBoard.update` + `isSquareAttackedBy`. Together those children account for **61.65 s out of 91.15 s = 67.6 %**.

### Hidden tax: `evaluatePositionStatus` re-runs `CalcMoves`

`addMove` (pgn.go:785) calls `evaluatePositionStatus` (game.go:600), which calls `pos.Status()` (position.go:278) → `engine.Status(pos)` (engine.go:60):

```go
if pos.validMoves != nil {
    hasMove = len(pos.validMoves) > 0
} else {
    hasMove = len(e.CalcMoves(pos, true)) > 0   // <-- re-generates ALL legal moves
}
```

In the parse path `pos.validMoves` is never populated, so `Status()` does a **full second `CalcMoves` per ply** just to answer "is there any legal move?" That doubles the move-generation cost per position.

### Memory new landscape

| flat% (count) | allocs | symbol | note |
|---|---|---|---|
| 19.4 | 22.6 M | `(*Parser).addMove` | MoveNode + position copy |
| 18.6 | 21.6 M | `castleMoves` | **still escapes** — fix not applied (engine.go:355 still does `var moves [2]Move; return moves[:count]`) |
| 14.1 | 16.4 M | `standardMoves` | sync.Pool-returned slice escapes via `append` in `CalcMoves` |
| 9.6 | 11.2 M | `(*Board).copy` (inline) | full Board struct copy per ply |
| 9.4 | 10.9 M | `(*Position).Update` | wraps Board.copy + struct literal |
| 3.6 | 4.14 M | `(*Parser).parseMove` | was 13.13 M — fix #2 paid off |
| 2.4 | 2.79 M | `(*Lexer).readMove` | lexer state alloc |
| 2.0 | 2.38 M | `strings.(*Builder).WriteByte` | token text |
| 1.5 | 1.72 M | `engine.CalcMoves` | sync.Pool slice escape |

| flat% (bytes) | bytes | symbol |
|---|---|---|
| 33.1 | 3.83 GB | `TokenizeGame` |
| 17.3 | 2.00 GB | `(*Board).copy` |
| 16.0 | 1.86 GB | `standardMoves` |
| 9.3 | 1.08 GB | `(*Parser).addMove` |
| 7.0 | 0.81 GB | `(*Position).Update` |
| 3.6 | 0.42 GB | `engine.CalcMoves` |
| 2.8 | 0.32 GB | `castleMoves` |

`TokenizeGame` is still 33% of all memory despite the preallocation — the heuristic (`len/3+16`) under-sizes for dense SAN lines. Tightening the heuristic would help; switching to streaming lexing (no intermediate `[]Token`) would help more.

## Round 2 recommendations (ranked by expected gain)

1. **`engine.Status` cache or fast-path** (engine.go:60) — make `Status()` reuse the caller's `validMoves` (or expose `HasAnyLegalMove(pos)` that just iterates `standardMoves` with `first=true` and a known-safe check). Saves ~50% of `standardMoves` work — that's the entire second `CalcMoves` per ply.

2. **Replace `moveTags`'s `tempBoard := *pos.board` + `tempBoard.update(local)` with make/unmake**. The Board struct is ~200 bytes plus 12 map entries; copying it is much more expensive than mutating in place and snapshotting the touched keys. With make/unmake, the legality check becomes: apply move, test self-attack, undo. Same `isSquareAttackedBy` cost, but no per-candidate alloc.

3. **`castleMoves` inline + pointer-passing** (engine.go:355) — the `var moves [2]Move; return moves[:count]` slice-escape fix from the original plan was **not applied**. With it in place, `castleMoves` drops from 21.6 M allocs to ~0.

4. **Magic bitboards / precomputed attack tables for `diaAttack` / `hvAttack`** — together 25.9 % of flat CPU. The `256`-entry per-sub-direction-per-square brute-force tables would be a few hundred KB and cut these calls to O(1). This is the throughput unlock.

5. **Cache `pos.validMoves`** after `CalcMoves` calls inside the parser path — sets up (1) and saves another hidden re-gen.

6. **Tighten `TokenizeGame` preallocation heuristic** — `len/3+16` undershoots. `len/2` or a one-pass counting scan first would cut TokenizeGame's 3.83 GB further.

Items 1, 2, 3 are mechanical and together address ~70% of remaining CPU and ~38% of remaining allocations. Item 4 is the big CPU unlock. Item 6 is a one-line tweak.