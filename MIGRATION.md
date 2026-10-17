# Migrating to v3

v3 is a breaking release focused on a smaller `Move` value type, extracted PGN
rendering, terminal-outcome guards, and modernized `opening`, `image`, and `uci`
packages. This guide lists every breaking change and how to update your code.

## Module path

The module path now reflects major version 3:

```bash
go get -u github.com/corentings/chess/v3
```

```go
import "github.com/corentings/chess/v3"
```

Update import paths in every sub-package you use (`/v3/image`, `/v3/opening`,
`/v3/uci`). Polyglot books moved to the new `/v3/polyglot` package.

## Move is a value, not a pointer

`Move` is now a 4-field value (`s1`, `s2`, `promo`, `tags`). Tree navigation and
annotations live on the separate `MoveNode` type. `LegalMoves()` (formerly
`ValidMoves()`) returns `[]Move` and the move methods accept `Move` by value.

```go
// v2
moves := game.ValidMoves()
if err := game.Move(&moves[0], nil); err != nil { /* ... */ }

// v3
moves := game.LegalMoves()
node, err := game.Move(moves[0], nil)
if err != nil { /* ... */ }
_ = node
```

The same change applies to `UnsafeMove`, `MoveText`, and `UnsafeMoveText`:
they now return `(*MoveNode, error)`. The old
`PushMoveOptions` type is replaced by `MoveInsertOptions` with
`PromoteToMainLine`.

Active move insertion remains a `Game` responsibility. Use the `Game` move
methods above to advance the current position so legality checks, terminal game
guards, and outcome evaluation stay in sync. `MoveTree` is exposed for
topology, cursor navigation, and variation traversal/editing; it does not have
a public active-move insertion method.
`Move.Clone()` has been removed — copy by assignment (`m2 := m1`) if you need a
value copy.

In v2, `*Move` carried both the move data and its position/context. In v3 these
are split: `Move` is the bare value; `MoveNode` holds the tree position,
children, comments, and NAGs. Code that iterated moves and accessed position
data must now navigate via `MoveNode`:

```go
// v2
for _, m := range game.Moves() {
    fmt.Println(m.Position().Turn()) // Move carried the resulting position
}

// v3
for _, node := range game.MoveTree().MainLine() {
    fmt.Println(node.Position().Turn()) // Position() replays via tree cursor (ADR-018)
}
```

`game.Moves()` still returns `[]Move` (bare values, main line only). Use
`game.MoveTree()` for tree access: `tree.Root()`, `tree.Current()`,
`tree.MainLine()`, `tree.Continuations(node)`, `tree.Variations(node)`,
`node.Comments()`, `node.SetCommand(key, val)`, `node.SetComment(text)`,
`node.AddComment(text)`, `node.NAG()`, and `node.Children()` / `node.Parent()`
for variation traversal.

## Notation APIs moved to move text codecs

v3 removes the old notation structs and game notation methods. Use
`MoveTextCodec` values instead:

- `AlgebraicNotation{}` -> `chess.SAN()`
- PGN import leniency -> `chess.PGNImportSAN()`
- `LongAlgebraicNotation{}` -> `chess.LongAlgebraic()`
- `UCINotation{}` -> `chess.UCI()`
- `Game.PushMove(san, opts)` -> `Game.MoveText(san, chess.SAN(), opts)`
- `Game.PushNotationMove(text, notation, opts)` -> `Game.MoveText(text, codec, opts)`
- `Game.UnsafePushNotationMove(text, notation, opts)` -> `Game.UnsafeMoveText(text, codec, opts)`

```go
game := chess.NewGame()
_, err := game.MoveText("e4", chess.SAN(), nil)
_, err = game.MoveText("e2e4", chess.UCI(), nil)
_, err = game.UnsafeMoveText("e2e4", chess.UCI(), nil)
```

Unsafe move text supports only fully specified formats such as UCI and long
algebraic notation; SAN and PGN import SAN require legal resolution and are
rejected by `UnsafeMoveText`.

## UCI commands are struct literals

Global command values are now struct types. Pass them as struct literals:

```go
// v2
eng.Run(uci.CmdUCI, uci.CmdIsReady, uci.CmdUCINewGame)

// v3
eng.Run(uci.CmdUCI{}, uci.CmdIsReady{}, uci.CmdUCINewGame{})
```

Commands that carry data (`CmdPosition`, `CmdGo`, `CmdSetOption`) are still
constructed as struct values:

```go
eng.Run(uci.CmdPosition{Position: game.Position()}, uci.CmdGo{MoveTime: time.Second / 100})
```

A new `Adapter` interface (implemented by `SubprocessAdapter`) backs the engine
and is injectable via `uci.NewWithAdapter`, e.g. with an in-memory fake in
tests. `eng.SearchResults().BestMove` is now
a `chess.Move` value.

## Image takes a Position and options

`image.SVG` now accepts `*chess.Position` (instead of `*Board`) and an
`*SVGOptions` struct:

```go
// v2
image.SVG(w, board, image.Perspective(chess.Black))

// v3
image.SVG(w, game.Position(), &image.SVGOptions{Perspective: chess.Black})
```

`SVGOptions` exposes `SquareSize` (squares are no longer fixed at 45px). Piece
SVGs are embedded with `//go:embed` and cached at init.

## Opening returns errors

`NewBookECO()` parsed the ECO table on every call and called `log.Fatal` on
failure. It is replaced by a lazily-initialized singleton and a reader-based
constructor, both returning errors:

```go
// v2
book := opening.NewBookECO() // *BookECO, panics on error

// v3
book, err := opening.DefaultBook()   // *BookECO, error, sync.Once
// or
book, err := opening.NewBook(r)      // *BookECO, error, from any io.Reader
```

## Renamed identifiers

Several v2 identifiers were renamed to follow Go naming conventions. All are
mechanical renames with no behaviour change; update the symbol at the call site.

| v2 | v3 | Reason |
|---|---|---|
| `opening.Opening` | `opening.Entry` | Stuttered at the call site (`opening.Opening{}`). |
| `chess.GetPolyglotHashBytes` | _(removed)_ | Use `HashFromFEN` / `Position.ZobristHash`; convert a `uint64` via `binary.LittleEndian`. |

The unexported `newOpening` constructor is renamed `newEntry` to match.

## Move listing uses "legal" everywhere

v3 uses the chess term *legal* for every move list, matching `IsLegal` and
`AnyLegalMove`:

| v2 | v3 |
|---|---|
| `Game.ValidMoves()` / `Position.ValidMoves()` | `LegalMoves()` |
| _(none)_ | `Position.LegalMovesSeq() iter.Seq[Move]` |
| _(none)_ | `Position.LegalMovesFast()`, legal moves without the `Check` tag |
| _(none)_ | `Position.PseudoLegalMoves()`, every pseudo-legal move; filter with `IsLegal` |

```go
for m := range pos.LegalMovesSeq() {
	next := pos.Update(m)
	_ = next
}
```

## Tags and commands

PGN tag accessors drop the `Get`/`Pair` wording, and a game's tags can now be
listed:

| v2 | v3 |
|---|---|
| `Game.GetTagPair(k) string` | `Game.Tag(k) string` |
| `Game.AddTagPair(k, v) bool` | `Game.SetTag(k, v)` (no return value) |
| `Game.RemoveTagPair(k) bool` | `Game.RemoveTag(k) bool` |
| _(none)_ | `Game.Tags() map[string]string` (a copy) |
| `chess.TagPairs` | _(unexported)_; use `Game.Tags()` |
| `MoveNode.GetCommand(key)` | `MoveNode.Command(key)` |

## Positions, boards and options

| v2 / v3 beta | v3 |
|---|---|
| `Position.String()` | `Position.FEN()` (`String()` still returns the FEN) |
| `Position.XFENString()` | `Position.CanonicalFEN()` |
| `Position.PositionKey()` | `Position.FENKey()` |
| `Position.UnmarshalText` / `NewGame(FEN(s)).Position()` to parse a FEN | `ParseFEN(s) (*Position, error)` |
| `Position.Checkers() []Square` | `Position.Checkers() Bitboard` |
| `Board.AttacksFrom(sq) []Square` | `Board.AttacksFrom(sq) Bitboard` |
| `Board.Draw2(perspective, dark)` | `Board.DrawWith(DrawOptions{Perspective: c, DarkMode: dark})` |
| `for sq := range bb.Squares` | `for sq := range bb.Squares()` |
| `MoveNode.ChildrenIter` | `MoveNode.ChildrenSeq()` |
| `MoveNode.Number()` | `MoveNode.FullMoveNumber()` |
| `Game.NullMove()` | `Game.NullMove(nil)` |
| `func(*Game)` options | `GameOption` (`FEN`, `Ignore*Draw` return it) |
| `func(*Engine)` options, `uci.Debug` | `uci.EngineOption`, `uci.Debug()` |
| `Engine.Getpid()` | `Engine.Pid()` |

Iterate a Bitboard's squares or an attack set:

```go
for sq := range pos.Board().AttacksFrom(chess.E4).Squares() {
	fmt.Println(sq)
}
```

## Removed APIs

These v2 exports have no direct v3 equivalent under the same name:

| v2 | v3 |
|---|---|
| `ColorFromString(s)` | `ParseColor(s) (Color, error)` |
| `SquareFromString(s)` | `ParseSquare(s) (Square, error)` |
| `PieceTypeFromString(s)` | `ParsePieceType(s) (PieceType, error)` |
| `PieceTypeFromByte(b)` | `ParsePieceTypeFromByte(b) (PieceType, error)` |
| `GetPolyglotHashes()` | _(removed)_; see [Hashing](#hashing) |
| `NewScanner` / `Scanner.HasNext` / `Scanner.ParseNext` | `PGNGames(r, opts...)`, `NewPGNDecoder(r, opts...).Decode()`, or `ParsePGN(r)` for a single game |
| `GameScanned`, `TokenizeGame`, `NewParser(tokens).Parse()` | _(removed)_; the PGN parser is internal. Use the decoders above. |
| `WithExpandVariations()` | `WithPGNExpandVariations()` |
| `NewLexer`, `Lexer`, `Token`, `TokenType` and its constants | _(removed)_; the PGN lexer is internal. Use `PGNEvents` for token-level access to moves, comments and tags. |
| `ParserError.TokenType` | _(unexported)_; it still appears in `Error()` |

The `Parse*` helpers return an error instead of the `NoColor` / `NoSquare` /
`NoPieceType` sentinel, so wrap them if you relied on the sentinel:

```go
// v2
sq := chess.SquareFromString("e4")

// v3
sq, err := chess.ParseSquare("e4")
if err != nil { /* ... */ }
```

Reading every game of a PGN file:

```go
// v2
scanner := chess.NewScanner(f)
for scanner.HasNext() {
	game, err := scanner.ParseNext()
	// ...
}

// v3
for game, err := range chess.PGNGames(f) {
	if err != nil { /* ... */ }
	// ...
}
```

## Resign returns an error

`Game.Resign` now validates state and returns an error instead of silently
no-oping after the game has ended:

```go
// v2
game.Resign(chess.Black)

// v3
if err := game.Resign(chess.Black); err != nil {
    // game already ended, etc.
}
```

## Terminal outcome guards

`Move`, `UnsafeMove`, and `PushMoveText` reject moves once the game has
reached a terminal outcome and return `chess.ErrGameAlreadyEnded`. Call
`Game.ClearOutcome()` to resume, or `Game.SetOutcomeMethod(method, outcome)` to
set an outcome explicitly with validation. `Split()` now recomputes each line's
outcome from its leaf position instead of copying the parent outcome.

## Cursor → MoveTree

`PositionCursor` is removed; `Game.Cursor()` and `MoveTree.Cursor()` are
removed. Navigate the tree directly:

```go
t := game.MoveTree()
t.Reset()
for t.GoForward() {
    // … reads via t.Peek() (zero-copy alias — MUST NOT mutate)
    // or via game.Position() / node.Position() (defensive copies)
}
```

`MoveTree` gains `Peek`, `Forward(idx)`, `Goto(node)`, and `Reset` as public
methods. `Peek` returns the live cursor position without copying; the pointer
is invalidated by any navigation. `Goto` rejects nodes from a different tree
with `false`.

## Null moves

v3 adds explicit null move support. A null move flips the side to move without
moving any piece — used by engines for null-move pruning and by PGN
annotations to indicate a side passed.

```go
g := chess.NewGame()
node, err := g.NullMove(nil) // flips turn, serializes as "Z0" in PGN
```

`NewNullMove()` returns a `Move` carrying the `Null` tag. Null moves are never
returned by `LegalMoves()` and are rejected by `Game.Move`. Insert them via
`Game.NullMove(nil)` or `Game.UnsafeMove(NewNullMove(), nil)`. When reading PGN,
the parser accepts five spellings: `Z0`, `Z1`, `--`, `@@`, and `0000`. The
renderer always writes `Z0` (ChessBase/Scid convention).

## PGN rendering

`Game.String()` returns the PGN and `Game.WritePGN(w)` writes it to an
`io.Writer`. There is no separate renderer type; the v3 betas exported
`PGNRenderer` and `DefaultPGNRenderer`, which are now internal.

## Hashing

v3 consolidates position hashing around the cached `Position.ZobristHash() uint64`
for O(1) comparison and repetition detection. Several v2 hashing APIs are
removed with no direct replacement:

- `Position.Hash() [16]byte` (MD5) is **removed**, not deprecated. Use
  `Position.ZobristHash() uint64`.
- `ZobristHasher`, `NewChessHasher()`, `NewZobristHasher()` are removed. The
  incremental hasher is now internal to `Position`. Callers who previously
  hashed FEN strings without instantiating a `Position` should use the free
  function `HashFromFEN(fen string) (uint64, error)`, which is a direct
  replacement for `ZobristHasher.HashPosition(fen)`.
- `ZobristHashToUint64(hash string) uint64` is removed. Convert hex hashes
  with `strconv.ParseUint(s, 16, 64)`.
- `GetPolyglotHashBytes(index int) []byte` and `GetPolyglotHashes() []string`
  are removed: the Polyglot random table is an implementation detail of the
  hash. For the raw 8-byte little-endian form of a position key, convert the
  `uint64` from `HashFromFEN` or `Position.ZobristHash` via
  `binary.LittleEndian.PutUint64`.

`Position.LegalMovesSeq()` (no allocation once cached) and
`Position.LegalMovesFast()` (skips the `Check` tag) serve hot paths.
`Board.Piece()` uses an internal mailbox for O(1) lookup.

## Polyglot books live in `polyglot`

Polyglot opening books moved from the root package to
`github.com/corentings/chess/v3/polyglot`, and the names lost their `Polyglot`
prefix. The `BookSource` adapters and `LoadFromSource` are gone; both loaders
read the whole book into memory.

| v2 | v3 |
|---|---|
| `chess.LoadFromSource(src)` / `chess.LoadFromReader(r)` | `polyglot.Load(r)` |
| `chess.LoadFromBytes(data)` | `polyglot.LoadBytes(data)` |
| `chess.NewPolyglotBookFromMap(m)` | `polyglot.NewBook(m)` |
| `chess.PolyglotBook` | `polyglot.Book` |
| `chess.PolyglotEntry` | `polyglot.Entry` |
| `chess.PolyglotMove` | `polyglot.Move` |
| `chess.MoveWithWeight` | `polyglot.WeightedMove` |
| `chess.MoveToPolyglot(m)` | `polyglot.EncodeMove(m)` |
| `chess.DecodeMove(code)` | `polyglot.DecodeMove(code)` |
| `(*PolyglotBook).GetChessMoves(hash)` | `(*Book).Moves(hash)` |
| `(*PolyglotBook).GetRandomMove(hash)` | `(*Book).RandomMove(hash)` |
| `(*PolyglotBook).ToMoveMap()` | `(*Book).WeightedMoves()` |
| `PieceType.ToPolyglotPromotionValue()` | _(removed)_ |

Books are keyed by `Position.ZobristHash()`, which is the Polyglot key:

```go
book, err := polyglot.Load(f)
if err != nil { /* ... */ }
moves, err := book.Moves(game.Position().ZobristHash())
```
