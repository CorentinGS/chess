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
`/v3/uci`).

## Move is a value, not a pointer

`Move` is now a 4-field value (`s1`, `s2`, `promo`, `tags`). Tree navigation and
annotations live on the separate `MoveNode` type. `ValidMoves()` returns `[]Move`
and the move methods accept `Move` by value.

```go
// v2
moves := game.ValidMoves()
if err := game.Move(&moves[0], nil); err != nil { /* ... */ }

// v3
moves := game.ValidMoves()
node, err := game.Move(moves[0], nil)
if err != nil { /* ... */ }
_ = node
```

The same change applies to `UnsafeMove`, `PushMove`, `PushNotationMove`, and
`UnsafePushNotationMove`: they now return `(*MoveNode, error)`. The old
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
    fmt.Println(node.Position().Turn()) // position is on MoveNode
}
```

`game.Moves()` still returns `[]Move` (bare values, main line only). Use
`game.MoveTree()` for tree access: `tree.Root()`, `tree.Current()`,
`tree.MainLine()`, `tree.Continuations(node)`, `tree.Variations(node)`,
`node.Comments()`, `node.SetCommand(key, val)`, `node.SetComment(text)`,
`node.AddComment(text)`, `node.NAG()`, and `node.Children()` / `node.Parent()`
for variation traversal.

## Notation uses value signatures

`Encode` and `Decode` now take and return `Move` values instead of pointers:

```go
// v2
m, err := alg.Decode(pos, "e4") // m *Move

// v3
m, err := alg.Decode(pos, "e4") // m Move
```

`Encode(pos, m Move) string` / `Decode(pos, s string) (Move, error)`.

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

A new `Adapter` interface (`SubprocessAdapter`, `FakeAdapter`) backs the engine
and is injectable via `uci.NewWithAdapter`. `eng.SearchResults().BestMove` is now
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

`Move`, `UnsafeMove`, and `PushNotationMove` reject moves once the game has
reached a terminal outcome and return `chess.ErrGameAlreadyEnded`. Call
`Game.ClearOutcome()` to resume, or `Game.SetOutcomeMethod(method, outcome)` to
set an outcome explicitly with validation. `Split()` now recomputes each line's
outcome from its leaf position instead of copying the parent outcome.

## Null moves

v3 adds explicit null move support. A null move flips the side to move without
moving any piece — used by engines for null-move pruning and by PGN
annotations to indicate a side passed.

```go
g := chess.NewGame()
node, err := g.NullMove() // flips turn, serializes as "Z0" in PGN
```

`NewNullMove()` returns a `Move` carrying the `Null` tag. Null moves are never
returned by `ValidMoves()` and are rejected by `Game.Move`. Insert them via
`Game.NullMove()` or `Game.UnsafeMove(NewNullMove(), nil)`. When reading PGN,
the parser accepts five spellings: `Z0`, `Z1`, `--`, `@@`, and `0000`. The
renderer always writes `Z0` (ChessBase/Scid convention).

## PGN rendering

`Game.String()` still returns PGN and now delegates to `chess.DefaultPGNRenderer`.
For custom rendering use the extracted renderer:

```go
r := &chess.PGNRenderer{}
fmt.Println(r.Render(game))         // string
r.RenderGameTo(game, w)             // write to an io.Writer
```

`Game.WritePGN(w)` is also available. The renderer is stateless.

## New position helpers

- `Position.ZobristHash() uint64` — cached Zobrist hash for O(1) position
  comparison and repetition detection. The legacy `Position.Hash() [16]byte` is
  kept as deprecated for compatibility.
- `Position.ValidMovesUnsafe()` and `Position.ValidMovesIter()` —
  allocation-sensitive access for hot paths; see package docs for safety notes.
- `Board.Piece()` uses an internal mailbox for O(1) lookup.
