# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## Unreleased

- - -

## v3.0.0-beta.2 - 2026-06-29

#### Performance
- speed up full PGN-to-`Game` decoding by resolving SAN moves from direct candidate origins instead of scanning every legal move.
- reduce parsed position snapshot allocation count by storing board state directly inside internal `Position` values while keeping public defensive-copy APIs unchanged.
- keep full PGN decode behavior compatible for move tags, checks, castling, promotions, en passant, variations, and parser errors.

#### Documentation
- document the v3 beta PGN decode performance profile and benchmark workflow.

- - -

## v3.0.0-beta.1 - 2026-06-19

v3 is a major redesign implementing [RFC-001](docs/adr/RFC-001-v3-redesign.md).
See [MIGRATION.md](MIGRATION.md) for a detailed guide to upgrading from v2.

#### Breaking Changes
- module path changed to `github.com/corentings/chess/v3`.
- `Move` is now a 4-field value type (`s1`, `s2`, `promo`, `tags`), passed by value everywhere — `Move()` and `UnsafeMove()` accept `Move` not `*Move`.
- UCI command globals replaced with struct literals: `CmdUCI{}`, `CmdIsReady{}`, `CmdUCINewGame{}`, etc.
- `Game.Resign(color)` now returns `error`.
- `Game.Outcome` and `Game.Method` merged into `Outcome` + `OutcomeMethodPair` with `SetOutcomeMethod`/`ClearOutcome`.
- `Opening.Game()` returns a caller-owned clone of a pre-computed game.
- `Position.Hash()` (MD5) deprecated in favour of `Position.ZobristHash()` (uint64).

#### Features
- move tree with full variation support (`Variations`, `AddVariation`, `Split`).
- extracted PGN renderer (`PGNRenderer`, `DefaultPGNRenderer`, `RenderGameTo`).
- `Game.WritePGN(w)` for direct writer output.
- UCI Adapter pattern (`Adapter`, `SubprocessAdapter`, `FakeAdapter`) for testability.
- SVG image generation API: `SVG(w, *Position, *SVGOptions)` with `//go:embed` piece assets.
- opening book: `NewBook(io.Reader)` for custom data, `DefaultBook()` singleton.
- notation `Encode`/`Decode` with value semantics.

#### Performance
- incremental Zobrist hashing with `Position.ZobristHash()`.
- mailbox `[64]Piece` for O(1) `Board.Piece()` lookups.
- zero-allocation `BookECO.Find` via compact `uint32` move keys.
- non-allocating `Position.SamePosition` fallback (direct struct comparison).
- `Position.ValidMovesUnsafe()` and `Position.ValidMovesIter()` for allocation-sensitive callers.

## v2.5.1 - 2026-06-19
#### Features
- preserve PGN annotation block structure and add `Move.CommentBlocks()` for ordered comment items.
#### Performance
- cache board mailbox lookups for O(1) `Piece()` access.
- eliminate heap allocations in move tag generation.
- reduce PGN parser position copies.
- add `Position.ValidMovesUnsafe()` and `Position.ValidMovesIter()` for allocation-sensitive callers.
- cache Zobrist hashes for fast current position comparison and repetition detection.
#### Refactoring
- standardize error handling and remove dead code.
- replace image module go-bindata assets with `//go:embed`.
- add opening book singleton and concurrency safety.
#### Tests
- add Game current position invariant coverage.

- - -
## v2.3.2 - 2025-10-08

- - -

## v2.3.1 - 2025-09-25
#### Bug Fixes
- pgn disambiguation squares (#75) - (362a897) - Corentin Giaufer Saubert
- Potential fix for code scanning alert no. 1: Workflow does not contain permissions (#74) - (34f57bc) - Corentin Giaufer Saubert
#### Documentation
- improve formatting and readability in README.md - (f9102d2) - CorentinGS
#### Refactoring
- clean up code by removing unnecessary blank lines and optimizing variable initialization - (eefa43f) - CorentinGS
- reorganize fields in Scanner and SearchResults structs for clarity - (6a3f038) - CorentinGS
- streamline variable initialization in parseMoveText function - (a508986) - CorentinGS
- improve error handling in TestBytesBookSource and enhance string comparison in TestChessHasher - (503a6d7) - CorentinGS
- optimize token handling in parseMoveText - (71f7da8) - CorentinGS
- simplify cloneChildren method by removing unnecessary nil check - (5f4e014) - CorentinGS
- simplify evaluatePositionStatus method and optimize path collection in Split - (9879d58) - CorentinGS
- remove unnecessary break statements in addTags function - (712f228) - CorentinGS

- - -

## v2.3.0 - 2025-09-18
#### Bug Fixes
- experiment for issue #61 - (5231e8f) - CorentinGS
#### Documentation
- update README examples to use PushNotationMove for consistency - (cd1dbc6) - CorentinGS
- update README with move validation enhancements and UnsafeMove method details - (63bdd33) - CorentinGS
#### Features
- enhance move validation and add UnsafePushNotationMove for performance - (dd84264) - CorentinGS
- add UnsafeMove method for high-performance move handling and related tests - (550a75b) - CorentinGS

- - -

## v2.2.0 - 2025-08-06
#### Features
- add ValidateSAN function and comprehensive tests - (30701c7) - CorentinGS
- Add support for root move comments in PGN output (#68) - (3f55e9c) - Corentin Giaufer Saubert

- - -

## v2.1.0 - 2025-07-13
#### Bug Fixes
- fatal formatting - (32ce606) - Corentin Giaufer Saubert
#### Refactoring
- rename example functions for clarity in opening tests - (87b468c) - CorentinGS

- - -

## v2.0.10 - 2025-07-07
#### Bug Fixes
- ambiguous moves in PGN; Add PGN test case (#47) - (0a2700b) - TheNipanen

- - -

## v2.0.9 - 2025-06-16
#### Bug Fixes
- PGN parser (#45) - (2246d84) - Corentin Giaufer Saubert

- - -

## v2.0.8 - 2025-05-13

- - -

## v2.0.7 - 2025-04-16
#### Bug Fixes
- handle castle tags correctly (#35) - (75912a7) - Tiago Gottardo
#### Features
- support multiple move notations via PushNotationMove (#33) - (4ae9552) - Tiago Gottardo

- - -

## v2.0.6 - 2025-03-31
#### Features
- enhance UCI move handling with position context and add tests (#30) - (ce3144c) - Corentin Giaufer Saubert

- - -

## v2.0.5 - 2025-02-13
#### Features
- enhance position handling in notation decoding (#27) - (e21d500) - Corentin Giaufer Saubert
- add SetComment method and related tests for Move struct (#28) - (7b215f0) - Corentin Giaufer Saubert

- - -

## v2.0.4 - 2025-02-05
#### Features
- improve perf and uci notation (#26) - (85f540d) - Corentin Giaufer Saubert

- - -

## v2.0.3 - 2025-02-04
#### Bug Fixes
- game.String() respects the defined order (#23) - (17ab924) - Sönke Werner Köster
#### Features
- update move number handling to use int type and improve parsing logic (#25) - (7e9b22e) - Corentin Giaufer Saubert
- implement UCI conversion for castling moves in polyglot - (ebe4434) - CorentinGS

- - -

## v2.0.2 - 2025-02-02
#### Bug Fixes
- output pgn for game string representation (#19) - (776f4ec) - Corentin Giaufer Saubert
- 🐛 add support for multiple commands in moves and update dependencies (#16) - (6e2ec58) - Corentin Giaufer Saubert
- better handling of map - (3ed4f1c) - CorentinGS
#### Features
- add tag prefix configuration for version handling - (dbf2007) - CorentinGS
- add GetRootMove method to retrieve the root move of the game - (81d9023) - CorentinGS
- add cog for version handling - (8659047) - CorentinGS

- - -

## 2.0.1 - 2025-01-23
#### Bug Fixes
- **(golanci-lint)** 🚨 try to fix some linters warnings (#2) - (44962f6) - Corentin Giaufer Saubert
- Handle ignored errors in fmt.Sscanf and fmt.Fprintf calls - (651d1d5) - CorentinGS
- parsing commands and variations - (397e104) - Corentin Giaufer Saubert
- golang version - (5c88ba1) - CorentinGS
- forgot reference - (7cf9b3e) - CorentinGS
- linter warnings - (8fa67cf) - CorentinGS
- notation convention - (cc7669c) - CorentinGS
- improve Move allocation and reduce heap allocations - (c9d58c0) - CorentinGS
- remove map allocation from the loop - (a48f2fd) - CorentinGS
- reduce allocations - (98fe8e2) - CorentinGS
- remove string usage which lead to a lot of allocation and replace with much more efficient code - (5b4e482) - CorentinGS
#### Continuous Integration
- 👷 add ci - (f25315f) - CorentinGS
#### Features
- add getters and setters for the move struct - (e811cd5) - CorentinGS
- improve godoc and simplify code (#14) - (cb10e7d) - Corentin Giaufer Saubert
- add codecov token - (ff62fc4) - CorentinGS
- work on the parser - (6246236) - CorentinGS
- use a byte buffer in board.String to improve performances - (4159504) - CorentinGS
- ChangeTurn function - (286b62a) - CorentinGS
- Expose getPiece as GetPiece - (f1cb08b) - Barak Michener
#### Miscellaneous Chores
- update packages - (b5d0abf) - CorentinGS
#### Performance Improvements
- **(allocs)** ⚡ reduce allocations by using string builder and replacing strings by bytes when possible. - (4f11f5c) - Corentin Giaufer Saubert
#### Refactoring
- `MoveStr` and integrate `PushMove` functionality. (#3) - (4c75fb7) - Corentin Giaufer Saubert
- declare my own module as I'm not going to make PR on the upstream but keep it as an active fork for my own - (3f44c4e) - CorentinGS

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).
