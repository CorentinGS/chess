/*
Package chess implements chess rules, positions, games and PGN/FEN encoding.

The API has three layers. Pick the lowest one that does what you need.

# Game: a played game with history

A Game owns a move tree (main line plus variations), PGN tags and the outcome.
Its move methods check legality and keep the outcome up to date.

	game := chess.NewGame()
	if _, err := game.MoveText("e4", chess.SAN(), nil); err != nil {
	    return err
	}
	for _, m := range game.LegalMoves() {
	    fmt.Println(m)
	}
	fmt.Println(game.Outcome(), game.Method())
	fmt.Println(game.String()) // PGN

Start from another position with the FEN option. Game.MoveTree holds the
main line and its variations, plus a cursor that is the game's current
position: GoBack, GoForward, GoTo, GoToStart and GoToEnd move it, and
Game.Position, Game.LegalMoves and the move methods act on it. A move played
from the end of the main line extends it; a move played anywhere else adds
a variation. The outcome belongs to the main line, so variations never
change it and a finished game only refuses moves at the end of its main
line.

Game values are mutable and not safe for concurrent use; callers sharing a
Game between goroutines must synchronize.

# Position: rules on a single position

A Position is one board state with side to move, castling rights, en passant
square and clocks. It generates moves and answers rule questions without
keeping history. Update returns the next position; ApplyInPlace mutates the
receiver for tight loops.

	pos, err := chess.ParseFEN("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1")
	if err != nil {
	    return err
	}
	for m := range pos.LegalMovesSeq() {
	    next := pos.Update(m)
	    fmt.Println(m, next.FEN(), next.IsCheck())
	}
	fmt.Println(pos.Perft(3), pos.ZobristHash())

Which move list to use:

  - LegalMoves: a copy you may keep or modify; tags include Check.
  - LegalMovesSeq: the same moves without allocating, for loops.
  - LegalMovesFast: the same moves without the Check tag, the cheapest when
    you only need to replay or index moves.
  - PseudoLegalMoves: every move by piece movement, including those that
    leave the king in check; filter with IsLegal.
  - IsLegal: check one move, such as one built with NewMove.

Moves carry tags (Capture, Check, KingSideCastle, ...) derived from the
position. Update and ApplyInPlace accept a move built with NewMove; only
Chess960 castling must come from LegalMoves or a MoveTextCodec. Build
positions from scratch with Setup and NewPosition, and read one back with
Position.Setup.

# Board and Bitboard: piece placement

Board is the piece placement alone. Bitboard is a set of squares with the
usual set operations; the attack functions (KnightAttacks, RookAttacks, ...)
and Board.AttacksFrom return Bitboards.

	board := pos.Board()
	for sq := range board.Pieces(chess.Knight, chess.White).Squares() {
	    fmt.Println(sq, chess.KnightAttacks(sq).Popcount())
	}

# Move text and file formats

MoveTextCodec converts between Move and text: SAN, PGNImportSAN (lenient),
LongAlgebraic and UCI. FEN is read by ParseFEN and written by Position.FEN.
PGN is read by ParsePGN (one game), PGNGames (all games), PGNRecords (raw
records, for filtering on tags before decoding) and PGNEvents (a token-level
stream), and written by Game.String and Game.WritePGN.

# Sub-packages

  - opening: ECO opening classification.
  - polyglot: Polyglot opening books, keyed by Position.ZobristHash.
  - image: SVG board rendering.
  - uci: driving UCI engines such as Stockfish.
*/
package chess
