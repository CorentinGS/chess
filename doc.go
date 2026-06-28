/*
Package chess is a go library designed to accomplish the following:
  - chess game state and move tree management
  - move validation
  - PGN encoding / decoding
  - FEN encoding / decoding

Game values are mutable and are not safe for concurrent use by multiple
goroutines. Callers that share a Game between goroutines must provide their own
synchronization.

Using Moves

	game := chess.NewGame()
	moves := game.ValidMoves()
	game.Move(moves[0])

Using Algebraic Notation

	game := chess.NewGame()
	game.MoveStr("e4")

Using PGN

	game, _ := chess.ParsePGN(pgnReader)

Using FEN

	fen, _ := chess.FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
	game := chess.NewGame(fen)

Random Game

	package main

	import (
	    "fmt"
	    "math/rand"

	    "github.com/corentings/chess/v3"
	)

	func main() {
	    game := chess.NewGame()
	    // generate moves until game is over
	    for game.Outcome() == chess.NoOutcome {
	        // select a random move
	        moves := game.ValidMoves()
	        move := moves[rand.Intn(len(moves))]
	        game.Move(move)
	    }
	    // print outcome and game PGN
	    fmt.Println(game.Position().Board().Draw())
	    fmt.Printf("Game completed. %s by %s.\n", game.Outcome(), game.Method())
	    fmt.Println(game.String())
	}
*/
package chess
