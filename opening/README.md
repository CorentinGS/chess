# opening

**opening** provides interactivity to opening books such as [Encyclopaedia of Chess Openings](https://en.wikipedia.org/wiki/Encyclopaedia_of_Chess_Openings) (ECO) which is loadable from the package.  Source: https://github.com/lichess-org/chess-openings

## Visual

Advance Variation subtree of the French Defense:

![subtree](test.png)

## Example

```go   
package main

import (
    "fmt"

    "github.com/corentings/chess"
    "github.com/corentings/chess/opening"
)

func main(){
    g := chess.NewGame()
	g.PushNotationMove("e4", chess.AlgebraicNotation{}, nil)
	g.PushNotationMove("e6", chess.AlgebraicNotation{}, nil)

	// print French Defense
	book := opening.NewBookECO()
	o := book.Find(g.Moves())
	fmt.Println(o.Title())
}
```