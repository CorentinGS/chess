package chess

import (
	"cmp"
	"errors"
	"fmt"
)

// PushMove adds a move in algebraic notation to the game.
// Returns an error if the move is invalid.
// This method now validates moves for consistency with other move methods.
//
// Example:
//
//	node, err := game.PushMove("e4", &MoveInsertOptions{PromoteToMainLine: true})
func (g *Game) PushMove(algebraicMove string, options *MoveInsertOptions) (*MoveNode, error) {
	return g.PushMoveText(algebraicMove, SAN(), options)
}

// PushMoveText adds a move to the game using an explicit move text codec.
// It decodes against the current position so the inserted move carries
// position-derived tags.
func (g *Game) PushMoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
	move, err := codec.Decode(g.currentPosition(), moveText)
	if err != nil {
		return nil, fmt.Errorf("chess: decode %s move text %q: %w", codec, moveText, err)
	}

	return g.Move(move, options)
}

// UnsafePushMoveText adds fully specified move text without legal move
// verification. It supports only codecs that can raw-decode a move without SAN
// resolution.
func (g *Game) UnsafePushMoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
	raw, err := codec.DecodeRaw(moveText)
	if err != nil {
		if errors.Is(err, ErrMoveTextUnsupportedRawDecode) {
			return nil, fmt.Errorf("%w: %s", ErrUnsafeMoveTextUnsupported, codec)
		}
		return nil, fmt.Errorf("chess: decode raw %s move text %q: %w", codec, moveText, err)
	}

	move := raw.Move()
	if !move.HasTag(Null) {
		pos := g.currentPosition()
		if pos == nil {
			return nil, ErrMoveTextMissingPosition
		}
		move.tags = moveTags(move, pos)
	}

	return g.UnsafeMove(move, options)
}

// Move method adds a move to the game using a Move struct.
// It returns an error if the move is invalid.
// This method validates the move before adding it to ensure game correctness.
// For high-performance scenarios where moves are pre-validated, use UnsafeMove.
//
// Example:
//
//	possibleMove := game.ValidMoves()[0]
//
//	err := game.Move(possibleMove, nil)
//	if err != nil {
//	    panic(err)
//	}
func (g *Game) Move(move Move, options *MoveInsertOptions) (*MoveNode, error) {
	options = cmp.Or(options, &MoveInsertOptions{})

	// Validate the move before adding it
	if err := g.validateMove(move); err != nil {
		return nil, err
	}

	return g.moveUnchecked(move, options)
}

// UnsafeMove adds a move to the game without validation.
// This method is intended for high-performance scenarios where moves are known to be valid.
// Use this method only when you have already validated the move or are certain it's legal.
// For general use, prefer the Move method which includes validation.
//
// Example:
//
//	// Only use when you're certain the move is valid
//	validMoves := game.ValidMoves()
//	move := validMoves[0] // We know this is valid
//	err := game.UnsafeMove(move, nil)
//	if err != nil {
//	    panic(err) // Should not happen with valid moves
//	}
func (g *Game) UnsafeMove(move Move, options *MoveInsertOptions) (*MoveNode, error) {
	options = cmp.Or(options, &MoveInsertOptions{})

	return g.moveUnchecked(move, options)
}

// moveUnchecked is the internal implementation that performs the move without validation.
// This is shared by both Move (after validation) and MoveUnchecked.
func (g *Game) moveUnchecked(move Move, options *MoveInsertOptions) (*MoveNode, error) {
	if g.outcome != NoOutcome {
		return nil, ErrGameAlreadyEnded
	}

	node, err := g.tree.addMove(move, options)
	if err != nil {
		return nil, err
	}

	g.evaluatePositionStatus()

	return node, nil
}

// NullMove appends a null move (a side-to-move flip with no piece movement)
// to the current position's main line. Null moves are never part of the
// legal moves returned by ValidMoves, so they cannot be inserted via Game.Move.
// Use this method or Game.UnsafeMove(NullMove()) to add one explicitly.
//
// Pass nil (or no argument) to use default options.
func (g *Game) NullMove(options ...*MoveInsertOptions) (*MoveNode, error) {
	var opts *MoveInsertOptions
	if len(options) > 0 {
		opts = options[0]
	}
	return g.insertByMove(NewNullMove(), opts)
}

// insertByMove is shared by UnsafeMove and NullMove. It validates only the
// structural preconditions (game in progress) and lets moveUnchecked handle
// tree wiring and position updates.
func (g *Game) insertByMove(move Move, options *MoveInsertOptions) (*MoveNode, error) {
	options = cmp.Or(options, &MoveInsertOptions{})
	return g.moveUnchecked(move, options)
}

// validateMove checks if the given move is valid for the current position.
// It returns an error if the move is invalid.
func (g *Game) validateMove(move Move) error {
	pos := g.currentPosition()
	if pos == nil {
		return errors.New("no current position")
	}

	// Null moves are never part of a position's legal moves; they must
	// be inserted via UnsafeMove or NullMove.
	if move.HasTag(Null) {
		return fmt.Errorf("null move %s is not valid for the current position", move.String())
	}

	// Check if the move exists in the list of valid moves for the current position
	validMoves := pos.ValidMovesUnsafe()
	for _, validMove := range validMoves {
		if validMove.s1 == move.s1 && validMove.s2 == move.s2 && validMove.promo == move.promo {
			return nil // Move is valid
		}
	}

	return fmt.Errorf("move %s is not valid for the current position", move.String())
}

// ValidateSAN checks if a string is valid Standard Algebraic notation (SAN) syntax.
// This function only validates the syntax, not whether the move is legal in any position.
// Examples of valid SAN: "e4", "Nf3", "O-O", "Qxd2+", "e8=Q#".
func ValidateSAN(s string) error {
	_, err := algebraicNotationParts(s)
	return err
}
