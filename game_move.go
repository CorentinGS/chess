package chess

import (
	"cmp"
	"errors"
	"fmt"
)

// MoveText adds a move to the game using an explicit move text codec.
// It decodes against the current position so the inserted move carries
// position-derived tags. The decoded Move is canonical (codec.Decode resolves
// legal moves), so it crosses the trusted fast path: no extra legal-move
// lookup is needed before insertion.
func (g *Game) MoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
	move, err := codec.Decode(g.currentPosition(), moveText)
	if err != nil {
		return nil, fmt.Errorf("chess: decode %s move text %q: %w", codec, moveText, err)
	}

	return g.insertCanonical(canonicalMoveFromCodec(move), options)
}

// UnsafeMoveText adds fully specified move text without legal move
// verification. It supports only codecs that can raw-decode a move without SAN
// resolution.
func (g *Game) UnsafeMoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
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
		tag, _ := newLegality(pos, generateLegalAnnotated).legal(move)
		move.tags = tag
	}

	return g.UnsafeMove(move, options)
}

// canonicalMoveFromCodec returns the codec-decoded Move, normalising any
// stray Null-tagged value through NewNullMove so the Move tree only ever
// stores the canonical Null shape.
func canonicalMoveFromCodec(m Move) Move {
	if m.HasTag(Null) {
		return NewNullMove()
	}
	return m
}

// insertCanonical inserts an already-resolved canonical Move without
// re-running the legal-move scan. Move tree repair-or-reject semantics still
// apply for existing Move occurrences with stale tags.
func (g *Game) insertCanonical(canonical Move, options *MoveInsertOptions) (*MoveNode, error) {
	options = cmp.Or(options, &MoveInsertOptions{})
	return g.moveUnchecked(canonical, options)
}

// PushMove adds a move in algebraic notation to the game.
// Returns an error if the move is invalid.
//
// Deprecated: use MoveText(algebraicMove, SAN(), options) instead.
func (g *Game) PushMove(algebraicMove string, options *MoveInsertOptions) (*MoveNode, error) {
	return g.MoveText(algebraicMove, SAN(), options)
}

// PushMoveText adds a move to the game using an explicit move text codec.
//
// Deprecated: use MoveText instead.
func (g *Game) PushMoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
	return g.MoveText(moveText, codec, options)
}

// UnsafePushMoveText adds fully specified move text without legal move
// verification.
//
// Deprecated: use UnsafeMoveText instead.
func (g *Game) UnsafePushMoveText(moveText string, codec MoveTextCodec, options *MoveInsertOptions) (*MoveNode, error) {
	return g.UnsafeMoveText(moveText, codec, options)
}

// Move method adds a move to the game using a Move struct.
// It returns an error if the move is invalid.
// This method validates the move before adding it to ensure game correctness.
// For high-performance scenarios where moves are pre-validated, use UnsafeMove.
//
// Null moves are rejected here: they are never part of ValidMovesUnsafe, so
// callers wanting to pass the side must use [Game.NullMove] or
// [Game.UnsafeMove] explicitly.
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

	if move.HasTag(Null) {
		return nil, errors.New("chess: null move is not valid for the current position")
	}

	canonical, err := resolveCanonicalMove(g.currentPosition(), move)
	if err != nil {
		return nil, err
	}

	return g.moveUnchecked(canonical, options)
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

// ValidateSAN checks if a string is valid Standard Algebraic notation (SAN) syntax.
// This function only validates the syntax, not whether the move is legal in any position.
// Examples of valid SAN: "e4", "Nf3", "O-O", "Qxd2+", "e8=Q#".
func ValidateSAN(s string) error {
	_, err := algebraicNotationParts(s)
	return err
}
