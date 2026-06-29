package chess

import (
	"errors"
	"fmt"
	"strings"
)

var (
	// ErrInvalidMoveText indicates syntactically invalid or unresolvable move text.
	ErrInvalidMoveText = errors.New("chess: invalid move text")
	// ErrMoveTextMissingPosition indicates that a codec operation needs a position.
	ErrMoveTextMissingPosition = errors.New("chess: move text requires position")
	// ErrMoveTextUnsupportedRawDecode indicates that a format cannot be decoded
	// without a position.
	ErrMoveTextUnsupportedRawDecode = errors.New("chess: move text raw decode unsupported")
	// ErrInvalidMoveTextCodec indicates an impossible codec format/policy pair.
	ErrInvalidMoveTextCodec = errors.New("chess: invalid move text codec")
	// ErrUnsafeMoveTextUnsupported indicates that a codec cannot be used with
	// unsafe move-text insertion.
	ErrUnsafeMoveTextUnsupported = errors.New("chess: unsafe move text unsupported")
)

// MoveTextFormat identifies the concrete move text grammar handled by a codec.
type MoveTextFormat uint8

const (
	// MoveTextFormatSAN is strict Standard Algebraic notation.
	MoveTextFormatSAN MoveTextFormat = iota
	// MoveTextFormatLongAlgebraic is coordinate-expanded algebraic notation.
	MoveTextFormatLongAlgebraic
	// MoveTextFormatUCI is Universal Chess Interface coordinate notation.
	MoveTextFormatUCI
)

// MoveTextPolicy identifies how permissive a codec is while reading move text.
type MoveTextPolicy uint8

const (
	// MoveTextPolicyStrict accepts only generated canonical text.
	MoveTextPolicyStrict MoveTextPolicy = iota
	// MoveTextPolicyPGNImport accepts documented PGN import spellings and
	// canonicalises them when writing.
	MoveTextPolicyPGNImport
)

// MoveTextCodec parses, resolves, validates, and formats move text.
//
// The zero value is the strict SAN codec returned by SAN().
type MoveTextCodec struct {
	format MoveTextFormat
	policy MoveTextPolicy
}

// RawMoveText is unclassified move data decoded without a board position.
type RawMoveText struct {
	from      Square
	to        Square
	promotion PieceType
	null      bool
}

// Move returns the raw move as a Move without legality-derived tags.
func (m RawMoveText) Move() Move {
	if m.null {
		return NewNullMove()
	}
	return Move{s1: m.from, s2: m.to, promo: m.promotion}
}

// From returns the raw origin square.
func (m RawMoveText) From() Square {
	return m.from
}

// To returns the raw destination square.
func (m RawMoveText) To() Square {
	return m.to
}

// Promotion returns the raw promotion piece type.
func (m RawMoveText) Promotion() PieceType {
	return m.promotion
}

// Null reports whether the raw move is a null move.
func (m RawMoveText) Null() bool {
	return m.null
}

// SAN returns the strict Standard Algebraic notation codec.
func SAN() MoveTextCodec {
	return MoveTextCodec{}
}

// PGNImportSAN returns the permissive SAN codec used by PGN import.
func PGNImportSAN() MoveTextCodec {
	return MoveTextCodec{
		format: MoveTextFormatSAN,
		policy: MoveTextPolicyPGNImport,
	}
}

// LongAlgebraic returns the coordinate-expanded algebraic notation codec.
func LongAlgebraic() MoveTextCodec {
	return MoveTextCodec{format: MoveTextFormatLongAlgebraic}
}

// UCI returns the Universal Chess Interface coordinate notation codec.
func UCI() MoveTextCodec {
	return MoveTextCodec{format: MoveTextFormatUCI}
}

// Format returns the codec's move text format.
func (c MoveTextCodec) Format() MoveTextFormat {
	return c.format
}

// Policy returns the codec's parsing policy.
func (c MoveTextCodec) Policy() MoveTextPolicy {
	return c.policy
}

// String implements fmt.Stringer.
func (c MoveTextCodec) String() string {
	switch c {
	case SAN():
		return "SAN"
	case PGNImportSAN():
		return "PGNImportSAN"
	case LongAlgebraic():
		return "LongAlgebraic"
	case UCI():
		return "UCI"
	default:
		return "InvalidMoveTextCodec"
	}
}

// Encode formats a move from the supplied position. Tag-dependent suffixes
// (check, capture, disambiguation) are recomputed from the resulting position
// rather than trusted from the input move; Decode returns already-tagged
// moves from the legal-move generator and does not recompute.
func (c MoveTextCodec) Encode(pos *Position, m Move) (string, error) {
	if err := c.validate(); err != nil {
		return "", err
	}

	switch c.format {
	case MoveTextFormatSAN:
		if m.HasTag(Null) {
			return "Z0", nil
		}
		if pos == nil {
			return "", ErrMoveTextMissingPosition
		}
		m.tags = moveTags(m, pos)
		return algebraicNotation{}.Encode(pos, m), nil
	case MoveTextFormatLongAlgebraic:
		if m.HasTag(Null) {
			return "0000", nil
		}
		if pos == nil {
			return "", ErrMoveTextMissingPosition
		}
		m.tags = moveTags(m, pos)
		return longAlgebraicNotation{}.Encode(pos, m), nil
	case MoveTextFormatUCI:
		return uciNotation{}.Encode(pos, m), nil
	default:
		return "", ErrInvalidMoveTextCodec
	}
}

// Decode resolves move text against the supplied position.
func (c MoveTextCodec) Decode(pos *Position, s string) (Move, error) {
	if err := c.validate(); err != nil {
		return Move{}, err
	}
	if pos == nil {
		return Move{}, ErrMoveTextMissingPosition
	}

	var (
		move Move
		err  error
	)
	switch c.format {
	case MoveTextFormatSAN:
		if c.policy == MoveTextPolicyPGNImport {
			s = normaliseImportSAN(s)
		}
		move, err = algebraicNotation{}.Decode(pos, s)
	case MoveTextFormatLongAlgebraic:
		move, err = longAlgebraicNotation{}.Decode(pos, s)
	case MoveTextFormatUCI:
		move, err = uciNotation{}.Decode(pos, s)
	default:
		return Move{}, ErrInvalidMoveTextCodec
	}
	if err != nil {
		return Move{}, fmt.Errorf("%w: %w", ErrInvalidMoveText, err)
	}
	if !move.HasTag(Null) {
		if err := validatePositionMove(pos, move); err != nil {
			return Move{}, fmt.Errorf("%w: %w", ErrInvalidMoveText, err)
		}
	}
	return move, nil
}

// DecodeRaw decodes fully specified move text without a position.
func (c MoveTextCodec) DecodeRaw(s string) (RawMoveText, error) {
	if err := c.validate(); err != nil {
		return RawMoveText{}, err
	}

	switch c.format {
	case MoveTextFormatUCI:
		return decodeRawUCI(s)
	case MoveTextFormatLongAlgebraic:
		return decodeRawLongAlgebraic(s)
	default:
		return RawMoveText{}, ErrMoveTextUnsupportedRawDecode
	}
}

// ValidateSyntax reports whether text matches the codec grammar without
// checking move legality.
func (c MoveTextCodec) ValidateSyntax(s string) error {
	if err := c.validate(); err != nil {
		return err
	}

	switch c.format {
	case MoveTextFormatSAN:
		if c.policy == MoveTextPolicyPGNImport {
			s = normaliseImportSAN(s)
		}
		if _, err := algebraicNotationParts(s); err != nil {
			return fmt.Errorf("%w: %w", ErrInvalidMoveText, err)
		}
		return nil
	case MoveTextFormatLongAlgebraic, MoveTextFormatUCI:
		_, err := c.DecodeRaw(s)
		return err
	default:
		return ErrInvalidMoveTextCodec
	}
}

func (c MoveTextCodec) validate() error {
	switch c.format {
	case MoveTextFormatSAN:
		if c.policy == MoveTextPolicyStrict || c.policy == MoveTextPolicyPGNImport {
			return nil
		}
	case MoveTextFormatLongAlgebraic, MoveTextFormatUCI:
		if c.policy == MoveTextPolicyStrict {
			return nil
		}
	}
	return ErrInvalidMoveTextCodec
}

func decodeRawUCI(s string) (RawMoveText, error) {
	move, err := uciNotation{}.Decode(nil, s)
	if err != nil {
		return RawMoveText{}, fmt.Errorf("%w: %w", ErrInvalidMoveText, err)
	}
	if move.HasTag(Null) {
		return RawMoveText{null: true}, nil
	}
	return RawMoveText{
		from:      move.S1(),
		to:        move.S2(),
		promotion: move.Promo(),
	}, nil
}

func decodeRawLongAlgebraic(s string) (RawMoveText, error) {
	if s == "0000" {
		return RawMoveText{null: true}, nil
	}

	text := strings.TrimSuffix(strings.TrimSuffix(s, "+"), "#")
	if len(text) > 0 && strings.Contains(text, "x") {
		text = strings.Replace(text, "x", "", 1)
	}
	if len(text) > 0 && strings.Contains(text, "=") {
		text = strings.Replace(text, "=", "", 1)
	}
	if len(text) == 5 && isAlgebraicPieceLetter(text[0]) {
		text = text[1:]
	}

	if len(text) != 4 && len(text) != 5 {
		return RawMoveText{}, fmt.Errorf("%w: invalid long algebraic move %q", ErrInvalidMoveText, s)
	}
	if !isSquareText(text[:2]) || !isSquareText(text[2:4]) {
		return RawMoveText{}, fmt.Errorf("%w: invalid long algebraic squares %q", ErrInvalidMoveText, s)
	}

	promotion := NoPieceType
	if len(text) == 5 {
		promotion = algebraicPromotionPiece(text[4])
		if promotion == NoPieceType {
			return RawMoveText{}, fmt.Errorf("%w: invalid long algebraic promotion %q", ErrInvalidMoveText, s)
		}
	}

	return RawMoveText{
		from:      squareFromFileRank(text[0], text[1]),
		to:        squareFromFileRank(text[2], text[3]),
		promotion: promotion,
	}, nil
}

func normaliseImportSAN(s string) string {
	if len(s) < 3 {
		return s
	}

	suffix := ""
	for len(s) > 0 {
		switch {
		case strings.HasSuffix(s, "+"):
			suffix = "+" + suffix
			s = strings.TrimSuffix(s, "+")
		case strings.HasSuffix(s, "#"):
			suffix = "#" + suffix
			s = strings.TrimSuffix(s, "#")
		case strings.HasSuffix(s, "!"), strings.HasSuffix(s, "?"):
			s = s[:len(s)-1]
		case strings.HasSuffix(s, "e.p."):
			s = strings.TrimSuffix(s, "e.p.")
		default:
			goto done
		}
	}

done:
	if len(s) >= 3 && s[len(s)-2] >= '1' && s[len(s)-2] <= '8' && algebraicPromotionPiece(s[len(s)-1]) != NoPieceType {
		s = s[:len(s)-1] + "=" + s[len(s)-1:]
	}
	return s + suffix
}

func isSquareText(s string) bool {
	return len(s) == 2 && s[0] >= 'a' && s[0] <= 'h' && s[1] >= '1' && s[1] <= '8'
}

func isAlgebraicPieceLetter(b byte) bool {
	switch b {
	case 'K', 'Q', 'R', 'B', 'N':
		return true
	default:
		return false
	}
}

func algebraicPromotionPiece(b byte) PieceType {
	switch b {
	case 'Q':
		return Queen
	case 'R':
		return Rook
	case 'B':
		return Bishop
	case 'N':
		return Knight
	default:
		return NoPieceType
	}
}
