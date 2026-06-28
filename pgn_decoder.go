package chess

import (
	"io"
	"iter"
)

// PGNOption configures PGN import.
type PGNOption func(*pgnOptions)

type pgnOptions struct {
	expandVariations bool
}

// WithPGNExpandVariations expands each Game's Variations into separate Games.
func WithPGNExpandVariations() PGNOption {
	return func(o *pgnOptions) {
		o.expandVariations = true
	}
}

// PGNDecoder streams Games from a PGN reader.
type PGNDecoder struct {
	scanner *Scanner
	index   int64
	offset  int64
}

// NewPGNDecoder creates a decoder that reads Games from r.
func NewPGNDecoder(r io.Reader, opts ...PGNOption) *PGNDecoder {
	options := pgnOptions{}
	for _, opt := range opts {
		opt(&options)
	}

	scannerOpts := make([]ScannerOption, 0, 1)
	if options.expandVariations {
		scannerOpts = append(scannerOpts, WithExpandVariations())
	}

	return &PGNDecoder{scanner: NewScanner(r, scannerOpts...)}
}

// Decode returns the next Game from the PGN stream.
func (d *PGNDecoder) Decode() (*Game, error) {
	game, err := d.scanner.ParseNext()
	if err != nil {
		return nil, err
	}
	d.index++
	return game, nil
}

// Index returns the number of Games successfully decoded by this decoder.
func (d *PGNDecoder) Index() int64 {
	return d.index
}

// Offset returns the byte offset of the current PGN record when available.
func (d *PGNDecoder) Offset() int64 {
	return d.offset
}

// ParsePGN parses the first Game from r.
func ParsePGN(r io.Reader, opts ...PGNOption) (*Game, error) {
	return NewPGNDecoder(r, opts...).Decode()
}

// PGNGames returns an iterator over Games decoded from r.
func PGNGames(r io.Reader, opts ...PGNOption) iter.Seq2[*Game, error] {
	return func(yield func(*Game, error) bool) {
		dec := NewPGNDecoder(r, opts...)
		for {
			game, err := dec.Decode()
			if err == io.EOF {
				return
			}
			if !yield(game, err) || err != nil {
				return
			}
		}
	}
}
