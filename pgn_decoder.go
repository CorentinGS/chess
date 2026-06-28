package chess

import (
	"bytes"
	"context"
	"io"
	"iter"
	"runtime"
	"sync"
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
	framer      *pgnFramer
	options     []PGNOption
	parsedGames []*Game
	index       int64
	offset      int64
}

// NewPGNDecoder creates a decoder that reads Games from r.
func NewPGNDecoder(r io.Reader, opts ...PGNOption) *PGNDecoder {
	return &PGNDecoder{framer: newPGNFramer(r), options: opts}
}

// Decode returns the next Game from the PGN stream.
func (d *PGNDecoder) Decode() (*Game, error) {
	if len(d.parsedGames) > 0 {
		game := d.parsedGames[0]
		d.parsedGames = d.parsedGames[1:]
		d.index++
		return game, nil
	}

	record, err := d.framer.next()
	if err != nil {
		return nil, err
	}
	d.offset = record.Offset

	game, err := record.Decode(d.options...)
	if err != nil {
		return nil, err
	}

	options := applyPGNOptions(d.options)
	if options.expandVariations {
		games := game.Split()
		if len(games) == 0 {
			return nil, io.EOF
		}
		d.parsedGames = games[1:]
		game = games[0]
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

// PGNRecord is one complete PGN game record from a stream.
type PGNRecord struct {
	Index  int64
	Offset int64
	Raw    []byte
}

// Decode parses this record into a Game.
func (r PGNRecord) Decode(opts ...PGNOption) (*Game, error) {
	game, err := parsePGNRecord(r.Raw)
	if err != nil {
		return nil, err
	}
	return game, nil
}

// PGNRecords returns an iterator over raw PGN records.
func PGNRecords(ctx context.Context, r io.Reader, _ ...PGNOption) iter.Seq2[PGNRecord, error] {
	return func(yield func(PGNRecord, error) bool) {
		framer := newPGNFramer(r)
		for {
			if err := ctx.Err(); err != nil {
				yield(PGNRecord{}, err)
				return
			}

			record, err := framer.next()
			if err == io.EOF {
				return
			}
			if !yield(record, err) || err != nil {
				return
			}
		}
	}
}

type pgnFramer struct {
	data    []byte
	pos     int
	index   int64
	readErr error
}

func newPGNFramer(r io.Reader) *pgnFramer {
	data, err := io.ReadAll(r)
	return &pgnFramer{data: data, readErr: err}
}

func (f *pgnFramer) next() (PGNRecord, error) {
	if f.readErr != nil {
		err := f.readErr
		f.readErr = nil
		return PGNRecord{}, err
	}
	for f.pos < len(f.data) {
		advance, token, err := splitPGNGames(f.data[f.pos:], true)
		if err != nil {
			return PGNRecord{}, err
		}
		if advance == 0 && token == nil {
			return PGNRecord{}, io.EOF
		}
		start := f.pos
		if len(token) > 0 {
			if rel := bytes.Index(f.data[f.pos:f.pos+advance], token); rel >= 0 {
				start = f.pos + rel
			}
		}
		f.pos += advance
		if len(token) == 0 {
			continue
		}
		f.index++
		return PGNRecord{Index: f.index, Offset: int64(start), Raw: append([]byte(nil), token...)}, nil
	}
	return PGNRecord{}, io.EOF
}

func parsePGNRecord(raw []byte) (*Game, error) {
	tokens, err := TokenizeGame(&GameScanned{Raw: string(raw)})
	if err != nil {
		return nil, err
	}
	return NewParser(tokens).Parse()
}

func applyPGNOptions(opts []PGNOption) pgnOptions {
	options := pgnOptions{}
	for _, opt := range opts {
		opt(&options)
	}
	return options
}

// PGNParallelOptions configures parallel PGN Game decoding.
type PGNParallelOptions struct {
	Workers int
	Buffer  int
	Ordered bool
	Options []PGNOption
}

// PGNResult is the result of decoding one PGN record.
type PGNResult struct {
	Game   *Game
	Err    error
	Index  int64
	Offset int64
}

// DecodePGNGamesParallel decodes PGN records across worker goroutines.
func DecodePGNGamesParallel(ctx context.Context, r io.Reader, opts PGNParallelOptions) <-chan PGNResult {
	if opts.Workers <= 0 {
		opts.Workers = runtime.GOMAXPROCS(0)
	}
	if opts.Buffer <= 0 {
		opts.Buffer = opts.Workers
	}

	out := make(chan PGNResult, opts.Buffer)
	jobs := make(chan PGNRecord, opts.Buffer)
	results := make(chan PGNResult, opts.Buffer)

	go func() {
		defer close(jobs)
		for record, err := range PGNRecords(ctx, r) {
			if err != nil {
				select {
				case results <- PGNResult{Err: err}:
				case <-ctx.Done():
				}
				return
			}
			select {
			case jobs <- record:
			case <-ctx.Done():
				sendPGNResult(ctx, out, PGNResult{Err: ctx.Err()})
				return
			}
		}
	}()

	var wg sync.WaitGroup
	wg.Add(opts.Workers)
	for range opts.Workers {
		go func() {
			defer wg.Done()
			for record := range jobs {
				game, err := record.Decode(opts.Options...)
				result := PGNResult{Game: game, Err: err, Index: record.Index, Offset: record.Offset}
				select {
				case results <- result:
				case <-ctx.Done():
					return
				}
			}
		}()
	}

	go func() {
		wg.Wait()
		close(results)
	}()

	go func() {
		defer close(out)
		if opts.Ordered {
			yieldOrderedPGNResults(ctx, out, results)
			return
		}
		for result := range results {
			if !sendPGNResult(ctx, out, result) {
				return
			}
		}
	}()

	return out
}

func sendPGNResult(ctx context.Context, out chan<- PGNResult, result PGNResult) bool {
	select {
	case out <- result:
		return true
	case <-ctx.Done():
		select {
		case out <- PGNResult{Err: ctx.Err()}:
		default:
		}
		return false
	}
}

func yieldOrderedPGNResults(ctx context.Context, out chan<- PGNResult, results <-chan PGNResult) {
	next := int64(1)
	pending := make(map[int64]PGNResult)
	for result := range results {
		pending[result.Index] = result
		for {
			ready, ok := pending[next]
			if !ok {
				break
			}
			delete(pending, next)
			if !sendPGNResult(ctx, out, ready) {
				return
			}
			next++
		}
	}
}
