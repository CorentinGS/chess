package chess

import (
	"bytes"
	"context"
	"io"
	"iter"
	"runtime"
	"strings"
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

// Tags returns the PGN tag pairs in this record without building a Game.
func (r PGNRecord) Tags() (map[string]string, error) {
	tokens, err := TokenizeGame(&GameScanned{Raw: string(r.Raw)})
	if err != nil {
		return nil, err
	}

	tags := make(map[string]string)
	for i := 0; i < len(tokens); i++ {
		if tokens[i].Type == EOF {
			break
		}
		if tokens[i].Type != TagStart {
			continue
		}
		if i+3 >= len(tokens) || tokens[i+1].Type != TagKey || tokens[i+2].Type != TagValue {
			continue
		}
		tags[tokens[i+1].Value] = tokens[i+2].Value
		i += 3
	}
	return tags, nil
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
				results <- PGNResult{Err: err}
				return
			}
			select {
			case jobs <- record:
			case <-ctx.Done():
				results <- PGNResult{Err: ctx.Err()}
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

// PGNEventKind identifies a semantic PGN event.
type PGNEventKind uint8

const (
	PGNEventUnknown PGNEventKind = iota
	PGNTag
	PGNMove
	PGNComment
	PGNNAG
	PGNVariationStart
	PGNVariationEnd
	PGNGameEnd
)

// PGNEvent is one semantic event from a PGN stream.
type PGNEvent struct {
	Kind   PGNEventKind
	Index  int64
	Offset int64
	Name   string
	Value  string
	Move   string
	NAG    string
}

// PGNEvents returns an iterator over semantic PGN events without building Games.
func PGNEvents(r io.Reader, opts ...PGNOption) iter.Seq2[PGNEvent, error] {
	return func(yield func(PGNEvent, error) bool) {
		for record, err := range PGNRecords(context.Background(), r, opts...) {
			if err != nil {
				yield(PGNEvent{}, err)
				return
			}
			if !yieldPGNRecordEvents(record, yield) {
				return
			}
		}
	}
}

func yieldPGNRecordEvents(record PGNRecord, yield func(PGNEvent, error) bool) bool {
	tokens, err := TokenizeGame(&GameScanned{Raw: string(record.Raw)})
	if err != nil {
		return yield(PGNEvent{}, err)
	}

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]
		event := PGNEvent{Index: record.Index, Offset: record.Offset}
		switch token.Type {
		case EOF:
			return yield(PGNEvent{Kind: PGNGameEnd, Index: record.Index, Offset: record.Offset}, nil)
		case TagStart:
			if i+2 < len(tokens) && tokens[i+1].Type == TagKey && tokens[i+2].Type == TagValue {
				event.Kind = PGNTag
				event.Name = tokens[i+1].Value
				event.Value = tokens[i+2].Value
				i += 2
				if !yield(event, nil) {
					return false
				}
			}
		case CommentStart:
			comment, next := collectPGNComment(tokens, i+1)
			i = next
			if comment != "" {
				event.Kind = PGNComment
				event.Value = comment
				if !yield(event, nil) {
					return false
				}
			}
		case NAG:
			event.Kind = PGNNAG
			event.NAG = token.Value
			if !yield(event, nil) {
				return false
			}
		case VariationStart:
			event.Kind = PGNVariationStart
			if !yield(event, nil) {
				return false
			}
		case VariationEnd:
			event.Kind = PGNVariationEnd
			if !yield(event, nil) {
				return false
			}
		default:
			if isPGNMoveToken(token.Type) {
				move, next := collectPGNMove(tokens, i)
				i = next - 1
				event.Kind = PGNMove
				event.Move = move
				if !yield(event, nil) {
					return false
				}
			}
		}
	}

	return yield(PGNEvent{Kind: PGNGameEnd, Index: record.Index, Offset: record.Offset}, nil)
}

func collectPGNComment(tokens []Token, start int) (string, int) {
	var b strings.Builder
	for i := start; i < len(tokens); i++ {
		switch tokens[i].Type {
		case COMMENT:
			b.WriteString(tokens[i].Value)
			if i+1 < len(tokens) && tokens[i+1].Type == CommandStart && b.Len() > 0 {
				b.WriteByte(' ')
			}
		case CommentEnd:
			return b.String(), i
		}
	}
	return b.String(), len(tokens)
}

func collectPGNMove(tokens []Token, start int) (string, int) {
	var b strings.Builder
	for i := start; i < len(tokens); i++ {
		if !isPGNMoveToken(tokens[i].Type) {
			return b.String(), i
		}
		b.WriteString(tokens[i].Value)
	}
	return b.String(), len(tokens)
}

func isPGNMoveToken(tokenType TokenType) bool {
	switch tokenType {
	case PIECE, SQUARE, CAPTURE, FILE, RANK, KingsideCastle, QueensideCastle,
		PROMOTION, PromotionPiece, CHECK, CHECKMATE, DeambiguationSquare, NullMove:
		return true
	default:
		return false
	}
}
