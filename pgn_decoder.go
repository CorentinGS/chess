package chess

import (
	"context"
	"errors"
	"io"
	"iter"
	"runtime"
	"strings"
	"sync"
)

// PGNOption configures PGN decoding.
type PGNOption func(*pgnOptions)

type pgnOptions struct {
	bufferSize       int
	expandVariations bool
	moveTextCodec    MoveTextCodec
}

// WithPGNExpandVariations expands each Game's Variations into separate Games.
func WithPGNExpandVariations() PGNOption {
	return func(o *pgnOptions) {
		o.expandVariations = true
	}
}

// WithPGNBufferSize configures the reader buffer size used for PGN record framing.
func WithPGNBufferSize(n int) PGNOption {
	return func(o *pgnOptions) {
		if n > 0 {
			o.bufferSize = n
		}
	}
}

// WithStrictSAN requires PGN movetext to use strict canonical SAN.
func WithStrictSAN() PGNOption {
	return func(o *pgnOptions) {
		o.moveTextCodec = SAN()
	}
}

// WithImportSAN accepts documented PGN import SAN spellings.
func WithImportSAN() PGNOption {
	return func(o *pgnOptions) {
		o.moveTextCodec = PGNImportSAN()
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
	return &PGNDecoder{framer: newPGNFramer(r, opts...), options: opts}
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
	d.index++
	d.offset = record.Offset

	options := applyPGNOptions(d.options)

	game, err := parsePGNText(record.Raw, options)
	if err != nil {
		return nil, err
	}

	if options.expandVariations {
		games := game.Split()
		if len(games) == 0 {
			return nil, io.EOF
		}
		d.parsedGames = games[1:]
		game = games[0]
	}

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
			if errors.Is(err, io.EOF) {
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
	Raw    string
}

// Decode parses this record into a Game.
func (r PGNRecord) Decode(opts ...PGNOption) (*Game, error) {
	return parsePGNText(r.Raw, applyPGNOptions(opts))
}

// Tags returns the PGN tag pairs in this record without building a Game.
func (r PGNRecord) Tags() (map[string]string, error) {
	lex := NewLexer(r.Raw)
	tags := make(map[string]string)
	for {
		tok := lex.NextToken()
		if tok.Type == EOF || tok.Type != TagStart {
			return tags, nil
		}
		key := lex.NextToken()
		val := lex.NextToken()
		end := lex.NextToken()
		if end.Type == TagEnd && key.Type == TagKey && val.Type == TagValue {
			tags[key.Value] = val.Value
		}
	}
}

// PGNRecords returns an iterator over raw PGN records.
func PGNRecords(ctx context.Context, r io.Reader, opts ...PGNOption) iter.Seq2[PGNRecord, error] {
	return func(yield func(PGNRecord, error) bool) {
		framer := newPGNFramer(r, opts...)
		for {
			if err := ctx.Err(); err != nil {
				yield(PGNRecord{}, err)
				return
			}

			record, err := framer.next()
			if errors.Is(err, io.EOF) {
				return
			}
			if !yield(record, err) || err != nil {
				return
			}
		}
	}
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
		for record, err := range PGNRecords(ctx, r, opts.Options...) {
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
	// Index zero or negative results (typically record-stream errors from
	// the framer goroutine) are emitted immediately without ordering.
	// Workers may still be decoding in-flight records when an error arrives;
	// their results will be dropped because the results channel closes after
	// this function returns.
	next := int64(1)
	pending := make(map[int64]PGNResult)
	for result := range results {
		if result.Index <= 0 {
			if !sendPGNResult(ctx, out, result) {
				return
			}
			continue
		}
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
	lex := NewLexer(record.Raw)
	var pending *Token
	for {
		var tok Token
		if pending != nil {
			tok = *pending
			pending = nil
		} else {
			tok = lex.NextToken()
		}
		if tok.Type == EOF {
			return yield(PGNEvent{Kind: PGNGameEnd, Index: record.Index, Offset: record.Offset}, nil)
		}
		event := PGNEvent{Index: record.Index, Offset: record.Offset}
		switch tok.Type {
		case TagStart:
			key := lex.NextToken()
			val := lex.NextToken()
			lex.NextToken() // TagEnd
			if key.Type == TagKey && val.Type == TagValue {
				event.Kind = PGNTag
				event.Name = key.Value
				event.Value = val.Value
				if !yield(event, nil) {
					return false
				}
			}
		case CommentStart:
			comment := collectPGNComment(lex)
			if comment != "" {
				event.Kind = PGNComment
				event.Value = comment
				if !yield(event, nil) {
					return false
				}
			}
		case NAG:
			event.Kind = PGNNAG
			event.NAG = tok.Value
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
			if isPGNMoveToken(tok.Type) {
				move, terminator := collectPGNMove(lex, tok)
				event.Kind = PGNMove
				event.Move = move
				if !yield(event, nil) {
					return false
				}
				pending = &terminator
			}
		}
	}
}

func collectPGNComment(lex *Lexer) string {
	var b strings.Builder
	lastWasComment := false
	for {
		tok := lex.NextToken()
		switch tok.Type {
		case COMMENT:
			b.WriteString(tok.Value)
			lastWasComment = true
		case CommandStart:
			if lastWasComment && b.Len() > 0 {
				b.WriteByte(' ')
			}
			lastWasComment = false
		case CommentEnd:
			return b.String()
		case EOF:
			return b.String()
		}
	}
}

func collectPGNMove(lex *Lexer, first Token) (string, Token) {
	var b strings.Builder
	b.WriteString(first.Value)
	for {
		tok := lex.NextToken()
		if !isPGNMoveToken(tok.Type) {
			return b.String(), tok
		}
		b.WriteString(tok.Value)
	}
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
