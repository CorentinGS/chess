package chess_test

import (
	"errors"
	"testing"

	"github.com/corentings/chess/v3"
)

func TestMoveTextCodecZeroValueIsStrictSAN(t *testing.T) {
	var zero chess.MoveTextCodec

	if zero != chess.SAN() {
		t.Fatalf("zero MoveTextCodec = %#v, want SAN()", zero)
	}
	if got := zero.Format(); got != chess.MoveTextFormatSAN {
		t.Fatalf("zero Format() = %v, want %v", got, chess.MoveTextFormatSAN)
	}
	if got := zero.Policy(); got != chess.MoveTextPolicyStrict {
		t.Fatalf("zero Policy() = %v, want %v", got, chess.MoveTextPolicyStrict)
	}
	if got := zero.String(); got != "SAN" {
		t.Fatalf("zero String() = %q, want %q", got, "SAN")
	}
}

func TestMoveTextCodecConstructors(t *testing.T) {
	tests := []struct {
		name       string
		codec      chess.MoveTextCodec
		wantFormat chess.MoveTextFormat
		wantPolicy chess.MoveTextPolicy
		wantString string
	}{
		{
			name:       "SAN",
			codec:      chess.SAN(),
			wantFormat: chess.MoveTextFormatSAN,
			wantPolicy: chess.MoveTextPolicyStrict,
			wantString: "SAN",
		},
		{
			name:       "PGN import SAN",
			codec:      chess.PGNImportSAN(),
			wantFormat: chess.MoveTextFormatSAN,
			wantPolicy: chess.MoveTextPolicyPGNImport,
			wantString: "PGNImportSAN",
		},
		{
			name:       "long algebraic",
			codec:      chess.LongAlgebraic(),
			wantFormat: chess.MoveTextFormatLongAlgebraic,
			wantPolicy: chess.MoveTextPolicyStrict,
			wantString: "LongAlgebraic",
		},
		{
			name:       "UCI",
			codec:      chess.UCI(),
			wantFormat: chess.MoveTextFormatUCI,
			wantPolicy: chess.MoveTextPolicyStrict,
			wantString: "UCI",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.codec.Format(); got != tt.wantFormat {
				t.Fatalf("Format() = %v, want %v", got, tt.wantFormat)
			}
			if got := tt.codec.Policy(); got != tt.wantPolicy {
				t.Fatalf("Policy() = %v, want %v", got, tt.wantPolicy)
			}
			if got := tt.codec.String(); got != tt.wantString {
				t.Fatalf("String() = %q, want %q", got, tt.wantString)
			}
		})
	}
}

func TestMoveTextCodecSANEncodeDecode(t *testing.T) {
	pos := chess.StartingPosition()
	rawMove, err := chess.UCI().DecodeRaw("e2e4")
	if err != nil {
		t.Fatalf("DecodeRaw() error = %v", err)
	}
	staleMove := rawMove.Move()

	encoded, err := chess.SAN().Encode(pos, staleMove)
	if err != nil {
		t.Fatalf("Encode() error = %v", err)
	}
	if encoded != "e4" {
		t.Fatalf("Encode() = %q, want %q", encoded, "e4")
	}

	decoded, err := chess.SAN().Decode(pos, encoded)
	if err != nil {
		t.Fatalf("Decode() error = %v", err)
	}
	if decoded != staleMove {
		t.Fatalf("Decode() = %v, want %v", decoded, staleMove)
	}
}

func TestMoveTextCodecRequiresPositionForResolution(t *testing.T) {
	_, err := chess.SAN().Decode(nil, "e4")
	if !errors.Is(err, chess.ErrMoveTextMissingPosition) {
		t.Fatalf("Decode(nil) error = %v, want ErrMoveTextMissingPosition", err)
	}
}

func TestMoveTextCodecDecodeRejectsIllegalCoordinateMove(t *testing.T) {
	_, err := chess.UCI().Decode(chess.StartingPosition(), "e2e5")
	if !errors.Is(err, chess.ErrInvalidMoveText) {
		t.Fatalf("UCI Decode() error = %v, want ErrInvalidMoveText", err)
	}
}

func TestPGNImportSANCodecAcceptsMissingPromotionEquals(t *testing.T) {
	opt, err := chess.FEN("6k1/4P3/8/8/8/8/8/4K3 w - - 0 1")
	if err != nil {
		t.Fatalf("FEN() error = %v", err)
	}
	pos := chess.NewGame(opt).Position()

	if _, err := chess.SAN().Decode(pos, "e8Q"); !errors.Is(err, chess.ErrInvalidMoveText) {
		t.Fatalf("strict SAN Decode() error = %v, want ErrInvalidMoveText", err)
	}

	move, err := chess.PGNImportSAN().Decode(pos, "e8Q")
	if err != nil {
		t.Fatalf("PGNImportSAN Decode() error = %v", err)
	}
	encoded, err := chess.SAN().Encode(pos, move)
	if err != nil {
		t.Fatalf("Encode() error = %v", err)
	}
	if encoded != "e8=Q+" {
		t.Fatalf("canonical SAN = %q, want %q", encoded, "e8=Q+")
	}
}

func TestMoveTextCodecDecodeRaw(t *testing.T) {
	rawUCI, err := chess.UCI().DecodeRaw("e7e8q")
	if err != nil {
		t.Fatalf("UCI DecodeRaw() error = %v", err)
	}
	if rawUCI.From() != chess.E7 || rawUCI.To() != chess.E8 || rawUCI.Promotion() != chess.Queen {
		t.Fatalf("UCI DecodeRaw() = %v-%v %v, want e7-e8 Queen", rawUCI.From(), rawUCI.To(), rawUCI.Promotion())
	}

	rawLong, err := chess.LongAlgebraic().DecodeRaw("Ne2xf4")
	if err != nil {
		t.Fatalf("LongAlgebraic DecodeRaw() error = %v", err)
	}
	if rawLong.From() != chess.E2 || rawLong.To() != chess.F4 || rawLong.Promotion() != chess.NoPieceType {
		t.Fatalf("LongAlgebraic DecodeRaw() = %v-%v %v, want e2-f4 no promotion", rawLong.From(), rawLong.To(), rawLong.Promotion())
	}

	_, err = chess.SAN().DecodeRaw("Nf3")
	if !errors.Is(err, chess.ErrMoveTextUnsupportedRawDecode) {
		t.Fatalf("SAN DecodeRaw() error = %v, want ErrMoveTextUnsupportedRawDecode", err)
	}
}
