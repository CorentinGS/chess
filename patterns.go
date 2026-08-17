package chess

import (
	"math/bits"
)

const (
	bbFileA bitboard = 9259542123273814144
	bbFileB bitboard = 4629771061636907072
	bbFileC bitboard = 2314885530818453536
	bbFileD bitboard = 1157442765409226768
	bbFileE bitboard = 578721382704613384
	bbFileF bitboard = 289360691352306692
	bbFileG bitboard = 144680345676153346
	bbFileH bitboard = 72340172838076673

	bbRank1 bitboard = 18374686479671623680
	bbRank2 bitboard = 71776119061217280
	bbRank3 bitboard = 280375465082880
	bbRank4 bitboard = 1095216660480
	bbRank5 bitboard = 4278190080
	bbRank6 bitboard = 16711680
	bbRank7 bitboard = 65280
	bbRank8 bitboard = 255
)

// bbForSquare returns the bitboard mask for the given square.
// This is a package-level function rather than a Square method because it
// accesses the package-level lookup table bbSquares.
func bbForSquare(sq Square) bitboard {
	return bbSquares[sq]
}

// Lookup tables for piece movement patterns and board masks.
//
//nolint:gochecknoglobals // this is a lookup table
var (
	bbKnightMoves = [64]bitboard{9077567998918656, 4679521487814656, 38368557762871296, 19184278881435648, 9592139440717824, 4796069720358912, 2257297371824128, 1128098930098176, 2305878468463689728, 1152939783987658752, 9799982666336960512, 4899991333168480256, 2449995666584240128, 1224997833292120064, 576469569871282176, 288234782788157440, 4620693356194824192, 11533718717099671552, 5802888705324613632, 2901444352662306816, 1450722176331153408, 725361088165576704, 362539804446949376, 145241105196122112, 18049583422636032, 45053588738670592, 22667534005174272, 11333767002587136, 5666883501293568, 2833441750646784, 1416171111120896, 567348067172352, 70506185244672, 175990581010432, 88545054707712, 44272527353856, 22136263676928, 11068131838464, 5531918402816, 2216203387392, 275414786112, 687463207072, 345879119952, 172939559976, 86469779988, 43234889994, 21609056261, 8657044482, 1075839008, 2685403152, 1351090312, 675545156, 337772578, 168886289, 84410376, 33816580, 4202496, 10489856, 5277696, 2638848, 1319424, 659712, 329728, 132096}

	bbKingMoves = [64]bitboard{4665729213955833856, 11592265440851656704, 5796132720425828352, 2898066360212914176, 1449033180106457088, 724516590053228544, 362258295026614272, 144959613005987840, 13853283560024178688, 16186183351374184448, 8093091675687092224, 4046545837843546112, 2023272918921773056, 1011636459460886528, 505818229730443264, 216739030602088448, 54114388906344448, 63227278716305408, 31613639358152704, 15806819679076352, 7903409839538176, 3951704919769088, 1975852459884544, 846636838289408, 211384331665408, 246981557485568, 123490778742784, 61745389371392, 30872694685696, 15436347342848, 7718173671424, 3307175149568, 825720045568, 964771708928, 482385854464, 241192927232, 120596463616, 60298231808, 30149115904, 12918652928, 3225468928, 3768639488, 1884319744, 942159872, 471079936, 235539968, 117769984, 50463488, 12599488, 14721248, 7360624, 3680312, 1840156, 920078, 460039, 197123, 49216, 57504, 28752, 14376, 7188, 3594, 1797, 770}

	bbSquares    = [64]bitboard{}
	alignedMasks = [64]bitboard{}
	betweenMasks = [64][64]bitboard{}
	rayFileSteps = [64][64]int8{}
	rayRankSteps = [64][64]int8{}
	rayDiagonals = [64][64]bool{}
)

// bbForPossibleMoves returns a bitboard with 1s in positions where the piece
// of the given type at the given square can potentially move, without considering
// whether the moves would be legal (e.g., leave the king in check).
//
// The function handles movement patterns for:
//   - King: One square in any direction
//   - Queen: Sliding moves in all directions
//   - Rook: Sliding moves horizontally and vertically
//   - Bishop: Sliding moves diagonally
//   - Knight: L-shaped jumps
//   - Pawn: Forward moves and captures, including en passant
func bbForPossibleMoves(pos *Position, pt PieceType, sq Square) bitboard {
	switch pt {
	case King:
		return bbKingMoves[sq]
	case Queen:
		return diaAttack(^pos.board.emptySqs, sq) | hvAttack(^pos.board.emptySqs, sq)
	case Rook:
		return hvAttack(^pos.board.emptySqs, sq)
	case Bishop:
		return diaAttack(^pos.board.emptySqs, sq)
	case Knight:
		return bbKnightMoves[sq]
	case Pawn:
		return pawnMoves(pos, sq)
	}
	return bitboard(0)
}

// pawnMoves returns a bitboard with 1s in positions where the pawn at the
// given square can potentially move.
//
// The function considers:
//   - Single and double forward moves
//   - Diagonal captures
//   - En passant captures
//
//nolint:mnd // this is a formula to determine the color of a square
func pawnMoves(pos *Position, sq Square) bitboard {
	bb := bbForSquare(sq)
	var bbEnPassant bitboard
	if pos.enPassantSquare != NoSquare {
		bbEnPassant = bbForSquare(pos.enPassantSquare)
	}
	if pos.Turn() == White {
		capRight := ((bb & ^bbFileH & ^bbRank8) >> 9) & (pos.board.blackSqs | bbEnPassant)
		capLeft := ((bb & ^bbFileA & ^bbRank8) >> 7) & (pos.board.blackSqs | bbEnPassant)
		upOne := ((bb & ^bbRank8) >> 8) & pos.board.emptySqs
		upTwo := ((upOne & bbRank3) >> 8) & pos.board.emptySqs
		return capRight | capLeft | upOne | upTwo
	}
	capRight := ((bb & ^bbFileH & ^bbRank1) << 7) & (pos.board.whiteSqs | bbEnPassant)
	capLeft := ((bb & ^bbFileA & ^bbRank1) << 9) & (pos.board.whiteSqs | bbEnPassant)
	upOne := ((bb & ^bbRank1) << 8) & pos.board.emptySqs
	upTwo := ((upOne & bbRank6) << 8) & pos.board.emptySqs
	return capRight | capLeft | upOne | upTwo
}

func rayStep(from Square, to Square) (int, int, bool) {
	fileDelta := int(to.File()) - int(from.File())
	rankDelta := int(to.Rank()) - int(from.Rank())
	switch {
	case fileDelta == 0:
		return 0, compareStep(rankDelta, 0), false
	case rankDelta == 0:
		return compareStep(fileDelta, 0), 0, false
	case abs(fileDelta) == abs(rankDelta):
		return compareStep(fileDelta, 0), compareStep(rankDelta, 0), true
	default:
		return 0, 0, false
	}
}

func piecePinsAlong(pt PieceType, diagonal bool) bool {
	if pt == Queen {
		return true
	}
	if diagonal {
		return pt == Bishop
	}
	return pt == Rook
}

func squaresBetween(a Square, b Square) bitboard {
	fileStep := compareStep(int(b.File()), int(a.File()))
	rankStep := compareStep(int(b.Rank()), int(a.Rank()))
	if fileStep == 0 && rankStep == 0 {
		return 0
	}
	if fileStep != 0 && rankStep != 0 && !sameDiagonal(a, b) {
		return 0
	}
	var out bitboard
	file := int(a.File()) + fileStep
	rank := int(a.Rank()) + rankStep
	for file != int(b.File()) || rank != int(b.Rank()) {
		out |= bbForSquare(NewSquare(File(file), Rank(rank)))
		file += fileStep
		rank += rankStep
	}
	return out
}

func compareStep(a, b int) int {
	switch {
	case a > b:
		return 1
	case a < b:
		return -1
	default:
		return 0
	}
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func sameDiagonal(a Square, b Square) bool {
	fileDelta := int(a.File()) - int(b.File())
	if fileDelta < 0 {
		fileDelta = -fileDelta
	}
	rankDelta := int(a.Rank()) - int(b.Rank())
	if rankDelta < 0 {
		rankDelta = -rankDelta
	}
	return fileDelta == rankDelta
}

func squareFromBit(bb bitboard) Square {
	return Square(63 - bits.TrailingZeros64(uint64(bb)))
}

// init populates the bbSquares lookup table. This is done at package
// initialization because the values are constants derived from square indices.
//
//nolint:gochecknoinits // Required for lookup table initialization.
func init() {
	const numOfSquaresInBoard = 64
	for sq := range numOfSquaresInBoard {
		bbSquares[sq] = bitboard(uint64(1) << (uint8(63) - uint8(sq)))
	}
	initAlignedMasks()
	initRayMasks()
	initMagicAttackTables()
}

func initAlignedMasks() {
	for a := range numOfSquaresInBoard {
		for b := range numOfSquaresInBoard {
			if squaresAligned(Square(a), Square(b)) {
				alignedMasks[a] |= bbForSquare(Square(b))
			}
		}
	}
}

func initRayMasks() {
	for a := range numOfSquaresInBoard {
		for b := range numOfSquaresInBoard {
			fileStep, rankStep, diagonal := rayStep(Square(a), Square(b))
			rayFileSteps[a][b] = int8(fileStep)
			rayRankSteps[a][b] = int8(rankStep)
			rayDiagonals[a][b] = diagonal
			betweenMasks[a][b] = squaresBetween(Square(a), Square(b))
		}
	}
}
