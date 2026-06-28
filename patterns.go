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
	bbFiles = [8]bitboard{bbFileA, bbFileB, bbFileC, bbFileD, bbFileE, bbFileF, bbFileG, bbFileH}
	bbRanks = [8]bitboard{bbRank1, bbRank2, bbRank3, bbRank4, bbRank5, bbRank6, bbRank7, bbRank8}

	bbDiagonals = [64]bitboard{9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 144396663052566528, 72057594037927936, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 144396663052566528, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 288794425616760832, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 577588855528488960, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 1155177711073755136, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 2310355422147575808, 32832, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872, 128, 32832, 8405024, 2151686160, 550831656968, 141012904183812, 36099303471055874, 9241421688590303745}

	bbAntiDiagonals = [64]bitboard{9223372036854775808, 4647714815446351872, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 4647714815446351872, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 2323998145211531264, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 1161999622361579520, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 580999813328273408, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 290499906672525312, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 145249953336295424, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 258, 72624976668147840, 283691315109952, 1108169199648, 4328785936, 16909320, 66052, 258, 1}

	bbKnightMoves = [64]bitboard{9077567998918656, 4679521487814656, 38368557762871296, 19184278881435648, 9592139440717824, 4796069720358912, 2257297371824128, 1128098930098176, 2305878468463689728, 1152939783987658752, 9799982666336960512, 4899991333168480256, 2449995666584240128, 1224997833292120064, 576469569871282176, 288234782788157440, 4620693356194824192, 11533718717099671552, 5802888705324613632, 2901444352662306816, 1450722176331153408, 725361088165576704, 362539804446949376, 145241105196122112, 18049583422636032, 45053588738670592, 22667534005174272, 11333767002587136, 5666883501293568, 2833441750646784, 1416171111120896, 567348067172352, 70506185244672, 175990581010432, 88545054707712, 44272527353856, 22136263676928, 11068131838464, 5531918402816, 2216203387392, 275414786112, 687463207072, 345879119952, 172939559976, 86469779988, 43234889994, 21609056261, 8657044482, 1075839008, 2685403152, 1351090312, 675545156, 337772578, 168886289, 84410376, 33816580, 4202496, 10489856, 5277696, 2638848, 1319424, 659712, 329728, 132096}

	bbBishopMoves = [64]bitboard{18049651735527937, 45053622886727936, 22667548931719168, 11334324221640704, 5667164249915392, 2833579985862656, 1416240237150208, 567382630219904, 4611756524879479810, 11529391036782871041, 5764696068147249408, 2882348036221108224, 1441174018118909952, 720587009051099136, 360293502378066048, 144117404414255168, 2323857683139004420, 1197958188344280066, 9822351133174399489, 4911175566595588352, 2455587783297826816, 1227793891648880768, 577868148797087808, 288793334762704928, 1161999073681608712, 581140276476643332, 326598935265674242, 9386671504487645697, 4693335752243822976, 2310639079102947392, 1155178802063085600, 577588851267340304, 580999811184992272, 290500455356698632, 145390965166737412, 108724279602332802, 9241705379636978241, 4620711952330133792, 2310355426409252880, 1155177711057110024, 290499906664153120, 145249955479592976, 72625527495610504, 424704217196612, 36100411639206946, 9241421692918565393, 4620710844311799048, 2310355422147510788, 145249953336262720, 72624976676520096, 283693466779728, 1659000848424, 141017232965652, 36099303487963146, 9241421688590368773, 4620710844295151618, 72624976668147712, 283691315142656, 1108177604608, 6480472064, 550848566272, 141012904249856, 36099303471056128, 9241421688590303744}

	bbRookMoves = [64]bitboard{9187484529235886208, 13781085504453754944, 16077885992062689312, 17226286235867156496, 17800486357769390088, 18087586418720506884, 18231136449196065282, 18302911464433844481, 9259260648297103488, 4665518383679160384, 2368647251370188832, 1220211685215703056, 645993902138460168, 358885010599838724, 215330564830528002, 143553341945872641, 9259541023762186368, 4629910699613634624, 2315095537539358752, 1157687956502220816, 578984165983651848, 289632270724367364, 144956323094725122, 72618349279904001, 9259542118978846848, 4629771607097753664, 2314886351157207072, 1157443723186933776, 578722409201797128, 289361752209228804, 144681423712944642, 72341259464802561, 9259542123257036928, 4629771063767613504, 2314885534022901792, 1157442769150545936, 578721386714368008, 289360695496279044, 144680349887234562, 72340177082712321, 9259542123273748608, 4629771061645230144, 2314885530830970912, 1157442765423841296, 578721382720276488, 289360691368494084, 144680345692602882, 72340172854657281, 9259542123273813888, 4629771061636939584, 2314885530818502432, 1157442765409283856, 578721382704674568, 289360691352369924, 144680345676217602, 72340172838141441, 9259542123273814143, 4629771061636907199, 2314885530818453727, 1157442765409226991, 578721382704613623, 289360691352306939, 144680345676153597, 72340172838076926}

	bbQueenMoves = [64]bitboard{9205534180971414145, 13826139127340482880, 16100553540994408480, 17237620560088797200, 17806153522019305480, 18090419998706369540, 18232552689433215490, 18303478847064064385, 13871017173176583298, 16194909420462031425, 8133343319517438240, 4102559721436811280, 2087167920257370120, 1079472019650937860, 575624067208594050, 287670746360127809, 11583398706901190788, 5827868887957914690, 12137446670713758241, 6068863523097809168, 3034571949281478664, 1517426162373248132, 722824471891812930, 361411684042608929, 10421541192660455560, 5210911883574396996, 2641485286422881314, 10544115227674579473, 5272058161445620104, 2600000831312176196, 1299860225776030242, 649930110732142865, 9840541934442029200, 4920271519124312136, 2460276499189639204, 1266167048752878738, 9820426766351346249, 4910072647826412836, 2455035776296487442, 1227517888139822345, 9550042029937901728, 4775021017124823120, 2387511058326581416, 1157867469641037908, 614821794359483434, 9530782384287059477, 4765391190004401930, 2382695595002168069, 9404792076610076608, 4702396038313459680, 2315169224285282160, 1157444424410132280, 578862399937640220, 325459994840333070, 9386102034266586375, 4693051017133293059, 9332167099941961855, 4630054752952049855, 2314886638996058335, 1157442771889699055, 578721933553179895, 289501704256556795, 180779649147209725, 9313761861428380670}

	bbKingMoves = [64]bitboard{4665729213955833856, 11592265440851656704, 5796132720425828352, 2898066360212914176, 1449033180106457088, 724516590053228544, 362258295026614272, 144959613005987840, 13853283560024178688, 16186183351374184448, 8093091675687092224, 4046545837843546112, 2023272918921773056, 1011636459460886528, 505818229730443264, 216739030602088448, 54114388906344448, 63227278716305408, 31613639358152704, 15806819679076352, 7903409839538176, 3951704919769088, 1975852459884544, 846636838289408, 211384331665408, 246981557485568, 123490778742784, 61745389371392, 30872694685696, 15436347342848, 7718173671424, 3307175149568, 825720045568, 964771708928, 482385854464, 241192927232, 120596463616, 60298231808, 30149115904, 12918652928, 3225468928, 3768639488, 1884319744, 942159872, 471079936, 235539968, 117769984, 50463488, 12599488, 14721248, 7360624, 3680312, 1840156, 920078, 460039, 197123, 49216, 57504, 28752, 14376, 7188, 3594, 1797, 770}

	bbSquares       = [64]bitboard{}
	lineSquares     = [4][64][8]Square{}
	lineBitboards   = [4][64][8]bitboard{}
	lineMasks       = [4][64]bitboard{}
	lineBitIndexes  = [4][64][64]uint8{}
	lineWordIndexes = [4][64][4][65536]uint8{}
	fileWordIndexes = [8][4][65536]uint8{}
	lineLens        = [4][64]int{}
	alignedMasks    = [64]bitboard{}
	betweenMasks    = [64][64]bitboard{}
	rayFileSteps    = [64][64]int8{}
	rayRankSteps    = [64][64]int8{}
	rayDiagonals    = [64][64]bool{}
	slidingAttacks  = [4][64][256]bitboard{}
)

const (
	slideRank = iota
	slideFile
	slideDiag
	slideAntiDiag
	slideLineCount
)

func lineIndex(occupied bitboard, line int, sq Square) uint8 {
	u := uint64(occupied)
	t := &lineWordIndexes[line][sq]
	return t[0][uint16(u>>48)] |
		t[1][uint16(u>>32)] |
		t[2][uint16(u>>16)] |
		t[3][uint16(u)]
}

func rankLineIndex(occupied bitboard, sq Square) uint8 {
	shift := uint((7 - sq.Rank()) * 8)
	return bits.Reverse8(byte(uint64(occupied) >> shift))
}

func fileLineIndex(occupied bitboard, sq Square) uint8 {
	u := uint64(occupied)
	t := &fileWordIndexes[sq.File()]
	return t[0][uint16(u>>48)] |
		t[1][uint16(u>>32)] |
		t[2][uint16(u>>16)] |
		t[3][uint16(u)]
}

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

func rayStep(from Square, to Square) (fileStep int, rankStep int, diagonal bool) {
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
	initSlidingAttackTables()
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

func initSlidingAttackTables() {
	for sq := range numOfSquaresInBoard {
		initSlidingLines(Square(sq))
	}
	initLineWordIndexes()
	for line := range slideLineCount {
		for sq := range numOfSquaresInBoard {
			for idx := range 256 {
				slidingAttacks[line][sq][idx] = attackOnLine(line, Square(sq), uint8(idx))
			}
		}
	}
}

func initLineWordIndexes() {
	for line := range slideLineCount {
		if line == slideRank || line == slideFile {
			continue
		}
		for sq := range numOfSquaresInBoard {
			for wordIndex := range 4 {
				shift := uint((3 - wordIndex) * 16)
				for value := range 65536 {
					chunk := bitboard(uint64(value) << shift)
					lineWordIndexes[line][sq][wordIndex][value] = lineIndexFromMaskedOccupancy(chunk, line, Square(sq))
				}
			}
		}
	}
	for file := range numOfSquaresInRow {
		sq := NewSquare(File(file), Rank1)
		for wordIndex := range 4 {
			shift := uint((3 - wordIndex) * 16)
			for value := range 65536 {
				chunk := bitboard(uint64(value) << shift)
				fileWordIndexes[file][wordIndex][value] = lineIndexFromMaskedOccupancy(chunk, slideFile, sq)
			}
		}
	}
}

func lineIndexFromMaskedOccupancy(occupied bitboard, line int, sq Square) uint8 {
	lineOccupied := occupied & lineMasks[line][sq]
	var idx uint8
	for lineOccupied != 0 {
		bit := lineOccupied & -lineOccupied
		idx |= lineBitIndexes[line][sq][bits.TrailingZeros64(uint64(bit))]
		lineOccupied &= lineOccupied - 1
	}
	return idx
}

func initSlidingLines(sq Square) {
	f := int(sq.File())
	r := int(sq.Rank())

	for file := range numOfSquaresInRow {
		appendLineSquare(slideRank, sq, NewSquare(File(file), Rank(r)))
	}
	for rank := range numOfSquaresInRow {
		appendLineSquare(slideFile, sq, NewSquare(File(f), Rank(rank)))
	}

	startF, startR := f, r
	for startF > 0 && startR > 0 {
		startF--
		startR--
	}
	for startF < 8 && startR < 8 {
		appendLineSquare(slideDiag, sq, NewSquare(File(startF), Rank(startR)))
		startF++
		startR++
	}

	startF, startR = f, r
	for startF > 0 && startR < 7 {
		startF--
		startR++
	}
	for startF < 8 && startR >= 0 {
		appendLineSquare(slideAntiDiag, sq, NewSquare(File(startF), Rank(startR)))
		startF++
		startR--
	}
}

func appendLineSquare(line int, src Square, sq Square) {
	idx := lineLens[line][src]
	lineSquares[line][src][idx] = sq
	bb := bbForSquare(sq)
	lineBitboards[line][src][idx] = bb
	lineMasks[line][src] |= bb
	lineBitIndexes[line][src][bits.TrailingZeros64(uint64(bb))] = 1 << idx
	lineLens[line][src]++
}

func attackOnLine(line int, src Square, idx uint8) bitboard {
	var srcIdx int
	for i := range lineLens[line][src] {
		if lineSquares[line][src][i] == src {
			srcIdx = i
			break
		}
	}

	var attacks bitboard
	for i := srcIdx + 1; i < lineLens[line][src]; i++ {
		sq := lineSquares[line][src][i]
		attacks |= bbForSquare(sq)
		if idx&(1<<i) != 0 {
			break
		}
	}
	for i := srcIdx - 1; i >= 0; i-- {
		sq := lineSquares[line][src][i]
		attacks |= bbForSquare(sq)
		if idx&(1<<i) != 0 {
			break
		}
	}
	return attacks
}
