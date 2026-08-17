package chess

import (
	"math/bits"
)

// diaAttack returns a bitboard representing possible diagonal moves for a
// sliding piece, considering occupied squares as blocking further movement.
//
// Implementation: index a deterministic magic-bitboard table generated at
// package init from checked-in magic constants.
func diaAttack(occupied bitboard, sq Square) bitboard {
	return bishopMagicAttacks[sq][((occupied&bishopMagicMasks[sq])*bishopMagics[sq])>>bishopMagicShifts[sq]]
}

func slowDiaAttack(occupied bitboard, sq Square) bitboard {
	f := int(sq) & 7
	r := int(sq) >> 3
	occ := uint64(occupied)
	var attacks uint64
	// NE: rank+1, file+1 (NE-SW diagonal = bbDiagonals[sq]).
	for d := 1; f+d < 8 && r+d < 8; d++ {
		bit := uint64(1) << (63 - ((r + d) << 3) - (f + d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// SW: rank-1, file-1 (NE-SW diagonal, opposite side).
	for d := 1; f-d >= 0 && r-d >= 0; d++ {
		bit := uint64(1) << (63 - ((r - d) << 3) - (f - d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// NW: rank+1, file-1 (NW-SE diagonal = bbAntiDiagonals[sq]).
	for d := 1; f-d >= 0 && r+d < 8; d++ {
		bit := uint64(1) << (63 - ((r + d) << 3) - (f - d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// SE: rank-1, file+1 (NW-SE diagonal, opposite side).
	for d := 1; f+d < 8 && r-d >= 0; d++ {
		bit := uint64(1) << (63 - ((r - d) << 3) - (f + d))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	return bitboard(attacks)
}

// hvAttack returns a bitboard representing possible horizontal and vertical
// moves for a sliding piece, considering occupied squares as blocking
// further movement.
//
// Implementation: index a deterministic magic-bitboard table generated at
// package init from checked-in magic constants.
func hvAttack(occupied bitboard, sq Square) bitboard {
	return rookMagicAttacks[sq][((occupied&rookMagicMasks[sq])*rookMagics[sq])>>rookMagicShifts[sq]]
}

func slowHVAttack(occupied bitboard, sq Square) bitboard {
	f := int(sq) & 7
	r := int(sq) >> 3
	occ := uint64(occupied)
	var attacks uint64
	// E: file+1.
	for df := 1; f+df < 8; df++ {
		bit := uint64(1) << (63 - (r << 3) - (f + df))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// W: file-1.
	for df := 1; f-df >= 0; df++ {
		bit := uint64(1) << (63 - (r << 3) - (f - df))
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// N: rank+1.
	for dr := 1; r+dr < 8; dr++ {
		bit := uint64(1) << (63 - ((r + dr) << 3) - f)
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	// S: rank-1.
	for dr := 1; r-dr >= 0; dr++ {
		bit := uint64(1) << (63 - ((r - dr) << 3) - f)
		attacks |= bit
		if occ&bit != 0 {
			break
		}
	}
	return bitboard(attacks)
}

var (
	rookMagicMasks     = [64]bitboard{}
	bishopMagicMasks   = [64]bitboard{}
	rookMagicShifts    = [64]uint{}
	bishopMagicShifts  = [64]uint{}
	rookMagicAttacks   = [64][4096]bitboard{}
	bishopMagicAttacks = [64][512]bitboard{}
)

// Deterministically generated for this package's A1=MSB square numbering.
var rookMagics = [64]bitboard{ //nolint:gochecknoglobals // lookup constants
	0x000009284d008402, 0x0602004108040082, 0x5262005024880102, 0x071200141020086a,
	0x4003008620500009, 0x0000220010088042, 0x4001004000208015, 0x3620120880410022,
	0x02804d0280542200, 0x80c0081001020400, 0x2058044010200801, 0x4108004200040040,
	0x00a0080050008180, 0x0818220080401200, 0x8000220045028600, 0x40800020014000c0,
	0x4080009408420005, 0x0000080210040001, 0x88c1002400090002, 0x0809001008010004,
	0x00100a0010420020, 0x102c100020008080, 0x003000200040c008, 0x4080004020014000,
	0x010010984200011c, 0x1800010204001008, 0x101200540a000830, 0x0020800800800400,
	0x0100100023000900, 0x0022002042001084, 0x0060200042401008, 0x4000400020800080,
	0x0028012200004084, 0x3900611400d01802, 0x0202000404001020, 0x0002002200100408,
	0x3080080080100080, 0x8000200080801000, 0x0000200280400084, 0x0450288080004000,
	0x000202000c205181, 0x00222c000e100108, 0x8031080110400420, 0x0048008008800400,
	0x0002020020081041, 0x1020018061801000, 0x0010024000200040, 0x2080004000402000,
	0x9182000102046484, 0x0000800200010080, 0x0083000300080400, 0x082a002030048a00,
	0x2181001000090020, 0x0401801000200180, 0x3200404000201000, 0x0000802080004006,
	0x0200048114002042, 0x0200430406002088, 0x2200211028020024, 0x4100080010020500,
	0x0a80080004100082, 0x2700200031000840, 0x0040001000200041, 0x0080006015804008,
}

var bishopMagics = [64]bitboard{ //nolint:gochecknoglobals // lookup constants
	0x0002100142040242, 0x0000400841140080, 0x2030e04064084220, 0x000120813092020a,
	0x0510c0040020a800, 0x0000810104010400, 0x0400078084100260, 0x0480808808020200,
	0x1284040842042000, 0x0221a00400808021, 0x4000068850010010, 0x0a10001020222022,
	0x6802124842020062, 0x8050002084100106, 0x6222092108020020, 0x4a2c012188600001,
	0x4408880100400020, 0x10421e0204004212, 0x006210b000804100, 0x1410401011010210,
	0x0800044200908800, 0x0000104828007000, 0x1004210108041001, 0x60009004200210c0,
	0x10a408a482082400, 0x0008081120009082, 0x0802208104020040, 0x0084200200082080,
	0x0100020080080080, 0x4142045101900100, 0x0022022023900100, 0x4002034080200812,
	0x04a2048008540081, 0x4004088530421004, 0x8268080800808400, 0x0000848014002000,
	0x800c080100220040, 0x0000208224080080, 0x0008208004091244, 0x0428400420024a81,
	0x0081020a00420204, 0x5002011080b0880a, 0x10f2001040422080, 0x0044004480a02000,
	0x0008000420401100, 0x0784026088001240, 0x0202040810210200, 0x2021120420420220,
	0xe000422508088400, 0x000001008820880a, 0x4006011048040004, 0x0008020210008000,
	0x8000040408800821, 0x8016048104090400, 0x002e080829042120, 0x1040a00210011104,
	0x0020240948141002, 0xe025880d48200108, 0x0041010840020620, 0x0002121040400450,
	0x0148248900004010, 0x2008180460811001, 0x0010308080808600, 0x0804101208011014,
}

func initMagicAttackTables() {
	for sq := range numOfSquaresInBoard {
		square := Square(sq)
		rookMagicMasks[sq] = rookMagicMask(square)
		rookMagicShifts[sq] = uint(numOfSquaresInBoard - bits.OnesCount64(uint64(rookMagicMasks[sq])))
		initMagicAttack(rookMagicAttacks[sq][:], rookMagicMasks[sq], rookMagics[sq], rookMagicShifts[sq], square, slowHVAttack)

		bishopMagicMasks[sq] = bishopMagicMask(square)
		bishopMagicShifts[sq] = uint(numOfSquaresInBoard - bits.OnesCount64(uint64(bishopMagicMasks[sq])))
		initMagicAttack(
			bishopMagicAttacks[sq][:],
			bishopMagicMasks[sq],
			bishopMagics[sq],
			bishopMagicShifts[sq],
			square,
			slowDiaAttack,
		)
	}
}

func initMagicAttack(
	attacks []bitboard,
	mask bitboard,
	magic bitboard,
	shift uint,
	sq Square,
	attack func(bitboard, Square) bitboard,
) {
	for _, occupied := range magicOccupancies(mask) {
		idx := ((occupied & mask) * magic) >> shift
		attacks[idx] = attack(occupied, sq)
	}
}

func magicOccupancies(mask bitboard) []bitboard {
	bitCount := bits.OnesCount64(uint64(mask))
	occupancies := make([]bitboard, 1<<bitCount)
	maskBits := make([]uint, 0, bitCount)
	for rest := mask; rest != 0; rest &= rest - 1 {
		maskBits = append(maskBits, uint(bits.TrailingZeros64(uint64(rest))))
	}
	for subset := range occupancies {
		var occupied bitboard
		for i, bit := range maskBits {
			if subset&(1<<i) != 0 {
				occupied |= bitboard(uint64(1) << bit)
			}
		}
		occupancies[subset] = occupied
	}
	return occupancies
}

func rookMagicMask(sq Square) bitboard {
	file := int(sq.File())
	rank := int(sq.Rank())
	var mask bitboard
	for r := rank + 1; r <= 6; r++ {
		mask |= bbForSquare(NewSquare(File(file), Rank(r)))
	}
	for r := rank - 1; r >= 1; r-- {
		mask |= bbForSquare(NewSquare(File(file), Rank(r)))
	}
	for f := file + 1; f <= 6; f++ {
		mask |= bbForSquare(NewSquare(File(f), Rank(rank)))
	}
	for f := file - 1; f >= 1; f-- {
		mask |= bbForSquare(NewSquare(File(f), Rank(rank)))
	}
	return mask
}

func bishopMagicMask(sq Square) bitboard {
	file := int(sq.File())
	rank := int(sq.Rank())
	var mask bitboard
	for f, r := file+1, rank+1; f <= 6 && r <= 6; f, r = f+1, r+1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file-1, rank-1; f >= 1 && r >= 1; f, r = f-1, r-1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file-1, rank+1; f >= 1 && r <= 6; f, r = f-1, r+1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	for f, r := file+1, rank-1; f <= 6 && r >= 1; f, r = f+1, r-1 {
		mask |= bbForSquare(NewSquare(File(f), Rank(r)))
	}
	return mask
}
