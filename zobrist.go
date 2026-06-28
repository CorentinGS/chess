package chess

// HashFromFEN computes the Zobrist hash of a chess position from its FEN string.
func HashFromFEN(fen string) (uint64, error) {
	pos, err := decodeFEN(fen)
	if err != nil {
		return 0, err
	}
	return pos.ZobristHash(), nil
}
