package chess

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func isWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

func isResult(s string) bool {
	return s == string(WhiteWon) || s == string(BlackWon) || s == string(Draw) || s == "*"
}

// Helper function to check if a character is a valid file.
func isFile(ch byte) bool {
	return ch >= 'a' && ch <= 'h'
}

func isAlphaNumeric(ch byte) bool {
	return isLetter(ch) || isDigit(ch)
}

// Helper function for piece validation.
func isPiece(p byte) bool {
	return p == byte('N') || p == byte('B') || p == byte('R') || p == byte('Q') || p == byte('K')
}

func isRank(ch byte) bool {
	return ch >= '1' && ch <= '8'
}
