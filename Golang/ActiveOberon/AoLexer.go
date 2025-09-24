/*
** Copyright (C) 2025 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the ActiveOberon language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// Translated from C++ Qt5 implementation

package ActiveOberon

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
)

// Scanner interface (equivalent to C++ abstract class)
type Scanner interface {
	Next() Token
	Peek(offset int) Token
	Source() string
}

// Lexer implements Scanner interface
type Lexer struct {
	input          io.Reader
	sloc           uint32
	lineNr         uint32
	colNr          uint16
	sourcePath     string
	line           []byte
	buffer         []Token
	lastToken      Token
	ignoreComments bool
	packComments   bool
	lineCounted    bool
	reader         *bufio.Reader
}

// NewLexer creates a new lexer
func NewLexer() *Lexer {
	return &Lexer{
		ignoreComments: true,
		packComments:   true,
		buffer:         make([]Token, 0),
	}
}

// SetStream sets the input stream and source path
func (l *Lexer) SetStream(input io.Reader, sourcePath string) {
	if input == nil {
		l.SetStreamFromFile(sourcePath)
		return
	}

	l.input = input
	l.reader = bufio.NewReader(input)
	l.lineNr = 0
	l.colNr = 0
	l.sourcePath = sourcePath
	l.lastToken = NewToken(TokInvalid, 0, 0, 0, nil)
	l.sloc = 0
	l.lineCounted = false
	l.buffer = l.buffer[:0]
}

// SetStreamFromFile opens a file and sets it as input
func (l *Lexer) SetStreamFromFile(sourcePath string) error {
	file, err := os.Open(sourcePath)
	if err != nil {
		return err
	}
	l.SetStream(file, sourcePath)
	return nil
}

// SetIgnoreComments sets whether to ignore comment tokens
func (l *Lexer) SetIgnoreComments(ignore bool) {
	l.ignoreComments = ignore
}

// SetPackComments sets whether to pack comments
func (l *Lexer) SetPackComments(pack bool) {
	l.packComments = pack
}

// Next implements Scanner interface
func (l *Lexer) Next() Token {
	return l.NextToken()
}

// Peek implements Scanner interface  
func (l *Lexer) Peek(offset int) Token {
	return l.PeekToken(uint8(offset))
}

// Source implements Scanner interface
func (l *Lexer) Source() string {
	return l.sourcePath
}

// NextToken returns the next token from the input
func (l *Lexer) NextToken() Token {
	var t Token
	if len(l.buffer) > 0 {
		t = l.buffer[0]
		l.buffer = l.buffer[1:]
	} else {
		t = l.nextTokenImp()
	}

	if t.Type == TokComment && l.ignoreComments {
		return l.NextToken()
	}
	return t
}

// PeekToken returns a token at the given lookahead position
func (l *Lexer) PeekToken(lookAhead uint8) Token {
	if lookAhead == 0 {
		return l.lastToken
	}

	for len(l.buffer) < int(lookAhead) {
		l.buffer = append(l.buffer, l.nextTokenImp())
	}
	return l.buffer[lookAhead-1]
}

// Tokens tokenizes a string and returns all tokens
func (l *Lexer) Tokens(code string) []Token {
	return l.TokensFromBytes([]byte(code), "")
}

// TokensFromBytes tokenizes bytes and returns all tokens
func (l *Lexer) TokensFromBytes(code []byte, path string) []Token {
	reader := bytes.NewReader(code)
	l.SetStream(reader, path)

	var tokens []Token
	for {
		t := l.NextToken()
		if !t.IsValid() {
			break
		}
		tokens = append(tokens, t)
	}
	return tokens
}

// GetSloc returns source lines of code count
func (l *Lexer) GetSloc() uint32 {
	return l.sloc
}

// nextTokenImp is the main tokenization logic
func (l *Lexer) nextTokenImp() Token {
	if l.input == nil {
		return l.makeToken(TokEof, 0, nil)
	}

	l.skipWhiteSpace()

	for l.colNr >= uint16(len(l.line)) {
		if l.isAtEnd() {
			return l.makeToken(TokEof, 0, nil)
		}
		l.nextLine()
		l.skipWhiteSpace()
	}

	if l.colNr >= uint16(len(l.line)) {
		return l.makeToken(TokEof, 0, nil)
	}

	ch := l.line[l.colNr]

	if ch == '"' || ch == '\'' {
		return l.parseString()
	} else if isAlpha(ch) {
		return l.parseIdent()
	} else if isDigit(ch) {
		return l.parseNumber()
	}

	// Try to parse operators and punctuation
	pos := int(l.colNr)
	tokenType, consumed := TokenTypeFromString(l.line[pos:])

	if tokenType == TokLatt {
		return l.parseComment()
	} else if tokenType == TokInvalid || consumed == 0 {
		return l.makeToken(TokInvalid, 1, []byte(fmt.Sprintf("unexpected character '%c' (%d)", ch, ch)))
	} else {
		return l.makeToken(tokenType, consumed, l.line[pos:pos+consumed])
	}
}

// skipWhiteSpace advances past whitespace characters
func (l *Lexer) skipWhiteSpace() int {
	startCol := l.colNr
	for l.colNr < uint16(len(l.line)) && isSpace(l.line[l.colNr]) {
		l.colNr++
	}
	return int(l.colNr) - int(startCol)
}

// nextLine reads the next line from input
func (l *Lexer) nextLine() {
	l.colNr = 0
	l.lineNr++
	l.lineCounted = false

	if l.reader == nil {
		l.line = nil
		return
	}

	line, err := l.reader.ReadBytes('\n')
	if err != nil && err != io.EOF {
		l.line = nil
		return
	}

	// Remove line endings
	line = bytes.TrimRight(line, "\n")
	l.line = line
}

// isAtEnd checks if at end of input
func (l *Lexer) isAtEnd() bool {
	if l.reader == nil {
		return true
	}
	// Try to peek one byte
	_, err := l.reader.Peek(1)
	return err == io.EOF
}

// lookAhead returns character at offset from current position
func (l *Lexer) lookAhead(off int) byte {
	pos := int(l.colNr) + off
	if pos < len(l.line) {
		return l.line[pos]
	}
	return 0
}

// makeToken creates a token
func (l *Lexer) makeToken(tokenType TokenType, length int, val []byte) Token {
	if tokenType != TokInvalid && tokenType != TokComment && tokenType != TokEof {
		l.countLine()
	}

	var tokenVal []byte
	if tokenType != TokComment && tokenType != TokInvalid {
		tokenVal = GetSymbol(val)
	} else {
		tokenVal = make([]byte, len(val))
		copy(tokenVal, val)
	}

	t := NewToken(tokenType, l.lineNr, uint32(l.colNr)+1, uint32(length), tokenVal)
	t.SourcePath = l.sourcePath
	l.lastToken = t
	l.colNr += uint16(length)
	return t
}

// parseIdent parses an identifier or keyword
func (l *Lexer) parseIdent() Token {
	start := l.colNr
	l.colNr++

	for l.colNr < uint16(len(l.line)) && (isAlnum(l.line[l.colNr]) || l.line[l.colNr] == '_') {
		l.colNr++
	}

	length := int(l.colNr) - int(start)
	str := l.line[start:l.colNr]

	// Check if it's a keyword
	tokenType, keywordLen := TokenTypeFromString(str)
	if tokenType != TokInvalid && keywordLen == len(str) {
		if tokenType == TokCODE {
			// Reset position and parse as assembler block
			l.colNr = start
			return l.parseAssembler()
		}
		l.colNr = start // Reset position for makeToken
		return l.makeToken(tokenType, length, str)
	}

	l.colNr = start // Reset position for makeToken
	return l.makeToken(TokIdent, length, str)
}

// parseNumber parses integer or real numbers
func (l *Lexer) parseNumber() Token {
	start := l.colNr

	// Parse digits
	for l.colNr < uint16(len(l.line)) && isHexDigit(l.line[l.colNr]) {
		l.colNr++
	}

	length := int(l.colNr) - int(start)
	isHex := false
	isChar := false
	isReal := false

	// Check for hex suffix
	if l.colNr < uint16(len(l.line)) {
		ch := l.line[l.colNr]
		if ch == 'H' || ch == 'h' {
			isHex = true
			l.colNr++
			length++
		} else if ch == 'X' || ch == 'x' {
			isChar = true
			l.colNr++
			length++
		} else if ch == '.' && l.lookAhead(1) != '.' {
			// Real number
			isReal = true
			l.colNr++ // consume '.'

			// Parse fractional part
			for l.colNr < uint16(len(l.line)) && isDigit(l.line[l.colNr]) {
				l.colNr++
			}

			// Check for exponent
			if l.colNr < uint16(len(l.line)) {
				ch := l.line[l.colNr]
				if ch == 'E' || ch == 'e' || ch == 'D' || ch == 'd' || ch == 'S' || ch == 's' {
					l.colNr++ // consume exponent marker

					// Optional sign
					if l.colNr < uint16(len(l.line)) && (l.line[l.colNr] == '+' || l.line[l.colNr] == '-') {
						l.colNr++
					}

					// Parse exponent digits
					if l.colNr >= uint16(len(l.line)) || !isDigit(l.line[l.colNr]) {
						l.colNr = start // Reset position
						return l.makeToken(TokInvalid, length, []byte("invalid real number"))
					}

					for l.colNr < uint16(len(l.line)) && isDigit(l.line[l.colNr]) {
						l.colNr++
					}
				}
			}

			length = int(l.colNr) - int(start)
		}
	}

	str := l.line[start:start+uint16(length)]

	// Validate number format
	if isHex && !l.checkHexNumber(str) {
		l.colNr = start
		return l.makeToken(TokInvalid, length, []byte("invalid hexadecimal integer"))
	} else if isChar && !l.checkHexNumber(str) {
		l.colNr = start
		return l.makeToken(TokInvalid, length, []byte("invalid hexadecimal character"))
	}

	l.colNr = start // Reset for makeToken

	if isChar {
		return l.makeToken(TokHexchar, length, str)
	} else if isReal {
		t := l.makeToken(TokReal, length, str)
		// Set double precision flag if needed (simplified logic)
		// t.Double = true // if precision requires it
		return t
	} else if !isHex && !l.checkDecNumber(str) {
		return l.makeToken(TokInvalid, length, []byte("invalid decimal integer"))
	} else {
		return l.makeToken(TokInteger, length, str)
	}
}

// parseString parses string literals
func (l *Lexer) parseString() Token {
	start := l.colNr
	quote := l.line[l.colNr]
	l.colNr++ // consume opening quote

	for l.colNr < uint16(len(l.line)) {
		if l.line[l.colNr] == quote {
			l.colNr++ // consume closing quote
			break
		}
		l.colNr++
	}

	if l.colNr > uint16(len(l.line)) || (l.colNr > 0 && l.line[l.colNr-1] != quote) {
		l.colNr = start
		return l.makeToken(TokInvalid, int(l.colNr)-int(start), []byte("non-terminated string"))
	}

	length := int(l.colNr) - int(start)
	str := l.line[start:l.colNr]

	l.colNr = start // Reset for makeToken
	return l.makeToken(TokString, length, str)
}

// parseComment parses comment blocks
func (l *Lexer) parseComment() Token {
	startLine := l.lineNr
	startCol := l.colNr

	level := 0
	pos := int(l.colNr)
	l.parseCommentLevel(l.line, &pos, &level)

	var commentText []byte
	commentText = append(commentText, l.line[l.colNr:pos]...)

	for level > 0 && !l.isAtEnd() {
		l.nextLine()
		pos = 0
		l.parseCommentLevel(l.line, &pos, &level)
		if len(commentText) > 0 {
			commentText = append(commentText, '\n')
		}
		commentText = append(commentText, l.line[:pos]...)
	}

	if l.packComments && level > 0 && l.isAtEnd() {
		return Token{
			Type:       TokInvalid,
			LineNr:     startLine,
			ColNr:      uint32(startCol) + 1,
			Len:        uint16(len(commentText)),
			Val:        []byte("non-terminated comment"),
			SourcePath: l.sourcePath,
		}
	}

	tokenType := TokComment
	if !l.packComments {
		tokenType = TokLatt
	}

	t := Token{
		Type:       tokenType,
		LineNr:     startLine,
		ColNr:      uint32(startCol) + 1,
		Len:        uint16(len(commentText)),
		Val:        commentText,
		SourcePath: l.sourcePath,
	}

	l.lastToken = t
	l.colNr = uint16(pos)

	return t
}

// parseAssembler parses assembler code blocks
func (l *Lexer) parseAssembler() Token {
	startLine := l.lineNr
	startCol := l.colNr

	// Find "END" that terminates the CODE block
	pos := bytes.Index(l.line[l.colNr:], []byte("END"))
	var codeText []byte

	if pos == -1 {
		codeText = append(codeText, l.line[l.colNr:]...)
	} else {
		codeText = append(codeText, l.line[l.colNr:l.colNr+uint16(pos)]...)
	}

	for pos == -1 && !l.isAtEnd() {
		l.nextLine()
		pos = bytes.Index(l.line, []byte("END"))

		if pos != -1 {
			// Check if semicolon comes before END
			semi := bytes.Index(l.line, []byte(";"))
			if semi != -1 && semi < pos {
				pos = -1
			}
		}

		if len(codeText) > 0 {
			codeText = append(codeText, '\n')
		}

		if pos == -1 {
			codeText = append(codeText, l.line...)
		} else {
			codeText = append(codeText, l.line[:pos]...)
		}
	}

	if pos == -1 {
		return Token{
			Type:       TokInvalid,
			LineNr:     startLine,
			ColNr:      uint32(startCol),
			Len:        4,
			Val:        []byte("non-terminated CODE section"),
			SourcePath: l.sourcePath,
		}
	}

	t := Token{
		Type:       TokCODE,
		LineNr:     startLine,
		ColNr:      uint32(startCol) + 1,
		Len:        uint16(len(codeText)),
		Val:        codeText,
		SourcePath: l.sourcePath,
	}

	l.lastToken = t
	if pos != -1 {
		l.colNr = uint16(pos)
	} else {
		l.colNr = uint16(len(l.line))
	}

	return t
}

// countLine increments source line count
func (l *Lexer) countLine() {
	if !l.lineCounted {
		l.sloc++
		l.lineCounted = true
	}
}

// parseCommentLevel parses comment nesting level
func (l *Lexer) parseCommentLevel(str []byte, pos *int, level *int) {
	state := 0 // 0=idle, 1=left_paren, 2=star

	for *pos < len(str) {
		c := str[*pos]
		*pos++

		switch state {
		case 0: // idle
			if c == '(' {
				state = 1
			} else if c == '*' {
				state = 2
			}
		case 1: // after '('
			if c == '*' {
				*level++
				state = 0
			} else if c != '(' {
				state = 0
			}
		case 2: // after '*'
			if c == ')' {
				*level--
				state = 0
			} else if c != '*' {
				state = 0
			}
			if *level <= 0 {
				return
			}
		}
	}
}

// checkHexNumber validates hexadecimal number format
func (l *Lexer) checkHexNumber(str []byte) bool {
	// Remove single quote if present
	if pos := bytes.IndexByte(str, '\''); pos != -1 {
		str = str[:pos]
	}

	if len(str) < 2 {
		return false
	}

	// Must end with H, h, X, or x
	last := str[len(str)-1]
	if !(last == 'H' || last == 'h' || last == 'X' || last == 'x') {
		return false
	}

	// Check all digits are hex
	for i := 0; i < len(str)-1; i++ {
		if !isHexDigit(str[i]) {
			return false
		}
	}

	return true
}

// checkDecNumber validates decimal number format
func (l *Lexer) checkDecNumber(str []byte) bool {
	for _, c := range str {
		if !isDigit(c) {
			return false
		}
	}
	return true
}

// Helper functions for character classification

func isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func isAlnum(c byte) bool {
	return isAlpha(c) || isDigit(c)
}

func isHexDigit(c byte) bool {
	return isDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

// ParseComment is a static utility function for parsing comments
func ParseComment(str []byte, pos *int, level *int) {
	lexer := &Lexer{}
	lexer.parseCommentLevel(str, pos, level)
}

// Utility functions for file format detection (simplified from C++ version)

// IsOberonFormat checks if input is in Oberon format
func IsOberonFormat(input io.ReadSeeker) bool {
	input.Seek(0, io.SeekStart)
	defer input.Seek(0, io.SeekStart)

	// Simple check for V4 format
	buf := make([]byte, 2)
	n, _ := input.Read(buf)
	if n == 2 && buf[0] == 0xf0 && buf[1] == 0x01 {
		return true
	}

	// Could add more format detection here
	return false
}

// ExtractText extracts text from Oberon format files
func ExtractText(input io.ReadSeeker) []byte {
	if !IsOberonFormat(input) {
		// Read as plain text
		input.Seek(0, io.SeekStart)
		data, _ := io.ReadAll(input)
		return data
	}

	// For now, just read as plain text
	// Full Oberon format support would require more complex parsing
	input.Seek(0, io.SeekStart)
	data, _ := io.ReadAll(input)
	return data
}

// SkipBOM skips UTF-8 byte order mark
func SkipBOM(input io.Reader) bool {
	if reader, ok := input.(*bufio.Reader); ok {
		buf, err := reader.Peek(3)
		if err == nil && len(buf) == 3 && buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf {
			reader.Discard(3)
			return true
		}
	}
	return false
}
