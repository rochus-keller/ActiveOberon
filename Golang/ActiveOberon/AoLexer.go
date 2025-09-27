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
	"bytes"
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
)

type Scanner interface {
	Next() Token
	Peek(offset int) Token
	Source() string
}

type Stream struct {
	data []byte
	pos  int
}

func NewStream() *Stream { return &Stream{} }

func (b *Stream) ReadFromFile(r io.Reader) error {
	buf, err := io.ReadAll(r)
	if err != nil {
		return err
	}
	b.data = buf
	b.pos = 0
	return nil
}

func (b *Stream) ReadFromBytes(in []byte) {
	b.data = in
	b.pos = 0
}

func (b *Stream) AtEnd() bool {
	return b.pos > len(b.data)-1
}

func (b *Stream) Seek(off int) {
	if off >= 0 && off <= len(b.data) {
		b.pos = off
	}
}

func (b *Stream) GetPos() int { return b.pos }

func (b *Stream) ToStart() {
	b.pos = 0
}

func (b *Stream) ReadByte() byte {
	if b.AtEnd() {
		b.pos++
		return 0
	}
	v := b.data[b.pos]
	b.pos++
	return v
}

func (b *Stream) Peek(count int) []byte {
	if count <= 0 {
		end := len(b.data)
		if b.pos < 0 || b.pos > end {
			return nil
		}
		return b.data[b.pos:b.pos]
	}
	start := b.pos
	end := start + count
	if end > len(b.data) {
		end = len(b.data)
	}
	if start >= end {
		if end < 0 {
			end = 0
		}
		if end > len(b.data) {
			end = len(b.data)
		}
		return b.data[end:end]
	}
	return b.data[start:end]
}

func (b *Stream) Read(count int) []byte {
	p := b.Peek(count)
	b.pos += len(p)
	return p
}

func (b *Stream) ReadAll() []byte {
	remaining := len(b.data) - b.pos
	if remaining < 0 {
		remaining = 0
	}
	return b.Read(remaining)
}

func (b *Stream) ReadLine() []byte {
	start := b.pos
	n := len(b.data)
	if start >= n {
		// Return empty slice at end
		return b.data[n:n]
	}
	i := start
	for i < n {
		switch b.data[i] {
		case '\n':
			i++
			goto DONE
		case '\r':
			if i+1 < n && b.data[i+1] == '\n' {
				i += 2
			} else {
				i++
			}
			goto DONE
		default:
			i++
		}
	}
DONE:
	if i > n {
		i = n
	}
	line := b.data[start:i]
	b.pos = i
	return line
}

// Lexer provides tokenization for ActiveOberon source or Oberon-format text.
type Lexer struct {
	in             *Stream
	sloc           uint32
	lineNr         uint32
	colNr          uint16
	sourcePath     string
	line           []byte
	buffer         []Token
	lastToken      Token
	ignoreComments bool // don't deliver comment tokens
	packComments   bool // deliver a single TokComment instead of TokLatt + TokRatt
	lineCounted    bool
}

// NewLexer constructs a new lexer.
func NewLexer() *Lexer {
	return &Lexer{
		lastToken:      Token{Type: TokInvalid},
		ignoreComments: true,
		packComments:   true,
	}
}

// SetStream sets the input stream and prepares the lexer.
// If the input is detected as Oberon format, the text is extracted and re-wrapped as UTF-8.
func (lx *Lexer) SetStream(in *Stream, sourcePath string) {
	if in == nil {
		lx.SetStreamFromFile(sourcePath)
		return
	}
	lx.in = in
	lx.lineNr = 0
	lx.colNr = 0
	lx.sourcePath = sourcePath
	lx.lastToken = Token{Type: TokInvalid}
	lx.sloc = 0
	lx.lineCounted = false

	if isOberonFormat(in) {
		text := extractText(in)
		in.ReadFromBytes(text)
	}
}

// SetStreamPath opens a file and sets it as the input stream.
func (lx *Lexer) SetStreamFromFile(sourcePath string) error {
	f, err := os.Open(sourcePath)
	if err != nil {
		return err
	}
	// Ensure ReadSeeker
	s := NewStream()
	s.ReadFromFile(f)
	lx.SetStream(s, sourcePath)
	return nil
}

// SetIgnoreComments configures whether comment tokens are suppressed.
func (lx *Lexer) SetIgnoreComments(b bool) { lx.ignoreComments = b }

// SetPackComments configures whether nested comments are packed.
func (lx *Lexer) SetPackComments(b bool) { lx.packComments = b }

// Next implements Scanner.Next.
func (lx *Lexer) Next() Token {
	return lx.NextToken()
}

// Peek implements Scanner.Peek.
func (lx *Lexer) Peek(lookAhead int) Token {
	return lx.PeekToken(lookAhead)
}

// Source implements Scanner.Source.
func (lx *Lexer) Source() string {
	return lx.sourcePath
}

// NextToken returns the next token, applying comment filtering policy.
func (lx *Lexer) NextToken() Token {
	var t Token
	if len(lx.buffer) > 0 {
		t = lx.buffer[0]
		lx.buffer = lx.buffer[1:]
	} else {
		t = lx.nextTokenImp()
	}
	if t.Type == TokComment && lx.ignoreComments {
		return lx.NextToken()
	}
	return t
}

// PeekToken returns a lookahead token without consuming it.
func (lx *Lexer) PeekToken(lookAhead int) Token {
	if lookAhead <= 0 {
		lookAhead = 1
	}
	for len(lx.buffer) < lookAhead {
		lx.buffer = append(lx.buffer, lx.nextTokenImp())
	}
	return lx.buffer[lookAhead-1]
}

// TokensFromString tokenizes a UTF-8 string as a source unit (utility).
func (lx *Lexer) TokensFromString(code string, path string) []Token {
	s := NewStream()
	s.ReadFromBytes([]byte(code))
	lx.SetStream(s, path)
	res := []Token{}
	t := lx.NextToken()
	for t.IsValid() {
		res = append(res, t)
		t = lx.NextToken()
	}
	return res
}

// nextTokenImp performs the actual lexing step.
func (lx *Lexer) nextTokenImp() Token {
	if lx.in == nil {
		return lx.token(TokEof, 0, nil)
	}
	lx.skipWhiteSpace()
	for int(lx.colNr) >= len(lx.line) {
		// Need a new line
		if lx.in.AtEnd() {
			return lx.token(TokEof, 0, nil)
		}
		if !lx.nextLine() {
			t := lx.token(TokEof, 0, nil)
			return t
		}
		lx.skipWhiteSpace()
	}
	for int(lx.colNr) < len(lx.line) {
		ch := lx.line[lx.colNr]
		if ch == '"' || ch == '\'' {
			return lx.string_()
		} else if isAlpha(ch) {
			return lx.ident()
		} else if isDigit(ch) {
			return lx.number()
		}
		// punctuation and operators
		pos := int(lx.colNr)
		tt := TokenTypeFromString(lx.line, &pos)
		if tt == TokLatt {
			return lx.comment()
		} else if tt == TokInvalid || pos == 0 {
			msg := []byte(fmt.Sprintf("unexpected character '%c' %d", ch, ch))
			return lx.token(TokInvalid, 1, msg)
		} else {
			l := pos - int(lx.colNr)
			return lx.token(tt, l, lx.line[int(lx.colNr):pos])
		}
	}
	// should not reach
	return lx.token(TokInvalid, 0, nil)
}

// skipWhiteSpace advances over whitespace on the current line.
func (lx *Lexer) skipWhiteSpace() int {
	col := int(lx.colNr)
	for int(lx.colNr) < len(lx.line) && isSpace(lx.line[lx.colNr]) {
		lx.colNr++
	}
	return int(lx.colNr) - col
}

// nextLine reads the next physical line from the stream into lx.line.
func (lx *Lexer) nextLine() bool {
	lx.colNr = 0
	lx.lineNr++
	line := lx.in.ReadLine()
	// Chop line endings and V4 line separator 0x15 if present
	if bytes.HasSuffix(line, []byte("\r\n")) {
		line = line[:len(line)-2]
	} else if len(line) > 0 && (line[len(line)-1] == '\n' || line[len(line)-1] == '\r' || line[len(line)-1] == 0x15) {
		line = line[:len(line)-1]
	}
	lx.line = line
	lx.lineCounted = false
	return true
}

// lookAhead returns the byte at current column + off or 0 if out of line bounds.
func (lx *Lexer) lookAhead(off int) byte {
	idx := int(lx.colNr) + off
	if idx >= 0 && idx < len(lx.line) {
		return lx.line[idx]
	}
	return 0
}

// token constructs a token and advances the column.
func (lx *Lexer) token(tt TokenType, length int, val []byte) Token {
	if tt != TokInvalid && tt != TokComment && tt != TokEof {
		lx.countLine()
	}
	v := val
	if tt != TokComment && tt != TokInvalid {
		v = GetSymbol(v)
	}
	t := NewToken(tt, lx.lineNr, uint32(lx.colNr)+1, uint32(length), v)
	lx.lastToken = t
	lx.colNr += uint16(length)
	t.SourcePath = lx.sourcePath
	return t
}

// ident scans identifiers and keywords (including CODE handling).
func (lx *Lexer) ident() Token {
	off := 1
	for {
		c := lx.lookAhead(off)
		if !isLetterOrNumber(c) {
			break
		}
		off++
	}
	str := lx.line[int(lx.colNr) : int(lx.colNr)+off]
	var pos int = 0
	tt := TokenTypeFromString(str, &pos)
	if tt != TokInvalid && pos != len(str) {
		tt = TokInvalid
	}
	if tt == TokCODE {
		return lx.assembler()
	}
	if tt != TokInvalid {
		return lx.token(tt, off, nil)
	}
	return lx.token(TokIdent, off, str)
}

func isHexDigit(c byte) bool {
	return isDigit(c) || (c >= 'A' && c <= 'F')
}

func checkHexNumber(str []byte) bool {
	if i := bytes.IndexByte(str, '\''); i != -1 {
		str = str[:i]
	}
	if len(str) < 2 {
		return false
	}
	last := str[len(str)-1]
	return last == 'H' || last == 'h' || last == 'X' || last == 'x'
}

func checkDecNumber(str []byte, oneOff bool) bool {
	n := len(str)
	if oneOff && n > 0 {
		n--
	}
	for i := 0; i < n; i++ {
		if !isDigit(str[i]) {
			return false
		}
	}
	return true
}

// number scans integers, reals, and hex char literals per Oberon syntax.
func (lx *Lexer) number() Token {
	// integer      ::=  digit {digit} | digit {hexDigit} 'H'
	// real         ::=  digit {digit} '.' {digit} [ScaleFactor]
	// ScaleFactor  ::=  ('E'|'D'|'S') ['+' | '-'] digit {digit}
	rhsPlaces, expPlaces := 0, 0
	off := 1
	for {
		c := lx.lookAhead(off)
		if !isHexDigit(c) { // also accepts d and e (handled later)
			break
		}
		off++
	}
	isHex := false
	is64bit := false
	is32bit := false
	isChar := false
	isReal := false
	commaPos, ePos := -1, -1

	o1 := lx.lookAhead(off)
	if o1 == 'H' || o1 == 'h' {
		isHex = true
		off++
	} else if o1 == 'X' || o1 == 'x' {
		isChar = true
		off++
	} else if o1 == '.' && lx.lookAhead(off+1) == '.' {
		// range operator, not a decimal point
	} else if o1 == '.' {
		if !checkDecNumber(lx.line[int(lx.colNr):int(lx.colNr)+off], false) {
			return lx.token(TokInvalid, off, []byte("invalid mantissa"))
		}
		commaPos = off
		off++
		isReal = true
		for {
			c := lx.lookAhead(off)
			if !isDigit(c) {
				break
			}
			off++
			rhsPlaces++
		}
		de := lx.lookAhead(off)
		if de == 'E' || de == 'D' || de == 'S' || de == 'e' || de == 'd' || de == 's' {
			is64bit = (de == 'D' || de == 'd')
			is32bit = (de == 'S' || de == 's')

			ePos = off
			off++
			o := lx.lookAhead(off)
			if o == '+' || o == '-' {
				off++
				o = lx.lookAhead(off)
			}
			if !isDigit(o) {
				return lx.token(TokInvalid, off, []byte("invalid real"))
			}
			for {
				c := lx.lookAhead(off)
				if !isDigit(c) {
					break
				}
				off++
				expPlaces++
			}
		}
	}
	str := lx.line[int(lx.colNr) : int(lx.colNr)+off]
	if isHex && !checkHexNumber(str) {
		return lx.token(TokInvalid, off, []byte("invalid hexadecimal integer"))
	} else if isChar && !checkHexNumber(str) {
		return lx.token(TokInvalid, off, []byte("invalid hexadecimal string"))
	}

	if isChar {
		return lx.token(TokHexchar, off, str)
	} else if isReal {
		tok := lx.token(TokReal, off, str)
		mantissa := str
		if ePos != -1 {
			mantissa = str[:ePos]
		}
		lhs := mantissa
		if commaPos != -1 {
			lhs = lhs[:commaPos]
			mantissa = append([]byte{}, mantissa...)
			mantissa = append(mantissa[:commaPos], mantissa[commaPos+1:]...)
		}
		lOk := true
		l := uint64(0)
		if len(lhs) > 0 {
			if v, err := strconv.ParseUint(string(lhs), 10, 64); err == nil {
				l = v
			} else {
				lOk = false
			}
		}
		mOk := true
		m := uint64(0)
		if len(mantissa) > 0 {
			if v, err := strconv.ParseUint(string(mantissa), 10, 64); err == nil {
				m = v
			} else {
				mOk = false
			}
		}
		e := 0
		if ePos != -1 && ePos+1 < len(str) {
			if v, err := strconv.Atoi(string(str[ePos+1:])); err == nil {
				e = v
			}
		}
		// Single vs double precision heuristic as in C++ code
		tok.Double = !is32bit && (!mOk || is64bit || e > 127 || e < -126 || m > 8388607)
		if is32bit && (!lOk || e > 127 || e < -126 || l > 8388607) {
			return lx.token(TokInvalid, off, []byte("literal too large for REAL"))
		}
		if tok.Double && (e > 1023 || e < -1022 || l > 9007199254740991) {
			return lx.token(TokInvalid, off, []byte("literal too large for LONGREAL"))
		}
		return tok
	} else if !isHex && !checkDecNumber(str, is32bit || is64bit) {
		return lx.token(TokInvalid, off, []byte("invalid decimal integer"))
	} else {
		// String already includes possible suffixes; return as integer literal
		return lx.token(TokInteger, off, str)
	}
}

// string_ scans a character or string literal.
func (lx *Lexer) string_() Token {
	quote := lx.lookAhead(0)
	off := 1
	for {
		c := lx.lookAhead(off)
		off++
		if c == quote {
			break
		}
		if c == 0 {
			return lx.token(TokInvalid, off, []byte("non-terminated string"))
		}
	}
	str := lx.line[int(lx.colNr) : int(lx.colNr)+off]
	return lx.token(TokString, off, str)
}

// comment scans a possibly nested comment "(* ... *)", optionally packed to a single token.
func (lx *Lexer) comment() Token {
	startLine := lx.lineNr
	startCol := lx.colNr

	level := 0
	pos := int(lx.colNr)
	parseComment(lx.line, &pos, &level)
	str := make([]byte, 0, pos-int(lx.colNr))
	str = append(str, lx.line[int(lx.colNr):pos]...)
	for level > 0 {
		if !lx.nextLine() {
			break
		}
		pos = 0
		parseComment(lx.line, &pos, &level)
		if len(str) > 0 {
			str = append(str, '\n')
		}
		str = append(str, lx.line[:pos]...)
	}
	if lx.packComments && level > 0 {
		// non-terminated
		t := NewToken(TokInvalid, startLine, uint32(startCol)+1, uint32(len(str)), []byte("non-terminated comment"))
		t.SourcePath = lx.sourcePath
		return t
	}
	// Column +1 because positions are 1-based
	var t Token
	if lx.packComments {
		t = NewToken(TokComment, startLine, uint32(startCol)+1, uint32(len(str)), str)
	} else {
		t = NewToken(TokLatt, startLine, uint32(startCol)+1, uint32(len(str)), str)
	}
	t.SourcePath = lx.sourcePath
	lx.lastToken = t
	lx.colNr = uint16(pos)
	if !lx.packComments && level == 0 {
		rt := NewToken(TokRatt, lx.lineNr, uint32(pos-2)+1, 2, GetSymbol([]byte("*)")))
		rt.SourcePath = lx.sourcePath
		lx.lastToken = rt
		lx.buffer = append(lx.buffer, rt)
	}
	return t
}

// assembler scans a CODE section until an END that is not preceded by a semicolon on the same line.
func (lx *Lexer) assembler() Token {
	startLine := lx.lineNr
	startCol := lx.colNr

	// Find "END" on current line with no earlier ';'
	pos := bytes.Index(lx.line[int(lx.colNr):], []byte("END"))
	if pos != -1 {
		pos += int(lx.colNr)
		semi := bytes.IndexByte(lx.line[int(lx.colNr):], ';')
		if semi != -1 && int(lx.colNr)+semi < pos {
			pos = -1
		}
	}
	var str []byte
	if pos == -1 {
		str = append(str, lx.line[int(lx.colNr):]...)
	} else {
		str = append(str, lx.line[int(lx.colNr):pos]...)
	}
	for pos == -1 {
		if !lx.nextLine() {
			break
		}
		i := bytes.Index(lx.line[int(lx.colNr):], []byte("END"))
		if i != -1 {
			i += int(lx.colNr)
			semi := bytes.IndexByte(lx.line[int(lx.colNr):], ';')
			if semi != -1 && int(lx.colNr)+semi < i {
				i = -1
			}
		}
		if len(str) > 0 {
			str = append(str, '\n')
		}
		if i == -1 {
			str = append(str, lx.line[int(lx.colNr):]...)
		} else {
			str = append(str, lx.line[int(lx.colNr):i]...)
			pos = i
		}
	}
	if lx.packComments && pos == -1 {
		t := NewToken(TokInvalid, startLine, uint32(startCol), 4, []byte("non-terminated CODE section"))
		return t
	}
	t := NewToken(TokCODE, startLine, uint32(startCol)+1, uint32(len(str)), str)
	t.SourcePath = lx.sourcePath
	lx.lastToken = t
	if pos != -1 {
		lx.colNr = uint16(pos)
	} else {
		lx.colNr = uint16(len(lx.line))
	}
	return t
}

// countLine increases the SLOC once per physical line that produced at least one non-comment token.
func (lx *Lexer) countLine() {
	if !lx.lineCounted {
		lx.sloc++
	}
	lx.lineCounted = true
}

// GetSloc returns the number of counted source lines of code.
func (lx *Lexer) GetSloc() uint32 { return lx.sloc }

// parseComment updates pos and level by scanning nested comment delimiters on a single line.
func parseComment(str []byte, pos *int, level *int) {
	type state int
	const (
		Idle state = iota
		Lb
		Star
	)
	s := Idle
	for *pos < len(str) {
		c := str[*pos]
		*pos++
		switch s {
		case Idle:
			if c == '(' {
				s = Lb
			} else if c == '*' {
				s = Star
			}
		case Lb:
			if c == '*' {
				*level++
				s = Idle
			} else if c != '(' {
				s = Idle
			}
		case Star:
			if c == ')' {
				*level--
				s = Idle
			} else if c != '*' {
				s = Idle
			}
			if *level <= 0 {
				return
			}
		}
	}
}

// Utilities

func isAlpha(c byte) bool {
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

func isDigit(c byte) bool { return c >= '0' && c <= '9' }

func isSpace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == 0x0b || c == 0x0c
}

func isLetterOrNumber(c byte) bool {
	return isAlpha(c) || isDigit(c) || c >= 128
}

// Oberon format detection and text extraction

func isOberonFormat(in *Stream) bool {
	if in == nil {
		return false
	}
	in.ToStart()
	v4 := isV4File(in)
	in.ToStart()
	if v4 {
		return true
	}
	off, ln := inferTextRange(in)
	in.ToStart()
	return off != 0 || ln != 0
}

// extractText recognizes Oberon V4/System-2/3 text or plain ASCII and returns UTF-8 bytes.
func extractText(in *Stream) []byte {
	if in == nil {
		return nil
	}
	if isV4File(in) {
		text := readV4Text(in)
		if convertCharSet(text) {
			text = latin1ToUTF8(text)
		}
		return text
	}
	off, ln := inferTextRange(in)
	in.ToStart()
	skipBom(in)
	in.Read(int(off))
	var text []byte
	if ln != 0 {
		text = in.Read(int(ln))
	} else {
		text = in.ReadAll()
	}
	if off != 0 {
		text = bytes.ReplaceAll(text, []byte{'\r'}, []byte{'\n'})
		text = bytes.ReplaceAll(text, []byte{0x00}, []byte{0x20})
		if convertCharSet(text) {
			text = latin1ToUTF8(text)
		}
	}
	return text
}

func isV4File(in *Stream) bool {
	in.ToStart()
	buf := in.Read(2)
	return len(buf) == 2 && buf[0] == 0xF0 && buf[1] == 0x01
}

func readUInt8(in *Stream) byte {
	return in.ReadByte()
}

func readUInt32(in *Stream) uint32 {
	b := in.Read(4)
	if len(b) != 4 {
		return 0
	}
	// little endian: raw[0] + (raw[1] << 8) + ...
	return uint32(b[0]) | uint32(b[1])<<8 | uint32(b[2])<<16 | uint32(b[3])<<24
}

func readStringRS(in *Stream) []byte {
	var res []byte
	for {
		ch := in.ReadByte()
		res = append(res, ch)
		if ch == 0 {
			break
		}
	}
	return res
}

type v4TextRun struct {
	col     uint32
	font    int32
	voff    uint32
	pos     uint32
	len     int32
	element int32
	width   uint32
	height  uint32
	data    []byte
}

func (a v4TextRun) less(b v4TextRun) bool { return a.pos < b.pos }

func extractV4Text(in *Stream, forElement bool) []byte {
	headerLen := readUInt32(in) // includes tag and version
	eltypes := map[int32][]byte{}
	// fonts kept for parity; not used
	_ = eltypes
	var runs []v4TextRun

	fontCount := 0
	elementCount := 0
	fontNumber := int(readUInt8(in))
	pos := int(headerLen)
	if forElement {
		pos-- // expects tag and version, but only a tag is present
	}
	for !in.AtEnd() && fontNumber != 0 {
		piece := v4TextRun{}
		piece.font = int32(fontNumber)
		piece.pos = uint32(pos)
		if fontNumber > fontCount {
			fontCount = fontNumber
			_ = readStringRS(in) // font name, unused
			// fonts.insert(fontNumber, fontName);
		}
		piece.col = uint32(readUInt8(in))
		piece.voff = uint32(readUInt8(in))
		piece.len = int32(readUInt32(in))
		if piece.len <= 0 {
			// this is an element
			elementDataLength := -piece.len
			piece.len = 1
			piece.width = readUInt32(in)
			piece.height = readUInt32(in)
			piece.element = int32(readUInt8(in))
			if int(piece.element) > elementCount {
				elementCount = int(piece.element)
				module := readStringRS(in)
				procedure := readStringRS(in)
				eltypes[piece.element] = append(append([]byte{}, module...), append([]byte("."), procedure...)...)
			}
			piece.data = in.Read(int(elementDataLength))
		}
		pos += int(piece.len)
		runs = append(runs, piece)
		fontNumber = int(readUInt8(in))
	}
	sort.Slice(runs, func(i, j int) bool { return runs[i].less(runs[j]) })
	var res []byte
	blockText := false
	for _, piece := range runs {
		in.Seek(int(piece.pos))
		if len(piece.data) != 0 {
			if bytes.Equal(eltypes[piece.element], []byte("FoldElems.New")) {
				if len(piece.data) > 0 && piece.data[0] == 0 {
					// nested text
					br := NewStream()
					br.ReadFromBytes(piece.data)
					br.ReadByte() // consume 0
					text := extractV4Text(br, true)
					text = bytes.ReplaceAll(text, []byte{'\r'}, []byte{'\n'})
					res = append(res, text...)
					blockText = true
				} else if len(piece.data) > 0 && piece.data[0] == 1 {
					blockText = false
				}
			}
		} else if !blockText {
			text := in.Read(int(piece.len))
			text = bytes.ReplaceAll(text, []byte{'\r'}, []byte{'\n'})
			res = append(res, text...)
		}
	}
	return res
}

func readV4Text(in *Stream) []byte {
	tag := readUInt8(in)
	ver := readUInt8(in)
	if tag == 0xF0 && ver == 0x01 {
		return extractV4Text(in, false)
	}
	return nil
}

func skipBom(in *Stream) bool {
	buf := in.Peek(3)
	if len(buf) == 3 && buf[0] == 0xEF && buf[1] == 0xBB && buf[2] == 0xBF {
		// leave after BOM
		in.Read(3)
		return true
	}
	return false
}

// inferTextRange returns the offset and length of text block for Oberon System 2/3/4 files.
func inferTextRange(in *Stream) (uint32, uint32) {
	var off, ln uint32
	if in == nil {
		return off, ln
	}
	skipBom(in)
	ch := readUInt8(in)

	const DocBlockId = 0xF7
	const TextBlockId = 0xF0
	const OldTextBlockId = 0x01

	if ch == DocBlockId {
		// V3 DocBlock; skip header and re-determine type
		readUInt8(in) // ignore
		readStringRS(in)
		in.Read(8) // four int16, ignore
		ch = readUInt8(in)
		if ch == 0xF7 {
			ch = readUInt8(in)
			if ch == 0x08 {
				l := readUInt32(in)
				in.Read(int(l)) // ignore
				ch = readUInt8(in)
			}
		}
	}
	if ch == TextBlockId || ch == OldTextBlockId {
		off = uint32(in.GetPos())
		_ = readUInt8(in) // type, not required
		hlen := readUInt32(in)
		in.Seek(int(off - 1 + hlen - 4))
		tlen := readUInt32(in)
		off = uint32(in.GetPos())
		ln = tlen
	}
	return off, ln
}

// CharSet conversion

// convert maps Oberon keyboard range 0x80..0x99 to Latin-1 equivalents.
func convert(ch byte) byte {
	// 0x80..0x99 mapping for "ÄÖÜäöüâêîôûàèìòùéëïçáñß£¶Ç"
	code := []byte{
		0xC4, 0xD6, 0xDC, 0xE4, 0xF6, 0xFC, 0xE2, 0xEA, 0xEE, 0xF4, 0xFB, 0xE0, 0xE8,
		0xEC, 0xF2, 0xF9, 0xE9, 0xEB, 0xEF, 0xE7, 0xE1, 0xF1, 0xDF, 0xA3, 0xB6, 0xC7,
	}
	c := ch
	if c >= 0x80 && c <= 0x99 {
		return code[c-0x80]
	}
	return ch
}

// convertCharSet normalizes control characters and maps special codes; returns true if non-ASCII present.
func convertCharSet(str []byte) bool {
	latin1 := false
	for i := 0; i < len(str); i++ {
		if str[i] > 127 {
			latin1 = true
		}
		ch := str[i]
		if isCtrl(ch) {
			if !(ch == 0x0A || ch == 0x0D) {
				ch = ' '
			}
		}
		str[i] = convert(ch)
	}
	return latin1
}

func isCtrl(ch byte) bool { return ch < 0x20 || ch == 0x7F }

func latin1ToUTF8(b []byte) []byte {
	// Each Latin-1 byte maps to the same code point
	r := make([]rune, len(b))
	for i := range b {
		r[i] = rune(b[i])
	}
	return []byte(string(r))
}
