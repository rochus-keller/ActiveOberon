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
	"fmt"
)

type TokenType int

const (
	TokInvalid TokenType = iota

	// Literals
	TTLiterals
	TokHash
	TokAmp
	TokLpar
	TokLatt
	TokRpar
	TokStar
	TokRatt
	TokPlus
	TokComma
	TokMinus
	TokDot
	Tok2Dot
	TokSlash
	TokColon
	TokColonEq
	TokSemi
	TokLt
	TokLeq
	TokEq
	TokGt
	TokGeq
	TokLbrack
	TokRbrack
	TokHat
	TokLbrace
	TokBar
	TokRbrace
	TokTilde

	// Keywords
	TTKeywords
	TokARRAY
	TokBEGIN
	TokBY
	TokCASE
	TokCODE
	TokCONST
	TokDIV
	TokDO
	TokELSE
	TokELSIF
	TokEND
	TokEXIT
	TokFOR
	TokIF
	TokIMPORT
	TokIN
	TokIS
	TokLOOP
	TokMOD
	TokMODULE
	TokNIL
	TokOBJECT
	TokOF
	TokOR
	TokPOINTER
	TokPROCEDURE
	TokRECORD
	TokREPEAT
	TokRETURN
	TokTHEN
	TokTO
	TokTYPE
	TokUNTIL
	TokVAR
	TokWHILE
	TokWITH

	// Specials
	TTSpecials
	TokIdent
	TokInteger
	TokReal
	TokString
	TokHexchar
	TokComment
	TokEof

	TTMaxToken
	TTMax
)

// TokenTypeString returns the string representation of a token
func TokenTypeString(t TokenType) string {
	switch t {
	case TokInvalid:
		return "<invalid>"
	case TokHash:
		return "#"
	case TokAmp:
		return "&"
	case TokLpar:
		return "("
	case TokLatt:
		return "(*"
	case TokRpar:
		return ")"
	case TokStar:
		return "*"
	case TokRatt:
		return "*)"
	case TokPlus:
		return "+"
	case TokComma:
		return ","
	case TokMinus:
		return "-"
	case TokDot:
		return "."
	case Tok2Dot:
		return ".."
	case TokSlash:
		return "/"
	case TokColon:
		return ":"
	case TokColonEq:
		return ":="
	case TokSemi:
		return ";"
	case TokLt:
		return "<"
	case TokLeq:
		return "<="
	case TokEq:
		return "="
	case TokGt:
		return ">"
	case TokGeq:
		return ">="
	case TokLbrack:
		return "["
	case TokRbrack:
		return "]"
	case TokHat:
		return "^"
	case TokLbrace:
		return "{"
	case TokBar:
		return "|"
	case TokRbrace:
		return "}"
	case TokTilde:
		return "~"
	case TokARRAY:
		return "ARRAY"
	case TokBEGIN:
		return "BEGIN"
	case TokBY:
		return "BY"
	case TokCASE:
		return "CASE"
	case TokCODE:
		return "CODE"
	case TokCONST:
		return "CONST"
	case TokDIV:
		return "DIV"
	case TokDO:
		return "DO"
	case TokELSE:
		return "ELSE"
	case TokELSIF:
		return "ELSIF"
	case TokEND:
		return "END"
	case TokEXIT:
		return "EXIT"
	case TokFOR:
		return "FOR"
	case TokIF:
		return "IF"
	case TokIMPORT:
		return "IMPORT"
	case TokIN:
		return "IN"
	case TokIS:
		return "IS"
	case TokLOOP:
		return "LOOP"
	case TokMOD:
		return "MOD"
	case TokMODULE:
		return "MODULE"
	case TokNIL:
		return "NIL"
	case TokOBJECT:
		return "OBJECT"
	case TokOF:
		return "OF"
	case TokOR:
		return "OR"
	case TokPOINTER:
		return "POINTER"
	case TokPROCEDURE:
		return "PROCEDURE"
	case TokRECORD:
		return "RECORD"
	case TokREPEAT:
		return "REPEAT"
	case TokRETURN:
		return "RETURN"
	case TokTHEN:
		return "THEN"
	case TokTO:
		return "TO"
	case TokTYPE:
		return "TYPE"
	case TokUNTIL:
		return "UNTIL"
	case TokVAR:
		return "VAR"
	case TokWHILE:
		return "WHILE"
	case TokWITH:
		return "WITH"
	case TokIdent:
		return "ident"
	case TokInteger:
		return "integer"
	case TokReal:
		return "real"
	case TokString:
		return "string"
	case TokHexchar:
		return "hexchar"
	case TokComment:
		return "Comment"
	case TokEof:
		return "<eof>"
	default:
		return ""
	}
}

// TokenTypeName returns the name of a token type
func TokenTypeName(t TokenType) string {
	switch t {
	case TokInvalid:
		return "TokInvalid"
	case TokHash:
		return "TokHash"
	case TokAmp:
		return "TokAmp"
	case TokLpar:
		return "TokLpar"
	case TokLatt:
		return "TokLatt"
	case TokRpar:
		return "TokRpar"
	case TokStar:
		return "TokStar"
	case TokRatt:
		return "TokRatt"
	case TokPlus:
		return "TokPlus"
	case TokComma:
		return "TokComma"
	case TokMinus:
		return "TokMinus"
	case TokDot:
		return "TokDot"
	case Tok2Dot:
		return "Tok2Dot"
	case TokSlash:
		return "TokSlash"
	case TokColon:
		return "TokColon"
	case TokColonEq:
		return "TokColonEq"
	case TokSemi:
		return "TokSemi"
	case TokLt:
		return "TokLt"
	case TokLeq:
		return "TokLeq"
	case TokEq:
		return "TokEq"
	case TokGt:
		return "TokGt"
	case TokGeq:
		return "TokGeq"
	case TokLbrack:
		return "TokLbrack"
	case TokRbrack:
		return "TokRbrack"
	case TokHat:
		return "TokHat"
	case TokLbrace:
		return "TokLbrace"
	case TokBar:
		return "TokBar"
	case TokRbrace:
		return "TokRbrace"
	case TokTilde:
		return "TokTilde"
	case TokARRAY:
		return "TokARRAY"
	case TokBEGIN:
		return "TokBEGIN"
	case TokBY:
		return "TokBY"
	case TokCASE:
		return "TokCASE"
	case TokCODE:
		return "TokCODE"
	case TokCONST:
		return "TokCONST"
	case TokDIV:
		return "TokDIV"
	case TokDO:
		return "TokDO"
	case TokELSE:
		return "TokELSE"
	case TokELSIF:
		return "TokELSIF"
	case TokEND:
		return "TokEND"
	case TokEXIT:
		return "TokEXIT"
	case TokFOR:
		return "TokFOR"
	case TokIF:
		return "TokIF"
	case TokIMPORT:
		return "TokIMPORT"
	case TokIN:
		return "TokIN"
	case TokIS:
		return "TokIS"
	case TokLOOP:
		return "TokLOOP"
	case TokMOD:
		return "TokMOD"
	case TokMODULE:
		return "TokMODULE"
	case TokNIL:
		return "TokNIL"
	case TokOBJECT:
		return "TokOBJECT"
	case TokOF:
		return "TokOF"
	case TokOR:
		return "TokOR"
	case TokPOINTER:
		return "TokPOINTER"
	case TokPROCEDURE:
		return "TokPROCEDURE"
	case TokRECORD:
		return "TokRECORD"
	case TokREPEAT:
		return "TokREPEAT"
	case TokRETURN:
		return "TokRETURN"
	case TokTHEN:
		return "TokTHEN"
	case TokTO:
		return "TokTO"
	case TokTYPE:
		return "TokTYPE"
	case TokUNTIL:
		return "TokUNTIL"
	case TokVAR:
		return "TokVAR"
	case TokWHILE:
		return "TokWHILE"
	case TokWITH:
		return "TokWITH"
	case TokIdent:
		return "TokIdent"
	case TokInteger:
		return "TokInteger"
	case TokReal:
		return "TokReal"
	case TokString:
		return "TokString"
	case TokHexchar:
		return "TokHexchar"
	case TokComment:
		return "TokComment"
	case TokEof:
		return "TokEof"
	default:
		return "TokUnknown"
	}
}

// TokenTypeIsLiteral checks if token is a literal
func TokenTypeIsLiteral(t TokenType) bool {
	return t > TTLiterals && t < TTKeywords
}

// TokenTypeIsKeyword checks if token is a keyword
func TokenTypeIsKeyword(t TokenType) bool {
	return t > TTKeywords && t < TTSpecials
}

// TokenTypeIsSpecial checks if token is special
func TokenTypeIsSpecial(t TokenType) bool {
	return t > TTSpecials && t < TTMax
}

// RowCol represents position in source code
type RowCol struct {
	Row uint32
	Col uint32
}

// Constants for RowCol bit manipulation
const (
	ROWBitLen        = 19
	COLBitLen        = 32 - ROWBitLen - 1
	MSB       uint32 = 0x80000000
)

// NewRowCol creates a new RowCol
func NewRowCol(row, col uint32) RowCol {
	return RowCol{Row: row, Col: col}
}

func (rc RowCol) String() string {
	return fmt.Sprintf("%d:%d", rc.Row, rc.Col)
}

// IsValid checks if position is valid
func (rc RowCol) IsValid() bool {
	return rc.Row > 0 && rc.Col > 0
}

// Packed returns packed representation
func (rc RowCol) Packed() uint32 {
	return (rc.Row << COLBitLen) | rc.Col | MSB
}

// IsPacked checks if value is packed
func IsPacked(rowCol uint32) bool {
	return rowCol&MSB != 0
}

// UnpackCol extracts column from packed value
func UnpackCol(rowCol uint32) uint32 {
	return rowCol & ((1 << COLBitLen) - 1)
}

// UnpackRow extracts row from packed value
func UnpackRow(rowCol uint32) uint32 {
	return ((rowCol & ^MSB) >> COLBitLen)
}

// Loc extends RowCol with file information
type Loc struct {
	RowCol
	File string
}

// NewLoc creates a new location
func NewLoc(row, col uint32, file string) Loc {
	return Loc{RowCol: NewRowCol(row, col), File: file}
}

// Range represents a range of positions
type Range struct {
	pos, End RowCol
}

// FilePos represents a position in a file
type FilePos struct {
	Pos      RowCol
	FilePath string
}

// Token represents a lexical token
type Token struct {
	Type       TokenType
	Len        uint16
	LineNr     uint32
	ColNr      uint32
	Double     bool // for floating point precision
	Val        []byte
	SourcePath string
}

// NewToken creates a new token
func NewToken(tokenType TokenType, line, col, length uint32, val []byte) Token {
	return Token{
		Type:   tokenType,
		Len:    uint16(length),
		LineNr: line,
		ColNr:  col,
		Double: false,
		Val:    val,
	}
}

// IsValid checks if token is valid
func (t Token) IsValid() bool {
	return t.Type != TokInvalid && t.Type != TokEof
}

// IsEof checks if token is EOF
func (t Token) IsEof() bool {
	return t.Type == TokEof
}

// GetName returns token type name
func (t Token) GetName() string {
	return TokenTypeName(t.Type)
}

// GetString returns token string representation
func (t Token) GetString() string {
	return TokenTypeString(t.Type)
}

// ToRowCol converts token position to RowCol
func (t Token) ToRowCol() RowCol {
	return NewRowCol(t.LineNr, t.ColNr)
}

// ToLoc converts token to location
func (t Token) ToLoc() Loc {
	return NewLoc(t.LineNr, t.ColNr, t.SourcePath)
}

// TokenList represents a list of tokens
type TokenList []Token

// Symbol table for tokens (equivalent to Qt's symbol table)
var symbolTable = make(map[string][]byte)

// GetSymbol returns canonical symbol representation
func GetSymbol(data []byte) []byte {
	str := string(data)
	if existing, found := symbolTable[str]; found {
		return existing
	}
	canonical := make([]byte, len(data))
	copy(canonical, data)
	symbolTable[str] = canonical
	return canonical
}

// It reads posing at 'pos' and returns token and new pos.
func TokenTypeFromString(str []byte, pos *int) TokenType {
	at := func(i int) byte {
		if i < len(str) {
			return str[i]
		}
		return 0
	}
	var i int = 0
	if pos != nil {
		i = *pos
	}
	var res TokenType = TokInvalid

	switch at(i) {
	case '#':
		res = TokHash
		i += 1
	case '&':
		res = TokAmp
		i += 1
	case '(':
		if at(i+1) == '*' {
			res = TokLatt
			i += 2
		} else {
			res = TokLpar
			i += 1
		}
	case ')':
		res = TokRpar
		i += 1
	case '*':
		if at(i+1) == ')' {
			res = TokRatt
			i += 2
		} else {
			res = TokStar
			i += 1
		}
	case '+':
		res = TokPlus
		i += 1
	case ',':
		res = TokComma
		i += 1
	case '-':
		res = TokMinus
		i += 1
	case '.':
		if at(i+1) == '.' {
			res = Tok2Dot
			i += 2
		} else {
			res = TokDot
			i += 1
		}
	case '/':
		res = TokSlash
		i += 1
	case ':':
		if at(i+1) == '=' {
			res = TokColonEq
			i += 2
		} else {
			res = TokColon
			i += 1
		}
	case ';':
		res = TokSemi
		i += 1
	case '<':
		if at(i+1) == '=' {
			res = TokLeq
			i += 2
		} else {
			res = TokLt
			i += 1
		}
	case '=':
		res = TokEq
		i += 1
	case '>':
		if at(i+1) == '=' {
			res = TokGeq
			i += 2
		} else {
			res = TokGt
			i += 1
		}
	case 'A':
		if at(i+1) == 'R' && at(i+2) == 'R' && at(i+3) == 'A' && at(i+4) == 'Y' {
			res = TokARRAY
			i += 5
		}
	case 'B':
		switch at(i + 1) {
		case 'E':
			if at(i+2) == 'G' && at(i+3) == 'I' && at(i+4) == 'N' {
				res = TokBEGIN
				i += 5
			}
		case 'Y':
			res = TokBY
			i += 2
		}
	case 'C':
		switch at(i + 1) {
		case 'A':
			if at(i+2) == 'S' && at(i+3) == 'E' {
				res = TokCASE
				i += 4
			}
		case 'O':
			switch at(i + 2) {
			case 'D':
				if at(i+3) == 'E' {
					res = TokCODE
					i += 4
				}
			case 'N':
				if at(i+3) == 'S' && at(i+4) == 'T' {
					res = TokCONST
					i += 5
				}
			}
		}
	case 'D':
		switch at(i + 1) {
		case 'I':
			if at(i+2) == 'V' {
				res = TokDIV
				i += 3
			}
		case 'O':
			res = TokDO
			i += 2
		}
	case 'E':
		switch at(i + 1) {
		case 'L':
			if at(i+2) == 'S' {
				switch at(i + 3) {
				case 'E':
					res = TokELSE
					i += 4
				case 'I':
					if at(i+4) == 'F' {
						res = TokELSIF
						i += 5
					}
				}
			}
		case 'N':
			if at(i+2) == 'D' {
				res = TokEND
				i += 3
			}
		case 'X':
			if at(i+2) == 'I' && at(i+3) == 'T' {
				res = TokEXIT
				i += 4
			}
		}
	case 'F':
		if at(i+1) == 'O' && at(i+2) == 'R' {
			res = TokFOR
			i += 3
		}
	case 'I':
		switch at(i + 1) {
		case 'F':
			res = TokIF
			i += 2
		case 'M':
			if at(i+2) == 'P' && at(i+3) == 'O' && at(i+4) == 'R' && at(i+5) == 'T' {
				res = TokIMPORT
				i += 6
			}
		case 'N':
			res = TokIN
			i += 2
		case 'S':
			res = TokIS
			i += 2
		}
	case 'L':
		if at(i+1) == 'O' && at(i+2) == 'O' && at(i+3) == 'P' {
			res = TokLOOP
			i += 4
		}
	case 'M':
		if at(i+1) == 'O' && at(i+2) == 'D' {
			if at(i+3) == 'U' && at(i+4) == 'L' && at(i+5) == 'E' {
				res = TokMODULE
				i += 6
			} else {
				res = TokMOD
				i += 3
			}
		}
	case 'N':
		if at(i+1) == 'I' && at(i+2) == 'L' {
			res = TokNIL
			i += 3
		}
	case 'O':
		switch at(i + 1) {
		case 'B':
			if at(i+2) == 'J' && at(i+3) == 'E' && at(i+4) == 'C' && at(i+5) == 'T' {
				res = TokOBJECT
				i += 6
			}
		case 'F':
			res = TokOF
			i += 2
		case 'R':
			res = TokOR
			i += 2
		}
	case 'P':
		switch at(i + 1) {
		case 'O':
			if at(i+2) == 'I' && at(i+3) == 'N' && at(i+4) == 'T' && at(i+5) == 'E' && at(i+6) == 'R' {
				res = TokPOINTER
				i += 7
			}
		case 'R':
			if at(i+2) == 'O' && at(i+3) == 'C' && at(i+4) == 'E' && at(i+5) == 'D' && at(i+6) == 'U' && at(i+7) == 'R' && at(i+8) == 'E' {
				res = TokPROCEDURE
				i += 9
			}
		}
	case 'R':
		if at(i+1) == 'E' {
			switch at(i + 2) {
			case 'C':
				if at(i+3) == 'O' && at(i+4) == 'R' && at(i+5) == 'D' {
					res = TokRECORD
					i += 6
				}
			case 'P':
				if at(i+3) == 'E' && at(i+4) == 'A' && at(i+5) == 'T' {
					res = TokREPEAT
					i += 6
				}
			case 'T':
				if at(i+3) == 'U' && at(i+4) == 'R' && at(i+5) == 'N' {
					res = TokRETURN
					i += 6
				}
			}
		}
	case 'T':
		switch at(i + 1) {
		case 'H':
			if at(i+2) == 'E' && at(i+3) == 'N' {
				res = TokTHEN
				i += 4
			}
		case 'O':
			res = TokTO
			i += 2
		case 'Y':
			if at(i+2) == 'P' && at(i+3) == 'E' {
				res = TokTYPE
				i += 4
			}
		}
	case 'U':
		if at(i+1) == 'N' && at(i+2) == 'T' && at(i+3) == 'I' && at(i+4) == 'L' {
			res = TokUNTIL
			i += 5
		}
	case 'V':
		if at(i+1) == 'A' && at(i+2) == 'R' {
			res = TokVAR
			i += 3
		}
	case 'W':
		switch at(i + 1) {
		case 'H':
			if at(i+2) == 'I' && at(i+3) == 'L' && at(i+4) == 'E' {
				res = TokWHILE
				i += 5
			}
		case 'I':
			if at(i+2) == 'T' && at(i+3) == 'H' {
				res = TokWITH
				i += 4
			}
		}
	case '[':
		res = TokLbrack
		i += 1
	case ']':
		res = TokRbrack
		i += 1
	case '^':
		res = TokHat
		i += 1
	case '{':
		res = TokLbrace
		i += 1
	case '|':
		res = TokBar
		i += 1
	case '}':
		res = TokRbrace
		i += 1
	case '~':
		res = TokTilde
		i += 1
	}

	if res == TokInvalid {
		return TokInvalid
	}
	*pos = i
	return res
}
