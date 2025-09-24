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
	"strconv"
	"strings"
)

// Parser implements the ActiveOberon parser
type Parser struct {
	scanner       Scanner
	model         *AstModel
	thisMod       *Declaration
	cur           Token
	la            Token // lookahead
	errors        []ParseError
	predefSymbols [][]byte
	beginSymbol   []byte
}

// ParseError represents a parsing error
type ParseError struct {
	Msg  string
	Pos  RowCol
	Path string
}

// NewParseError creates a new parse error
func NewParseError(msg string, pos RowCol, path string) ParseError {
	return ParseError{Msg: msg, Pos: pos, Path: path}
}

// ID represents an identifier with visibility
type ID struct {
	Name     Token
	Visi     Visi
	Untraced bool
}

// NewID creates a new ID
func NewID() ID {
	return ID{Visi: VISI_Private}
}

// IsValid checks if ID is valid
func (id ID) IsValid() bool {
	return id.Name.Type == TokIdent
}

// Attributes for declarations
type Attrs int

const (
	ATTR_UNTRACED Attrs = iota
	ATTR_ACTIVE
	ATTR_DELEGATE
	ATTR_EXCLUSIVE
	ATTR_PRIORITY
	ATTR_SAFE
	ATTR_FFI
	ATTR_MaxAttr
)

// NewParser2 creates a new parser
func NewParser(model *AstModel, scanner Scanner) *Parser {
	p := &Parser{
		scanner:       scanner,
		model:         model,
		errors:        make([]ParseError, 0),
		predefSymbols: make([][]byte, ATTR_MaxAttr),
	}

	// Initialize predefined symbols
	p.predefSymbols[ATTR_UNTRACED] = GetSymbol([]byte("UNTRACED"))
	p.predefSymbols[ATTR_ACTIVE] = GetSymbol([]byte("ACTIVE"))
	p.predefSymbols[ATTR_DELEGATE] = GetSymbol([]byte("DELEGATE"))
	p.predefSymbols[ATTR_EXCLUSIVE] = GetSymbol([]byte("EXCLUSIVE"))
	p.predefSymbols[ATTR_PRIORITY] = GetSymbol([]byte("PRIORITY"))
	p.predefSymbols[ATTR_SAFE] = GetSymbol([]byte("SAFE"))
	p.predefSymbols[ATTR_FFI] = GetSymbol([]byte("C"))
	p.beginSymbol = GetSymbol([]byte("BEGIN"))

	return p
}

// RunParser starts the parsing process
func (p *Parser) RunParser() {
	p.errors = p.errors[:0]
	p.next()
	p.parseModule()
}

// TakeResult returns the parsed module and transfers ownership
func (p *Parser) TakeResult() *Declaration {
	result := p.thisMod
	p.thisMod = nil
	return result
}

// next advances to the next token
func (p *Parser) next() {
	p.cur = p.la
	p.la = p.scanner.Next()

	for p.la.Type == TokInvalid {
		p.errors = append(p.errors, NewParseError(string(p.la.Val), p.la.ToRowCol(), p.la.SourcePath))
		p.la = p.scanner.Next()
	}
}

// peek returns token at offset
func (p *Parser) peek(off int) Token {
	if off == 1 {
		return p.la
	} else if off == 0 {
		return p.cur
	} else {
		return p.scanner.Peek(off - 1)
	}
}

// expect checks for expected token and advances
func (p *Parser) expect(tokenType TokenType, where string) bool {
	if p.la.Type == tokenType {
		p.next()
		return true
	}

	p.errors = append(p.errors, NewParseError(
		fmt.Sprintf("'%s' expected in %s", TokenTypeString(tokenType), where),
		p.la.ToRowCol(), p.la.SourcePath))
	return false
}

// error adds a parsing error
func (p *Parser) syntaxError(msg string) {
	p.errors = append(p.errors, NewParseError(msg, p.la.ToRowCol(), p.la.SourcePath))
}

// semanticError adds a semantic error at specific position
func (p *Parser) semanticError(msg string, pos RowCol) {
	p.errors = append(p.errors, NewParseError(msg, pos, p.la.SourcePath))
}

// addDecl adds a declaration to the current scope
func (p *Parser) addDecl(token Token, visi Visi, kind DeclKind) *Declaration {
	name := string(token.Val)
	decl := p.model.AddDecl(name)
	if decl == nil {
		p.semanticError(fmt.Sprintf("duplicate declaration: %s", name), token.ToRowCol())
		return nil
	}

	decl.Kind = int(kind)
	decl.Visi = visi
	decl.Pos = token.ToRowCol()
	return decl
}

// parseModule parses a module
func (p *Parser) parseModule() {
	if p.la.Type != TokMODULE {
		p.syntaxError("not an ActiveOberon module")
		return
	}

	m := NewDeclaration()
	m.Kind = int(DECL_Module)
	p.thisMod = m
	p.model.OpenScope(m)

	p.expect(TokMODULE, "Module")
	p.expect(TokIdent, "Module")

	m.Name = p.cur.Val
	m.Pos = p.cur.ToRowCol()

	moduleData := &ModuleData{
		SourcePath: p.scanner.Source(),
		FullName:   p.cur.Val,
	}
	m.Data = moduleData

	p.expect(TokSemi, "Module")

	if p.firstImportList() {
		p.parseImportList()
	}

	p.parseDeclSeq(false)

	if p.firstBody() {
		p.la.Val = p.beginSymbol
		procDecl := p.addDecl(p.la, VISI_Private, DECL_Procedure)
		if procDecl != nil {
			procDecl.Begin = true
			p.model.OpenScope(procDecl)
			procDecl.Body = p.parseBody()
			p.model.CloseScope()
		}
	}

	p.expect(TokEND, "Module")
	p.expect(TokIdent, "Module")

	moduleData.End = p.cur.ToRowCol()
	p.model.CloseScope()

	if p.la.Type != TokDot {
		p.syntaxError("expecting a dot at the end of a module")
	}
}

// parseImportList parses import declarations
func (p *Parser) parseImportList() {
	p.expect(TokIMPORT, "ImportList")
	p.parseImportDecl()

	for p.la.Type == TokComma {
		p.expect(TokComma, "ImportList")
		p.parseImportDecl()
	}

	p.expect(TokSemi, "ImportList")
}

// parseImportDecl parses a single import declaration
func (p *Parser) parseImportDecl() {
	var localName Token

	if p.peek(1).Type == TokIdent && p.peek(2).Type == TokColonEq {
		p.expect(TokIdent, "ImportDecl")
		localName = p.cur
		p.expect(TokColonEq, "ImportDecl")
	}

	p.expect(TokIdent, "ImportDecl")
	module := p.cur

	if localName.Type == 0 {
		localName = module
	}

	importDecl := p.addDecl(localName, VISI_NA, DECL_Import)
	if importDecl != nil {
		importData := &Import{
			ModuleName: module.Val,
			ImportedAt: module.ToRowCol(),
			Importer:   p.thisMod,
		}
		importDecl.Data = importData
	}
}

// parseDeclSeq parses declaration sequence
func (p *Parser) parseDeclSeq(inObjectType bool) {
	for p.firstDeclSeq() {
		switch p.la.Type {
		case TokCONST:
			p.parseConstDecl()
		case TokTYPE:
			p.parseTypeDecl()
		case TokVAR:
			p.parseVarDecl(inObjectType)
		case TokPROCEDURE:
			p.parseProcDecl()
		}
	}
}

// parseConstDecl parses constant declarations
func (p *Parser) parseConstDecl() {
	p.expect(TokCONST, "ConstDecl")

	for p.firstConstDecl() {
		id := p.parseIdentDef()
		if !id.IsValid() {
			break
		}

		decl := p.addDecl(id.Name, id.Visi, DECL_ConstDecl)
		if decl == nil {
			break
		}

		p.expect(TokEq, "ConstDecl")
		decl.Expr = p.parseConstExpr()
		p.expect(TokSemi, "ConstDecl")
	}
}

// parseTypeDecl parses type declarations
func (p *Parser) parseTypeDecl() {
	p.expect(TokTYPE, "TypeDecl")

	for p.firstTypeDecl() {
		id := p.parseIdentDef()
		if !id.IsValid() {
			break
		}

		decl := p.addDecl(id.Name, id.Visi, DECL_TypeDecl)
		if decl == nil {
			break
		}

		p.expect(TokEq, "TypeDecl")
		decl.SetType(p.parseType())
		p.expect(TokSemi, "TypeDecl")
	}
}

// parseVarDecl parses variable declarations
func (p *Parser) parseVarDecl(inObjectType bool) {
	p.expect(TokVAR, "VarDecl")

	for p.firstVarDecl() {
		identList := p.parseIdentList()
		p.expect(TokColon, "VarDecl")
		varType := p.parseType()

		for _, id := range identList {
			var kind DeclKind
			if inObjectType {
				kind = DECL_Field
			} else {
				kind = DECL_VarDecl
			}

			decl := p.addDecl(id.Name, id.Visi, kind)
			if decl != nil {
				decl.SetType(varType)
			}
		}

		p.expect(TokSemi, "VarDecl")
	}
}

// parseProcDecl parses procedure declarations
func (p *Parser) parseProcDecl() {
	p.expect(TokPROCEDURE, "ProcDecl")

	// Check for procedure heading modifiers
	var attrs map[Attrs]bool
	if p.la.Type == TokLbrace {
		attrs = p.parseAttributes()
	}

	id := p.parseIdentDef()
	if !id.IsValid() {
		return
	}

	procDecl := p.addDecl(id.Name, id.Visi, DECL_Procedure)
	if procDecl == nil {
		return
	}

	// Apply attributes
	if attrs != nil {
		if attrs[ATTR_ACTIVE] {
			procDecl.Active = true
		}
		if attrs[ATTR_EXCLUSIVE] {
			procDecl.Exclusive = true
		}
		// Apply other attributes as needed
	}

	p.model.OpenScope(procDecl)

	// Parse formal parameters
	if p.la.Type == TokLpar {
		p.parseFormalParams()
	}

	// Parse return type
	if p.la.Type == TokColon {
		p.expect(TokColon, "ProcDecl")
		returnType := p.parseType()
		procDecl.SetType(returnType)
	} else {
		procDecl.SetType(p.model.GetType(TYPE_NoType))
	}

	p.expect(TokSemi, "ProcDecl")

	// Parse procedure body
	if p.firstDeclSeq() {
		p.parseDeclSeq(false)
	}

	if p.firstBody() {
		procDecl.Body = p.parseBody()
	}

	p.expect(TokEND, "ProcDecl")
	p.expect(TokIdent, "ProcDecl")
	p.expect(TokSemi, "ProcDecl")

	p.model.CloseScope()
}

// parseFormalParams parses formal parameters
func (p *Parser) parseFormalParams() {
	p.expect(TokLpar, "FormalParams")

	if p.firstFormalParam() {
		p.parseFormalParam()
		for p.la.Type == TokSemi {
			p.expect(TokSemi, "FormalParams")
			p.parseFormalParam()
		}
	}

	p.expect(TokRpar, "FormalParams")
}

// parseFormalParam parses a formal parameter
func (p *Parser) parseFormalParam() {
	var varParam bool
	if p.la.Type == TokVAR {
		p.expect(TokVAR, "FormalParam")
		varParam = true
	}

	identList := p.parseIdentList()
	p.expect(TokColon, "FormalParam")
	paramType := p.parseType()

	for _, id := range identList {
		paramDecl := p.addDecl(id.Name, id.Visi, DECL_ParamDecl)
		if paramDecl != nil {
			paramDecl.SetType(paramType)
			paramDecl.VarParam = varParam
		}
	}
}

// parseType parses type expressions
func (p *Parser) parseType() *Type {
	switch p.la.Type {
	case TokIdent:
		return p.parseNamedType()
	case TokARRAY:
		return p.parseArrayType()
	case TokRECORD:
		return p.parseRecordType()
	case TokOBJECT:
		return p.parseObjectType()
	case TokPOINTER:
		return p.parsePointerType()
	case TokPROCEDURE:
		return p.parseProcType()
	default:
		p.syntaxError("type expected")
		return p.model.GetType(TYPE_Undefined)
	}
}

// parseNamedType parses named types and qualidents
func (p *Parser) parseNamedType() *Type {
	t := NewType()
	t.Kind = int(TYPE_NameRef)
	t.Quali = p.parseQualident()
	return t
}

// parseArrayType parses array types
func (p *Parser) parseArrayType() *Type {
	p.expect(TokARRAY, "ArrayType")

	t := NewType()
	t.Kind = int(TYPE_Array)

	// Parse array length
	if p.la.Type != TokOF {
		t.Expr = p.parseConstExpr()
	}

	p.expect(TokOF, "ArrayType")
	t.TypeRef = p.parseType()

	return t
}

// parseRecordType parses record types
func (p *Parser) parseRecordType() *Type {
	p.expect(TokRECORD, "RecordType")

	t := NewType()
	t.Kind = int(TYPE_Record)

	// Parse base type
	if p.la.Type == TokLpar {
		p.expect(TokLpar, "RecordType")
		t.TypeRef = p.parseType()
		p.expect(TokRpar, "RecordType")
	}

	p.model.OpenScope(NewDeclaration())

	// Parse field list
	if p.firstFieldList() {
		p.parseFieldList()
	}

	fields := p.model.CloseScope()
	for _, field := range fields {
		t.Subs = append(t.Subs, field)
	}

	p.expect(TokEND, "RecordType")

	return t
}

// parseObjectType parses object types
func (p *Parser) parseObjectType() *Type {
	p.expect(TokOBJECT, "ObjectType")

	t := NewType()
	t.Kind = int(TYPE_Object)

	// Parse base class
	if p.la.Type == TokLpar {
		p.expect(TokLpar, "ObjectType")
		t.TypeRef = p.parseType()
		p.expect(TokRpar, "ObjectType")
	}

	p.model.OpenScope(NewDeclaration())

	// Parse declarations
	p.parseDeclSeq(true)

	members := p.model.CloseScope()
	for _, member := range members {
		t.Subs = append(t.Subs, member)
	}

	p.expect(TokEND, "ObjectType")

	return t
}

// parsePointerType parses pointer types
func (p *Parser) parsePointerType() *Type {
	p.expect(TokPOINTER, "PointerType")
	p.expect(TokTO, "PointerType")

	t := NewType()
	t.Kind = int(TYPE_Pointer)
	t.TypeRef = p.parseType()

	return t
}

// parseProcType parses procedure types
func (p *Parser) parseProcType() *Type {
	p.expect(TokPROCEDURE, "ProcType")

	t := NewType()
	t.Kind = int(TYPE_Procedure)

	p.model.OpenScope(NewDeclaration())

	// Parse formal parameters
	if p.la.Type == TokLpar {
		p.parseFormalParams()
	}

	// Parse return type
	if p.la.Type == TokColon {
		p.expect(TokColon, "ProcType")
		t.TypeRef = p.parseType()
	}

	params := p.model.CloseScope()
	for _, param := range params {
		t.Subs = append(t.Subs, param)
	}

	return t
}

// parseFieldList parses record field lists
func (p *Parser) parseFieldList() {
	p.parseFieldDecl()
	for p.la.Type == TokSemi {
		p.expect(TokSemi, "FieldList")
		if p.firstFieldDecl() {
			p.parseFieldDecl()
		}
	}
}

// parseFieldDecl parses field declarations
func (p *Parser) parseFieldDecl() {
	identList := p.parseIdentList()
	p.expect(TokColon, "FieldDecl")
	fieldType := p.parseType()

	for _, id := range identList {
		fieldDecl := p.addDecl(id.Name, id.Visi, DECL_Field)
		if fieldDecl != nil {
			fieldDecl.SetType(fieldType)
		}
	}
}

// parseQualident parses qualified identifiers
func (p *Parser) parseQualident() *Qualident {
	p.expect(TokIdent, "Qualident")
	first := p.cur.Val

	if p.la.Type == TokDot {
		p.expect(TokDot, "Qualident")
		p.expect(TokIdent, "Qualident")
		second := p.cur.Val
		return NewQualident(first, second)
	}

	return NewQualident(first, nil)
}

// parseIdentDef parses identifier definitions with visibility
func (p *Parser) parseIdentDef() ID {
	id := NewID()

	if p.la.Type == TokIdent {
		p.expect(TokIdent, "IdentDef")
		id.Name = p.cur

		// Check for visibility marker
		if p.la.Type == TokStar {
			p.expect(TokStar, "IdentDef")
			id.Visi = VISI_ReadOnly
		} else if p.la.Type == TokMinus {
			p.expect(TokMinus, "IdentDef")
			id.Visi = VISI_ReadWrite
		}
	}

	return id
}

// parseIdentList parses identifier lists
func (p *Parser) parseIdentList() []ID {
	var identList []ID

	id := p.parseIdentDef()
	if id.IsValid() {
		identList = append(identList, id)

		for p.la.Type == TokComma {
			p.expect(TokComma, "IdentList")
			id = p.parseIdentDef()
			if id.IsValid() {
				identList = append(identList, id)
			}
		}
	}

	return identList
}

// parseAttributes parses procedure attributes
func (p *Parser) parseAttributes() map[Attrs]bool {
	attrs := make(map[Attrs]bool)

	p.expect(TokLbrace, "Attributes")

	if p.la.Type == TokIdent {
		attr := p.parseAttribute()
		if attr != ATTR_MaxAttr {
			attrs[attr] = true
		}

		for p.la.Type == TokComma {
			p.expect(TokComma, "Attributes")
			attr = p.parseAttribute()
			if attr != ATTR_MaxAttr {
				attrs[attr] = true
			}
		}
	}

	p.expect(TokRbrace, "Attributes")

	return attrs
}

// parseAttribute parses a single attribute
func (p *Parser) parseAttribute() Attrs {
	if p.la.Type == TokIdent {
		p.expect(TokIdent, "Attribute")
		attrName := string(p.cur.Val)

		switch strings.ToUpper(attrName) {
		case "UNTRACED":
			return ATTR_UNTRACED
		case "ACTIVE":
			return ATTR_ACTIVE
		case "DELEGATE":
			return ATTR_DELEGATE
		case "EXCLUSIVE":
			return ATTR_EXCLUSIVE
		case "PRIORITY":
			return ATTR_PRIORITY
		case "SAFE":
			return ATTR_SAFE
		case "C":
			return ATTR_FFI
		default:
			p.syntaxError("unknown attribute: " + attrName)
		}
	}
	return ATTR_MaxAttr
}

// Expression parsing methods

func (p *Parser) firstRelation() bool {
	return p.la.Type == TokEq || p.la.Type == TokHash || p.la.Type == TokLt ||
		p.la.Type == TokLeq || p.la.Type == TokGt || p.la.Type == TokGeq ||
		p.la.Type == TokIN || p.la.Type == TokIS
}

// createFromTokenRelation creates expression from relational token
func (p *Parser) createFromTokenRelation() *Expression {
	tok := p.la
	p.next()
	return CreateFromToken(tok.Type, tok.ToRowCol())
}

// parseExpr parses expressions
func (p *Parser) parseExpr() *Expression {
	res := p.parseSimpleExpr()
	if res == nil {
		return nil
	}

	if p.firstRelation() {
		tmp := p.createFromTokenRelation()
		tmp.Lhs = res
		tmp.SetType(p.model.GetType(TYPE_BOOLEAN))
		res = tmp
		res.Rhs = p.parseSimpleExpr()
		if res.Rhs == nil {
			return nil
		}
	}
	return res
}

func (p *Parser) parseSimpleExpr() *Expression {
	var op TokenType = TokInvalid
	var tok Token = p.la

	// Handle unary operators (+ or -)
	if p.la.Type == TokPlus || p.la.Type == TokMinus {
		if p.la.Type == TokPlus {
			p.expect(TokPlus, "SimpleExpr")
			op = TokPlus
		} else if p.la.Type == TokMinus {
			p.expect(TokMinus, "SimpleExpr")
			op = TokMinus
		}
		tok = p.cur
	}

	res := p.parseTerm()
	if res == nil {
		return nil
	}

	if op != TokInvalid {
		var kind ExprKind
		if op == TokPlus {
			kind = EXPR_Plus
		} else {
			kind = EXPR_Minus
		}

		tmp := NewExpression(kind, tok.ToRowCol())
		tmp.Lhs = res
		tmp.SetType(res.GetType()) // Unary operators preserve type
		res = tmp
	}

	for p.firstAddOp() {
		tok := p.la
		addOpType := p.parseAddOp()
		if addOpType == TokInvalid {
			break
		}

		tmp := CreateFromToken(addOpType, tok.ToRowCol())
		tmp.Lhs = res
		res = tmp

		res.Rhs = p.parseTerm()
		if res.Rhs == nil {
			return nil
		}
	}

	return res
}

func (p *Parser) firstAddOp() bool {
	return p.la.Type == TokPlus || p.la.Type == TokMinus || p.la.Type == TokOR
}

func (p *Parser) parseAddOp() TokenType {
	if p.la.Type == TokPlus {
		p.expect(TokPlus, "AddOp")
		return TokPlus
	} else if p.la.Type == TokMinus {
		p.expect(TokMinus, "AddOp")
		return TokMinus
	} else if p.la.Type == TokOR {
		p.expect(TokOR, "AddOp")
		return TokOR
	}
	return TokInvalid
}

func (p *Parser) firstMulOp() bool {
	return p.la.Type == TokStar || p.la.Type == TokDIV || p.la.Type == TokMOD ||
		p.la.Type == TokSlash || p.la.Type == TokAmp
}

func (p *Parser) parseMulOp() TokenType {
	if p.la.Type == TokStar {
		p.expect(TokStar, "MulOp")
		return TokStar
	} else if p.la.Type == TokDIV {
		p.expect(TokDIV, "MulOp")
		return TokDIV
	} else if p.la.Type == TokMOD {
		p.expect(TokMOD, "MulOp")
		return TokMOD
	} else if p.la.Type == TokSlash {
		p.expect(TokSlash, "MulOp")
		return TokSlash
	} else if p.la.Type == TokAmp {
		p.expect(TokAmp, "MulOp")
		return TokAmp
	}
	return TokInvalid
}

func (p *Parser) parseTerm() *Expression {
	res := p.parseFactor()
	if res == nil {
		return nil
	}

	for p.firstMulOp() {
		tok := p.la
		mulOpType := p.parseMulOp()
		if mulOpType == TokInvalid {
			return nil
		}

		tmp := CreateFromToken(mulOpType, tok.ToRowCol())
		tmp.Lhs = res
		res = tmp

		// Parse right-hand side factor
		res.Rhs = p.parseFactor() // rhs is never lvalue
		if res.Rhs == nil {
			return nil
		}
	}

	return res
}

func (p *Parser) firstDesignator() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstNumber() bool {
	return p.la.Type == TokInteger || p.la.Type == TokReal
}

func (p *Parser) smallestIntType(i uint64) *Type {
	if i <= 127 { // max value for SHORTINT
		return p.model.GetType(TYPE_SHORTINT)
	} else if i <= 32767 { // max value for INTEGER
		return p.model.GetType(TYPE_INTEGER)
	} else if i <= 2147483647 { // max value for LONGINT
		return p.model.GetType(TYPE_LONGINT)
	} else {
		return p.model.GetType(TYPE_HUGEINT)
	}
}

func (p *Parser) guessRealType(val string) *Type {
	// Find the decimal point
	dotPos := strings.Index(val, ".")
	if dotPos == -1 {
		if len(val) > 5 {
			return p.model.GetType(TYPE_LONGREAL)
		} else {
			return p.model.GetType(TYPE_REAL)
		}
	}

	// Count digits after decimal point
	fractionalPart := val[dotPos+1:]

	// Remove any exponent part (e/E and following)
	if ePos := strings.IndexAny(fractionalPart, "eE"); ePos != -1 {
		fractionalPart = fractionalPart[:ePos]
	}

	// Count significant decimal digits
	digitCount := 0
	for _, ch := range fractionalPart {
		if ch >= '0' && ch <= '9' {
			digitCount++
		} else {
			break // Stop at first non-digit
		}
	}

	// Type inference based on precision (matches C++ logic):
	// More than 5 decimal digits -> LONGREAL (double precision)
	// 5 or fewer digits -> REAL (single precision)
	if digitCount > 5 {
		return p.model.GetType(TYPE_LONGREAL)
	} else {
		return p.model.GetType(TYPE_REAL)
	}
}

func (p *Parser) parseNumber() *Expression {
	res := CreateFromToken(p.la.Type, p.la.ToRowCol())

	if p.la.Type == TokInteger {
		p.expect(TokInteger, "Number")
		val := string(p.cur.Val)

		var i uint64
		if strings.HasSuffix(val, "H") || strings.HasSuffix(val, "h") {
			// Hexadecimal integer: remove H suffix and parse as hex
			hexVal := val[:len(val)-1]
			if parsed, err := strconv.ParseUint(hexVal, 16, 64); err == nil {
				i = parsed
			} else {
				p.syntaxError("invalid hexadecimal number")
			}
		} else {
			// Decimal integer
			if parsed, err := strconv.ParseUint(val, 10, 64); err == nil {
				i = parsed
			} else {
				p.syntaxError("invalid decimal number")
			}
		}

		// Determine smallest suitable integer type
		res.SetType(p.smallestIntType(i))
		res.Val = i

	} else if p.la.Type == TokReal {
		p.expect(TokReal, "Number")
		val := string(p.cur.Val)
		originalVal := val

		// Handle suffix-based type determination
		var explicitType *Type

		if strings.HasSuffix(val, "d") || strings.HasSuffix(val, "D") {
			// Double precision (LONGREAL) - remove D suffix
			val = val[:len(val)-1]
			explicitType = p.model.GetType(TYPE_LONGREAL)
		} else if strings.HasSuffix(val, "s") || strings.HasSuffix(val, "S") {
			// Single precision (REAL) - remove S suffix
			val = val[:len(val)-1]
			explicitType = p.model.GetType(TYPE_REAL)
		}

		// Parse the numeric value
		if parsed, err := strconv.ParseFloat(val, 64); err == nil {
			res.Val = parsed

			if explicitType != nil {
				// Explicit type from suffix
				res.SetType(explicitType)
			} else {
				// NO SUFFIX - GUESS TYPE BASED ON PRECISION (like C++ version)
				// This is the key missing functionality!
				guessedType := p.guessRealType(originalVal)
				res.SetType(guessedType)
			}
		} else {
			p.syntaxError("invalid real number")
		}
	} else {
		p.syntaxError("number expected")
	}

	return res
}

func (p *Parser) firstSet() bool {
	return p.la.Type == TokLbrace
}

// firstSelector checks if current token is a selector
func (p *Parser) firstSelector() bool {
	return p.la.Type == TokDot || p.la.Type == TokLbrack ||
		p.la.Type == TokHat || p.la.Type == TokLpar
}

func (p *Parser) dequote(val []byte) []byte {
	str := string(val)
	if (strings.HasPrefix(str, "\"") && strings.HasSuffix(str, "\"")) ||
		(strings.HasPrefix(str, "'") && strings.HasSuffix(str, "'")) {
		return []byte(str[1 : len(str)-1])
	}
	return val
}

func (p *Parser) firstElement() bool {
	return p.firstExpr()
}

func (p *Parser) parseElement() *Expression {
	res := p.parseExpr()
	if res == nil {
		return nil
	}

	if p.la.Type == Tok2Dot {
		// Range element: start..end
		tok := p.la
		p.expect(Tok2Dot, "Element")

		rhs := p.parseExpr()
		if rhs == nil {
			return nil
		}

		rangeExpr := NewExpression(EXPR_Range, tok.ToRowCol())
		rangeExpr.Lhs = res
		rangeExpr.Rhs = rhs
		res = rangeExpr
	}

	return res
}

func (p *Parser) parseSet() *Expression {
	res := NewExpression(EXPR_Constructor, p.la.ToRowCol())
	p.expect(TokLbrace, "Set")

	if p.firstElement() {
		first := p.parseElement()
		if first == nil {
			return nil
		}
		res.Rhs = first

		current := first
		for p.la.Type == TokComma {
			p.expect(TokComma, "Set")
			element := p.parseElement()
			if element == nil {
				return nil
			}
			current.Next = element
			current = element
		}
	}

	p.expect(TokRbrace, "Set")
	return res
}

func (p *Parser) parseFactor() *Expression {
	var res *Expression

	if p.firstDesignator() {
		// Parse designator (variable reference, field access, array access, etc.)
		res = p.parseDesignator(false)
	} else if p.firstNumber() {
		// Parse numeric literal
		res = p.parseNumber()
	} else if p.la.Type == TokHexchar {
		// Parse hexadecimal character literal
		p.expect(TokHexchar, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.model.GetType(TYPE_CHAR))

		// Remove 'X' suffix and convert from hex
		tmp := string(p.cur.Val)
		if len(tmp) > 0 && (tmp[len(tmp)-1] == 'X' || tmp[len(tmp)-1] == 'x') {
			tmp = tmp[:len(tmp)-1]
		}
		if val, err := strconv.ParseUint(tmp, 16, 8); err == nil {
			res.Val = val
		}
	} else if p.la.Type == TokString {
		// Parse string literal
		p.expect(TokString, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.model.GetType(TYPE_StrLit))
		res.Val = p.dequote(p.cur.Val)
	} else if p.la.Type == TokNIL {
		// Parse NIL literal
		p.expect(TokNIL, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.model.GetType(TYPE_NIL))
		res.Val = nil
	} else if p.firstSet() {
		// Parse set constructor
		res = p.parseSet()
	} else if p.la.Type == TokLpar {
		// Parse parenthesized expression
		p.expect(TokLpar, "Factor")
		res = p.parseExpr()
		p.expect(TokRpar, "Factor")
	} else if p.la.Type == TokTilde {
		// Parse NOT operator
		tok := p.la.ToRowCol()
		p.expect(TokTilde, "Factor")

		tmp := p.parseFactor()
		if tmp == nil {
			return nil
		}

		res = NewExpression(EXPR_Not, tok)
		res.Lhs = tmp
		// res.SetType(p.model.GetType(TYPE_BOOLEAN))
	} else {
		p.syntaxError("factor expected")
		return NewExpression(EXPR_Invalid, p.la.ToRowCol())
	}

	return res
}

// parseLiteral parses literal expressions
func (p *Parser) parseLiteral() *Expression {
	pos := p.la.ToRowCol()
	tokenType := p.la.Type

	p.next()

	expr := NewExpression(EXPR_Literal, pos)
	expr.Val = p.cur.Val

	// Set appropriate type based on literal
	switch tokenType {
	case TokInteger:
		expr.SetType(p.model.GetType(TYPE_INTEGER))
	case TokReal:
		expr.SetType(p.model.GetType(TYPE_REAL))
	case TokString:
		expr.SetType(p.model.GetType(TYPE_StrLit))
	case TokNIL:
		expr.SetType(p.model.GetType(TYPE_NIL))
	}

	return expr
}

func (p *Parser) parseMaybeQualident() *Expression {
	p.expect(TokIdent, "Designator")
	tok := p.cur

	// Check if this is a qualified identifier (module.identifier)
	decl := p.model.FindDecl(string(tok.Val), false)
	if decl != nil && decl.Kind == int(DECL_Import) {
		// This is an import, expect dot and second identifier
		p.expect(TokDot, "Qualident")
		p.expect(TokIdent, "Qualident")

		quali := NewQualident(tok.Val, p.cur.Val)
		res := NewExpression(EXPR_NameRef, tok.ToRowCol())
		res.Val = quali
		return res
	} else {
		// Simple identifier or save dot for later
		quali := NewQualident(nil, tok.Val)
		res := NewExpression(EXPR_NameRef, tok.ToRowCol())
		res.Val = quali
		return res
	}
}

// parseDesignator parses designators (variable references)
func (p *Parser) parseDesignator(needsLvalue bool) *Expression {
	// Parse the base identifier (possibly qualified)
	res := p.parseMaybeQualident()
	if res == nil {
		return nil
	}

	// Parse selectors (field access, array indexing, dereferencing, calls)
	for p.firstSelector() {
		if p.peek(1).Type == TokDot && p.peek(2).Type == TokIdent {
			// Field selection: obj.field
			p.expect(TokDot, "Selector")
			p.expect(TokIdent, "Selector")

			tmp := NewExpression(EXPR_Select, p.cur.ToRowCol())
			tmp.Val = p.cur.Val
			tmp.Lhs = res
			res = tmp
		} else if p.la.Type == TokLbrack {
			// Array indexing: arr[index1, index2, ...]
			p.expect(TokLbrack, "Selector")

			indices := p.parseExprList()
			if len(indices) == 0 {
				return nil
			}

			// Chain multiple indices for multi-dimensional arrays
			for _, index := range indices {
				tmp := NewExpression(EXPR_Index, p.cur.ToRowCol())
				tmp.Lhs = res
				tmp.Rhs = index
				res = tmp
			}

			p.expect(TokRbrack, "Selector")
		} else if p.la.Type == TokHat {
			// Pointer dereferencing: ptr^
			p.expect(TokHat, "Selector")

			tmp := NewExpression(EXPR_Deref, p.cur.ToRowCol())
			tmp.Lhs = res
			res = tmp
		} else if p.la.Type == TokLpar {
			// Procedure call: proc(args) or type cast: TYPE(expr)
			p.expect(TokLpar, "Selector")
			lpar := p.cur.ToRowCol()

			var args *Expression
			if p.firstExpr() {
				argList := p.parseExprList()
				if len(argList) > 0 {
					args = argList[0]
					// Chain arguments
					current := args
					for i := 1; i < len(argList); i++ {
						current.Next = argList[i]
						current = argList[i]
					}
				}
			}

			p.expect(TokRpar, "Selector")

			tmp := NewExpression(EXPR_Call, lpar)
			tmp.Lhs = res  // procedure
			tmp.Rhs = args // arguments
			res = tmp
		} else {
			p.errors = append(p.errors, NewParseError("Invalid selector", p.la.ToRowCol(), p.la.SourcePath))
		}
	}

	// Set lvalue flag if requested
	res.NeedsLval = needsLvalue
	return res
}

// parseConstExpr parses constant expressions
func (p *Parser) parseConstExpr() *Expression {
	return p.parseExpr() // For now, same as regular expression
}

// Statement parsing methods

// parseBody parses procedure/module bodies
func (p *Parser) parseBody() *Statement {
	p.expect(TokBEGIN, "Body")
	stmt := p.parseStatSeq()
	p.expect(TokEND, "Body")
	return stmt
}

// parseStatSeq parses statement sequences
func (p *Parser) parseStatSeq() *Statement {
	stmt := p.parseStatement()

	for p.la.Type == TokSemi {
		p.expect(TokSemi, "StatSeq")
		if p.firstStatement() {
			nextStmt := p.parseStatement()
			stmt.Append(nextStmt)
		}
	}

	return stmt
}

// parseStatement parses individual statements
func (p *Parser) parseStatement() *Statement {
	switch p.la.Type {
	case TokIdent:
		return p.parseAssignOrCall()
	case TokIF:
		return p.parseIfStatement()
	case TokCASE:
		return p.parseCaseStatement()
	case TokWHILE:
		return p.parseWhileStatement()
	case TokREPEAT:
		return p.parseRepeatStatement()
	case TokFOR:
		return p.parseForStatement()
	case TokLOOP:
		return p.parseLoopStatement()
	case TokWITH:
		return p.parseWithStatement()
	case TokEXIT:
		return p.parseExitStatement()
	case TokRETURN:
		return p.parseReturnStatement()
	default:
		// Empty statement
		return NewStatement(STMT_StatBlock, p.la.ToRowCol())
	}
}

// parseAssignOrCall parses assignment or procedure call
func (p *Parser) parseAssignOrCall() *Statement {
	designator := p.parseDesignator(true) // TODO check

	if p.la.Type == TokColonEq {
		// Assignment
		p.expect(TokColonEq, "Assignment")
		expr := p.parseExpr()

		stmt := NewStatement(STMT_Assig, designator.Pos)
		stmt.Lhs = designator
		stmt.Rhs = expr
		return stmt
	} else {
		// Procedure call
		if p.la.Type == TokLpar {
			p.expect(TokLpar, "ProcCall")
			if p.firstExpr() {
				var args *Expression
				argList := p.parseExprList()
				if len(argList) > 0 {
					args = argList[0]
					// Chain arguments
					current := args
					for i := 1; i < len(argList); i++ {
						current.Next = argList[i]
						current = argList[i]
					}
				}
				designator.Rhs = args
			}
			p.expect(TokRpar, "ProcCall")
		}

		stmt := NewStatement(STMT_Call, designator.Pos)
		stmt.Lhs = designator
		return stmt
	}
}

// parseIfStatement parses IF statements
func (p *Parser) parseIfStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokIF, "IfStatement")

	condition := p.parseExpr()
	p.expect(TokTHEN, "IfStatement")
	thenBody := p.parseStatSeq()

	stmt := NewStatement(STMT_If, pos)
	stmt.Rhs = condition
	stmt.Body = thenBody

	// Parse ELSIF clauses
	current := stmt
	for p.la.Type == TokELSIF {
		p.expect(TokELSIF, "IfStatement")
		elsifCondition := p.parseExpr()
		p.expect(TokTHEN, "IfStatement")
		elsifBody := p.parseStatSeq()

		elsifStmt := NewStatement(STMT_Elsif, p.la.ToRowCol())
		elsifStmt.Rhs = elsifCondition
		elsifStmt.Body = elsifBody

		current.Next = elsifStmt
		current = elsifStmt
	}

	// Parse ELSE clause
	if p.la.Type == TokELSE {
		p.expect(TokELSE, "IfStatement")
		elseBody := p.parseStatSeq()

		elseStmt := NewStatement(STMT_Else, p.la.ToRowCol())
		elseStmt.Body = elseBody

		current.Next = elseStmt
	}

	p.expect(TokEND, "IfStatement")

	return stmt
}

// parseCaseStatement parses CASE statements
func (p *Parser) parseCaseStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokCASE, "CaseStatement")

	caseExpr := p.parseExpr()
	p.expect(TokOF, "CaseStatement")

	stmt := NewStatement(STMT_Case, pos)
	stmt.Rhs = caseExpr

	// Parse case labels and statements
	if p.firstCaseLabel() {
		caseBody := p.parseCaseBody()
		stmt.Body = caseBody
	}

	// Parse ELSE clause
	if p.la.Type == TokELSE {
		p.expect(TokELSE, "CaseStatement")
		elseBody := p.parseStatSeq()

		elseStmt := NewStatement(STMT_Else, p.la.ToRowCol())
		elseStmt.Body = elseBody
		stmt.Next = elseStmt
	}

	p.expect(TokEND, "CaseStatement")

	return stmt
}

// parseCaseBody parses case body with labels
func (p *Parser) parseCaseBody() *Statement {
	labelStmt := p.parseCaseLabels()
	p.expect(TokColon, "CaseBody")
	body := p.parseStatSeq()
	labelStmt.Body = body

	current := labelStmt
	for p.la.Type == TokBar {
		p.expect(TokBar, "CaseBody")
		if p.firstCaseLabel() {
			nextLabel := p.parseCaseLabels()
			p.expect(TokColon, "CaseBody")
			nextBody := p.parseStatSeq()
			nextLabel.Body = nextBody

			current.Next = nextLabel
			current = nextLabel
		}
	}

	return labelStmt
}

// parseCaseLabels parses case labels
func (p *Parser) parseCaseLabels() *Statement {
	pos := p.la.ToRowCol()
	label := p.parseConstExpr()

	// Handle ranges
	if p.la.Type == Tok2Dot {
		p.expect(Tok2Dot, "CaseLabels")
		endLabel := p.parseConstExpr()

		rangeExpr := NewExpression(EXPR_Range, pos)
		rangeExpr.Lhs = label
		rangeExpr.Rhs = endLabel
		label = rangeExpr
	}

	stmt := NewStatement(STMT_CaseLabel, pos)
	stmt.Rhs = label

	// Parse additional labels
	for p.la.Type == TokComma {
		p.expect(TokComma, "CaseLabels")
		nextLabel := p.parseConstExpr()

		if p.la.Type == Tok2Dot {
			p.expect(Tok2Dot, "CaseLabels")
			endLabel := p.parseConstExpr()

			rangeExpr := NewExpression(EXPR_Range, nextLabel.Pos)
			rangeExpr.Lhs = nextLabel
			rangeExpr.Rhs = endLabel
			nextLabel = rangeExpr
		}

		nextLabel.Next = stmt.Rhs
		stmt.Rhs = nextLabel
	}

	return stmt
}

// parseWhileStatement parses WHILE statements
func (p *Parser) parseWhileStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokWHILE, "WhileStatement")

	condition := p.parseExpr()
	p.expect(TokDO, "WhileStatement")
	body := p.parseStatSeq()
	p.expect(TokEND, "WhileStatement")

	stmt := NewStatement(STMT_While, pos)
	stmt.Rhs = condition
	stmt.Body = body

	return stmt
}

// parseRepeatStatement parses REPEAT statements
func (p *Parser) parseRepeatStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokREPEAT, "RepeatStatement")

	body := p.parseStatSeq()
	p.expect(TokUNTIL, "RepeatStatement")
	condition := p.parseExpr()

	stmt := NewStatement(STMT_Repeat, pos)
	stmt.Rhs = condition
	stmt.Body = body

	return stmt
}

// parseForStatement parses FOR statements
func (p *Parser) parseForStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokFOR, "ForStatement")

	// Parse control variable assignment
	p.expect(TokIdent, "ForStatement")
	controlVar := p.cur
	p.expect(TokColonEq, "ForStatement")
	startExpr := p.parseExpr()

	// Create assignment statement
	assignStmt := NewStatement(STMT_ForAssig, pos)
	controlVarExpr := NewExpression(EXPR_NameRef, controlVar.ToRowCol())
	controlVarExpr.Val = NewQualident(controlVar.Val, nil)
	assignStmt.Lhs = controlVarExpr
	assignStmt.Rhs = startExpr

	p.expect(TokTO, "ForStatement")
	endExpr := p.parseExpr()

	// Parse optional BY clause
	var byExpr *Expression
	if p.la.Type == TokBY {
		p.expect(TokBY, "ForStatement")
		byExpr = p.parseConstExpr()
	}

	p.expect(TokDO, "ForStatement")
	body := p.parseStatSeq()
	p.expect(TokEND, "ForStatement")

	// Create FOR statement
	stmt := NewStatement(STMT_ForToBy, pos)
	stmt.Lhs = controlVarExpr
	stmt.Rhs = endExpr
	if byExpr != nil {
		stmt.Rhs.Next = byExpr
	}
	stmt.Body = body
	stmt.Next = assignStmt

	return stmt
}

// parseLoopStatement parses LOOP statements
func (p *Parser) parseLoopStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokLOOP, "LoopStatement")

	body := p.parseStatSeq()
	p.expect(TokEND, "LoopStatement")

	stmt := NewStatement(STMT_Loop, pos)
	stmt.Body = body

	return stmt
}

// parseWithStatement parses WITH statements
func (p *Parser) parseWithStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokWITH, "WithStatement")

	// Parse guard
	guard := p.parseDesignator(false)
	p.expect(TokColon, "WithStatement")
	guardType := p.parseType()
	p.expect(TokDO, "WithStatement")

	body := p.parseStatSeq()

	// Parse ELSE clause
	var elseBody *Statement
	if p.la.Type == TokELSE {
		p.expect(TokELSE, "WithStatement")
		elseBody = p.parseStatSeq()
	}

	p.expect(TokEND, "WithStatement")

	stmt := NewStatement(STMT_With, pos)
	stmt.Lhs = guard

	// Create type guard expression
	typeGuard := NewExpression(EXPR_Is, guard.Pos)
	typeGuard.Lhs = guard
	typeGuard.SetType(guardType)
	stmt.Rhs = typeGuard

	stmt.Body = body
	if elseBody != nil {
		elseStmt := NewStatement(STMT_Else, pos)
		elseStmt.Body = elseBody
		stmt.Next = elseStmt
	}

	return stmt
}

// parseExitStatement parses EXIT statements
func (p *Parser) parseExitStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokEXIT, "ExitStatement")

	return NewStatement(STMT_Exit, pos)
}

// parseReturnStatement parses RETURN statements
func (p *Parser) parseReturnStatement() *Statement {
	pos := p.la.ToRowCol()
	p.expect(TokRETURN, "ReturnStatement")

	stmt := NewStatement(STMT_Return, pos)

	if p.firstExpr() {
		stmt.Rhs = p.parseExpr()
	}

	return stmt
}

func (p *Parser) parseExprList() []*Expression {
	var result []*Expression

	expr := p.parseExpr()
	if expr == nil {
		return nil
	}

	result = append(result, expr)

	for p.la.Type == TokComma {
		p.expect(TokComma, "ExprList")
		expr = p.parseExpr()
		if expr == nil {
			return nil
		}
		result = append(result, expr)
	}

	return result
}

// First set checking functions

func (p *Parser) firstImportList() bool {
	return p.la.Type == TokIMPORT
}

func (p *Parser) firstDeclSeq() bool {
	return p.la.Type == TokVAR || p.la.Type == TokCONST ||
		p.la.Type == TokPROCEDURE || p.la.Type == TokTYPE
}

func (p *Parser) firstConstDecl() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstTypeDecl() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstVarDecl() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstBody() bool {
	return p.la.Type == TokBEGIN
}

func (p *Parser) firstFormalParam() bool {
	return p.la.Type == TokVAR || p.la.Type == TokIdent
}

func (p *Parser) firstFieldList() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstFieldDecl() bool {
	return p.la.Type == TokIdent
}

func (p *Parser) firstExpr() bool {
	return p.la.Type == TokIdent || p.la.Type == TokInteger ||
		p.la.Type == TokReal || p.la.Type == TokString ||
		p.la.Type == TokNIL || p.la.Type == TokLpar || p.la.Type == TokTilde
}

func (p *Parser) firstStatement() bool {
	return p.la.Type == TokIdent || p.la.Type == TokIF || p.la.Type == TokCASE ||
		p.la.Type == TokWHILE || p.la.Type == TokREPEAT || p.la.Type == TokFOR ||
		p.la.Type == TokLOOP || p.la.Type == TokWITH || p.la.Type == TokEXIT ||
		p.la.Type == TokRETURN
}

func (p *Parser) firstCaseLabel() bool {
	return p.firstExpr()
}

func (p *Parser) Errors() []ParseError {
	return p.errors
}

func (p *Parser) ErrorCount() int {
	return len(p.errors)
}

func (p *Parser) HasErrors() bool {
	return len(p.errors) > 0
}
