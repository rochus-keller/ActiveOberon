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
	"runtime"
)

// ParseError represents a parser error with message and location info.
type ParseError struct {
	Msg  string
	Pos  RowCol
	Path string
}

// ID models an identifier definition with visibility and attributes.
type idVisi uint8

const (
	visiNA idVisi = iota
	visiPrivate
	visiReadOnly
	visiPublic
)

type ID struct {
	Name     Token
	Visi     idVisi
	Untraced bool
}

func (id ID) IsValid() bool { return id.Name.Type == TokIdent }

// Parser is the ActiveOberon recursive-descent parser.
type Parser struct {
	thisMod       *Declaration
	mdl           *AstModel
	cur           Token
	la            Token
	scanner       Scanner
	errors        []ParseError
	predefSymbols [][]byte // size MaxAttr
	BEGIN         []byte
}

type attrKind int

const (
	attrUNTRACED attrKind = iota
	attrACTIVE
	attrDELEGATE
	attrEXCLUSIVE
	attrPRIORITY
	attrSAFE
	attrFFI
	attrMaxAttr
)

// FIRST(x) helper predicates (verbatim mapping from C++ FIRST_*)
func firstImportList(tt TokenType) bool { return tt == TokIMPORT }

func firstDeclSeq(tt TokenType) bool {
	return tt == TokVAR || tt == TokCONST || tt == TokPROCEDURE || tt == TokTYPE
}

func firstConstDecl(tt TokenType) bool { return tt == TokIdent }
func firstTypeDecl(tt TokenType) bool  { return tt == TokIdent }
func firstVarDecl(tt TokenType) bool   { return tt == TokIdent }
func firstAssembler(tt TokenType) bool { return tt == TokCODE }
func firstProcDecl(tt TokenType) bool  { return tt == TokPROCEDURE }

func firstProcHead(tt TokenType) bool {
	return tt == TokAmp || tt == TokMinus || tt == TokStar || tt == TokLbrack || tt == TokIdent
}
func firstSysFlag(tt TokenType) bool    { return tt == TokLbrack }
func firstFormalPars(tt TokenType) bool { return tt == TokLpar }

func firstFPSection(tt TokenType) bool     { return tt == TokVAR || tt == TokIdent }
func firstArrayType(tt TokenType) bool     { return tt == TokARRAY }
func firstRecordType(tt TokenType) bool    { return tt == TokRECORD }
func firstPointerType(tt TokenType) bool   { return tt == TokPOINTER }
func firstObjectType(tt TokenType) bool    { return tt == TokOBJECT }
func firstProcedureType(tt TokenType) bool { return tt == TokPROCEDURE }
func firstAliasType(tt TokenType) bool     { return tt == TokIdent }

func firstFieldList(tt TokenType) bool  { return tt == TokSemi || tt == TokIdent }
func firstBody(tt TokenType) bool       { return tt == TokBEGIN }
func firstAttributes(tt TokenType) bool { return tt == TokLbrace }
func firstStatBlock(tt TokenType) bool  { return tt == TokBEGIN }

func firstStatSeq(tt TokenType) bool {
	switch tt {
	case TokCASE, TokFOR, TokEXIT, TokIF, TokLOOP, TokREPEAT, TokWITH, TokWHILE, TokBEGIN, TokRETURN, TokIdent, TokSemi:
		return true
	default:
		return false
	}
}

func firstAssigOrCall(tt TokenType) bool { return tt == TokIdent }
func firstIfStat(tt TokenType) bool      { return tt == TokIF }
func firstCaseStat(tt TokenType) bool    { return tt == TokCASE }
func firstWhileStat(tt TokenType) bool   { return tt == TokWHILE }
func firstRepeatStat(tt TokenType) bool  { return tt == TokREPEAT }
func firstForStat(tt TokenType) bool     { return tt == TokFOR }
func firstLoopStat(tt TokenType) bool    { return tt == TokLOOP }
func firstWithStat(tt TokenType) bool    { return tt == TokWITH }
func firstReturnStat(tt TokenType) bool  { return tt == TokRETURN }

func firstCase(tt TokenType) bool {
	switch tt {
	case TokNIL, TokPlus, TokTilde, TokMinus, TokString, TokLpar, TokHexchar, TokInteger, TokReal, TokLbrace, TokIdent:
		return true
	default:
		return false
	}
}

func firstCaseLabels(tt TokenType) bool {
	return firstCase(tt)
}

func firstConstExpr(tt TokenType) bool {
	return firstCase(tt)
}

func firstExpr(tt TokenType) bool {
	return firstCase(tt)
}

func firstSet(tt TokenType) bool { return tt == TokLbrace }

func firstElement(tt TokenType) bool {
	return firstCase(tt)
}

func firstRelation(tt TokenType) bool {
	switch tt {
	case TokEq, TokLt, TokIN, TokLeq, TokIS, TokGeq, TokHash, TokGt:
		return true
	default:
		return false
	}
}

func firstMulOp(tt TokenType) bool {
	return tt == TokDIV || tt == TokAmp || tt == TokStar || tt == TokMOD || tt == TokSlash
}

func firstAddOp(tt TokenType) bool {
	return tt == TokPlus || tt == TokMinus || tt == TokOR
}

func firstDesignator(tt TokenType) bool { return tt == TokIdent }

func firstSelector(tt TokenType) bool {
	return tt == TokDot || tt == TokLpar || tt == TokLbrack || tt == TokHat
}

func firstExprList(tt TokenType) bool {
	return firstCase(tt)
}

func firstIdentList(tt TokenType) bool { return tt == TokIdent }
func firstNumber(tt TokenType) bool    { return tt == TokInteger || tt == TokReal }

// NewParser constructs a new parser instance.
func NewParser(m *AstModel, s Scanner) *Parser {
	p := &Parser{
		scanner: s,
		mdl:     m,
		thisMod: nil,
	}
	p.predefSymbols = make([][]byte, attrMaxAttr)
	p.predefSymbols[attrUNTRACED] = GetSymbol([]byte("UNTRACED"))
	p.predefSymbols[attrACTIVE] = GetSymbol([]byte("ACTIVE"))
	p.predefSymbols[attrDELEGATE] = GetSymbol([]byte("DELEGATE"))
	p.predefSymbols[attrEXCLUSIVE] = GetSymbol([]byte("EXCLUSIVE"))
	p.predefSymbols[attrPRIORITY] = GetSymbol([]byte("PRIORITY"))
	p.predefSymbols[attrSAFE] = GetSymbol([]byte("SAFE"))
	p.predefSymbols[attrFFI] = GetSymbol([]byte("C"))
	p.BEGIN = GetSymbol([]byte("BEGIN"))
	return p
}

// RunParser runs the parser on the provided scanner.
func (p *Parser) RunParser() {
	p.errors = nil
	p.next()
	p.Module()
}

// TakeResult returns the parsed module declaration and transfers ownership.
func (p *Parser) TakeResult() *Declaration {
	res := p.thisMod
	p.thisMod = nil
	return res
}

func (p *Parser) error(msg string, pos RowCol, path string) {
	p.errors = append(p.errors, ParseError{
		Msg:  msg,
		Pos:  pos,
		Path: path,
	})
}

func (p *Parser) next() {
	p.cur = p.la
	p.la = p.scanner.Next()
	for p.la.Type == TokInvalid {
		p.error(string(p.la.Val), p.la.ToRowCol(), p.la.SourcePath)
		p.la = p.scanner.Next()
	}
}

func (p *Parser) peek(off int) Token {
	if off == 1 {
		return p.la
	} else if off == 0 {
		return p.cur
	}
	return p.scanner.Peek(off - 1)
}

func (p *Parser) invalid(what string) {
	p.error(fmt.Sprintf("invalid %s", what), p.la.ToRowCol(), p.la.SourcePath)
}

func (p *Parser) expect(tt TokenType, _ bool, where string) bool {
	if p.la.Type == tt {
		p.next()
		return true
	}
	p.error(fmt.Sprintf("'%s' expected in %s", TokenTypeString(tt), where),
		p.la.ToRowCol(), p.la.SourcePath)
	return false
}

// Module -> parses a MODULE
func (p *Parser) Module() {
	if p.la.Type != TokMODULE {
		p.la.SourcePath = p.scanner.Source()
		p.la.LineNr = 1
		p.errorTok(p.la, "not an Oberon module")
		return
	}

	m := NewDeclaration()
	m.Kind = int(DECL_Module)
	if p.thisMod != nil {
		// GC in Go; in C++ code would free tree here
	}
	p.thisMod = m

	p.mdl.OpenScope(m)

	p.expect(TokMODULE, false, "Module")
	p.expect(TokIdent, false, "Module")
	m.Name = p.cur.Val
	m.Pos = p.cur.ToRowCol()

	md := ModuleData{}
	md.SourcePath = p.scanner.Source()
	md.FullName = p.cur.Val

	p.expect(TokSemi, false, "Module")

	if firstImportList(p.la.Type) {
		p.ImportList()
	}
	p.DeclSeq(false)

	if firstBody(p.la.Type) {
		// BEGIN ... turned into an injected procedure
		p.la.Val = p.BEGIN
		procDecl := p.addDecl(p.la, uint8(DECL_Private()), int(DECL_Procedure)) // visi value mapped later
		if procDecl == nil {
			return
		}
		procDecl.Begin = true
		p.mdl.OpenScope(procDecl)
		procDecl.Body = p.Body()
		p.mdl.CloseScope(false)
	}

	p.expect(TokEND, false, "Module")
	p.expect(TokIdent, false, "Module")
	md.End = p.cur.ToRowCol()
	m.Data = md

	p.mdl.CloseScope(false)

	if p.la.Type != TokDot {
		// do not call next()
		p.error("expecting a dot at the end of a module", p.la.ToRowCol(), p.la.SourcePath)
	}
}

func (p *Parser) ImportDecl() {
	var localName Token
	if p.peek(1).Type == TokIdent && p.peek(2).Type == TokColonEq {
		p.expect(TokIdent, false, "ImportDecl")
		localName = p.cur
		p.expect(TokColonEq, false, "ImportDecl")
	}
	p.expect(TokIdent, false, "ImportDecl")
	module := p.cur
	if localName.Type == TokInvalid {
		localName = module
	}
	importDecl := p.addDecl(localName, 0, int(DECL_Import))
	if importDecl == nil {
		return
	}
	imp := &Import{
		ModuleName: module.Val,
		ImportedAt: module.ToRowCol(),
		Importer:   p.thisMod,
	}
	importDecl.Data = imp
}

func (p *Parser) ImportList() {
	p.expect(TokIMPORT, false, "ImportList")
	p.ImportDecl()
	for p.la.Type == TokComma {
		p.expect(TokComma, false, "ImportList")
		p.ImportDecl()
	}
	p.expect(TokSemi, false, "ImportList")
}

// DeclSeq handles CONST, TYPE, VAR, and PROCEDURE sequences
func (p *Parser) DeclSeq(inObjectType bool) {
	for p.la.Type == TokCONST || p.la.Type == TokTYPE || p.la.Type == TokVAR || firstProcDecl(p.la.Type) {
		if !inObjectType && p.la.Type == TokCONST {
			p.expect(TokCONST, false, "DeclSeq")
			for firstConstDecl(p.la.Type) {
				p.ConstDecl()
				p.expect(TokSemi, false, "DeclSeq")
			}
		} else if !inObjectType && p.la.Type == TokTYPE {
			p.expect(TokTYPE, false, "DeclSeq")
			for firstTypeDecl(p.la.Type) {
				p.TypeDecl()
				p.expect(TokSemi, false, "DeclSeq")
			}
		} else if p.la.Type == TokVAR {
			p.expect(TokVAR, false, "DeclSeq")
			for firstVarDecl(p.la.Type) {
				p.VarDecl(inObjectType)
				p.expect(TokSemi, false, "DeclSeq")
			}
		} else if firstProcDecl(p.la.Type) ||
			p.la.Type == TokEND || p.la.Type == TokCONST || p.la.Type == TokVAR || p.la.Type == TokTYPE ||
			p.la.Type == TokCODE || p.la.Type == TokBEGIN || p.la.Type == TokPROCEDURE {
			for firstProcDecl(p.la.Type) {
				p.ProcDecl()
				p.expect(TokSemi, false, "DeclSeq")
			}
		} else {
			p.invalid("DeclSeq")
			p.next()
		}
	}
}

func (p *Parser) ConstDecl() {
	id := p.IdentDef()
	p.expect(TokEq, false, "ConstDecl")
	d := p.addDecl(id.Name, uint8(id.Visi), int(DECL_ConstDecl))
	if d == nil {
		return
	}
	d.Expr = p.ConstExpr()
}

func (p *Parser) TypeDecl() {
	id := p.IdentDef()
	p.expect(TokEq, false, "TypeDecl")
	d := p.addDecl(id.Name, uint8(id.Visi), int(DECL_TypeDecl))
	if d == nil {
		return
	}
	d.SetType(p.Type_())
	if d.GetType() != nil && d.GetType().Decl == nil {
		d.GetType().Decl = d
	}
}

func (p *Parser) VarDecl(inObjectType bool) {
	ids := p.IdentList()
	p.expect(TokColon, false, "VarDecl")
	t := p.Type_()
	if t == nil {
		return
	}
	outer := p.mdl.GetTopScope()
	for i := range ids {
		id := ids[i]
		kind := int(DECL_VarDecl)
		if outer.Kind != int(DECL_Module) {
			kind = int(DECL_LocalDecl)
		}
		d := p.addDecl(id.Name, uint8(id.Visi), kind)
		if d == nil {
			continue
		}
		if inObjectType {
			d.Kind = int(DECL_Field)
		} else {
			d.Outer = outer
		}
		d.SetType(t)
	}
}

func (p *Parser) Assembler() []byte {
	p.expect(TokCODE, false, "Assembler")
	return p.cur.Val
}

func (p *Parser) ProcDecl() {
	p.expect(TokPROCEDURE, false, "ProcDecl")
	if firstProcHead(p.la.Type) {
		procDecl := p.ProcHead(false)
		p.expect(TokSemi, false, "ProcDecl")
		p.mdl.OpenScope(procDecl)
		p.DeclSeq(false)
		if firstBody(p.la.Type) || firstAssembler(p.la.Type) {
			if firstBody(p.la.Type) {
				procDecl.Body = p.Body()
			} else if firstAssembler(p.la.Type) {
				procDecl.Data = p.Assembler()
				procDecl.Body = NewStatement(STMT_Assembler, p.cur.ToRowCol())
			} else {
				p.invalid("ProcDecl")
			}
		}
		p.expect(TokEND, false, "ProcDecl")
		p.expect(TokIdent, false, "ProcDecl")
		p.mdl.CloseScope(false)
	} else if p.la.Type == TokHat {
		p.expect(TokHat, false, "ProcDecl")
		p.ProcHead(true)
	} else {
		p.invalid("ProcDecl")
	}
}

func (p *Parser) ProcHead(forwardDecl bool) *Declaration {
	if firstSysFlag(p.la.Type) {
		p.SysFlag() // ignore
	}
	isConstr := false
	if p.la.Type == TokStar || p.la.Type == TokAmp || p.la.Type == TokMinus {
		if p.la.Type == TokStar {
			p.expect(TokStar, false, "ProcHead")
		} else if p.la.Type == TokAmp {
			p.expect(TokAmp, false, "ProcHead")
			isConstr = true
		} else if p.la.Type == TokMinus {
			p.expect(TokMinus, false, "ProcHead")
		} else {
			p.invalid("ProcHead")
		}
	}
	id := p.IdentDef()
	if forwardDecl {
		if firstFormalPars(p.la.Type) {
			p.mdl.OpenScope(nil)
			ret := p.FormalPars()
			// discard parsing-only type
			_ = ret
			d := p.mdl.CloseScope(true)
			// GC will reclaim
			_ = d
		}
		return nil
	}
	procDecl := p.addDecl(id.Name, uint8(id.Visi), int(DECL_Procedure))
	if procDecl == nil {
		return nil
	}
	procDecl.Constructor = isConstr

	procDecl.Outer = p.mdl.GetTopScope()
	p.mdl.OpenScope(procDecl)
	if firstFormalPars(p.la.Type) {
		procDecl.SetType(p.FormalPars())
	}
	p.mdl.CloseScope(false)
	return procDecl
}

func (p *Parser) SysFlag() bool {
	p.expect(TokLbrack, false, "SysFlag")
	p.expect(TokIdent, false, "SysFlag")
	res := bytes.Equal(p.cur.Val, p.predefSymbols[attrUNTRACED]) ||
		bytes.Equal(p.cur.Val, p.predefSymbols[attrFFI])
	if !res {
		p.errorTok(p.cur, fmt.Sprintf("unknown system flag '%s'", string(p.cur.Val)))
	}
	p.expect(TokRbrack, false, "SysFlag")
	return res
}

func (p *Parser) FormalPars() *Type {
	p.expect(TokLpar, false, "FormalPars")
	if firstFPSection(p.la.Type) {
		p.FPSection()
		for p.la.Type == TokSemi {
			p.expect(TokSemi, false, "FormalPars")
			p.FPSection()
		}
	}
	p.expect(TokRpar, false, "FormalPars")
	var res *Type = nil
	if p.la.Type == TokColon {
		p.expect(TokColon, false, "FormalPars")
		res = p.NamedType()
	}
	if res == nil {
		res = p.mdl.GetType(TYPE_NoType)
	}
	return res
}

func (p *Parser) FPSection() {
	varParam := false
	if p.la.Type == TokVAR {
		p.expect(TokVAR, false, "FPSection")
		varParam = true
	}
	p.expect(TokIdent, false, "FPSection")
	var l TokenList
	l = append(l, p.cur)
	for p.la.Type == TokComma {
		p.expect(TokComma, false, "FPSection")
		p.expect(TokIdent, false, "FPSection")
		l = append(l, p.cur)
	}
	p.expect(TokColon, false, "FPSection")
	t := p.Type_()
	for i := range l {
		d := p.addDecl(l[i], 0, int(DECL_ParamDecl))
		if d == nil {
			continue
		}
		d.VarParam = varParam
		d.SetType(t)
	}
}

func (p *Parser) ArrayType() *Type {
	var lens []*Expression
	p.expect(TokARRAY, false, "ArrayType")
	tok := p.cur
	if firstSysFlag(p.la.Type) {
		p.SysFlag() // ignore
	}
	if firstConstExpr(p.la.Type) {
		e := p.ConstExpr()
		if e == nil {
			return nil
		}
		lens = append(lens, e)
		for p.la.Type == TokComma {
			p.expect(TokComma, false, "ArrayType")
			e = p.ConstExpr()
			if e == nil {
				return nil
			}
			lens = append(lens, e)
		}
	}
	p.expect(TokOF, false, "ArrayType")
	et := p.Type_()
	arr := &Type{}
	arr.Kind = int(TYPE_Array)
	arr.Pos = tok.ToRowCol()
	if len(lens) != 0 {
		arr.Expr = lens[0]
		arr.Pos = lens[0].Pos
		curDim := arr
		// multi-dim: chain Type::Array nodes
		for i := 1; i < len(lens); i++ {
			nextDim := &Type{}
			nextDim.Kind = int(TYPE_Array)
			nextDim.Pos = lens[i].Pos
			nextDim.Expr = lens[i]
			curDim.SetType(nextDim)
			curDim = nextDim
		}
		curDim.SetType(et)
	} else {
		arr.SetType(et)
	}
	return arr
}

func (p *Parser) RecordType() *Type {
	p.expect(TokRECORD, false, "RecordType")
	rec := &Type{}
	rec.Pos = p.cur.ToRowCol()
	rec.Kind = int(TYPE_Record)
	p.mdl.OpenScope(nil)
	if firstSysFlag(p.la.Type) {
		p.SysFlag() // ignored
	}
	if p.la.Type == TokLpar {
		p.expect(TokLpar, false, "RecordType")
		rec.SetType(p.NamedType())
		p.expect(TokRpar, false, "RecordType")
	}
	if firstFieldList(p.la.Type) {
		p.FieldList()
	}
	rec.Subs = p.mdl.toList(p.mdl.CloseScope(true))
	outer := p.mdl.GetTopScope()
	for i := range rec.Subs {
		rec.Subs[i].Outer = outer
	}
	p.expect(TokEND, false, "RecordType")
	return rec
}

func (p *Parser) PointerType() *Type {
	p.expect(TokPOINTER, false, "PointerType")
	tok := p.cur
	if firstSysFlag(p.la.Type) {
		p.SysFlag() // ignore
	}
	ptr := &Type{}
	ptr.Kind = int(TYPE_Pointer)
	ptr.Pos = tok.ToRowCol()
	p.expect(TokTO, false, "PointerType")
	ptr.SetType(p.Type_())
	return ptr
}

func (p *Parser) ObjectType() *Type {
	p.expect(TokOBJECT, false, "ObjectType")
	obj := &Type{}
	obj.Pos = p.cur.ToRowCol()
	obj.Kind = int(TYPE_Object)
	if firstSysFlag(p.la.Type) || p.la.Type == TokLpar || firstDeclSeq(p.la.Type) || firstBody(p.la.Type) || p.la.Type == TokEND {
		if firstSysFlag(p.la.Type) {
			p.SysFlag()
		}
		p.mdl.OpenScope(nil)
		if p.la.Type == TokLpar {
			p.expect(TokLpar, false, "ObjectType")
			obj.SetType(p.NamedType())
			p.expect(TokRpar, false, "ObjectType")
		}
		p.DeclSeq(true)
		if firstBody(p.la.Type) {
			p.la.Val = p.BEGIN
			procDecl := p.addDecl(p.la, uint8(DECL_Private()), int(DECL_Procedure))
			if procDecl == nil {
				// defensive
			} else {
				procDecl.Begin = true
				p.mdl.OpenScope(procDecl)
				procDecl.Body = p.Body()
				p.mdl.CloseScope(false)
			}
		}
		obj.Subs = p.mdl.toList(p.mdl.CloseScope(true))
		p.expect(TokEND, false, "ObjectType")
		if p.la.Type == TokIdent {
			p.expect(TokIdent, false, "ObjectType")
		}
	}
	return obj
}

func (p *Parser) ProcedureType() *Type {
	p.expect(TokPROCEDURE, false, "ProcedureType")
	p.mdl.OpenScope(nil)
	t := &Type{}
	t.Kind = int(TYPE_Procedure)
	t.Pos = p.cur.ToRowCol()
	if firstSysFlag(p.la.Type) {
		p.SysFlag()
	}
	if firstAttributes(p.la.Type) {
		a := p.Attributes()
		if a[attrDELEGATE] {
			t.Delegate = true
		}
	}
	ret := p.mdl.GetType(TYPE_NoType)
	if firstFormalPars(p.la.Type) {
		ret = p.FormalPars()
	}
	t.Subs = p.mdl.toList(p.mdl.CloseScope(true))
	t.SetType(ret)
	return t
}

func (p *Parser) AliasType() *Type {
	return p.NamedType()
}

func (p *Parser) Type_() *Type {
	var res *Type
	switch {
	case firstAliasType(p.la.Type):
		res = p.AliasType()
	case firstArrayType(p.la.Type):
		res = p.ArrayType()
	case firstRecordType(p.la.Type):
		res = p.RecordType()
	case firstPointerType(p.la.Type):
		res = p.PointerType()
	case firstObjectType(p.la.Type):
		res = p.ObjectType()
	case firstProcedureType(p.la.Type):
		res = p.ProcedureType()
	default:
		p.invalid("Type")
	}
	return res
}

func (p *Parser) FieldDecl() {
	if firstIdentList(p.la.Type) {
		ids := p.IdentList()
		p.expect(TokColon, false, "FieldDecl")
		t := p.Type_()
		for i := range ids {
			d := p.addDecl(ids[i].Name, uint8(ids[i].Visi), int(DECL_Field))
			if d == nil {
				continue
			}
			d.SetType(t)
		}
	}
}

func (p *Parser) FieldList() {
	p.FieldDecl()
	for p.la.Type == TokSemi {
		p.expect(TokSemi, false, "FieldList")
		p.FieldDecl()
	}
}

func (p *Parser) Body() *Statement {
	return p.StatBlock()
}

func (p *Parser) Attributes() [attrMaxAttr]bool {
	var res [attrMaxAttr]bool
	p.expect(TokLbrace, false, "Attributes")
	if p.la.Type == TokIdent {
		a := p.Attribute()
		if a >= 0 {
			res[attrKind(a)] = true
		}
		for p.la.Type == TokComma {
			p.expect(TokComma, false, "Attributes")
			a = p.Attribute()
			if a >= 0 {
				res[attrKind(a)] = true
			}
		}
	}
	p.expect(TokRbrace, false, "Attributes")
	return res
}

func (p *Parser) Attribute() int {
	p.expect(TokIdent, false, "Attribute")
	res := -1
	for i := 1; i < int(attrMaxAttr); i++ {
		if bytes.Equal(p.predefSymbols[i], p.cur.Val) {
			res = i
			break
		}
	}
	if p.la.Type == TokLpar {
		p.expect(TokLpar, false, "Attribute")
		p.ConstExpr() // ignore value
		p.expect(TokRpar, false, "Attribute")
	}
	return res
}

func (p *Parser) StatBlock() *Statement {
	p.expect(TokBEGIN, false, "StatBlock")
	s := NewStatement(STMT_StatBlock, p.cur.ToRowCol())
	if firstAttributes(p.la.Type) {
		a := p.Attributes()
		s.Active = a[attrACTIVE]
		s.Exclusive = a[attrEXCLUSIVE]
	}
	if firstStatSeq(p.la.Type) {
		s.Body = p.StatSeq()
	}
	return s
}

func (p *Parser) StatSeq() *Statement {
	res := p.Statement_()
	for p.la.Type == TokSemi {
		p.expect(TokSemi, false, "StatSeq")
		s := p.Statement_()
		if s == nil {
			continue
		}
		if res != nil {
			res.Append(s)
		} else {
			res = s
		}
	}
	if res != nil && p.la.Type == TokEND {
		end := NewStatement(STMT_End, p.la.ToRowCol())
		res.Append(end)
	}
	return res
}

func (p *Parser) AssigOrCall() *Statement {
	t := p.la
	lhs := p.Designator(true)
	if lhs == nil {
		return nil
	}
	if p.la.Type == TokColonEq {
		p.expect(TokColonEq, false, "AssigOrCall")
		t = p.cur
		rhs := p.Expr(false)
		if rhs == nil {
			return nil
		}
		stat := NewStatement(STMT_Assig, t.ToRowCol())
		stat.Lhs = lhs
		stat.Rhs = rhs
		return stat
	}
	stat := NewStatement(STMT_Call, t.ToRowCol())
	stat.Lhs = lhs
	return stat
}

func breakIf(cond bool) {
	if cond {
		runtime.Breakpoint()
	}
}
func (p *Parser) IfStat() *Statement {
	p.expect(TokIF, false, "IfStat")
	first := NewStatement(STMT_If, p.cur.ToRowCol())
	first.Rhs = p.Expr(false)
	if first.Rhs == nil {
		return nil
	}
	p.expect(TokTHEN, false, "IfStat")
	first.Body = p.StatSeq()
	last := first
	for p.la.Type == TokELSIF {
		p.expect(TokELSIF, false, "IfStat")
		stat := NewStatement(STMT_Elsif, p.cur.ToRowCol())
		stat.Rhs = p.Expr(false)
		if stat.Rhs == nil {
			return nil
		}
		p.expect(TokTHEN, false, "IfStat")
		stat.Body = p.StatSeq()
		last.Append(stat)
		last = stat
	}
	if p.la.Type == TokELSE {
		p.expect(TokELSE, false, "IfStat")
		stat := p.StatSeq()
		if stat != nil {
			last.Append(stat)
		}
	}
	p.expect(TokEND, false, "IfStat")
	return first
}

func (p *Parser) CaseStat() *Statement {
	p.expect(TokCASE, false, "CaseStat")
	first := NewStatement(STMT_Case, p.cur.ToRowCol())
	first.Rhs = p.Expr(false)
	if first.Rhs == nil {
		return nil
	}
	if p.la.Type == TokDO {
		p.expect(TokDO, false, "CaseStat")
	} else if p.la.Type == TokOF {
		p.expect(TokOF, false, "CaseStat")
	} else {
		p.invalid("CaseStat")
	}
	last := first
	if firstCaseLabels(p.la.Type) {
		stat := p.Case()
		if stat == nil {
			return nil
		}
		last.Append(stat)
		last = stat
	}
	for p.la.Type == TokBar {
		p.expect(TokBar, false, "CaseStat")
		if firstCaseLabels(p.la.Type) {
			stat := p.Case()
			if stat == nil {
				return nil
			}
			last.Append(stat)
			last = stat
		}
	}
	if p.la.Type == TokELSE {
		p.expect(TokELSE, false, "CaseStat")
		stat := NewStatement(STMT_Else, p.cur.ToRowCol())
		last.Append(stat)
		last = stat
		stat.Body = p.StatSeq()
	}
	p.expect(TokEND, false, "CaseStat")
	return first
}

func (p *Parser) WhileStat() *Statement {
	p.expect(TokWHILE, false, "WhileStat")
	res := NewStatement(STMT_While, p.cur.ToRowCol())
	res.Rhs = p.Expr(false)
	if res.Rhs == nil {
		return nil
	}
	p.expect(TokDO, false, "WhileStat")
	res.Body = p.StatSeq()
	p.expect(TokEND, false, "WhileStat")
	return res
}

func (p *Parser) RepeatStat() *Statement {
	p.expect(TokREPEAT, false, "RepeatStat")
	res := NewStatement(STMT_Repeat, p.cur.ToRowCol())
	res.Body = p.StatSeq()
	p.expect(TokUNTIL, false, "RepeatStat")
	res.Rhs = p.Expr(false)
	if res.Rhs == nil {
		return nil
	}
	return res
}

func (p *Parser) ForStat() *Statement {
	p.expect(TokFOR, false, "ForStat")
	res := NewStatement(STMT_ForAssig, p.cur.ToRowCol())
	p.expect(TokIdent, false, "ForStat")
	res.Lhs = NewExpression(EXPR_NameRef, p.cur.ToRowCol())
	res.Lhs.Val = NewQualident(nil, p.cur.Val)
	p.expect(TokColonEq, false, "ForStat")
	res.Rhs = p.Expr(false)
	if res.Rhs == nil {
		return nil
	}
	p.expect(TokTO, false, "ForStat")
	forby := NewStatement(STMT_ForToBy, p.cur.ToRowCol())
	res.Append(forby)
	forby.Lhs = p.Expr(false) // to
	if forby.Lhs == nil {
		return nil
	}
	if p.la.Type == TokBY {
		p.expect(TokBY, false, "ForStat")
		forby.Rhs = p.ConstExpr()
	}
	p.expect(TokDO, false, "ForStat")
	res.Body = p.StatSeq()
	p.expect(TokEND, false, "ForStat")
	return res
}

func (p *Parser) LoopStat() *Statement {
	p.expect(TokLOOP, false, "LoopStat")
	res := NewStatement(STMT_Loop, p.cur.ToRowCol())
	res.Body = p.StatSeq()
	p.expect(TokEND, false, "LoopStat")
	return res
}

func (p *Parser) WithStat() *Statement {
	p.expect(TokWITH, false, "WithStat")
	res := NewStatement(STMT_With, p.cur.ToRowCol())
	t := p.la
	q := p.Qualident_()
	res.Lhs = NewExpression(EXPR_NameRef, t.ToRowCol())
	res.Lhs.Val = q
	p.expect(TokColon, false, "WithStat")
	res.Rhs = &Expression{} // dummy to hold type
	res.Rhs.Kind = int(EXPR_Literal)
	res.Rhs.SetType(p.NamedType())
	p.expect(TokDO, false, "WithStat")
	res.Body = p.StatSeq()
	p.expect(TokEND, false, "WithStat")
	return res
}

func (p *Parser) ReturnStat() *Statement {
	p.expect(TokRETURN, false, "ReturnStat")
	res := NewStatement(STMT_Return, p.cur.ToRowCol())
	if firstExpr(p.la.Type) {
		res.Rhs = p.Expr(false)
	}
	return res
}

func (p *Parser) Statement_() *Statement {
	var s *Statement
	if firstAssigOrCall(p.la.Type) || firstIfStat(p.la.Type) || firstCaseStat(p.la.Type) ||
		firstWhileStat(p.la.Type) || firstRepeatStat(p.la.Type) || firstForStat(p.la.Type) ||
		firstLoopStat(p.la.Type) || firstWithStat(p.la.Type) || p.la.Type == TokEXIT ||
		firstReturnStat(p.la.Type) || firstStatBlock(p.la.Type) {
		switch {
		case firstAssigOrCall(p.la.Type):
			s = p.AssigOrCall()
		case firstIfStat(p.la.Type):
			s = p.IfStat()
		case firstCaseStat(p.la.Type):
			s = p.CaseStat()
		case firstWhileStat(p.la.Type):
			s = p.WhileStat()
		case firstRepeatStat(p.la.Type):
			s = p.RepeatStat()
		case firstForStat(p.la.Type):
			s = p.ForStat()
		case firstLoopStat(p.la.Type):
			s = p.LoopStat()
		case firstWithStat(p.la.Type):
			s = p.WithStat()
		case p.la.Type == TokEXIT:
			p.expect(TokEXIT, false, "Statement")
			s = NewStatement(STMT_Exit, p.cur.ToRowCol())
		case firstReturnStat(p.la.Type):
			s = p.ReturnStat()
		case firstStatBlock(p.la.Type):
			s = p.StatBlock()
			p.expect(TokEND, false, "Statement")
		default:
			p.invalid("Statement")
			p.next()
		}
	}
	return s
}

func (p *Parser) Case() *Statement {
	res := NewStatement(STMT_CaseLabel, p.la.ToRowCol())
	res.Rhs = p.CaseLabels()
	if res.Rhs == nil {
		return nil
	}
	for p.la.Type == TokComma {
		p.expect(TokComma, false, "Case")
		e := p.CaseLabels()
		if e == nil {
			return nil
		}
		appendExpr(res.Rhs, e)
	}
	p.expect(TokColon, false, "Case")
	res.Body = p.StatSeq()
	return res
}

func (p *Parser) CaseLabels() *Expression {
	res := p.ConstExpr()
	if res == nil {
		return nil
	}
	if p.la.Type == Tok2Dot {
		p.expect(Tok2Dot, false, "CaseLabels")
		t := p.cur
		rhs := p.ConstExpr()
		if rhs == nil {
			return nil
		}
		rangeE := NewExpression(EXPR_Range, t.ToRowCol())
		rangeE.Lhs = res
		rangeE.Rhs = rhs
		res = rangeE
	}
	return res
}

func (p *Parser) ConstExpr() *Expression {
	return p.Expr(false)
}

func (p *Parser) Expr(lvalue bool) *Expression {
	res := p.SimpleExpr(lvalue)
	if res == nil {
		return nil
	}
	if firstRelation(p.la.Type) {
		tok := p.la
		tmp := CreateFromToken(p.Relation(), tok.ToRowCol())
		tmp.Lhs = res
		tmp.SetType(p.mdl.GetType(TYPE_BOOLEAN))
		res = tmp
		res.Rhs = p.SimpleExpr(false)
		if res.Rhs == nil {
			return nil
		}
	}
	return res
}

func (p *Parser) SimpleExpr(lvalue bool) *Expression {
	var op TokenType
	tok := p.la
	if p.la.Type == TokPlus || p.la.Type == TokMinus {
		if p.la.Type == TokPlus {
			p.expect(TokPlus, false, "Term")
			op = TokPlus
		} else if p.la.Type == TokMinus {
			p.expect(TokMinus, false, "Term")
			op = TokMinus
		} else {
			p.invalid("Term")
		}
	}
	res := p.Term(lvalue)
	if res == nil {
		return nil
	}
	if op != 0 {
		tmp := NewExpression(
			map[TokenType]ExprKind{TokPlus: EXPR_Plus, TokMinus: EXPR_Minus}[op],
			tok.ToRowCol(),
		)
		tmp.Lhs = res
		tmp.SetType(res.GetType())
		res = tmp
	}
	for firstAddOp(p.la.Type) {
		tok := p.la
		tmp := CreateFromToken(p.AddOp(), tok.ToRowCol())
		tmp.Lhs = res
		res = tmp
		res.Rhs = p.Term(false)
		if res.Rhs == nil {
			return nil
		}
	}
	return res
}

func (p *Parser) Term(lvalue bool) *Expression {
	res := p.Factor(lvalue)
	if res == nil {
		return nil
	}
	for firstMulOp(p.la.Type) {
		tok := p.la
		tmp := CreateFromToken(p.MulOp(), tok.ToRowCol())
		tmp.Lhs = res
		res = tmp
		res.Rhs = p.Factor(false)
		if res.Rhs == nil {
			return nil
		}
	}
	return res
}

func dequote(str []byte) []byte {
	if (len(str) >= 2 && str[0] == '\'' && str[len(str)-1] == '\'') ||
		(len(str) >= 2 && str[0] == '"' && str[len(str)-1] == '"') {
		return append([]byte{}, str[1:len(str)-1]...)
	}
	return append([]byte{}, str...)
}

func (p *Parser) Factor(lvalue bool) *Expression {
	var res *Expression
	switch {
	case firstDesignator(p.la.Type):
		res = p.Designator(lvalue)
	case firstNumber(p.la.Type):
		res = p.number()
	case p.la.Type == TokHexchar:
		p.expect(TokHexchar, false, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.mdl.GetType(TYPE_CHAR))
		tmp := make([]byte, len(p.cur.Val))
		copy(tmp, p.cur.Val)
		if len(tmp) > 0 {
			tmp = tmp[:len(tmp)-1] // remove X postfix
		}
		// interpret as hex
		var v uint64
		fmt.Sscanf(string(tmp), "%x", &v)
		res.Val = v
	case p.la.Type == TokString:
		p.expect(TokString, false, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.mdl.GetType(TYPE_StrLit))
		res.Val = dequote(p.cur.Val)
	case p.la.Type == TokNIL:
		p.expect(TokNIL, false, "Factor")
		res = NewExpression(EXPR_Literal, p.cur.ToRowCol())
		res.SetType(p.mdl.GetType(TYPE_NIL))
		res.Val = nil
	case firstSet(p.la.Type):
		res = p.Set()
	case p.la.Type == TokLpar:
		p.expect(TokLpar, false, "Factor")
		res = p.Expr(false)
		p.expect(TokRpar, false, "Factor")
	case p.la.Type == TokTilde:
		p.expect(TokTilde, false, "Factor")
		tmp := p.Factor(false)
		if tmp == nil {
			return nil
		}
		res = NewExpression(EXPR_Not, p.cur.ToRowCol())
		res.Lhs = tmp
	default:
		p.invalid("Factor")
	}
	return res
}

func (p *Parser) Set() *Expression {
	res := NewExpression(EXPR_Constructor, p.la.ToRowCol())
	p.expect(TokLbrace, false, "Set")
	if firstElement(p.la.Type) {
		e := p.Element()
		if e == nil {
			return nil
		}
		res.AppendRhs(e)
		for p.la.Type == TokComma {
			p.expect(TokComma, false, "Set")
			e = p.Element()
			if e == nil {
				return nil
			}
			res.AppendRhs(e)
		}
	}
	p.expect(TokRbrace, false, "Set")
	return res
}

func (p *Parser) Element() *Expression {
	res := p.Expr(false)
	if res == nil {
		return nil
	}
	if p.la.Type == Tok2Dot {
		p.expect(Tok2Dot, false, "Element")
		t := p.cur
		rhs := p.Expr(false)
		if rhs == nil {
			return nil
		}
		rangeE := NewExpression(EXPR_Range, t.ToRowCol())
		rangeE.Lhs = res
		rangeE.Rhs = rhs
		res = rangeE
	}
	return res
}

func (p *Parser) Relation() TokenType {
	switch {
	case p.la.Type == TokEq:
		p.expect(TokEq, false, "Relation")
	case p.la.Type == TokHash:
		p.expect(TokHash, false, "Relation")
	case p.la.Type == TokLt:
		p.expect(TokLt, false, "Relation")
	case p.la.Type == TokLeq:
		p.expect(TokLeq, false, "Relation")
	case p.la.Type == TokGt:
		p.expect(TokGt, false, "Relation")
	case p.la.Type == TokGeq:
		p.expect(TokGeq, false, "Relation")
	case p.la.Type == TokIN:
		p.expect(TokIN, false, "Relation")
	case p.la.Type == TokIS:
		p.expect(TokIS, false, "Relation")
	default:
		p.invalid("Relation")
	}
	return p.cur.Type
}

func (p *Parser) MulOp() TokenType {
	switch {
	case p.la.Type == TokStar:
		p.expect(TokStar, false, "MulOp")
	case p.la.Type == TokDIV:
		p.expect(TokDIV, false, "MulOp")
	case p.la.Type == TokMOD:
		p.expect(TokMOD, false, "MulOp")
	case p.la.Type == TokSlash:
		p.expect(TokSlash, false, "MulOp")
	case p.la.Type == TokAmp:
		p.expect(TokAmp, false, "MulOp")
	default:
		p.invalid("MulOp")
	}
	return p.cur.Type
}

func (p *Parser) AddOp() TokenType {
	switch {
	case p.la.Type == TokPlus:
		p.expect(TokPlus, false, "AddOp")
	case p.la.Type == TokMinus:
		p.expect(TokMinus, false, "AddOp")
	case p.la.Type == TokOR:
		p.expect(TokOR, false, "AddOp")
	default:
		p.invalid("AddOp")
	}
	return p.cur.Type
}

// maybeQualident parses a name or qualified name and returns NameRef expression
func (p *Parser) maybeQualident() *Expression {
	p.expect(TokIdent, false, "designator")
	tok := p.cur
	d := p.mdl.FindDecl(string(p.cur.Val), false)
	if d != nil && d.Kind == int(DECL_Import) {
		// full qualident; require dot
		p.expect(TokDot, false, "selector")
		p.expect(TokIdent, false, "selector")
		q := NewQualident(tok.Val, p.cur.Val)
		res := NewExpression(EXPR_NameRef, tok.ToRowCol())
		res.Val = q
		return res
	}
	// Qualident without module ref; keep dot for downstream
	res := NewExpression(EXPR_NameRef, tok.ToRowCol())
	q := NewQualident(nil, tok.Val)
	res.Val = q
	return res
}

// Designator results in an lvalue if possible, unless needsLvalue is false
func (p *Parser) Designator(needsLvalue bool) *Expression {
	res := p.maybeQualident()
	if res == nil {
		return nil
	}
	for firstSelector(p.la.Type) {
		if p.peek(1).Type == TokDot && p.peek(2).Type == TokIdent {
			p.expect(TokDot, false, "Selector")
			p.expect(TokIdent, false, "Selector")
			tmp := NewExpression(EXPR_Select, p.cur.ToRowCol())
			tmp.Val = p.cur.Val
			tmp.Lhs = res
			res = tmp
		} else if p.la.Type == TokLbrack {
			p.expect(TokLbrack, false, "Selector")
			l := p.ExprList()
			if len(l) == 0 {
				return nil
			}
			for i := 0; i < len(l); i++ {
				tmp := NewExpression(EXPR_Index, p.cur.ToRowCol())
				tmp.Lhs = res
				res = tmp
				res.Rhs = l[i]
			}
			p.expect(TokRbrack, false, "Selector")
		} else if p.la.Type == TokHat {
			p.expect(TokHat, false, "Selector")
			tmp := NewExpression(EXPR_Deref, p.cur.ToRowCol())
			tmp.Lhs = res
			res = tmp
		} else if p.la.Type == TokLpar {
			p.expect(TokLpar, false, "Selector")
			lpar := p.cur
			var args *Expression
			if firstExprList(p.la.Type) {
				lst := p.ExprList()
				if len(lst) == 0 {
					return nil
				}
				for i := 1; i < len(lst); i++ {
					appendExpr(lst[0], lst[i])
				}
				args = lst[0]
			}
			p.expect(TokRpar, false, "Selector")
			tmp := NewExpression(EXPR_Call, lpar.ToRowCol()) // could be call or typecast
			tmp.Lhs = res
			tmp.Rhs = args
			res = tmp
		} else {
			p.invalid("Selector")
			p.next()
		}
	}
	res.NeedsLval = needsLvalue
	return res
}

func (p *Parser) ExprList() []*Expression {
	e := p.Expr(false)
	if e == nil {
		return nil
	}
	var res []*Expression
	res = append(res, e)
	for p.la.Type == TokComma {
		p.expect(TokComma, false, "ExprList")
		e := p.Expr(false)
		if e == nil {
			return nil
		}
		res = append(res, e)
	}
	return res
}

func (p *Parser) IdentList() []ID {
	var res []ID
	res = append(res, p.IdentDef())
	for p.la.Type == TokComma {
		p.expect(TokComma, false, "IdentList")
		res = append(res, p.IdentDef())
	}
	return res
}

func (p *Parser) Qualident_() *Qualident {
	var q Qualident
	if p.peek(1).Type == TokIdent && p.peek(2).Type == TokDot {
		p.expect(TokIdent, false, "Qualident")
		q.First = p.cur.Val
		p.expect(TokDot, false, "Qualident")
		p.expect(TokIdent, false, "Qualident")
		q.Second = p.cur.Val
	} else if p.la.Type == TokIdent {
		p.expect(TokIdent, false, "Qualident")
		q.Second = p.cur.Val
	} else {
		p.invalid("Qualident")
	}
	return &q
}

func (p *Parser) NamedType() *Type {
	t := p.la
	q := p.Qualident_()
	res := &Type{}
	res.Kind = int(TYPE_NameRef)
	res.Quali = q
	res.Pos = t.ToRowCol()
	return res
}

func (p *Parser) IdentDef() ID {
	p.expect(TokIdent, false, "IdentDef")
	var res ID
	res.Name = p.cur
	res.Visi = visiPrivate
	if p.la.Type == TokStar || p.la.Type == TokMinus {
		if p.la.Type == TokStar {
			p.expect(TokStar, false, "IdentDef")
			res.Visi = visiPublic
		} else if p.la.Type == TokMinus {
			p.expect(TokMinus, false, "IdentDef")
			res.Visi = visiReadOnly
		} else {
			p.invalid("IdentDef")
		}
	}
	if firstSysFlag(p.la.Type) {
		res.Untraced = p.SysFlag()
	}
	return res
}

func (p *Parser) smallestIntType(i uint64) *Type {
	// Note: use platform-independent limits mirroring C++
	if i <= 127 {
		return p.mdl.GetType(TYPE_SHORTINT)
	} else if i <= 32767 {
		return p.mdl.GetType(TYPE_INTEGER)
	} else if i <= 2147483647 {
		return p.mdl.GetType(TYPE_LONGINT)
	}
	return p.mdl.GetType(TYPE_HUGEINT)
}

func (p *Parser) number() *Expression {
	res := CreateFromToken(p.la.Type, p.la.ToRowCol())
	if p.la.Type == TokInteger {
		p.expect(TokInteger, false, "number")
		var i uint64 = 0
		v := p.cur.Val
		l := len(v)
		if l >= 1 && (v[l-1] == 'h' || v[l-1] == 'H') {
			fmt.Sscanf(string(v[:l-1]), "%x", &i)
		} else {
			fmt.Sscanf(string(v), "%d", &i)
		}
		res.SetType(p.smallestIntType(i))
		res.Val = i
	} else if p.la.Type == TokReal {
		p.expect(TokReal, false, "number")
		// Double/Single inference based on suffix / precision
		text := append([]byte{}, p.cur.Val...)
		isDouble := false
		for i := range text {
			if text[i] == 'd' || text[i] == 'D' {
				text[i] = 'e'
				isDouble = true
			} else if text[i] == 's' || text[i] == 'S' {
				text[i] = 'e'
			}
		}
		if isDouble {
			res.SetType(p.mdl.GetType(TYPE_LONGREAL))
		}
		var d float64
		fmt.Sscanf(string(text), "%g", &d)
		if res.GetType() == nil {
			// guess by fraction length
			dot := -1
			for i := range text {
				if text[i] == '.' {
					dot = i
					break
				}
			}
			fracLen := 0
			if dot != -1 {
				for j := dot + 1; j < len(text) && text[j] >= '0' && text[j] <= '9'; j++ {
					fracLen++
				}
			}
			if fracLen > 5 {
				res.SetType(p.mdl.GetType(TYPE_LONGREAL))
			} else {
				res.SetType(p.mdl.GetType(TYPE_REAL))
			}
		}
		res.Val = d
	} else {
		p.invalid("number")
	}
	return res
}

func (p *Parser) addDecl(id Token, visi uint8, mode int) *Declaration {
	// NOTE: uniqueness is checked here (same as C++ model)
	d := p.mdl.AddDecl(string(id.Val))
	if d == nil {
		p.errorTok(id, "a declaration with this name already exists")
		return nil
	}
	d.Kind = mode
	// map Parser visibility to AST visibility
	switch idVisi(visi) {
	case visiPrivate:
		d.Visi = VISI_Private
	case visiReadOnly:
		d.Visi = VISI_ReadOnly
	case visiPublic:
		d.Visi = VISI_ReadWrite
	default:
		d.Visi = VISI_Private
	}
	rc := NewRowCol(id.LineNr, id.ColNr)
	d.Pos = rc
	return d
}

func (p *Parser) errorTok(t Token, msg string) {
	if msg == "" {
		msg = "error"
	}
	p.error(msg, NewRowCol(t.LineNr, t.ColNr), t.SourcePath)
}

// DECL_Private helper for injected BEGIN-proc visibility mapping
func DECL_Private() Visi { return VISI_Private }

func (p *Parser) Errors() []ParseError {
	return p.errors
}

func (p *Parser) ErrorCount() int {
	return len(p.errors)
}

func (p *Parser) HasErrors() bool {
	return len(p.errors) > 0
}
