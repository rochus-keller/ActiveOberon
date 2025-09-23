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

package ActiveOberonParser

import (
	"fmt"
	"strconv"
	"strings"
)

// Parser implements the ActiveOberon parser
type Parser struct {
	scanner    Scanner
	model      *AstModel
	thisMod    *Declaration
	cur        Token
	la         Token // lookahead
	errors     []ParseError
	predefSymbols [][]byte
	beginSymbol []byte
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
func NewParser2(model *AstModel, scanner Scanner) *Parser {
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
func (p *Parser) error(msg string, pos RowCol) {
	p.errors = append(p.errors, NewParseError(msg, pos, p.la.SourcePath))
}

// parseModule parses a module
func (p *Parser) parseModule() {
	if p.la.Type != TokMODULE {
		p.error("not an ActiveOberon module", p.la.ToRowCol())
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
		p.error("expecting a dot at the end of a module", p.la.ToRowCol())
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

// addDecl adds a declaration to the current scope
func (p *Parser) addDecl(token Token, visi Visi, kind DeclKind) *Declaration {
	name := string(token.Val)
	decl := p.model.AddDecl(name)
	if decl == nil {
		p.error(fmt.Sprintf("duplicate declaration: %s", name), token.ToRowCol())
		return nil
	}
	
	decl.Kind = int(kind)
	decl.Visi = visi
	decl.Pos = token.ToRowCol()
	return decl
}

// TODO
func (p *Parser) parseIdentDef() ID { return NewID() }
func (p *Parser) parseIdentList() []ID { return nil }
func (p *Parser) parseType() *Type { return nil }
func (p *Parser) parseConstExpr() *Expression { return nil }
func (p *Parser) parseBody() *Statement { return nil }
func (p *Parser) parseProcDecl() {}

