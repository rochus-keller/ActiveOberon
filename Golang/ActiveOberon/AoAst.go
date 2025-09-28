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
	"strings"
)

// BuiltinKind represents built-in functions and procedures
type BuiltinKind int

const (
	// Built-in functions
	BUILTIN_ABS BuiltinKind = iota
	BUILTIN_ODD
	BUILTIN_CAP
	BUILTIN_ASH
	BUILTIN_LEN
	BUILTIN_MAX
	BUILTIN_MIN
	BUILTIN_SIZE
	BUILTIN_ORD
	BUILTIN_CHR
	BUILTIN_SHORT
	BUILTIN_LONG
	BUILTIN_ENTIER

	// Built-in procedures
	BUILTIN_INC
	BUILTIN_DEC
	BUILTIN_INCL
	BUILTIN_EXCL
	BUILTIN_COPY
	BUILTIN_NEW
	BUILTIN_HALT
	BUILTIN_AWAIT
	BUILTIN_ASSERT

	BUILTIN_SYSTEM

	// System functions
	BUILTIN_SYSTEM_ADR
	BUILTIN_SYSTEM_BIT
	BUILTIN_SYSTEM_CC
	BUILTIN_SYSTEM_LSH
	BUILTIN_SYSTEM_ROT
	BUILTIN_SYSTEM_VAL
	BUILTIN_SYSTEM_TYPECODE

	// System procedures
	BUILTIN_SYSTEM_GET
	BUILTIN_SYSTEM_PUT
	BUILTIN_SYSTEM_MOVE
	BUILTIN_SYSTEM_NEW
	BUILTIN_SYSTEM_PORTOUT
	BUILTIN_SYSTEM_PORTIN
	BUILTIN_SYSTEM_CLI
	BUILTIN_SYSTEM_STI
	BUILTIN_SYSTEM_GET8
	BUILTIN_SYSTEM_GET16
	BUILTIN_SYSTEM_GET32
	BUILTIN_SYSTEM_GET64
	BUILTIN_SYSTEM_PUT8
	BUILTIN_SYSTEM_PUT16
	BUILTIN_SYSTEM_PUT32
	BUILTIN_SYSTEM_PUT64
	BUILTIN_SYSTEM_GETREG
	BUILTIN_SYSTEM_PUTREG
	MAX_BUILTIN
)

// BuiltinNames provides string names for built-ins
var BuiltinNames = []string{
	"ABS", "ODD", "CAP", "ASH", "LEN", "MAX", "MIN", "SIZE", "ORD", "CHR", "SHORT", "LONG", "ENTIER",
	"INC", "DEC", "INCL", "EXCL", "COPY", "NEW", "HALT", "AWAIT", "ASSERT",
	"SYSTEM",
	"ADR", "BIT", "CC", "LSH", "ROT", "VAL", "TYPECODE",
	"GET", "PUT", "MOVE", "NEW", "PORTOUT", "PORTIN", "CLI", "STI",
	"GET8", "GET16", "GET32", "GET64", "PUT8", "PUT16", "PUT32", "PUT64",
	"GETREG", "PUTREG",
}

// Meta represents node metadata
type Meta int

const (
	MetaType Meta = iota // T
	MetaDecl             // D
	MetaExpr             // E
)

// Node is the base AST node
type Node struct {
	Meta    Meta
	Kind    int
	Pos     RowCol
	TypeRef *Type

	// Flags
	Validated   bool
	InList      bool
	Deferred    bool
	Delegate    bool
	Allocated   bool
	Owned       bool
	VarParam    bool
	Constructor bool
	Begin       bool
	OwnsType    bool
	HasErrors   bool
	HasSubs     bool
	Receiver    bool
	ByVal       bool
	NeedsLval   bool
	Nonlocal    bool
	Active      bool
	Exclusive   bool
}

// GetType returns the node's type
func (n *Node) GetType() *Type {
	return n.TypeRef
}

// SetType sets the node's type
func (n *Node) SetType(t *Type) {
	n.TypeRef = t
}

func (n *Node) OverrideType(t *Type) *Type {
	old := n.TypeRef
	n.TypeRef = t
	return old
}

// TypeKind represents different type kinds
type TypeKind int

const (
	TYPE_Undefined TypeKind = iota
	TYPE_NoType
	TYPE_StrLit
	TYPE_NIL
	TYPE_BOOLEAN
	TYPE_CHAR     // 1 byte
	TYPE_SHORTINT // 1 byte
	TYPE_BYTE     // 1 byte, SYSTEM.BYTE
	TYPE_INTEGER  // 2 bytes
	TYPE_LONGINT  // 4 bytes
	TYPE_HUGEINT  // 8 bytes
	TYPE_REAL     // 4 bytes
	TYPE_LONGREAL // 8 bytes
	TYPE_SET      // 4 bytes
	TYPE_PTR
	TYPE_ANY
	TYPE_MaxBasicType
	TYPE_Pointer
	TYPE_Procedure
	TYPE_Array
	TYPE_Record
	TYPE_Object
	TYPE_NameRef
)

// TypeNames provides string names for types
var TypeNames = []string{
	"Undefined", "NoType", "StrLit", "ByteArrayLit", "NIL", "BOOLEAN",
	"SHORTINT", "BYTE", "INTEGER", "LONGINT", "REAL", "LONGREAL",
	"SET", "PTR", "ANY",
}

// Type represents a type in the AST
type Type struct {
	Node
	Len   uint32         // array length
	Quali *Qualident     // for NameRef
	Subs  []*Declaration // record fields, enum elements, or proc params
	Decl  *Declaration   // if NameRef includes pos and name
	Expr  *Expression    // array length expression
}

// IsNumber checks if type is numeric
func (t *Type) IsNumber() bool {
	return t.Kind >= int(TYPE_SHORTINT) && t.Kind <= int(TYPE_LONGREAL)
}

// IsReal checks if type is real
func (t *Type) IsReal() bool {
	return t.Kind == int(TYPE_REAL) || t.Kind == int(TYPE_LONGREAL)
}

// IsInteger checks if type is integer
func (t *Type) IsInteger() bool {
	return t.Kind >= int(TYPE_SHORTINT) && t.Kind <= int(TYPE_HUGEINT)
}

// IsSet checks if type is set
func (t *Type) IsSet() bool {
	return t.Kind == int(TYPE_SET)
}

// IsBoolean checks if type is boolean
func (t *Type) IsBoolean() bool {
	return t.Kind == int(TYPE_BOOLEAN)
}

// IsStructured checks if type is structured
func (t *Type) IsStructured() bool {
	return t.Kind == int(TYPE_Array) || t.Kind == int(TYPE_Record)
}

// Deref dereferences a type (handles NameRef)
func (t *Type) Deref() *Type {
	if t.Kind == int(TYPE_NameRef) {
		if t.TypeRef == nil {
			return t
		}
		return t.TypeRef.Deref()
	}
	return t
}

// Find searches for a declaration by name in type
func (t *Type) Find(name string, recursive bool) *Declaration {
	for _, d := range t.Subs {
		if string(d.Name) == name {
			return d
		}
	}
	if recursive && (t.Kind == int(TYPE_Record) || t.Kind == int(TYPE_Object)) && t.TypeRef != nil {
		super := t.TypeRef.Deref()
		if super.Kind == int(TYPE_Pointer) && super.TypeRef != nil {
			super = super.TypeRef.Deref()
		}
		return super.Find(name, true)
	}
	return nil
}

// DeclKind represents different declaration kinds
type DeclKind int

const (
	DECL_Invalid DeclKind = iota
	DECL_Helper
	DECL_Scope
	DECL_Module
	DECL_TypeDecl
	DECL_Builtin
	DECL_ConstDecl
	DECL_Import
	DECL_Field
	DECL_VarDecl
	DECL_LocalDecl
	DECL_Procedure
	DECL_ParamDecl
	DECL_Max
)

// Visi represents visibility levels
type Visi uint8

const (
	VISI_NA Visi = iota
	VISI_Private
	VISI_ReadOnly
	VISI_ReadWrite
)

// Declaration represents a declaration in the AST
type Declaration struct {
	Node
	Link  *Declaration // member list or imported module decl
	Outer *Declaration // the owning declaration
	Super *Declaration // super class or overridden method
	Next  *Declaration // list of declarations in scope
	Body  *Statement   // procedure body
	Name  []byte
	Visi  Visi
	ID    uint16      // used for built-in code and local/param number
	Data  interface{} // value for const/enum, path for import
	Expr  *Expression // const decl, enum, meta actuals
}

// NewDeclaration creates a new declaration
func NewDeclaration() *Declaration {
	d := &Declaration{ID: 0xFFFF} // NoSlot equivalent
	d.Meta = MetaDecl
	return d
}

// GetParams returns parameter list
func (d *Declaration) GetParams(skipReceiver bool) []*Declaration {
	var params []*Declaration
	current := d.Link
	for current != nil && current.Kind == int(DECL_ParamDecl) {
		if !skipReceiver || !current.Receiver {
			params = append(params, current)
		}
		current = current.Next
	}
	return params
}

// IsLvalue checks if declaration is an lvalue
func (d *Declaration) IsLvalue() bool {
	return d.Kind == int(DECL_VarDecl) || d.Kind == int(DECL_LocalDecl) || d.Kind == int(DECL_ParamDecl)
}

// IsPublic checks if declaration is public
func (d *Declaration) IsPublic() bool {
	return d.Visi >= VISI_ReadOnly
}

// Find searches for declaration by name
func (d *Declaration) Find(name string, recursive bool) *Declaration {
	current := d.Link
	for current != nil {
		if string(current.Name) == name {
			return current
		}
		current = current.Next
	}
	if recursive && d.Outer != nil {
		return d.Outer.Find(name, true)
	}
	return nil
}

// GetModule returns the containing module
func (d *Declaration) GetModule() *Declaration {
	if d.Kind == int(DECL_Module) {
		return d
	} else if d.Outer != nil {
		return d.Outer.GetModule()
	}
	return nil
}

// AppendMember appends a member to declaration
func (d *Declaration) AppendMember(decl *Declaration) {
	if d.Link == nil {
		d.Link = decl
	} else {
		current := d.Link
		for current != nil && current.Next != nil {
			current = current.Next
		}
		current.Next = decl
		decl.InList = true
	}
}

// ExprKind represents different expression kinds
type ExprKind int

const (
	EXPR_Invalid ExprKind = iota
	EXPR_Plus
	EXPR_Minus
	EXPR_Not // Unary

	EXPR_Eq
	EXPR_Neq
	EXPR_Lt
	EXPR_Leq
	EXPR_Gt
	EXPR_Geq
	EXPR_In
	EXPR_Is // Relation

	EXPR_Add
	EXPR_Sub
	EXPR_Or // AddOp

	EXPR_Mul
	EXPR_Fdiv
	EXPR_Div
	EXPR_Mod
	EXPR_And // MulOp

	EXPR_DeclRef // val is declaration
	EXPR_Deref
	EXPR_Select // f.g, val is field declaration
	EXPR_Index  // a[i]
	EXPR_Cast
	EXPR_Call
	EXPR_Literal
	EXPR_Constructor
	EXPR_Range
	EXPR_NameRef // temporary, resolved by validator
	EXPR_ConstVal
	EXPR_Super // ^ supercall
	EXPR_MAX
)

// Expression represents an expression in the AST
type Expression struct {
	Node
	Val  interface{} // literal value or declaration reference
	Lhs  *Expression // left-hand side
	Rhs  *Expression // right-hand side
	Next *Expression // for argument lists
}

// NewExpression creates a new expression
func NewExpression(kind ExprKind, pos RowCol) *Expression {
	e := &Expression{}
	e.Meta = MetaExpr
	e.Kind = int(kind)
	e.Pos = pos
	return e
}

func (e *Expression) allArgsConst(args *Expression) bool {
	current := args
	for current != nil {
		if !current.IsConst() {
			return false
		}
		current = current.Next
	}
	return true
}

func (e *Expression) isBuiltinConstant(builtin *Declaration, args *Expression) bool {
	if builtin == nil {
		return false
	}

	builtinID := BuiltinKind(builtin.ID)

	switch builtinID {
	case BUILTIN_LEN:
		// LEN can be constant for arrays with compile-time known size
		if e.countArgs(args) != 1 {
			return true // Error case, but don't crash
		}

		if args.IsConst() {
			return true
		}

		// Check if it's an array type with constant size
		if args.GetType() != nil {
			arrayType := args.GetType().Deref()
			if arrayType.Kind == int(TYPE_Array) {
				// If array has compile-time size, LEN is constant
				return arrayType.Expr == nil || arrayType.Expr.IsConst()
			}
		}

		return true

	case BUILTIN_MIN, BUILTIN_MAX:
		// MIN and MAX are constant if they have exactly 1 arg and all args are constant
		return e.countArgs(args) == 1 && e.allArgsConst(args)

	default:
		return e.allArgsConst(args)
	}
}

func (e *Expression) countArgs(args *Expression) int {
	count := 0
	current := args
	for current != nil {
		count++
		current = current.Next
	}
	return count
}

func (e *Expression) IsLvalue() bool {
	if e == nil {
		return false
	}

	switch ExprKind(e.Kind) {
	case EXPR_DeclRef:
		// Declaration references are lvalues if they're variables
		if decl, ok := e.Val.(*Declaration); ok && decl != nil {
			return decl.Kind == int(DECL_LocalDecl) ||
				decl.Kind == int(DECL_VarDecl) ||
				decl.Kind == int(DECL_ParamDecl)
		}
		return false

	case EXPR_Select, EXPR_Index:
		return true

	default:
		return false
	}
}

// IsConst checks if expression is constant
func (e *Expression) IsConst() bool {
	if e == nil {
		return false
	}

	switch ExprKind(e.Kind) {
	case EXPR_DeclRef:
		// Reference to declaration - check what kind of declaration
		if decl, ok := e.Val.(*Declaration); ok && decl != nil {
			// Variables, locals, and parameters are NOT constant
			if decl.Kind == int(DECL_VarDecl) ||
				decl.Kind == int(DECL_LocalDecl) ||
				decl.Kind == int(DECL_ParamDecl) {
				return false
			}
			// Constants, types, procedures, etc. are considered constant references
			return true
		}
		return true

	case EXPR_ConstVal, EXPR_Literal:
		// Literal values and resolved constants are always constant
		return true

	case EXPR_Call:
		// Function/procedure calls - special handling for built-ins
		if e.Lhs == nil {
			return true // Error case, but don't crash
		}

		// Check if it's a declaration reference (built-in or procedure)
		if decl, ok := e.Lhs.Val.(*Declaration); ok && decl != nil {
			if decl.Kind == int(DECL_Procedure) {
				// User procedures are not constant
				return e.allArgsConst(e.Rhs)
			} else if decl.Kind == int(DECL_Builtin) {
				// Built-in functions - some can be evaluated at compile time
				return e.isBuiltinConstant(decl, e.Rhs)
			}
		}

		// Other calls are not constant unless all args are constant
		return e.allArgsConst(e.Rhs)

	default:
		// Binary and unary operations - all operands must be constant
		if e.Lhs != nil && !e.Lhs.IsConst() {
			return false
		}
		if e.Rhs != nil && !e.Rhs.IsConst() {
			return false
		}
		return true
	}
}

func appendExpr(list *Expression, elem *Expression) {
	for list != nil && list.Next != nil {
		list = list.Next
	}
	if list != nil {
		list.Next = elem
	}
}

// AppendRhs appends to right-hand side list
func (e *Expression) AppendRhs(expr *Expression) {
	if e.Rhs == nil {
		e.Rhs = expr
	} else {
		appendExpr(e.Rhs, expr)
	}
}

// StatementKind represents different statement kinds
type StatementKind int

const (
	STMT_Invalid   StatementKind = iota
	STMT_StatBlock               // head of statement sequence
	STMT_Assig
	STMT_Call
	STMT_If
	STMT_Elsif
	STMT_Else
	STMT_Case
	STMT_TypeCase
	STMT_CaseLabel
	STMT_With
	STMT_Loop
	STMT_While
	STMT_Repeat
	STMT_Exit
	STMT_Return
	STMT_ForAssig
	STMT_ForToBy
	STMT_End
	STMT_Assembler
)

// Statement represents a statement in the AST
type Statement struct {
	Kind      StatementKind
	Active    bool
	Exclusive bool
	Pos       RowCol
	Lhs       *Expression // procedure, assignment lhs
	Rhs       *Expression // rhs, args, condition, case, label, return
	Body      *Statement  // then body
	Next      *Statement  // statement list
}

// NewStatement creates a new statement
func NewStatement(kind StatementKind, pos RowCol) *Statement {
	return &Statement{
		Kind: kind,
		Pos:  pos,
	}
}

// GetLast returns the last statement in the list
func (s *Statement) GetLast() *Statement {
	current := s
	for current.Next != nil {
		current = current.Next
	}
	return current
}

// Append appends a statement to the list
func (s *Statement) Append(stmt *Statement) {
	last := s.GetLast()
	last.Next = stmt
}

// Import represents an import declaration
type Import struct {
	ModuleName []byte
	Importer   *Declaration
	ImportedAt RowCol
	Resolved   *Declaration // resolved module
}

// Equals checks if two imports are equal
func (i *Import) Equals(other *Import) bool {
	return string(i.ModuleName) == string(other.ModuleName)
}

// ModuleData represents module metadata
type ModuleData struct {
	SourcePath string
	FullName   []byte
	End        RowCol
}

// Qualident represents a qualified identifier
type Qualident struct {
	First  []byte
	Second []byte
}

// NewQualident creates a qualified identifier
func NewQualident(first, second []byte) *Qualident {
	return &Qualident{First: first, Second: second}
}

// Symbol represents a symbol for cross-reference
type Symbol struct {
	Kind SymbolKind
	Len  uint8
	Pos  RowCol
	Decl *Declaration
	Next *Symbol
}

// SymbolKind represents different symbol kinds
type SymbolKind int

const (
	SYM_Invalid SymbolKind = iota
	SYM_Module
	SYM_Decl
	SYM_DeclRef
	SYM_Lval
)

// Xref represents cross-reference information
type Xref struct {
	Syms map[*Declaration][]*Symbol
	Uses map[*Declaration][]*Declaration
	Subs map[*Declaration][]*Declaration
}

// AstModel manages the AST and symbol table
type AstModel struct {
	scopes      []*Declaration
	globalScope *Declaration
	SYSTEM      *Declaration
	types       []*Type // basic types
}

// NewAstModel creates a new AST model
func NewAstModel() *AstModel {
	model := &AstModel{
		types: make([]*Type, TYPE_MaxBasicType),
	}
	model.initializeBuiltins()
	return model
}

// initializeBuiltins sets up built-in types and declarations
func (m *AstModel) initializeBuiltins() {
	if m.globalScope == nil {
		m.globalScope = NewDeclaration()
		m.globalScope.Kind = int(DECL_Scope)
		m.OpenScope(m.globalScope)

		// Initialize basic types
		m.types[TYPE_Undefined] = m.newType(TYPE_Undefined)
		m.types[TYPE_NoType] = m.newType(TYPE_NoType)
		m.types[TYPE_StrLit] = m.newType(TYPE_StrLit)
		m.types[TYPE_NIL] = m.newType(TYPE_NIL)

		m.types[TYPE_BOOLEAN] = m.addType("BOOLEAN", TYPE_BOOLEAN)
		m.types[TYPE_CHAR] = m.addType("CHAR", TYPE_CHAR)
		m.types[TYPE_BYTE] = m.addType("BYTE", TYPE_BYTE)
		m.types[TYPE_SHORTINT] = m.addType("SHORTINT", TYPE_SHORTINT)
		m.types[TYPE_INTEGER] = m.addType("INTEGER", TYPE_INTEGER)
		m.types[TYPE_LONGINT] = m.addType("LONGINT", TYPE_LONGINT)
		m.types[TYPE_HUGEINT] = m.addType("HUGEINT", TYPE_HUGEINT)
		m.types[TYPE_REAL] = m.addType("REAL", TYPE_REAL)
		m.types[TYPE_LONGREAL] = m.addType("LONGREAL", TYPE_LONGREAL)
		m.types[TYPE_SET] = m.addType("SET", TYPE_SET)
		m.types[TYPE_PTR] = m.addType("PTR", TYPE_PTR)
		m.types[TYPE_ANY] = m.addType("ANY", TYPE_ANY)

		// Add type alias
		m.addTypeAlias("INT", m.types[TYPE_INTEGER])

		// Add built-in functions and procedures
		for i := BUILTIN_ABS; i < BUILTIN_SYSTEM; i++ {
			m.addBuiltin(BuiltinNames[i], i)
		}

		// Add constants
		m.addConst("TRUE", TYPE_BOOLEAN, true)
		m.addConst("FALSE", TYPE_BOOLEAN, false)

		// Add SYSTEM module
		system := m.AddDecl("SYSTEM")
		system.Kind = int(DECL_Module)
		m.OpenScope(system)
		for i := BUILTIN_SYSTEM + 1; i < MAX_BUILTIN; i++ {
			m.addBuiltin(BuiltinNames[i], i)
		}
		m.addTypeAlias("PTR", m.types[TYPE_PTR])
		m.addTypeAlias("BYTE", m.types[TYPE_BYTE])
		m.addBuiltin("HALT", BUILTIN_HALT)

		// Add register constants
		m.addConst("EBP", TYPE_BYTE, 5)
		m.addConst("ESP", TYPE_BYTE, 4)
		m.addConst("EAX", TYPE_BYTE, 0)
		m.addConst("EDI", TYPE_BYTE, 0)
		m.addConst("AX", TYPE_BYTE, 0)
		m.addConst("DX", TYPE_BYTE, 0)
		m.addConst("AL", TYPE_BYTE, 0)
		m.addConst("DH", TYPE_BYTE, 0)

		m.CloseScope(false)

		m.SYSTEM = system
	} else {
		m.OpenScope(m.globalScope)
	}
}

// newType creates a new type
func (m *AstModel) newType(kind TypeKind) *Type {
	t := &Type{}
	t.Meta = MetaType
	t.Kind = int(kind)
	t.Owned = true
	return t
}

// addType adds a named type
func (m *AstModel) addType(name string, kind TypeKind) *Type {
	t := m.newType(kind)
	m.addTypeAlias(name, t)
	return t
}

// addTypeAlias adds a type alias
func (m *AstModel) addTypeAlias(name string, t *Type) {
	d := m.AddDecl(name)
	d.Validated = true
	d.Kind = int(DECL_TypeDecl)
	d.SetType(t)
	if t.Decl == nil {
		t.Decl = d
	}
}

// addBuiltin adds a built-in function or procedure
func (m *AstModel) addBuiltin(name string, builtin BuiltinKind) {
	d := m.AddDecl(strings.ToUpper(name))
	d.Kind = int(DECL_Builtin)
	d.SetType(m.types[TYPE_NoType])
	d.ID = uint16(builtin)
	d.Validated = true
}

// addConst adds a constant
func (m *AstModel) addConst(name string, typeKind TypeKind, data interface{}) {
	d := m.AddDecl(name)
	d.Kind = int(DECL_ConstDecl)
	d.SetType(m.types[typeKind])
	d.Data = data
	d.Validated = true
}

// OpenScope opens a new scope
func (m *AstModel) OpenScope(scope *Declaration) {
	if scope == nil {
		scope = NewDeclaration()
		scope.Kind = int(DECL_Scope)
	}
	m.scopes = append(m.scopes, scope)
}

// CloseScope closes current scope
func (m *AstModel) CloseScope(takeMembers bool) *Declaration {
	var res *Declaration = nil
	if takeMembers {
		res = m.scopes[len(m.scopes)-1].Link
		m.scopes[len(m.scopes)-1].Link = nil
	}
	m.scopes = m.scopes[:len(m.scopes)-1]
	return res
}

// AddDecl adds a declaration to current scope
func (m *AstModel) AddDecl(name string) *Declaration {
	scope := m.scopes[len(m.scopes)-1]

	decl := NewDeclaration()
	decl.Name = []byte(name)
	if scope.Kind != int(DECL_Scope) {
		decl.Outer = scope
	}

	if scope.Link == nil {
		scope.Link = decl
	} else {
		// Check for duplicates
		current := scope.Link
		for current != nil {
			if string(current.Name) == name {
				return nil // duplicate
			}
			if current.Next == nil {
				break
			}
			current = current.Next
		}
		current.Next = decl
		decl.InList = true
	}
	return decl
}

// FindDecl finds a declaration by name
func (m *AstModel) FindDecl(name string, recursive bool) *Declaration {
	for i := len(m.scopes) - 1; i >= 0; i-- {
		current := m.scopes[i].Link
		for current != nil {
			if string(current.Name) == name {
				return current
			}
			current = current.Next
		}
		if !recursive {
			return nil
		}
	}
	return nil
}

// GetType returns a basic type
func (m *AstModel) GetType(kind TypeKind) *Type {
	return m.types[kind]
}

// GetTopScope returns the top-level scope (module or procedure)
func (m *AstModel) GetTopScope() *Declaration {
	for i := len(m.scopes) - 1; i >= 0; i-- {
		d := m.scopes[i]
		if d.Kind == int(DECL_Module) || d.Kind == int(DECL_Procedure) {
			return d
		}
	}
	return nil
}

// toList converts linked list to slice
func (m *AstModel) toList(d *Declaration) []*Declaration {
	var result []*Declaration
	current := d
	for current != nil {
		result = append(result, current)
		old := current
		current = current.Next
		old.Next = nil
	}
	return result
}

func (m *AstModel) FindDeclInImport(import_ *Declaration, name string) *Declaration {
	if import_ == nil {
		return m.FindDecl(name, false)
	}
	obj := import_.Link
	for obj != nil && name == string(obj.Name) {
		obj = obj.Next
	}
	return obj
}

// CreateFromToken creates expression from token
func CreateFromToken(tokenType TokenType, pos RowCol) *Expression {
	var kind ExprKind
	switch tokenType {
	case TokPlus:
		kind = EXPR_Add
	case TokMinus:
		kind = EXPR_Sub
	case TokStar:
		kind = EXPR_Mul
	case TokSlash:
		kind = EXPR_Fdiv
	case TokDIV:
		kind = EXPR_Div
	case TokMOD:
		kind = EXPR_Mod
	case TokEq:
		kind = EXPR_Eq
	case TokHash:
		kind = EXPR_Neq
	case TokLt:
		kind = EXPR_Lt
	case TokLeq:
		kind = EXPR_Leq
	case TokGt:
		kind = EXPR_Gt
	case TokGeq:
		kind = EXPR_Geq
	case TokIN:
		kind = EXPR_In
	case TokIS:
		kind = EXPR_Is
	case TokAmp:
		kind = EXPR_And
	case TokOR:
		kind = EXPR_Or
	default:
		kind = EXPR_Invalid
	}
	return NewExpression(kind, pos)
}
