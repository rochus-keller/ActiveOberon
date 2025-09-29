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

// Importer matches the C++ interface: loadModule(Import) -> *Declaration
type Importer interface {
	LoadModule(imp *Import) *Declaration
}

// Validator performs name resolution and light semantic validation.
type Validator struct {
	mdl    *AstModel
	imp    Importer
	module *Declaration

	// state
	scopeStack        []*Declaration
	boundProcs        []*Declaration
	curObjectTypeDecl *Declaration

	// xref (optional; stubbed to keep structure)
	haveXref bool
	firstSym *Symbol
	lastSym  *Symbol
	xref     map[*Declaration][]*Symbol
	subs     map[*Declaration][]*Declaration

	// errors
	Errors []Error
}

type Error struct {
	Msg  string
	Pos  RowCol
	Path string
}

func NewValidator(mdl *AstModel, imp Importer, haveXref bool) *Validator {
	v := &Validator{
		mdl:      mdl,
		imp:      imp,
		haveXref: haveXref,
		xref:     make(map[*Declaration][]*Symbol),
		subs:     make(map[*Declaration][]*Declaration),
	}
	if haveXref {
		v.firstSym = &Symbol{}
		v.lastSym = v.firstSym
	}
	return v
}

// Validate one module; set ModuleData.fullname and walk scopes.
func (v *Validator) Validate(module *Declaration, importInfo *Import) bool {
	if module == nil || DeclKind(module.Kind) != DECL_Module {
		return false
	}
	v.module = module

	// xref module header
	if v.haveXref {
		v.firstSym.Decl = module
		v.firstSym.Kind = SYM_Module
		v.firstSym.Pos = module.Pos
		v.firstSym.Len = uint8(len(module.Name))
	}

	v.markDecl(module)

	// Set module metadata full name if provided
	if importInfo != nil {
		md := ModuleData{}
		md.FullName = importInfo.ModuleName
		// source path is typically already in md.SourcePath set by front-end
		module.Data = md
	}

	// Visit entire module
	v.visitScope(module)

	// Close xref ring
	if v.haveXref {
		v.lastSym.Next = v.firstSym
	}

	return len(v.Errors) == 0
}

func (v *Validator) TakeXref() Xref {
	if !v.haveXref {
		return Xref{}
	}
	res := Xref{
		Syms: v.xref,
		Subs: v.subs,
		Uses: map[*Declaration][]*Declaration{}, // not populated in this port
	}
	// Reset ring
	v.xref = make(map[*Declaration][]*Symbol)
	v.subs = make(map[*Declaration][]*Declaration)
	v.firstSym = &Symbol{}
	v.lastSym = v.firstSym
	return res
}

func (v *Validator) error(pos RowCol, msg string) {
	// Pull module source path if present
	path := ""
	if md, ok := v.module.Data.(ModuleData); ok {
		path = md.SourcePath
	}
	v.Errors = append(v.Errors, Error{Msg: msg, Pos: pos, Path: path})
}

func (v *Validator) markDecl(d *Declaration) {
	if !v.haveXref || d == nil {
		return
	}
	s := &Symbol{Kind: SYM_Decl, Decl: d, Pos: d.Pos, Len: uint8(len(d.Name))}
	v.xref[d] = append(v.xref[d], s)
	v.lastSym.Next = s
	v.lastSym = s
}

func (v *Validator) markRef(d *Declaration, pos RowCol) *Symbol {
	if !v.haveXref {
		return nil
	}
	s := &Symbol{Kind: SYM_DeclRef, Decl: d, Pos: pos, Len: uint8(len(d.Name))}
	if d != nil {
		v.xref[d] = append(v.xref[d], s)
	}
	v.lastSym.Next = s
	v.lastSym = s
	return s
}

func (v *Validator) markUnref(length int, pos RowCol) *Symbol {
	if !v.haveXref {
		return nil
	}
	s := &Symbol{Kind: SYM_DeclRef, Pos: pos, Len: uint8(length)}
	v.lastSym.Next = s
	v.lastSym = s
	return s
}

// visitScope performs the two-phase header-then-body traversal.
func (v *Validator) visitScope(scope *Declaration) {
	if scope == nil {
		return
	}
	v.scopeStack = append(v.scopeStack, scope)

	// 1) evaluate all decl headers in this scope
	for cur := scope.Link; cur != nil; cur = cur.Next {
		v.visitDecl(cur)
	}
	// 2) recursively visit nested procedures (bodies later)
	for cur := scope.Link; cur != nil; cur = cur.Next {
		if DeclKind(cur.Kind) == DECL_Procedure {
			v.visitScope(cur)
		}
	}
	// 3) validate the body of the scope (statements)
	v.visitBody(scope.Body)

	// pop scope
	v.scopeStack = v.scopeStack[:len(v.scopeStack)-1]
}

func (v *Validator) visitDecl(d *Declaration) {
	if d == nil || d.Validated {
		return
	}
	d.Validated = true
	v.markDecl(d)

	switch DeclKind(d.Kind) {
	case DECL_TypeDecl:
		// If object type, remember for bound methods
		if d.GetType() != nil && TypeKind(d.GetType().Kind) == TYPE_Object {
			v.curObjectTypeDecl = d
		}
		v.visitType(d.GetType())

		// Handle bound procedures collected during object type traversal
		if d.GetType() != nil && TypeKind(d.GetType().Kind) == TYPE_Object {
			// Visit delayed bound procs
			bounds := append([]*Declaration(nil), v.boundProcs...)
			v.boundProcs = v.boundProcs[:0]
			for _, proc := range bounds {
				// receiver already injected as first param ("SELF")
				// link to super if overriding
				objType := proc.Link.GetType().Deref()
				if objType != d.GetType() {
					// defensive: should match
				}
				if objType.TypeRef != nil {
					// connect to overridden method if present
					superDecl := v.findInType(objType.TypeRef.Deref(), string(proc.Name))
					if superDecl != nil {
						superDecl.HasSubs = true
						proc.Super = superDecl
						if v.haveXref {
							v.subs[superDecl] = append(v.subs[superDecl], proc)
						}
					}
				}
				// visit full nested scope of bound proc
				v.visitScope(proc)
			}
			v.curObjectTypeDecl = nil
		}

		// Connect derived record/object to base
		if d.GetType() != nil &&
			(TypeKind(d.GetType().Kind) == TYPE_Object || TypeKind(d.GetType().Kind) == TYPE_Record) &&
			d.GetType().TypeRef != nil {
			base := d.GetType().TypeRef
			if !base.Validated || base.TypeRef == nil {
				// base unresolved yet; skip
				break
			}
			super := base.TypeRef.Deref().Decl
			if super != nil {
				super.HasSubs = true
				d.Super = super
				if v.haveXref {
					v.subs[super] = append(v.subs[super], d)
				}
			}
		}

	case DECL_VarDecl, DECL_LocalDecl, DECL_ParamDecl, DECL_Field:
		v.visitType(d.GetType())

	case DECL_ConstDecl:
		if d.Expr != nil {
			v.visitExpr(d.Expr, true)
			d.SetType(d.Expr.GetType())
			d.Data = d.Expr.Val
		} else if d.GetType() == nil {
			d.SetType(v.mdl.GetType(TYPE_NoType))
		} else {
			v.visitType(d.GetType())
		}

	case DECL_Import:
		v.visitImport(d)

	case DECL_Procedure:
		// only header types here
		v.visitType(d.GetType())
		ret := v.deref(d.GetType())
		if ret != nil && ret.IsStructured() {
			v.error(d.Pos, "return type cannot be structured")
		}
		for _, p := range d.GetParams(true) {
			v.visitDecl(p)
		}

	default:
		// other decl kinds: nothing to do
	}
}

func (v *Validator) visitImport(importDecl *Declaration) {
	if importDecl == nil {
		return
	}
	if importDecl.Outer == nil || DeclKind(importDecl.Outer.Kind) != DECL_Module {
		v.error(importDecl.Pos, "imports are only supported on module level")
	}
	var mod *Declaration
	impData, _ := importDecl.Data.(*Import)
	if v.imp != nil {
		mod = v.imp.LoadModule(impData)
	}
	if mod != nil {
		// use members of resolved module
		mod.HasSubs = true
		importDecl.Link = mod.Link
		impData.Resolved = mod
		importDecl.Data = impData
		v.markRef(mod, impData.ImportedAt)
	} else {
		v.error(importDecl.Pos, fmt.Sprintf("cannot import module '%s'", string(impData.ModuleName)))
		// match C++: throw; here just stop further resolution on this import
	}
	importDecl.Validated = true
}

func (v *Validator) visitBody(s *Statement) {
	cur := s
	for cur != nil {
		// Normalize a bare STMT_Call with non-call expr on Lhs into an EXPR_Call node
		if cur.Kind == STMT_Call && (cur.Lhs != nil) && ExprKind(cur.Lhs.Kind) != EXPR_Call {
			call := NewExpression(EXPR_Call, cur.Lhs.Pos)
			call.Lhs = cur.Lhs
			cur.Lhs = call
		}
		v.visitExpr(cur.Lhs, true)
		v.visitExpr(cur.Rhs, true)

		if cur.Kind == STMT_With && cur.Lhs != nil {
			// With: temporarily override type of designator
			if ExprKind(cur.Lhs.Kind) == EXPR_DeclRef {
				if d, ok := cur.Lhs.Val.(*Declaration); ok && d != nil {
					t := d.OverrideType(cur.Rhs.GetType())
					v.visitBody(cur.Body)
					d.OverrideType(t)
				}
			}
		} else {
			v.visitBody(cur.Body)
		}
		cur = cur.Next
	}
}

func (v *Validator) visitExpr(e *Expression, followNext bool) {
	if e == nil {
		return
	}
	switch ExprKind(e.Kind) {
	case EXPR_Plus, EXPR_Minus, EXPR_Not:
		v.visitExpr(e.Lhs, true)
		v.unaryOp(e)

	case EXPR_Eq, EXPR_Neq, EXPR_Lt, EXPR_Leq, EXPR_Gt, EXPR_Geq,
		EXPR_In, EXPR_Is, EXPR_Add, EXPR_Sub, EXPR_Or, EXPR_Mul,
		EXPR_Fdiv, EXPR_Div, EXPR_Mod, EXPR_And:
		v.visitExpr(e.Lhs, true)
		v.visitExpr(e.Rhs, true)
		v.binaryOp(e)

	case EXPR_Literal, EXPR_DeclRef, EXPR_Cast:
		v.resolveIfNamedType(e.GetType())

	case EXPR_NameRef:
		v.resolveDesig(e)

	case EXPR_Select:
		v.visitExpr(e.Lhs, true)
		v.selectOp(e)

	case EXPR_Deref:
		v.visitExpr(e.Lhs, true)
		v.derefOp(e)

	case EXPR_Index:
		v.visitExpr(e.Lhs, true)
		v.visitExpr(e.Rhs, true)
		v.indexOp(e)

	case EXPR_Call:
		// super call handling: if Lhs is EXPR_Super, evaluate Lhs.Lhs
		if e.Lhs != nil && ExprKind(e.Lhs.Kind) == EXPR_Super {
			v.visitExpr(e.Lhs.Lhs, true)
		} else {
			v.visitExpr(e.Lhs, true)
		}
		v.callOp(e)

	case EXPR_Super:
		v.error(e.Pos, "super call cannot be used here")

	case EXPR_Constructor:
		v.constructor(e)

	case EXPR_Range:
		v.visitExpr(e.Lhs, true)
		v.visitExpr(e.Rhs, true)
		e.SetType(e.Lhs.GetType())

	default:
		// Invalid or not expected
	}
	if followNext && e.Next != nil {
		v.visitExpr(e.Next, true)
	}
}

func (v *Validator) unaryOp(e *Expression) {
	if e.Lhs == nil || e.Lhs.GetType() == nil {
		return
	}
	lhsT := v.deref(e.Lhs.GetType())
	switch ExprKind(e.Kind) {
	case EXPR_Plus:
		if lhsT == nil || !lhsT.IsNumber() {
			v.error(e.Pos, "unary operator not applicable to this type")
			return
		}
	case EXPR_Minus:
		if lhsT == nil || (!lhsT.IsNumber() && !lhsT.IsSet()) {
			v.error(e.Pos, "unary operator not applicable to this type")
			return
		}
	case EXPR_Not:
		if lhsT == nil || !lhsT.IsBoolean() {
			v.error(e.Pos, "unary '~' or 'NOT' not applicable to this type")
			return
		}
	}
	e.SetType(lhsT)
}

func (v *Validator) binaryOp(e *Expression) {
	if e.Lhs == nil || e.Rhs == nil || e.Lhs.GetType() == nil || e.Rhs.GetType() == nil {
		return
	}
	switch ExprKind(e.Kind) {
	// arithmetic-like: take lhs type (as in C++ “we assume code already passed compiler”)
	case EXPR_Mul, EXPR_Fdiv, EXPR_Div, EXPR_Mod, EXPR_Add, EXPR_Sub, EXPR_And, EXPR_Or:
		e.SetType(v.deref(e.Lhs.GetType()))
	// relations: boolean
	case EXPR_Eq, EXPR_Neq, EXPR_Lt, EXPR_Leq, EXPR_Gt, EXPR_Geq, EXPR_In, EXPR_Is:
		e.SetType(v.mdl.GetType(TYPE_BOOLEAN))
	default:
	}
}

func (v *Validator) selectOp(e *Expression) {
	if e.Lhs == nil || e.Lhs.GetType() == nil {
		return
	}
	lhsT := v.deref(e.Lhs.GetType())

	// auto-deref pointer left side for field selection
	if TypeKind(lhsT.Kind) == TYPE_Pointer && lhsT.TypeRef != nil {
		tmp := NewExpression(EXPR_Deref, e.Lhs.Pos)
		tmp.Lhs = e.Lhs
		tmp.SetType(lhsT.TypeRef)
		e.Lhs = tmp
		lhsT = v.deref(e.Lhs.GetType())
	}

	if TypeKind(lhsT.Kind) == TYPE_Record || TypeKind(lhsT.Kind) == TYPE_Object {
		// ensure left isn't a type decl for "f.g"
		if ld, ok := e.Lhs.Val.(*Declaration); ok && ld != nil && DeclKind(ld.Kind) == DECL_TypeDecl {
			v.error(e.Lhs.Pos, "selector expects a variable on the left side")
			return
		}
		field := v.findInType(lhsT, string(e.ValAsBytes()))
		if field == nil {
			v.error(e.Pos, fmt.Sprintf("the record doesn't have a field named '%s'", e.ValAsString()))
			v.markUnref(len(e.ValAsString()), e.Pos)
			return
		}
		s := v.markRef(field, e.Pos)
		if e.NeedsLval {
			if s != nil {
				s.Kind = SYM_Lval
			}
		}
		e.Val = field // field decl or bound proc
		e.SetType(field.GetType())
	} else {
		v.error(e.Pos, "cannot select a field in given type")
	}
}

func (v *Validator) derefOp(e *Expression) {
	if e.Lhs == nil || e.Lhs.GetType() == nil {
		return
	}
	// supercall detection: (Select|DeclRef) to a Procedure then '^' => super
	if (ExprKind(e.Lhs.Kind) == EXPR_Select || ExprKind(e.Lhs.Kind) == EXPR_DeclRef) &&
		e.Lhs.Val != nil {
		if d, ok := e.Lhs.Val.(*Declaration); ok && d != nil && DeclKind(d.Kind) == DECL_Procedure {
			if !d.Receiver {
				v.error(e.Pos, "super calls only supported for type-bound procedures")
			} else {
				e.Kind = int(EXPR_Super)
			}
			return
		}
	}
	lhsT := v.deref(e.Lhs.GetType())
	if TypeKind(lhsT.Kind) == TYPE_Pointer || TypeKind(lhsT.Kind) == TYPE_Object {
		if lhsT.TypeRef != nil {
			e.SetType(lhsT.TypeRef)
		}
	} else {
		v.error(e.Pos, "can only dereference a pointer")
	}
}

func (v *Validator) indexOp(e *Expression) {
	if e.Lhs == nil || e.Rhs == nil || e.Lhs.GetType() == nil || e.Rhs.GetType() == nil {
		return
	}
	lhsT := v.deref(e.Lhs.GetType())
	rhsT := v.deref(e.Rhs.GetType())

	// auto-deref pointer for indexing
	if TypeKind(lhsT.Kind) == TYPE_Pointer && lhsT.TypeRef != nil {
		tmp := NewExpression(EXPR_Deref, e.Lhs.Pos)
		tmp.Lhs = e.Lhs
		tmp.SetType(lhsT.TypeRef)
		e.Lhs = tmp
		lhsT = v.deref(e.Lhs.GetType())
	}

	// result type is element type if array or pointer-to-array
	if lhsT.TypeRef != nil {
		e.SetType(lhsT.TypeRef)
	}
	if TypeKind(lhsT.Kind) == TYPE_Array {
		if rhsT == nil || !rhsT.IsInteger() {
			v.error(e.Rhs.Pos, "expecting an array index of integer type")
		}
	} else {
		v.error(e.Pos, "cannot index an element in given type")
	}
}

func (v *Validator) callOp(e *Expression) {
	supercall := false
	lhs := e.Lhs
	if lhs != nil && ExprKind(lhs.Kind) == EXPR_Super {
		supercall = true
		lhs = lhs.Lhs
	}
	if lhs == nil || lhs.GetType() == nil {
		return
	}

	// Determine procedure and proc type
	var proc *Declaration
	if lhs.Val != nil {
		if d, ok := lhs.Val.(*Declaration); ok {
			proc = d
		}
	}
	procType := v.deref(lhs.GetType())
	if proc != nil && DeclKind(proc.Kind) != DECL_Procedure && DeclKind(proc.Kind) != DECL_Builtin {
		proc = nil
	}
	if procType != nil && TypeKind(procType.Kind) != TYPE_Procedure {
		procType = nil
	}

	// Visit args first
	for arg := e.Rhs; arg != nil; arg = arg.Next {
		v.visitExpr(arg, false)
	}

	// Type guard shorthand: call with DeclRef type as arg and non-builtin => cast
	isTypeCast := (proc == nil || DeclKind(proc.Kind) != DECL_Builtin) &&
		e.Rhs != nil &&
		ExprKind(e.Rhs.Kind) == EXPR_DeclRef &&
		func() bool {
			if d, ok := e.Rhs.Val.(*Declaration); ok && d != nil {
				return DeclKind(d.Kind) == DECL_TypeDecl
			}
			return false
		}()

	lhsT := v.deref(lhs.GetType())
	if isTypeCast {
		if supercall {
			v.error(e.Pos, "super call operator cannot be used here")
			return
		}
		e.Kind = int(EXPR_Cast)
		e.SetType(v.deref(e.Rhs.GetType()))
		if e.Rhs.Next != nil {
			v.error(e.Rhs.Next.Pos, "type guard requires a single argument")
		}
		if TypeKind(lhsT.Kind) == TYPE_Pointer && lhsT.TypeRef != nil {
			lhsT = v.deref(lhsT.TypeRef)
		}
		if TypeKind(lhsT.Kind) != TYPE_Record &&
			TypeKind(lhsT.Kind) != TYPE_Object &&
			TypeKind(lhsT.Kind) != TYPE_PTR && // pragmatic deviations from legacy code
			TypeKind(lhsT.Kind) != TYPE_ANY {
			v.error(e.Rhs.Pos, "a type guard is not supported for this type")
		}
		return
	}

	// Normal call
	var ret *Type
	if proc != nil {
		if DeclKind(proc.Kind) != DECL_Procedure && DeclKind(proc.Kind) != DECL_Builtin {
			v.error(lhs.Pos, "this expression cannot be called")
			return
		}
		ret = proc.GetType()
	} else if procType == nil {
		v.error(lhs.Pos, "this expression cannot be called")
		return
	} else {
		ret = procType.TypeRef
	}

	if supercall && (proc == nil || !proc.Receiver || proc.Super == nil) {
		v.error(e.Pos, "super call operator cannot be used here")
		return
	}

	// Builtins: check args, infer return where necessary
	if proc != nil && DeclKind(proc.Kind) == DECL_Builtin {
		if v.checkBuiltinArgs(BuiltinKind(proc.ID), e.Rhs, &ret, e.Pos) {
			// no extra evaluation here
		}
	} else {
		// basic arity check (types mostly unchecked like original)
		formals := []*Declaration{}
		if proc != nil {
			formals = proc.GetParams(true)
		} else if procType != nil {
			formals = procType.Subs
		}
		argc := 0
		for a := e.Rhs; a != nil; a = a.Next {
			argc++
		}
		if argc != len(formals) {
			v.error(e.Pos, "number of actual doesn't fit number of formal arguments")
		}
	}
	if ret != nil {
		e.SetType(ret)
	}
}

func (v *Validator) constructor(e *Expression) {
	e.SetType(v.mdl.GetType(TYPE_SET))
	comp := e.Rhs
	for comp != nil {
		if ExprKind(comp.Kind) == EXPR_Constructor {
			v.error(comp.Pos, "component type not supported for SET constructors")
			return
		}
		v.visitExpr(comp, false)
		if comp.GetType() != nil && !v.deref(comp.GetType()).IsInteger() {
			v.error(comp.Pos, "expecting integer compontents for SET constructors")
			return
		}
		comp = comp.Next
	}
}

func (v *Validator) deref(t *Type) *Type {
	if t == nil {
		return v.mdl.GetType(TYPE_NoType)
	}
	if TypeKind(t.Kind) == TYPE_NameRef {
		// Resolve local NameRef lazily
		if !t.Validated {
			v.resolveIfNamedType(t)
		}
		if t.TypeRef != nil {
			return v.deref(t.TypeRef)
		}
		return t
	}
	return t
}

func (v *Validator) resolveIfNamedType(nameRef *Type) {
	if nameRef == nil || TypeKind(nameRef.Kind) != TYPE_NameRef {
		return
	}
	if nameRef.Validated {
		return
	}
	if nameRef.Quali == nil {
		return
	}
	q := *nameRef.Quali
	rFirst, rSecond := v.find(q, nameRef.Pos)
	if rSecond == nil {
		rFirst, rSecond = v.find(q, nameRef.Pos) // TEST
		return
	}
	pos := nameRef.Pos
	if rFirst != nil {
		v.markRef(rFirst, pos)
		pos.Col += uint32(len(q.First)) + 1
	}
	v.markRef(rSecond, pos)
	nameRef.Validated = true
	nameRef.SetType(rSecond.GetType())
	if DeclKind(rSecond.Kind) != DECL_TypeDecl {
		v.error(nameRef.Pos, "identifier doesn't refer to a type declaration")
		return
	}
	// ensure nested NameRefs are resolved
	v.resolveIfNamedType(rSecond.GetType())
}

func (v *Validator) resolveDesig(nameRef *Expression) {
	if nameRef == nil || ExprKind(nameRef.Kind) != EXPR_NameRef || len(v.scopeStack) == 0 {
		return
	}
	q, _ := nameRef.Val.(*Qualident)
	rFirst, rSecond := v.find(*q, nameRef.Pos)
	pos := nameRef.Pos
	if rSecond == nil {
		// unresolved: still mark what we can
		if rFirst != nil {
			v.markRef(rFirst, pos)
			pos.Col += uint32(len(q.First)) + 1
		}
		v.markUnref(len(q.Second), pos)
		return
	}
	if rFirst != nil {
		v.markRef(rFirst, pos)
		pos.Col += uint32(len(q.First)) + 1
	}
	s := v.markRef(rSecond, pos)
	if nameRef.NeedsLval && s != nil {
		s.Kind = SYM_Lval
	}
	v.resolveIfNamedType(rSecond.GetType())
	nameRef.Kind = int(EXPR_DeclRef)
	nameRef.Val = rSecond
	nameRef.SetType(rSecond.GetType())

	// Nonlocal flag for locals/params captured from outer scope
	if DeclKind(rSecond.Kind) == DECL_LocalDecl || DeclKind(rSecond.Kind) == DECL_ParamDecl {
		if rSecond.Outer != v.scopeStack[len(v.scopeStack)-1] {
			nameRef.Nonlocal = true
		}
	}
}

// find resolves Qualident to (importDecl, memberDecl) or (nil, decl)
func (v *Validator) find(q Qualident, pos RowCol) (*Declaration, *Declaration) {
	if len(v.scopeStack) == 0 {
		return nil, nil
	}
	// qualified: module import
	var res1 *Declaration = nil
	var res2 *Declaration = nil
	if len(q.First) > 0 {
		importDecl := v.scopeStack[len(v.scopeStack)-1].Find(string(q.First), true)
		if importDecl == nil || DeclKind(importDecl.Kind) != DECL_Import {
			v.error(pos, "identifier doesn't refer to an imported module")
			v.markUnref(len(q.First), pos)
			return nil, nil
		}
		if !importDecl.Validated {
			v.visitImport(importDecl)
		}
		res1 = importDecl
		// lookup member in imported module
		member := v.mdl.FindDeclInImport(importDecl, string(q.Second))
		pos.Col += uint32(len(q.First)) + 1
		if member == nil {
			v.error(pos, fmt.Sprintf("declaration '%s' not found in imported module '%s'", string(q.Second), string(q.First)))
			v.markUnref(len(q.Second), pos)
		} else {
			if member.Visi == VISI_Private {
				v.error(pos, fmt.Sprintf("cannot access private declaration '%s' from module '%s'", string(q.Second), string(q.First)))
			}
			res2 = member
		}
	} else {

		// unqualified: search current + outers up to module
		var d *Declaration
		nested := v.scopeStack[len(v.scopeStack)-1]
		for d == nil && nested != nil && DeclKind(nested.Kind) != DECL_Module {
			d = nested.Find(string(q.Second), false) // local vars first
			nested = nested.Outer
		}
		// within object type scope (type-bound procs), also search fields
		if d == nil && v.curObjectTypeDecl != nil {
			d = v.findInType(v.deref(v.curObjectTypeDecl.GetType()), string(q.Second))
		}
		// module-level
		if d == nil {
			d = v.module.Find(string(q.Second), false)
		}
		// built-ins
		if d == nil {
			d = v.mdl.FindDecl(string(q.Second), false)
		}
		if d == nil {
			v.error(pos, fmt.Sprintf("declaration '%s' not found", string(q.Second)))
			v.markUnref(len(q.Second), pos)
		}
		res2 = d
	}
	return res1, res2
}

func (v *Validator) findInType(t *Type, field string) *Declaration {
	if t == nil {
		return nil
	}
	// direct lookup
	res := t.Find(field, false)
	done := map[*Type]bool{}
	for res == nil && (TypeKind(t.Kind) == TYPE_Record || TypeKind(t.Kind) == TYPE_Object) && t.TypeRef != nil {
		super := v.deref(t.TypeRef)
		if TypeKind(super.Kind) == TYPE_Pointer && super.TypeRef != nil {
			super = v.deref(super.TypeRef)
		}
		res = super.Find(field, false)
		t = super
		if done[t] {
			// protect against accidental cycles found in legacy codebases
			break
		}
		done[t] = true
	}
	return res
}

func (v *Validator) visitType(typ *Type) {
	if typ == nil || typ.Validated {
		return
	}
	typ.Validated = true
	switch TypeKind(typ.Kind) {
	case TYPE_Pointer:
		v.visitType(typ.TypeRef)

	case TYPE_Record, TYPE_Object, TYPE_Procedure:
		// Resolve base object before processing subs
		if TypeKind(typ.Kind) == TYPE_Object && typ.TypeRef != nil && !typ.TypeRef.Validated {
			// Temporarily disable curObjectTypeDecl to avoid infinite recursion in findInType
			tmp := v.curObjectTypeDecl
			v.curObjectTypeDecl = nil
			v.resolveIfNamedType(typ.TypeRef)
			v.curObjectTypeDecl = tmp
		} else {
			v.visitType(typ.TypeRef)
		}
		// Visit subs (fields or params or methods)
		for _, d := range typ.Subs {
			if DeclKind(d.Kind) == DECL_Procedure && v.curObjectTypeDecl != nil {
				// Turn procedure into type-bound method: inject receiver SELF
				d.Receiver = true
				d.Outer = v.curObjectTypeDecl
				v.boundProcs = append(v.boundProcs, d)

				self := NewDeclaration()
				self.Kind = int(DECL_ParamDecl)
				self.Name = []byte("SELF")
				self.SetType(v.curObjectTypeDecl.GetType())
				self.Pos = v.curObjectTypeDecl.Pos
				self.Receiver = true
				self.Next = d.Link
				d.Link = self
			}
			v.visitDecl(d)
		}

		// Validate base record/object compatibility
		if TypeKind(typ.Kind) == TYPE_Record && typ.TypeRef != nil {
			base := v.deref(typ.TypeRef)
			if TypeKind(base.Kind) == TYPE_Pointer && base.TypeRef != nil {
				base = v.deref(base.TypeRef)
			}
			if TypeKind(base.Kind) != TYPE_Record && TypeKind(base.Kind) != TYPE_Object {
				v.error(typ.Pos, "invalid base record")
			}
		}
		if TypeKind(typ.Kind) == TYPE_Object && typ.TypeRef != nil {
			base := v.deref(typ.TypeRef)
			if TypeKind(base.Kind) == TYPE_Pointer && base.TypeRef != nil {
				base = v.deref(base.TypeRef)
			}
			if TypeKind(base.Kind) == TYPE_NameRef {
				base = v.deref(base)
			}
			if TypeKind(base.Kind) != TYPE_Object && TypeKind(base.Kind) != TYPE_Record {
				if typ.TypeRef != nil && typ.TypeRef.Decl != nil {
					v.error(typ.TypeRef.Decl.Pos, "invalid base object")
				} else {
					v.error(typ.Pos, "invalid base object")
				}
			}
			// Note: connecting super methods is handled when visiting TypeDecl above
		}

		if TypeKind(typ.Kind) == TYPE_Procedure {
			ret := v.deref(typ.TypeRef)
			if ret != nil && ret.IsStructured() {
				v.error(typ.Pos, "return type cannot be structured")
			}
		}

	case TYPE_Array:
		// Array size expression or element type
		if typ.Expr != nil {
			v.visitExpr(typ.Expr, true)
			// constant integer size?
			if !typ.Expr.IsConst() || !v.deref(typ.Expr.GetType()).IsInteger() {
				v.error(typ.Expr.Pos, "expecting constant integer expression")
			} else {
				// optional: compute typ.Len if desired
			}
		}
		v.visitType(typ.TypeRef)

	case TYPE_NameRef:
		// force resolve
		typ.Validated = false
		v.resolveIfNamedType(typ)

	default:
		// basic types or no-type do nothing
	}
}

// Minimal builtin argument check; mirrors structure of C++ but simplified.
func (v *Validator) checkBuiltinArgs(builtin BuiltinKind, args *Expression, ret **Type, pos RowCol) bool {
	if ret == nil {
		return false
	}
	*ret = v.mdl.GetType(TYPE_NoType)

	argc := 0
	for a := args; a != nil; a = a.Next {
		argc++
		if a.GetType() == nil {
			return false
		}
	}

	switch builtin {
	case BUILTIN_ABS:
		if argc != 1 {
			v.error(pos, "expecting 1 argument")
			return false
		}
		*ret = args.GetType()
		return true
	case BUILTIN_CHR:
		if argc != 1 {
			v.error(pos, "expecting 1 argument")
			return false
		}
		*ret = v.mdl.GetType(TYPE_CHAR)
		return true
	case BUILTIN_ENTIER:
		if argc != 1 {
			v.error(pos, "expecting 1 argument")
			return false
		}
		*ret = v.mdl.GetType(TYPE_INTEGER)
		return true
	case BUILTIN_LEN:
		if argc < 1 || argc > 2 {
			v.error(pos, "expecting 1 to 2 arguments")
			return false
		}
		*ret = v.mdl.GetType(TYPE_INTEGER)
		return true
	case BUILTIN_MAX, BUILTIN_MIN:
		if argc < 1 || argc > 2 {
			v.error(pos, "expecting 1 to 2 arguments")
			return false
		}
		if v.deref(args.GetType()).Kind == int(TYPE_SET) {
			*ret = v.mdl.GetType(TYPE_BYTE)
		} else {
			*ret = args.GetType()
		}
		return true
	case BUILTIN_ODD:
		if argc != 1 {
			v.error(pos, "expecting 1 argument")
			return false
		}
		*ret = v.mdl.GetType(TYPE_BOOLEAN)
		return true
	case BUILTIN_ORD:
		if argc != 1 {
			v.error(pos, "expecting 1 argument")
			return false
		}
		*ret = v.mdl.GetType(TYPE_INTEGER)
		return true
	case BUILTIN_ASSERT, BUILTIN_INC, BUILTIN_DEC, BUILTIN_EXCL, BUILTIN_INCL, BUILTIN_NEW,
		BUILTIN_COPY, BUILTIN_HALT, BUILTIN_CAP, BUILTIN_ASH, BUILTIN_SHORT, BUILTIN_LONG,
		BUILTIN_SIZE, BUILTIN_AWAIT,
		BUILTIN_SYSTEM_ADR, BUILTIN_SYSTEM_BIT, BUILTIN_SYSTEM_CC, BUILTIN_SYSTEM_LSH,
		BUILTIN_SYSTEM_ROT, BUILTIN_SYSTEM_VAL, BUILTIN_SYSTEM_TYPECODE,
		BUILTIN_SYSTEM_GET, BUILTIN_SYSTEM_PUT, BUILTIN_SYSTEM_MOVE, BUILTIN_SYSTEM_NEW,
		BUILTIN_SYSTEM_PORTOUT, BUILTIN_SYSTEM_PORTIN, BUILTIN_SYSTEM_CLI, BUILTIN_SYSTEM_STI,
		BUILTIN_SYSTEM_GET8, BUILTIN_SYSTEM_GET16, BUILTIN_SYSTEM_GET32, BUILTIN_SYSTEM_GET64,
		BUILTIN_SYSTEM_PUT8, BUILTIN_SYSTEM_PUT16, BUILTIN_SYSTEM_PUT32, BUILTIN_SYSTEM_PUT64,
		BUILTIN_SYSTEM_GETREG, BUILTIN_SYSTEM_PUTREG:
		// Accept with minimal shape checks; set plausible returns for some
		switch builtin {
		case BUILTIN_ASH:
			if argc != 2 {
				v.error(pos, "expecting 2 arguments")
				return false
			}
			*ret = v.mdl.GetType(TYPE_LONGINT)
		case BUILTIN_SIZE, BUILTIN_SYSTEM_ADR, BUILTIN_SYSTEM_TYPECODE:
			if argc != 1 {
				v.error(pos, "expecting 1 argument")
				return false
			}
			*ret = v.mdl.GetType(TYPE_LONGINT)
		case BUILTIN_SYSTEM_BIT, BUILTIN_SYSTEM_CC:
			if argc != 1 && argc != 2 {
				// CC has 1, BIT has 2 in some dialects; keep lenient
			}
			*ret = v.mdl.GetType(TYPE_BOOLEAN)
		default:
			// leave *ret as NoType or passthrough
		}
		return true
	default:
		// Unknown builtin id
		return false
	}
}

// Helpers to get string/[]byte stored in Expression.Val for selectors.
func (e *Expression) ValAsBytes() []byte {
	if b, ok := e.Val.([]byte); ok {
		return b
	}
	if s, ok := e.Val.(string); ok {
		return []byte(s)
	}
	return nil
}
func (e *Expression) ValAsString() string {
	if b, ok := e.Val.([]byte); ok {
		return string(b)
	}
	if s, ok := e.Val.(string); ok {
		return s
	}
	return ""
}
