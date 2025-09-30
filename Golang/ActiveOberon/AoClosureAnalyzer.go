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

package ActiveOberon

import (
	"fmt"
	"sort"
	"strings"
)

// ClosureParam represents a parameter that needs to be added for closure conversion.
type ClosureParam struct {
	Name         string
	OriginalName string
	SourceProc   *Declaration
	SourceDecl   *Declaration
	Renamed      bool
}

// NestedProcInfo contains information about a nested procedure and its closure requirements.
type NestedProcInfo struct {
	Proc          *Declaration
	Path          []string
	ClosureParams []ClosureParam
	AccessedVars  []*Declaration
	OuterProcs    []*Declaration
}

// ClosureAnalyzer performs closure analysis on a single module.
type ClosureAnalyzer struct {
	module     *Declaration
	results    []NestedProcInfo
	procStack  []*Declaration
	pathStack  []string
	moduleName string
}

// NewClosureAnalyzerForModule constructs an analyzer for a single module.
func NewClosureAnalyzerForModule(module *Declaration) *ClosureAnalyzer {
	return &ClosureAnalyzer{
		module:     module,
		results:    make([]NestedProcInfo, 0),
		procStack:  make([]*Declaration, 0),
		pathStack:  make([]string, 0),
		moduleName: string(module.Name),
	}
}

// Analyze walks this module and returns closure requirements for all nested procedures
// that actually need extra parameters (i.e., have at least one non-local capture).
func (ca *ClosureAnalyzer) Analyze() []NestedProcInfo {
	ca.results = ca.results[:0]
	if ca.module == nil || DeclKind(ca.module.Kind) != DECL_Module {
		return ca.results
	}
	// Seed path with module name.
	ca.pathStack = append(ca.pathStack, string(ca.module.Name))

	// Walk top-level declarations and analyze procedures.
	for d := ca.module.Link; d != nil; d = d.Next {
		if DeclKind(d.Kind) == DECL_Procedure {
			ca.analyzeProcedure(d)
		}
	}

	// Pop module name.
	ca.pathStack = ca.pathStack[:len(ca.pathStack)-1]
	return ca.results
}

func (ca *ClosureAnalyzer) analyzeProcedure(proc *Declaration) {
	if proc == nil || DeclKind(proc.Kind) != DECL_Procedure {
		return
	}
	// Enter this procedure.
	ca.procStack = append(ca.procStack, proc)
	ca.pathStack = append(ca.pathStack, string(proc.Name))

	// If nested, analyze closure needs.
	if len(ca.procStack) > 1 {
		info := ca.analyzeNestedProcedure(proc)
		// Only keep if there is at least one capture.
		if len(info.ClosureParams) > 0 {
			ca.results = append(ca.results, info)
		}
	}

	// Recurse into nested procedures declared within this procedure.
	for d := proc.Link; d != nil; d = d.Next {
		if DeclKind(d.Kind) == DECL_Procedure {
			ca.analyzeProcedure(d)
		}
	}

	// Leave this procedure.
	ca.procStack = ca.procStack[:len(ca.procStack)-1]
	ca.pathStack = ca.pathStack[:len(ca.pathStack)-1]
}

func (ca *ClosureAnalyzer) analyzeNestedProcedure(proc *Declaration) NestedProcInfo {
	info := NestedProcInfo{
		Proc:          proc,
		Path:          append([]string(nil), ca.pathStack...),
		ClosureParams: make([]ClosureParam, 0),
		AccessedVars:  make([]*Declaration, 0),
		OuterProcs:    append([]*Declaration(nil), ca.procStack[:len(ca.procStack)-1]...),
	}

	// Collect non-local accessed declarations.
	accessedVars := ca.findNonLocalAccesses(proc)
	info.AccessedVars = accessedVars

	// Seed names with existing parameters and locals of the nested proc.
	existingParams := ca.getExistingParamNames(proc)
	existingLocals := ca.getExistingLocalNames(proc)

	seen := make(map[*Declaration]bool)
	for _, decl := range accessedVars {
		if seen[decl] {
			continue
		}
		seen[decl] = true
		param := ca.createClosureParam(decl, existingParams, existingLocals)
		info.ClosureParams = append(info.ClosureParams, param)
		existingParams[param.Name] = true
	}

	return info
}

func (ca *ClosureAnalyzer) findNonLocalAccesses(proc *Declaration) []*Declaration {
	accessed := make(map[*Declaration]bool)

	// Walk the statement tree of the procedure body, if present.
	if proc.Body != nil {
		ca.walkStmt(proc.Body, proc, accessed)
	}

	// Stabilize order: outer proc nesting depth first, then by identifier name.
	out := make([]*Declaration, 0, len(accessed))
	for d := range accessed {
		out = append(out, d)
	}
	sort.Slice(out, func(i, j int) bool {
		di := ca.procDepthOfDecl(out[i])
		dj := ca.procDepthOfDecl(out[j])
		if di != dj {
			return di < dj
		}
		return string(out[i].Name) < string(out[j].Name)
	})
	return out
}

func (ca *ClosureAnalyzer) walkStmt(s *Statement, currentProc *Declaration, acc map[*Declaration]bool) {
	if s == nil {
		return
	}
	// Visit attached expressions (covers calls, assignments, conditions, returns, etc.).
	ca.walkExpr(s.Lhs, currentProc, acc)
	ca.walkExpr(s.Rhs, currentProc, acc)
	// Visit nested body and then the list.
	ca.walkStmt(s.Body, currentProc, acc)
	ca.walkStmt(s.Next, currentProc, acc)
}

func (ca *ClosureAnalyzer) walkExpr(e *Expression, currentProc *Declaration, acc map[*Declaration]bool) {
	if e == nil {
		return
	}
	// Direct declaration reference.
	if ExprKind(e.Kind) == EXPR_DeclRef {
		if decl, ok := e.Val.(*Declaration); ok && decl != nil {
			if ca.needsCapture(decl, currentProc) {
				acc[decl] = true
			}
		}
	}
	// Recurse through subexpressions and argument lists.
	ca.walkExpr(e.Lhs, currentProc, acc)
	ca.walkExpr(e.Rhs, currentProc, acc)
	ca.walkExpr(e.Next, currentProc, acc)
}

// needsCapture reports whether decl referenced inside currentProc must be captured.
func (ca *ClosureAnalyzer) needsCapture(decl *Declaration, currentProc *Declaration) bool {
	if decl == nil || currentProc == nil {
		return false
	}
	// Only variable-like entities can be captured.
	k := DeclKind(decl.Kind)
	if k != DECL_VarDecl && k != DECL_LocalDecl && k != DECL_ParamDecl {
		return false
	}
	// If declared at module scope (no enclosing procedure), no capture is needed.
	declaringProc := ca.findDeclaringProc(decl)
	if declaringProc == nil {
		return false
	}
	// Must be declared in an ancestor procedure different from currentProc.
	return declaringProc != currentProc && ca.isAncestorProc(declaringProc, currentProc)
}

func (ca *ClosureAnalyzer) findDeclaringProc(decl *Declaration) *Declaration {
	for o := decl.Outer; o != nil; o = o.Outer {
		if DeclKind(o.Kind) == DECL_Procedure {
			return o
		}
	}
	return nil // module-level or otherwise not inside a procedure
}

func (ca *ClosureAnalyzer) isAncestorProc(ancestor, descendant *Declaration) bool {
	for o := descendant.Outer; o != nil; o = o.Outer {
		if o == ancestor {
			return true
		}
	}
	return false
}

func (ca *ClosureAnalyzer) procDepthOfDecl(decl *Declaration) int {
	depth := 0
	for o := decl.Outer; o != nil; o = o.Outer {
		if DeclKind(o.Kind) == DECL_Procedure {
			depth++
		}
	}
	return depth
}

func (ca *ClosureAnalyzer) getExistingParamNames(proc *Declaration) map[string]bool {
	names := make(map[string]bool)
	for _, p := range proc.GetParams(false) {
		names[string(p.Name)] = true
	}
	return names
}

func (ca *ClosureAnalyzer) getExistingLocalNames(proc *Declaration) map[string]bool {
	names := make(map[string]bool)
	for d := proc.Link; d != nil; d = d.Next {
		if DeclKind(d.Kind) == DECL_LocalDecl {
			names[string(d.Name)] = true
		}
	}
	return names
}

func (ca *ClosureAnalyzer) createClosureParam(accessedVar *Declaration, existingParams, existingLocals map[string]bool) ClosureParam {
	orig := string(accessedVar.Name)
	name := orig
	renamed := false
	i := 1
	for existingParams[name] || existingLocals[name] {
		name = fmt.Sprintf("%s_%d", orig, i)
		i++
		renamed = true
	}
	return ClosureParam{
		Name:         name,
		OriginalName: orig,
		SourceProc:   ca.findDeclaringProc(accessedVar),
		SourceDecl:   accessedVar,
		Renamed:      renamed,
	}
}

// PrintResults prints a human-readable summary.
func (ca *ClosureAnalyzer) PrintResults() {
	if len(ca.results) == 0 {
		// fmt.Println("No nested procedures requiring closure parameters found.")
		return
	}
	fmt.Printf("Module %s found %d nested procedures requiring closure parameters:\n\n", ca.moduleName, len(ca.results))
	for i, info := range ca.results {
		fmt.Printf("%d. Proc: %s\n", i+1, string(info.Proc.Name))
		fmt.Printf("   Path: %s\n", strings.Join(info.Path, "."))
		fmt.Printf("   Nesting depth: %d\n", len(info.OuterProcs))
		fmt.Println("   Additional parameters:")
		for j, p := range info.ClosureParams {
			from := "<module>"
			if p.SourceProc != nil {
				from = string(p.SourceProc.Name)
			}
			ren := ""
			if p.Renamed {
				ren = fmt.Sprintf(" (renamed from %s)", p.OriginalName)
			}
			fmt.Printf("     %d) %s%s <- %s from %s\n", j+1, p.Name, ren, p.OriginalName, from)
		}
		fmt.Println()
	}
}

// Results returns the collected results.
func (ca *ClosureAnalyzer) Results() []NestedProcInfo { return ca.results }
