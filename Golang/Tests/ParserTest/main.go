// main.go - AST Dumper for ActiveOberon Compiler
// This program parses an ActiveOberon source file and dumps the complete AST

package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	ao "github.com/rochus-keller/ActiveOberon/Golang/ActiveOberon"
)

func main() {
	// Check command line arguments
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <oberon-file.Mod>\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "\nThis program parses an ActiveOberon source file and dumps the complete AST to stdout.\n")
		fmt.Fprintf(os.Stderr, "\nExample: %s TestModule.Mod\n", os.Args[0])
		os.Exit(1)
	}

	filename := os.Args[1]

	// Check if file exists
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		fmt.Fprintf(os.Stderr, "Error: File '%s' does not exist\n", filename)
		os.Exit(1)
	}

	// Check for .Mod extension
	ext := filepath.Ext(filename)
	if ext != ".Mod" && ext != ".mod" {
		fmt.Fprintf(os.Stderr, "Warning: File '%s' does not have .Mod extension\n", filename)
	}

	fmt.Printf("Parsing ActiveOberon file: %s\n", filename)
	fmt.Printf("=" + strings.Repeat("=", 60) + "\n\n")

	// Create lexer
	lexer := ao.NewLexer()
	lexer.SetIgnoreComments(true) // Parser doesn't need comments

	err := lexer.SetStreamFromFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file '%s': %v\n", filename, err)
		os.Exit(1)
	}

	// Create AST model
	model := ao.NewAstModel()

	// Create parser
	parser := ao.NewParser(model, lexer)

	// Parse the file
	parser.RunParser()

	// Check for parsing errors
	if parser.HasErrors() {
		fmt.Printf("PARSING FAILED with %d error(s):\n\n", parser.ErrorCount())
		for i, err := range parser.Errors() {
			fmt.Printf("Error %d: %s\n", i+1, err.Msg)
			fmt.Printf("  Position: %s\n", err.Pos.String())
			if err.Path != "" {
				fmt.Printf("  File: %s\n", err.Path)
			}
			fmt.Println()
		}
		os.Exit(1)
	}

	// Get the parsed module
	module := parser.TakeResult()
	if module == nil {
		fmt.Printf("Error: No module was parsed\n")
		os.Exit(1)
	}

	fmt.Printf("PARSING SUCCESSFUL!\n\n")
	fmt.Printf("AST DUMP:\n")
	fmt.Printf("=" + strings.Repeat("=", 60) + "\n\n")

	// Dump the complete AST
	dumpDeclaration(module, 0)

	fmt.Printf("\n" + strings.Repeat("=", 60) + "\n")
	fmt.Printf("AST dump complete.\n")
}

// dumpDeclaration recursively dumps a declaration and its children
func dumpDeclaration(decl *ao.Declaration, indent int) {
	if decl == nil {
		return
	}

	indentStr := strings.Repeat("  ", indent)

	// Dump declaration header
	fmt.Printf("%s%s: %s", indentStr, getDeclKindName(ao.DeclKind(decl.Kind)), string(decl.Name))

	// Add position info
	if decl.Pos.IsValid() {
		fmt.Printf(" [%s]", decl.Pos.String())
	}

	// Add visibility info
	if decl.Visi != ao.VISI_Private {
		fmt.Printf(" (%s)", getVisibilityName(decl.Visi))
	}

	// Add flags
	var flags []string
	if decl.Active {
		flags = append(flags, "ACTIVE")
	}
	if decl.Exclusive {
		flags = append(flags, "EXCLUSIVE")
	}
	if decl.VarParam {
		flags = append(flags, "VAR")
	}
	if decl.Begin {
		flags = append(flags, "BEGIN")
	}
	if len(flags) > 0 {
		fmt.Printf(" {%s}", strings.Join(flags, ", "))
	}

	fmt.Println()

	// Dump type information
	if decl.GetType() != nil && decl.Kind != int(ao.DECL_Module) {
		fmt.Printf("%s  Type: ", indentStr)
		dumpType(decl.GetType(), indent+1, false)
		fmt.Println()
	}

	// Dump constant value
	if decl.Kind == int(ao.DECL_ConstDecl) && decl.Data != nil {
		fmt.Printf("%s  Value: %v\n", indentStr, decl.Data)
	}

	// Dump import information
	if decl.Kind == int(ao.DECL_Import) && decl.Data != nil {
		if importData, ok := decl.Data.(*ao.Import); ok {
			fmt.Printf("%s  Module: %s\n", indentStr, string(importData.ModuleName))
		}
	}

	// Dump module information
	if decl.Kind == int(ao.DECL_Module) && decl.Data != nil {
		if moduleData, ok := decl.Data.(*ao.ModuleData); ok {
			fmt.Printf("%s  Source: %s\n", indentStr, moduleData.SourcePath)
			fmt.Printf("%s  FullName: %s\n", indentStr, string(moduleData.FullName))
		}
	}

	// Dump constant expression
	if decl.Expr != nil {
		fmt.Printf("%s  Expression:\n", indentStr)
		dumpExpression(decl.Expr, indent+2)
	}

	// Dump procedure body
	if decl.Body != nil {
		fmt.Printf("%s  Body:\n", indentStr)
		dumpStatement(decl.Body, indent+2)
	}

	// Recursively dump child declarations
	current := decl.Link
	if current != nil {
		fmt.Printf("%s  Members:\n", indentStr)
		for current != nil {
			dumpDeclaration(current, indent+2)
			current = current.Next
		}
	}
}

// dumpType dumps type information
func dumpType(t *ao.Type, indent int, inline bool) {
	if t == nil {
		fmt.Print("<nil type>")
		return
	}

	indentStr := ""
	if !inline {
		indentStr = strings.Repeat("  ", indent)
	}

	typeName := getTypeKindName(ao.TypeKind(t.Kind))
	fmt.Printf("%s%s", indentStr, typeName)

	switch ao.TypeKind(t.Kind) {
	case ao.TYPE_Array:
		if t.Len > 0 {
			fmt.Printf("[%d]", t.Len)
		} else if t.Expr != nil {
			fmt.Printf("[")
			dumpExpression(t.Expr, 0)
			fmt.Printf("]")
		} else {
			fmt.Printf("[]")
		}
		if t.TypeRef != nil {
			fmt.Printf(" OF ")
			dumpType(t.TypeRef, 0, true)
		}

	case ao.TYPE_Pointer:
		fmt.Printf(" TO ")
		if t.TypeRef != nil {
			dumpType(t.TypeRef, 0, true)
		}

	case ao.TYPE_NameRef:
		if t.Quali != nil {
			if t.Quali.Second != nil {
				fmt.Printf(" %s.%s", string(t.Quali.First), string(t.Quali.Second))
			} else {
				fmt.Printf(" %s", string(t.Quali.First))
			}
		}

	case ao.TYPE_Record, ao.TYPE_Object:
		if t.TypeRef != nil {
			fmt.Printf("(")
			dumpType(t.TypeRef, 0, true)
			fmt.Printf(")")
		}
		if !inline && len(t.Subs) > 0 {
			fmt.Printf("\n%s  Fields:\n", indentStr)
			for _, field := range t.Subs {
				dumpDeclaration(field, indent+2)
			}
		}

	case ao.TYPE_Procedure:
		if len(t.Subs) > 0 {
			fmt.Printf("(")
			first := true
			for _, param := range t.Subs {
				if !first {
					fmt.Printf("; ")
				}
				if param.VarParam {
					fmt.Printf("VAR ")
				}
				fmt.Printf("%s: ", string(param.Name))
				if param.GetType() != nil {
					dumpType(param.GetType(), 0, true)
				}
				first = false
			}
			fmt.Printf(")")
		}
		if t.TypeRef != nil {
			fmt.Printf(": ")
			dumpType(t.TypeRef, 0, true)
		}
	}
}

// dumpExpression dumps expression trees
func dumpExpression(expr *ao.Expression, indent int) {
	if expr == nil {
		return
	}

	indentStr := strings.Repeat("  ", indent)
	exprName := getExprKindName(ao.ExprKind(expr.Kind))

	fmt.Printf("%s%s", indentStr, exprName)

	// Add position info if available
	if expr.Pos.IsValid() {
		fmt.Printf(" [%s]", expr.Pos.String())
	}

	// Dump value for literals and references
	if expr.Val != nil {
		switch ao.ExprKind(expr.Kind) {
		case ao.EXPR_Literal:
			fmt.Printf(" = %s", string(expr.Val.([]byte)))
		case ao.EXPR_NameRef:
			if quali, ok := expr.Val.(*ao.Qualident); ok {
				if quali.Second != nil {
					fmt.Printf(" %s.%s", string(quali.First), string(quali.Second))
				} else {
					fmt.Printf(" %s", string(quali.First))
				}
			}
		case ao.EXPR_DeclRef:
			if decl, ok := expr.Val.(*ao.Declaration); ok {
				fmt.Printf(" -> %s", string(decl.Name))
			}
		case ao.EXPR_Select:
			fmt.Printf(" .%s", string(expr.Val.([]byte)))
		}
	}

	fmt.Println()

	// Dump child expressions
	if expr.Lhs != nil {
		fmt.Printf("%s  Left:\n", indentStr)
		dumpExpression(expr.Lhs, indent+2)
	}

	if expr.Rhs != nil {
		fmt.Printf("%s  Right:\n", indentStr)
		current := expr.Rhs
		for current != nil {
			dumpExpression(current, indent+2)
			current = current.Next
		}
	}
}

// dumpStatement dumps statement trees
func dumpStatement(stmt *ao.Statement, indent int) {
	if stmt == nil {
		return
	}

	indentStr := strings.Repeat("  ", indent)
	stmtName := getStatementKindName(stmt.Kind)

	fmt.Printf("%s%s", indentStr, stmtName)

	// Add position info
	if stmt.Pos.IsValid() {
		fmt.Printf(" [%s]", stmt.Pos.String())
	}

	// Add flags
	var flags []string
	if stmt.Active {
		flags = append(flags, "ACTIVE")
	}
	if stmt.Exclusive {
		flags = append(flags, "EXCLUSIVE")
	}
	if len(flags) > 0 {
		fmt.Printf(" {%s}", strings.Join(flags, ", "))
	}

	fmt.Println()

	// Dump left-hand side (procedure, assignment target)
	if stmt.Lhs != nil {
		fmt.Printf("%s  Target:\n", indentStr)
		dumpExpression(stmt.Lhs, indent+2)
	}

	// Dump right-hand side (arguments, conditions, values)
	if stmt.Rhs != nil {
		fmt.Printf("%s  Expression:\n", indentStr)
		dumpExpression(stmt.Rhs, indent+2)
	}

	// Dump statement body
	if stmt.Body != nil {
		fmt.Printf("%s  Body:\n", indentStr)
		dumpStatement(stmt.Body, indent+2)
	}

	// Dump next statements in sequence
	if stmt.Next != nil {
		dumpStatement(stmt.Next, indent)
	}
}

// Helper functions to convert enums to readable names

func getDeclKindName(kind ao.DeclKind) string {
	switch kind {
	case ao.DECL_Module:
		return "MODULE"
	case ao.DECL_Import:
		return "IMPORT"
	case ao.DECL_ConstDecl:
		return "CONST"
	case ao.DECL_TypeDecl:
		return "TYPE"
	case ao.DECL_VarDecl:
		return "VAR"
	case ao.DECL_Field:
		return "FIELD"
	case ao.DECL_Procedure:
		return "PROCEDURE"
	case ao.DECL_ParamDecl:
		return "PARAM"
	case ao.DECL_LocalDecl:
		return "LOCAL"
	case ao.DECL_Builtin:
		return "BUILTIN"
	default:
		return fmt.Sprintf("DECL_%d", int(kind))
	}
}

func getTypeKindName(kind ao.TypeKind) string {
	switch kind {
	case ao.TYPE_BOOLEAN:
		return "BOOLEAN"
	case ao.TYPE_CHAR:
		return "CHAR"
	case ao.TYPE_SHORTINT:
		return "SHORTINT"
	case ao.TYPE_INTEGER:
		return "INTEGER"
	case ao.TYPE_LONGINT:
		return "LONGINT"
	case ao.TYPE_REAL:
		return "REAL"
	case ao.TYPE_LONGREAL:
		return "LONGREAL"
	case ao.TYPE_SET:
		return "SET"
	case ao.TYPE_Array:
		return "ARRAY"
	case ao.TYPE_Record:
		return "RECORD"
	case ao.TYPE_Object:
		return "OBJECT"
	case ao.TYPE_Pointer:
		return "POINTER"
	case ao.TYPE_Procedure:
		return "PROCEDURE"
	case ao.TYPE_NameRef:
		return "NAMEREF"
	case ao.TYPE_StrLit:
		return "STRING"
	case ao.TYPE_NIL:
		return "NIL"
	default:
		return fmt.Sprintf("TYPE_%d", int(kind))
	}
}

func getExprKindName(kind ao.ExprKind) string {
	switch kind {
	case ao.EXPR_Literal:
		return "LITERAL"
	case ao.EXPR_NameRef:
		return "NAMEREF"
	case ao.EXPR_DeclRef:
		return "DECLREF"
	case ao.EXPR_Select:
		return "SELECT"
	case ao.EXPR_Index:
		return "INDEX"
	case ao.EXPR_Deref:
		return "DEREF"
	case ao.EXPR_Call:
		return "CALL"
	case ao.EXPR_Add:
		return "ADD"
	case ao.EXPR_Sub:
		return "SUB"
	case ao.EXPR_Mul:
		return "MUL"
	case ao.EXPR_Fdiv:
		return "FDIV"
	case ao.EXPR_Div:
		return "DIV"
	case ao.EXPR_Mod:
		return "MOD"
	case ao.EXPR_And:
		return "AND"
	case ao.EXPR_Or:
		return "OR"
	case ao.EXPR_Eq:
		return "EQ"
	case ao.EXPR_Neq:
		return "NEQ"
	case ao.EXPR_Lt:
		return "LT"
	case ao.EXPR_Leq:
		return "LEQ"
	case ao.EXPR_Gt:
		return "GT"
	case ao.EXPR_Geq:
		return "GEQ"
	case ao.EXPR_In:
		return "IN"
	case ao.EXPR_Is:
		return "IS"
	case ao.EXPR_Not:
		return "NOT"
	case ao.EXPR_Plus:
		return "PLUS"
	case ao.EXPR_Minus:
		return "MINUS"
	case ao.EXPR_Range:
		return "RANGE"
	case ao.EXPR_Constructor:
		return "CONSTRUCTOR"
	case ao.EXPR_Cast:
		return "CAST"
	default:
		return fmt.Sprintf("EXPR_%d", int(kind))
	}
}

func getStatementKindName(kind ao.StatementKind) string {
	switch kind {
	case ao.STMT_StatBlock:
		return "BLOCK"
	case ao.STMT_Assig:
		return "ASSIGN"
	case ao.STMT_Call:
		return "CALL"
	case ao.STMT_If:
		return "IF"
	case ao.STMT_Elsif:
		return "ELSIF"
	case ao.STMT_Else:
		return "ELSE"
	case ao.STMT_Case:
		return "CASE"
	case ao.STMT_CaseLabel:
		return "CASE_LABEL"
	case ao.STMT_While:
		return "WHILE"
	case ao.STMT_Repeat:
		return "REPEAT"
	case ao.STMT_ForAssig:
		return "FOR_ASSIGN"
	case ao.STMT_ForToBy:
		return "FOR_TO_BY"
	case ao.STMT_Loop:
		return "LOOP"
	case ao.STMT_With:
		return "WITH"
	case ao.STMT_Exit:
		return "EXIT"
	case ao.STMT_Return:
		return "RETURN"
	default:
		return fmt.Sprintf("STMT_%d", int(kind))
	}
}

func getVisibilityName(visi ao.Visi) string {
	switch visi {
	case ao.VISI_Private:
		return "PRIVATE"
	case ao.VISI_ReadOnly:
		return "READONLY"
	case ao.VISI_ReadWrite:
		return "READWRITE"
	default:
		return "UNKNOWN"
	}
}
