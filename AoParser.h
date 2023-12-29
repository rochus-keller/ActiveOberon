#ifndef __AO_PARSER__
#define __AO_PARSER__
// This file was automatically generated by EbnfStudio; don't modify it!

#include <AoSynTree.h>

namespace Ao {

	class Scanner {
	public:
		virtual Token next() = 0;
		virtual Token peek(int offset) = 0;
	};

	class Parser {
	public:
		Parser(Scanner* s):scanner(s) {}
		void RunParser();
		SynTree root;
		struct Error {
		    QString msg;
		    int row, col;
		    QString path;
		    Error( const QString& m, int r, int c, const QString& p):msg(m),row(r),col(c),path(p){}
		};
		QList<Error> errors;
	protected:
		void Module(SynTree*);
		void ImportList(SynTree*);
		void Definition(SynTree*);
		void DeclSeq(SynTree*);
		void ConstDecl(SynTree*);
		void TypeDecl(SynTree*);
		void VarDecl(SynTree*);
		void Assembler(SynTree*);
		void ProcDecl(SynTree*);
		void ProcHead(SynTree*);
		void SysFlag(SynTree*);
		void FormalPars(SynTree*);
		void FPSection(SynTree*);
		void ArrayType(SynTree*);
		void RecordType(SynTree*);
		void PointerType(SynTree*);
		void ObjectType(SynTree*);
		void ProcedureType(SynTree*);
		void AliasType(SynTree*);
		void Type(SynTree*);
		void FieldDecl(SynTree*);
		void FieldList(SynTree*);
		void Body(SynTree*);
		void Attributes(SynTree*);
		void StatBlock(SynTree*);
		void StatSeq(SynTree*);
		void AssigOrCall(SynTree*);
		void IfStat(SynTree*);
		void CaseStat(SynTree*);
		void WhileStat(SynTree*);
		void RepeatStat(SynTree*);
		void ForStat(SynTree*);
		void LoopStat(SynTree*);
		void WithStat(SynTree*);
		void ReturnStat(SynTree*);
		void Statement(SynTree*);
		void Case(SynTree*);
		void CaseLabels(SynTree*);
		void ConstExpr(SynTree*);
		void Expr(SynTree*);
		void SimpleExpr(SynTree*);
		void Term(SynTree*);
		void Factor(SynTree*);
		void Set(SynTree*);
		void Element(SynTree*);
		void Relation(SynTree*);
		void MulOp(SynTree*);
		void AddOp(SynTree*);
		void Designator(SynTree*);
		void ExprList(SynTree*);
		void IdentList(SynTree*);
		void Qualident(SynTree*);
		void IdentDef(SynTree*);
		void number(SynTree*);
	protected:
		Token cur;
		Token la;
		Scanner* scanner;
		void next();
		Token peek(int off);
		void invalid(const char* what);
		bool expect(int tt, bool pkw, const char* where);
		void addTerminal(SynTree* st);
	};
}
#endif // include