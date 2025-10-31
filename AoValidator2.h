#ifndef __AO_VALIDATOR2__
#define __AO_VALIDATOR2__

/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the ActiveOberon project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include <AoAst.h>
#include <QList>

namespace Ao {

    class Validator2 {
	public:
        Validator2() {}
        void validate(Ast::Declaration* module);
		struct Error {
		    QString msg;
            RowCol pos;
		    QString path;
            Error( const QString& m, const RowCol& rc, const QString& p):msg(m),pos(rc),path(p){}
		};
		QList<Error> errors;
	protected:
        void Module(Ast::Declaration* module);
        void ImportDecl(Ast::Declaration *import);
        Ast::Declaration *ImportList(Ast::Declaration *import);
        Ast::Declaration *DeclSeq(Ast::Declaration *d);
        void ConstDecl(Ast::Declaration *);
        void TypeDecl(Ast::Declaration *);
        void VarDecl(Ast::Declaration *);
        void Assembler(Ast::Declaration *proc);
        void ProcDecl(Ast::Declaration*);
		void SysFlag();
        bool ArrayType(Ast::Type *t);
        bool RecordType(Ast::Type *t);
        bool PointerType(Ast::Type *t);
        bool ObjectType(Ast::Type *t);
        bool ProcedureType(Ast::Type *t);
        bool AliasType(Ast::Type *t);
        bool Type(Ast::Type *t);
        bool FieldList(Ast::Type *t);
        void Body(Ast::Statement *s);
		void Attributes();
        void StatBlock(Ast::Statement *);
        void StatSeq(Ast::Statement *s);
		void IfStat();
		void CaseStat();
		void WhileStat();
		void RepeatStat();
		void ForStat();
		void LoopStat();
		void WithStat();
        void ReturnStat(Ast::Statement *s);
        Ast::Statement *Statement(Ast::Statement *s);
		void Case();
		void CaseLabels();
        bool ConstExpr(Ast::Expression *e);
        bool Expr(Ast::Expression *e);
		void Qualident();
    protected:
        bool relation(Ast::Expression *e);
        bool unaryOp(Ast::Expression *e);
        bool arithOp(Ast::Expression *e);
        bool logicOp(Ast::Expression *e);
        bool declRef(Ast::Expression *e);
        bool select(Ast::Expression *e);
        bool index(Ast::Expression *e);
        bool deref(Ast::Expression *e);
        bool cast(Ast::Expression *e);
        bool call(Ast::Expression *e);
        bool literal(Ast::Expression *e);
        bool constructor(Ast::Expression *e);
        bool nameRef(Ast::Expression *e);
        void assig(Ast::Statement* s);
        void call(Ast::Statement* s);

	protected:
        void invalid(const char* what, const RowCol&);

    private:
        QString sourcePath;
	};
}
#endif // include __AO_VALIDATOR2__
