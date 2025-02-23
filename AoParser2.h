#ifndef __AO_PARSER2__
#define __AO_PARSER2__
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

#include "AoToken.h"
#include "AoAst.h"
#include <QList>
#include <QVector>
#include <bitset>

namespace Ao {

	class Scanner2 {
	public:
		virtual Token next() = 0;
		virtual Token peek(int offset) = 0;
        virtual QString source() const = 0;
    };

	class Parser2 {
	public:
        Parser2(Ast::AstModel* m, Scanner2* s);
		void RunParser();
		struct Error {
		    QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };
		QList<Error> errors;
	protected:
        struct ID {
            Token name;
            enum Visi { Private, ReadOnly, Public };
            quint8 visi;
            bool untraced;
            ID():visi(Private),untraced(false){}
            bool isValid() const { return name.d_type == Tok_ident; }
        };
        enum Attrs { UNTRACED, ACTIVE, DELEGATE, EXCLUSIVE, PRIORITY, SAFE, MaxAttr };

        void Module();
		void ImportDecl();
		void ImportList();
		void DeclSeq();
		void ConstDecl();
		void TypeDecl();
		void VarDecl();
        QByteArray Assembler();
        void ProcDecl();
        Ast::Declaration* ProcHead(bool forwardDecl);
        bool SysFlag();
        Ast::Type* FormalPars();
		void FPSection();
        Ast::Type* ArrayType();
        Ast::Type* RecordType();
        Ast::Type* PointerType();
        Ast::Type* ObjectType();
        Ast::Type* ProcedureType();
        Ast::Type* AliasType();
        Ast::Type* NamedType();
        Ast::Type* Type_(bool deanonymize = true);
		void FieldDecl();
		void FieldList();
        Ast::Statement* Body();
        std::bitset<MaxAttr> Attributes();
        int Attribute();
        Ast::Statement* StatBlock();
        Ast::Statement* StatSeq();
        Ast::Statement* AssigOrCall();
        Ast::Statement* IfStat();
        Ast::Statement* CaseStat();
        Ast::Statement* WhileStat();
        Ast::Statement* RepeatStat();
        Ast::Statement* ForStat();
        Ast::Statement* LoopStat();
        Ast::Statement* WithStat();
        Ast::Statement* ReturnStat();
        Ast::Statement* Statement_();
        Ast::Statement* Case();
        Ast::Expression* CaseLabels();
        Ast::Expression* ConstExpr();
        Ast::Expression* Expr(bool lvalue = false);
        Ast::Expression* SimpleExpr(bool lvalue);
        Ast::Expression* Term(bool lvalue);
        Ast::Expression* Factor(bool lvalue);
        Ast::Expression* Set();
        Ast::Expression* Element();
        quint8 Relation();
        quint8 MulOp();
        quint8 AddOp();
        Ast::Expression* Designator(bool lvalue);
        Ast::ExpList ExprList();
        QList<ID> IdentList();
        Ast::Qualident Qualident_();
        ID IdentDef();
        Ast::Expression* number();

    protected:
        Ast::Declaration* addDecl(const Token& id, quint8 visi, quint8 mode);
        void error( const Token&, const QString& msg);
        void error( const RowCol&, const QString& msg );
        Ast::Declaration* addHelper(Ast::Type* t);

	protected:
        Ast::Declaration* thisMod;
        Ast::AstModel* mdl;
        Token cur;
		Token la;
		Scanner2* scanner;
		void next();
		Token peek(int off);
		void invalid(const char* what);
		bool expect(int tt, bool pkw, const char* where);
        QVector<QByteArray> predefSymbols; // MaxAttrs
	};
}
#endif // include
