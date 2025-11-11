#ifndef AOCEEGEN_H
#define AOCEEGEN_H

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

#include <ActiveOberon/AoAst.h>
#include <QTextStream>
#include <QSet>

class QIODevice;

namespace Ao
{
class CeeGen
{
public:
    CeeGen() {}
    bool generate(Ast::Declaration* module, QIODevice* header, QIODevice* body);
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
    bool Type_(Ast::Type *t);
    bool FieldList(Ast::Type *t);
    void Body(Ast::Statement *s);
    void Attributes();
    void StatBlock(Ast::Statement *);
    void StatSeq(Ast::Statement *s);
    Ast::Statement * IfStat(Ast::Statement *s);
    Ast::Statement * CaseStat(Ast::Statement *s);
    void WhileStat(Ast::Statement *s);
    void RepeatStat(Ast::Statement *s);
    Ast::Statement * ForStat(Ast::Statement *s);
    void LoopStat(Ast::Statement *s);
    Ast::Statement * WithStat(Ast::Statement *s);
    void ReturnStat(Ast::Statement *s);
    Ast::Statement *Statement(Ast::Statement *s);
    bool ConstExpr(Ast::Expression *e);
    bool Expr(Ast::Expression *e);

protected:
    bool relation(Ast::Expression *e);
    bool unaryOp(Ast::Expression *e);
    bool arithOp(Ast::Expression *e);
    bool logicOp(Ast::Expression *e);
    bool declRef(Ast::Expression *e);
    bool select(Ast::Expression *e);
    bool index(Ast::Expression *e);
    bool depointer(Ast::Expression *e);
    bool cast(Ast::Expression *e);
    bool call(Ast::Expression *e);
    bool literal(Ast::Expression *e);
    bool constructor(Ast::Expression *e);
    bool range(Ast::Expression *e);
    bool nameRef(Ast::Expression *e);
    void assig(Ast::Statement* s);
    void call(Ast::Statement* s);

protected:
    void invalid(const char* what, const RowCol&);
    void typeDecl(Ast::Declaration* type);
    static QString genDedication();
    Ast::Type* deref(Ast::Type*);
    QByteArray typeRef(Ast::Type*);
    void pointerTo(QTextStream& out, Ast::Type* ptr);
    void printHelper(Ast::Declaration*);
    inline QByteArray ws(int level)
    {
        curLevel = level;
        return QByteArray((level+1)*4,' ');
    }
    void parameter(QTextStream& out, Ast::Declaration* param)
    {
        out << typeRef(param->type()) << " " << param->name;
    }
    void variable(QTextStream& out, Ast::Declaration* var);
    void procHeader(QTextStream& out, Ast::Declaration* proc);

private:
    Ast::Declaration* curMod;
    QTextStream hout;
    QTextStream bout;
    Ast::Declaration* curProc;
    QSet<Ast::Declaration*> done;
    int curLevel;
};

}

#endif // AOCEEGEN_H
