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
#include <ActiveOberon/AoClosureLifter.h>
#include <QTextStream>
#include <QSet>

class QIODevice;

namespace Ao
{
class CeeGen
{
public:
    CeeGen();
    bool generate(Ast::Declaration* module, QIODevice* header, QIODevice* body, bool generateMain = false);
    static QString genDedication();
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
    Ast::Declaration *DeclSeq(Ast::Declaration *d, bool doProcs, bool doOthers);
    void ConstDecl(Ast::Declaration *);
    void TypeDecl(Ast::Declaration *);
    void VarDecl(Ast::Declaration *);
    void Assembler(Ast::Declaration *proc);
    void ProcDecl(Ast::Declaration*);
    void SysFlag();
    bool Type_(Ast::Type *t);
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
    bool ConstExpr(Ast::Expression *e, QTextStream& out);
    bool Expr(Ast::Expression *e, QTextStream& out, bool isConst = false);

protected:
    bool relation(Ast::Expression *e, QTextStream& out, bool isConst);
    bool inOp(Ast::Expression *e, QTextStream &out, bool isConst);
    bool isOp(Ast::Expression *e, QTextStream &out);
    bool unaryOp(Ast::Expression *e, QTextStream& out, bool isConst);
    bool arithOp(Ast::Expression *e, QTextStream& out, bool isConst);
    bool logicOp(Ast::Expression *e, QTextStream& out);
    bool declRef(Ast::Expression *e, QTextStream& out, bool isConst);
    bool select(Ast::Expression *e, QTextStream& out);
    bool index(Ast::Expression *e, QTextStream& out);
    bool depointer(Ast::Expression *e, QTextStream& out);
    bool cast(Ast::Expression *e, QTextStream& out, bool isConst);
    bool call(Ast::Expression *e, QTextStream& out, bool isConst = false);
    bool literal(Ast::Expression *e, QTextStream& out);
    bool constructor(Ast::Expression *e, QTextStream& out, bool isConst);
    void assig(Ast::Statement* s);
    void call(Ast::Statement* s, bool isConst = false);
    void metaDecl(Ast::Declaration *d);
    bool builtin(int bi, Ast::Expression * args, QTextStream &out, bool isConst);


protected:
    bool invalid(const char* what, const RowCol&);
    void typeDecl(Ast::Declaration* type);
    Ast::Type* deref(Ast::Type*);
    QByteArray typeRef(Ast::Type*);
    void printHelper(Ast::Declaration*);
    inline QByteArray ws()
    {
        return QByteArray(curLevel*4,' ');
    }
    inline QByteArray ws(int level)
    {
        return QByteArray(level*4,' ');
    }
    void parameter(QTextStream& out, Ast::Declaration* param);
    void parameter(QTextStream& out, Ast::Type* orig, const QByteArray& name, const RowCol &pos);
    void liftedParam(QTextStream& out, Ast::Declaration* what, const QByteArray &name);
    void variable(QTextStream& out, Ast::Declaration* var, bool body);
    void procHeader(Ast::Declaration* proc, bool header);
    QByteArray escape(const QByteArray&);
    QByteArray qualident(Ast::Declaration* d);
    QByteArray qualident(Ast::Type* t);
    typedef QPair<Ast::ExpList,Ast::Type*> ArrayType;
    ArrayType arrayType(Ast::Type*);
    void emitInitializer(Ast::Type*);
    void emitVariableInit(Ast::Declaration*, int level);
    bool checkOpenArray(Ast::Type* t, const RowCol &pos);
    void renderArrayPtr(int ak, Ast::Expression* e, QTextStream &out );
    void renderArrayLen(int ak, Ast::Expression* e, QTextStream &out );

private:
    Ast::Declaration* curMod;
    QTextStream hout;
    QTextStream bout;
    Ast::Declaration* curProc;
    QSet<Ast::Declaration*> done;
    QList<Ast::Statement*> loopStack;
    QSet<const char*> keywords;
    int curLevel, localId;
    Ast::ClosureLifter cl;
    const Ast::ClosureLifter::ProcPlan* curPlan;
};

}

#endif // AOCEEGEN_H
