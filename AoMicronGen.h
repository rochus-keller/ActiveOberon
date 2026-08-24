#ifndef AOMICRONGEN_H
#define AOMICRONGEN_H

/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
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

// Generates Micron source code out of the Oberon 90 subset of the ActiveOberon AST

#include <ActiveOberon/AoAst.h>
#include <ActiveOberon/AoComments.h>
#include <ActiveOberon/AoClosureLifter.h>
#include <QTextStream>
#include <QSet>
#include <QHash>
#include <QMap>

class QIODevice;

namespace Ao
{
    class MicronModel
    {
    public:
        // for the whole program analysis

        MicronModel();

        void analyze(const QList<Ast::Declaration*>& modules);

        bool isObject(Ast::Type* record) const;
        QByteArray pointerName(Ast::Type* record) const;
        bool hasSynthesizedPointer(Ast::Declaration* typeDecl) const;
        QByteArray recordName(Ast::Type* record) const;
        Ast::Declaration* recordOwner(Ast::Type* record) const;
        bool needsLen(Ast::Declaration* param) const;
        bool needsForward(Ast::Declaration* proc) const;
        bool needsAnyPtr(Ast::Declaration* module) const;
        bool needsSys(Ast::Declaration* module) const;
        bool needsBytePtr(Ast::Declaration* module) const;
        bool needsBytesPtr(Ast::Declaration* module) const;
        QList<Ast::Type*> array2dElems(Ast::Declaration* module) const;
        QList<int> castPtrKinds(Ast::Declaration* module) const;
        QList<int> arrPtrKinds(Ast::Declaration* module) const;
        static Ast::Expression* varValRef(Ast::Declaration* formal, Ast::Expression* actual);
        static bool isByteRef(Ast::Declaration* formal, Ast::Expression* actual);
        static bool isBytesRef(Ast::Declaration* formal, Ast::Expression* actual);
        static bool isAnyRef(Ast::Declaration* formal, Ast::Expression* actual);
        bool isAddress(Ast::Declaration*) const;
        bool isAddrExpr(Ast::Expression*) const;
        static Ast::Expression* ptrMove(Ast::Expression*);
        Ast::Expression* moveChain(Ast::Expression*, QList<Ast::Expression*>* ops, QList<Ast::Expression*>* offs) const;
        bool hasAddrBase(Ast::Expression*) const;
        static Ast::Declaration* designated(Ast::Expression*);

        static QByteArray lenName(Ast::Declaration* param);
    private:
        void collectObjects(Ast::Declaration* module);
        void collectPointers(Ast::Declaration* module);
        void collectRecords(Ast::Declaration* module);
        void collectLen(Ast::Declaration* scope);
        void markObject(Ast::Type* record);
        void visitStatForObjects(Ast::Statement*);
        void visitExprForObjects(Ast::Expression*);
        void visitStatForLen(Ast::Statement*);
        void visitExprForLen(Ast::Expression*);
        void collectRefs(Ast::Declaration* scope);
        void visitStatForRefs(Ast::Statement*, Ast::Declaration* scope);
        void visitExprForRefs(Ast::Expression*, Ast::Declaration* scope);
        void requireLen(Ast::Expression*);
        void unifyLen(const Ast::DeclList& a, const Ast::DeclList& b);
        void procValue(Ast::Expression* actual, Ast::Type* formalType);
        void markAnyPtr(Ast::Type*, int level = 0, bool nested = false);
        void markBytePtr(Ast::Expression* call);
        void collectAddr(Ast::Declaration* scope);
        void visitStatForAddr(Ast::Statement*, Ast::Declaration* scope);
        void visitExprForAddr(Ast::Expression*);
        void banAddr(Ast::Expression*);
        static bool isAddrDiff(Ast::Expression*);
        bool isAddrSub(Ast::Expression*) const;
        void demoteAddr(Ast::Expression*);
        void unifyAddr(const Ast::DeclList&, const Ast::DeclList&);
        void procAddr(Ast::Expression* actual, Ast::Type* formalType);
        void markAddr(Ast::Expression*, bool strong = false);
        void markAddrDecl(Ast::Declaration*, bool strong = false);
        bool isStrongAddr(Ast::Expression*) const;

        QSet<Ast::Type*> objects;
        QSet<Ast::Type*> castTargets; // types SYSTEM.VAL casts to a pointer of
        QHash<Ast::Type*,QByteArray> ptrName; // record type -> pointer type name
        QHash<Ast::Type*,Ast::Declaration*> synthesized; // records which need a pointer decl
        QHash<Ast::Type*,QByteArray> recName; // anonymous record type -> synthesized name
        QHash<Ast::Type*,Ast::Declaration*> recOwner; // anonymous record type -> pointer decl
        QSet<Ast::Declaration*> lenParams; // open array params which need a length param
        QSet<Ast::Declaration*> extRefs; // procs referenced from another procedure
        QSet<Ast::Declaration*> sys; // modules which need the SYS module
        QSet<Ast::Declaration*> anyPtr; // modules which need a pointer to any type
        QSet<Ast::Declaration*> bytePtr; // modules which need a pointer to byte type
        QSet<Ast::Declaration*> bytesPtr; // modules which need a pointer to array of byte type
        QHash<Ast::Declaration*,QSet<int> > castPtrs; // modules which need a helper pointer type per basic type
        QHash<Ast::Declaration*,QSet<int> > arrPtrs;
        QHash<Ast::Declaration*,QSet<Ast::Type*> > array2d; // modules which need a descriptor type per element type of a 2d open array
        QSet<Ast::Declaration*> addrDecls; // integer declarations holding an address
        QSet<Ast::Declaration*> strongAddr; // addresses which are dereferenced somewhere
        QSet<Ast::Declaration*> noAddr; // integer declarations used in arithmetic
        Ast::Declaration* curMod; // the module being analyzed
        bool banPass;
        bool varPass; // resolve VAR parameters which disagree on being an address
        QSet<Ast::Expression*> noBanExprs; // this arithmetic is rendered with INC or DEC
        bool noBan; // the arithmetic is rendered as an integer
        bool dirty; // fixpoint iteration
    };

    class MicronGen
    {
    public:
        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& msg, const RowCol& pos, const QString& path ):msg(msg),pos(pos),path(path){}
        };
        QList<Error> errors;
        QStringList manual; // list of places which need manual post processing
        QStringList srcLines; // the Oberon source of the module being generated

        MicronGen(MicronModel* = 0);

        void setModel(MicronModel* m) { mdl = m; }
        void setLevel(int l) { level = l; }
        void setObDiv(bool on) { obDiv = on; }

        bool generate(Ast::Declaration* module, QIODevice* out, Ast::CommentTable* comments = 0);

        static QString genDedication();
        static QByteArray escape(const QByteArray& name);
        static bool isKeyword(const QByteArray& name);
        static Ast::Type* deref(Ast::Type*);
        static bool isStructuredVal(Ast::Expression*, Ast::Type*& to, Ast::Expression*& arg);
        static qint64 constVal(Ast::Expression*, bool* ok = 0);
    protected:
        void Module(Ast::Declaration* module);
        Ast::Declaration* ImportList(Ast::Declaration*);
        Ast::Declaration* DeclSeq(Ast::Declaration*);
        void ConstSection(const Ast::DeclList&);
        void TypeSection(const Ast::DeclList&);
        void VarSection(const Ast::DeclList&);
        bool typesFirst(Ast::Declaration*);
        Ast::DeclList forwardDecls(const Ast::DeclList& procs);
        Ast::DeclList sortTypes(const Ast::DeclList& section);
        void collectConstFeeds(Ast::Declaration*);

        QList<QPair<Ast::Type*,Ast::Declaration*> > hoisted; // anonymous record types moved out of a pointer to record declaration
        Ast::DeclList boundProcs; // type bound procedures declared in an Ao object

        void typeDeps(Ast::Type*, const Ast::DeclList& section, Ast::DeclList& deps, Ast::Declaration* owner);
        void ConstDecl(Ast::Declaration*);
        void TypeDecl(Ast::Declaration*);
        void VarDecl(const Ast::DeclList& group);
        void ProcDecl(Ast::Declaration*);
        void ProcHeader(Ast::Declaration*, bool forward = false);
        void Assembler(Ast::Declaration*);
        void RecordBody(Ast::Type*);

        void StatSeq(Ast::Statement*);
        Ast::Statement* Statement(Ast::Statement*);
        void Assig(Ast::Statement*);
        void recordDesig(Ast::Expression*);
        Ast::Statement* IfStat(Ast::Statement*);
        QPair<Ast::Declaration*,QByteArray> typeTest(Ast::Expression*);
        typedef QPair<Ast::Statement*,QByteArray> Branch;
        bool typeCaseChain(Ast::Statement*, Ast::Declaration*&, QList<Branch>&);
        Ast::Statement* TypeCaseStat(Ast::Statement*, Ast::Declaration*, const QList<Branch>&);
        Ast::Statement* CaseStat(Ast::Statement*);
        Ast::Statement* WithStat(Ast::Statement*);
        Ast::Statement* ForStat(Ast::Statement*);
        void WhileStat(Ast::Statement*);
        void RepeatStat(Ast::Statement*);
        void LoopStat(Ast::Statement*);
        void ReturnStat(Ast::Statement*);
        void CallStat(Ast::Statement*);

        enum Prec { NoPrec, Relation, AddOp, MulOp, Unary, Atom };
        static int precOf(Ast::Expression*);
        void Expr(Ast::Expression*, int prec = NoPrec);
        void condition(Ast::Expression*);
        void designator(Ast::Expression*, bool wantValue = true);
        QByteArray toStr(Ast::Expression*, bool desig = false, bool addr = false, Ast::Type* to = 0);
        void literal(Ast::Expression*);
        void constructor(Ast::Expression*);
        void call(Ast::Expression*);
        void arguments(Ast::Expression* call, const Ast::DeclList& formals, Ast::Declaration* proc);
        QByteArray varRefCast(Ast::Expression*, Ast::Declaration*, Ast::Type*);
    void actual(Ast::Expression* arg, Ast::Declaration* formal);
        bool builtin(int, Ast::Expression* args, Ast::Expression* call);
        static bool isShift(Ast::Expression*);
        void shift(Ast::Expression* x, Ast::Expression* n, bool arithmetic, Ast::Expression*);
        static bool interval(Ast::Expression*, qint64& lo, qint64& hi);
        QByteArray negStr(Ast::Expression*);
        void rotate(Ast::Expression* x, Ast::Expression* n, Ast::Expression*);
        void addressOf(Ast::Expression*);
        Ast::Type* narrowed(Ast::Expression* arg, Ast::Type* ft);
        Ast::Type* fieldOwner(Ast::Declaration* field);
        void caseLabel(Ast::Expression*, bool charCase, bool numLabels, Ast::Type* labelType);
        static QByteArray hexChar(quint32 ch);
        void lenOf(Ast::Expression*);

        QByteArray typeRef(Ast::Type*);
        QByteArray recordRef(Ast::Type* record);
        static QByteArray uniqueName(Ast::Declaration* module, const QByteArray& name);
        QByteArray typeExpr(Ast::Type*, bool declaring = false);
        QByteArray formalType(Ast::Declaration* param);
        QByteArray addressType(Ast::Type*);
        QByteArray arr2dDecl(Ast::Type* elem);
        void arr2dSection();
        QByteArray openArrOf(Ast::Type* elem);
        void anonPtrSection(Ast::Declaration*);
        void collectAnonPtrs(Ast::Declaration* scope, QSet<QByteArray>& used);
        bool depsEmitted(Ast::Type* t, QSet<Ast::Type*>& seen);

        QByteArray toMicron(Ast::Expression*);
        Ast::Expression* arr2dOf(Ast::Expression*) const;
        bool isArr2d(Ast::Type*) const;
        void addrExpr(Ast::Expression*, Ast::Type* to = 0);
        bool elemAddr(Ast::Expression*);
        bool memAccess(Ast::Expression* adr, Ast::Expression* val, bool get);
        bool memMove(Ast::Expression* src, Ast::Expression* dst, Ast::Expression* n);
        void moveAdr(Ast::Expression*);
        QByteArray memIndex(const QList<Ast::Expression*>& ops, const QList<Ast::Expression*>& offs, int size);
        QByteArray numStr(Ast::Expression*);
        QByteArray byteOffset(const QList<Ast::Expression*>& ops, const QList<Ast::Expression*>& offs);
        QByteArray constAddr(Ast::Expression*, qint64 c);
        QByteArray addrTypeName();
        bool numToPtr(Ast::Expression*, const QByteArray& toName);
        bool ptrBitOp(Ast::Statement*);
        bool ptrMove(Ast::Statement*);
        Ast::Expression* addrOperand(Ast::Expression*);
        bool ptrOffAssign(const QByteArray&, Ast::Expression*, Ast::Type*);
        bool ptrAssign(const QByteArray& tgt, Ast::Expression*, Ast::Type* to);
        bool setAssig(Ast::Statement*);
        bool isSetOf(Ast::Expression*);
        bool isPtrOrAddr(Ast::Expression*);
        bool isPtrForm(Ast::Expression*);
        bool ptrOffsetForm(Ast::Expression*, Ast::Expression** base = 0, Ast::Expression** off = 0);
        bool isNil(Ast::Expression*);
        bool isZeroLit(Ast::Expression*);
        bool nonNegative(Ast::Expression*);
        bool needsObDiv(Ast::Expression*);
        static bool isRelation(int op);
        bool isChrCall(Ast::Expression*);
        bool isCharExpr(Ast::Expression*);
        void charAsNum(Ast::Expression*);
        static QByteArray hexNum(quint64);
        void ordExpr(Ast::Expression*, bool signedRes = true);
        QByteArray declType(Ast::Declaration*);
        QByteArray qualident(Ast::Declaration*);
        QByteArray moduleRef(Ast::Declaration* module);
        QByteArray basicType(Ast::Type*);
        QByteArray zeroValue(Ast::Type*);
        QByteArray anyPtr; // name of helper type
        QByteArray bytePtr;
        QByteArray bytesPtr;
        QMap<int,QByteArray> castPtr;
        QMap<int,QByteArray> arrPtr;
        QHash<Ast::Type*,QByteArray> arr2dName;
        bool no2d; // suppress the 2d descriptor mapping
        QSet<Ast::Type*> arr2dPending; // descriptors whose element type is not yet declared
        QHash<Ast::Type*,QByteArray> openArrName;
        QSet<Ast::Type*> openArrPending;
        QHash<Ast::Type*,QByteArray> anonPtrName;
        QList<Ast::Type*> anonPtrPending;
        QHash<Ast::Type*,Ast::Declaration*> anonPtrScope; // the scope a hoisted pointer type belongs to
        QSet<Ast::Declaration*> emittedTypes;
        QHash<Ast::Declaration*,Ast::Type*> fieldOwners; // field -> the record which declares it
        bool wantAddr; // the expression being rendered is an address
        Ast::Type* wantType; // the pointer type the address is converted to
        QByteArray pointerTo(Ast::Type* record);
        static QByteArray charLit(quint32 ch);
        QByteArray suffix(Ast::Type*, qint64 val);
        static Ast::Type* commonType(Ast::Type* lhs, Ast::Type* rhs);
        void coerce(Ast::Expression*, Ast::Type* to, int prec = NoPrec);
        QByteArray limit(Ast::Type*, bool max);

        void leading(const RowCol&);
        void trailing(quint32 row);
        void flushUntil(quint32 row);
        void emitComment(Ast::Comment*, bool ownLine);
        void blankLine(const RowCol&);
        QByteArray ws(int level = -1) const;
        void todo(const QString&, const RowCol&);
        QString sourceLine(const RowCol&) const;
        void error(const QString&, const RowCol&);

        bool isVarParam(Ast::Declaration*) const;
        const Ast::ClosureLifter::LiftParam* lifted(Ast::Declaration*) const;
    private:
        MicronModel* mdl;
        MicronModel ownModel;
        Ast::Declaration* curMod;
        Ast::Declaration* curProc;
        Ast::CommentTable* cmts;
        QString buffer;
        QTextStream out;
        int curLevel;
        int level;
        bool obDiv;
        quint32 lastRow;
        Ast::ClosureLifter cl;
        const Ast::ClosureLifter::ProcPlan* curPlan;
        QList<Ast::Statement*> loopStack;
        // the guarded variables of a WITH, which are rendered with a type guard in the body
        QHash<Ast::Declaration*,QByteArray> withCast;
        // the subset of withCast which needs a reinterpretation cast instead of a type guard
        QSet<Ast::Declaration*> castOnly;
        // the name each imported module is designated with in the module being generated
        QHash<Ast::Declaration*,QByteArray> importAlias;
        QByteArray sysModule; // the local name of the SYS module, if imported
        bool addSys; // the SYS module is imported by the generated module only
        int constCtx; // > 0 while an expression is rendered whose operands are all constants
        QSet<Ast::Declaration*> constFeeds; // the constants which are used in the expression of another constant
        int noPromote; // > 0 while an operand is rendered where an unsigned byte is sufficient
        QSet<Ast::Declaration*> narrowVars;  // narrowed by an enclosing type case
        // the type tests established by the enclosing conditions
        QList<QPair<Ast::Declaration*,QByteArray> > knownTests;
        bool plainConst(Ast::Expression*, Ast::Type* to);
        struct ConstScope
        {
            MicronGen* gen;
            bool on;
            ConstScope(MicronGen* g, bool on):gen(g),on(on) { if( on ) gen->constCtx++; }
            ~ConstScope() { if( on ) gen->constCtx--; }
        };
    };
}

#endif // AOMICRONGEN_H
