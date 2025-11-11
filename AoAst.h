#ifndef AOAST_H
#define AOAST_H

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

// adopted from Luon

#include <QByteArray>
#include <ActiveOberon/AoRowCol.h>
#include <QVariant>

namespace Ao
{
namespace Ast
{
    class Declaration;

    struct Builtin
    {
        enum Kind {
            // built-in functions
            ABS, ODD, CAP, ASH, LEN, MAX, MIN, SIZE, ORD, CHR, SHORT, LONG, ENTIER,
            // build-in procs
            INC, DEC, INCL, EXCL, COPY, NEW, HALT, AWAIT,
            ASSERT,

            SYSTEM,
            // system functions
            SYSTEM_ADR, SYSTEM_BIT, SYSTEM_CC, SYSTEM_LSH, SYSTEM_ROT, SYSTEM_VAL, SYSTEM_TYPECODE,
            // system procs
            SYSTEM_GET, SYSTEM_PUT, SYSTEM_MOVE, SYSTEM_NEW,
            SYSTEM_PORTOUT, SYSTEM_PORTIN,
            SYSTEM_CLI, SYSTEM_STI,
            SYSTEM_GET8, SYSTEM_GET16, SYSTEM_GET32, SYSTEM_GET64,
            SYSTEM_PUT8, SYSTEM_PUT16, SYSTEM_PUT32, SYSTEM_PUT64,
            SYSTEM_GETREG, SYSTEM_PUTREG,
            SYSTEM_ENABLEINTERRUPTS, SYSTEM_DISABLEINTERRUPTS, SYSTEM_RESTOREINTERRUPTS, // wegen Shark Oberon
            Max
        };
        static const char* name[];
    };

    class Type;

    class Node
    {
    public:
        enum Meta { T, D, E }; // Type, Declaration, Expression
        uint meta : 2;
#ifndef _DEBUG
        uint kind : 5;
#endif

        uint validated : 1;
        uint inList : 1; // private
        uint forward : 1; // Type and Expression NameRefs
        uint nonlocal : 1; // namerefs, proc decls

        // Type:
        uint deferred : 1;
        uint delegate : 1;
        uint allocated : 1;
        uint owned : 1;
        uint anonymous : 1;

        // Declaration:
        uint varParam : 1; // var param
        uint constructor : 1;
        uint begin : 1;
        uint ownstype : 1;
        uint hasErrors : 1;
        uint hasSubs : 1; // class/method: has overrides; module: has clients
        uint receiver : 1;

        // Expression:
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index
        uint needsLval : 1;

        // 27 bits

        RowCol pos;

        Type* type() const { return _ty; }
        void setType(Type*);
        Type* overrideType(Type*);

        Node():meta(0),
    #ifndef _DEBUG
            kind(0),
    #endif
            validated(0),deferred(0),delegate(0),allocated(0),receiver(0),
            varParam(0),constructor(0),begin(0),ownstype(0),inList(0),hasErrors(0),hasSubs(0),
            byVal(0),needsLval(0),nonlocal(0),_ty(0),owned(0),anonymous(0), forward(0){}
        ~Node();
    private:
        Type* _ty;
    };

    class Expression;
    class Statement;

    typedef QPair<QByteArray,QByteArray> Quali;

    class Type : public Node
    {
    public:
        enum Kind {
            Undefined,
            NoType,
            StrLit,
            NIL,
            BOOLEAN,
            CHAR,       // 1 byte
            BYTE,       // 1 byte, SYSTEM.BYTE
            SHORTINT,   // 1 byte
            INTEGER,    // 2 bytes
            LONGINT,    // 4 bytes
            HUGEINT,    // 8 bytes
            REAL,       // 4 bytes
            LONGREAL,   // 8 bytes
            SET,        // 4 bytes
            PTR,
            ANY,
            ANYOBJ,
            MaxBasicType,
            Pointer,
            Procedure,
            Array,
            Record,
            Object,
            NameRef
        };
        static const char* name[];

#ifdef _DEBUG
        Kind kind;
#endif

        union {
            quint32 len; // array length
            Quali* quali; // NameRef
        };

        // see Node Type* type; // array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl; // if NameRef includes pos and name
        Expression* expr; // array len

        bool isNumber() const { return kind >= SHORTINT && kind <= LONGREAL; }
        bool isNumberOrByte() const { return isNumber() || kind == BYTE; }
        bool isReal() const { return kind == REAL || kind == LONGREAL; }
        bool isInteger() const { return kind >= SHORTINT && kind <= HUGEINT;  }
        bool isIntegerOrByte() const { return isInteger() || kind == BYTE;  }
        bool isSet() const { return kind == SET; }
        bool isBoolean() const { return kind == BOOLEAN; }
        bool isStructured() const { return kind == Array || kind == Record; }
        QPair<int,int> countAllocRecordMembers(bool recursive = false);
        static bool isSubtype(Type* super, Type* sub);
        bool isSO() const { return kind == Record || kind == Object; }
        bool isSOA() const { return isSO() || (kind == Array && len); }
        bool isPtrToOpenArray() const;

        bool isDerefCharArray() const;
        bool isDerefByteArray() const;
        Type* deref() const;

        Declaration* find(const QByteArray& name, bool recursive = true) const;
        QList<Declaration*> fieldList() const;
        QList<Declaration*> methodList(bool recursive = false) const;

        static QVariant getMax(quint8 form);
        static QVariant getMin(quint8 form);

        Type():expr(0),quali(0),decl(0){meta = T; kind = Undefined;}
        ~Type();
    };

    class Declaration : public Node
    {
    public:
        enum Kind { Invalid, Helper, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field,
                    VarDecl, LocalDecl,
                    Procedure, ParamDecl,
                    Max };
#ifdef _DEBUG
        Kind kind;
#endif
        enum { IdWidth = 16, NoSlot = (1 << IdWidth) - 1 };
        Declaration* link; // member list or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Declaration* super; // super class or overridden method
        Declaration* next; // list of all declarations in outer scope
        Statement* body; // procs, owned
        QByteArray name;
        enum Visi { NA, Private, ReadOnly, ReadWrite };
        uint visi : 2;
        uint id : IdWidth; // used for built-in code and local/param number
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        union {
        Expression* expr; // const decl
        Declaration* helper; // type, var, param, local and field decls
        };

        Declaration():next(0),link(0),outer(0),super(0),body(0),id(NoSlot),expr(0),visi(0) { meta = D; kind = Invalid; }

        QList<Declaration*> getParams(bool skipReceiver = false) const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return kind == VarDecl || kind == LocalDecl || kind == ParamDecl || kind == Field; }
        bool isPublic() const { return visi >= ReadOnly; }
        Declaration* getNext() const { return next; }
        Declaration* getLast() const;
        Declaration* find(const QByteArray& name, bool recursive = true);
        Declaration* getModule() const;
        void appendMember(Declaration*);
        RowCol getEndPos() const;
        QString getSourcePath() const;
        QByteArray scopedName(bool withModule = false) const;
        QByteArray getModuleFullName(bool dots = false) const;
        static void deleteAll(Declaration*);

    private:
        ~Declaration();
        friend class AstModel;
    };
    typedef QList<Declaration*> DeclList;

    class Expression : public Node
    {
    public:
        enum Kind {
            Invalid,
            Plus, Minus, Not, // Unary
            Eq, Neq, Lt, Leq, Gt, Geq, In, Is, // Relation
            Add, Sub, Or, // AddOp
            Mul, Fdiv, Div, Mod, And, // MulOp
            DeclRef, // val is declaration
            Deref,
            Select, // f.g, val is field declaration
            Index, // a[i]
            Cast,
            Call,
            Literal,
            Constructor, Range,
            NameRef, // temporary, will be resolved by validator to DeclRef and ConstVal
            ConstVal,
            Super,   // ^ supercall
            MAX
        };
#ifdef _DEBUG
        Kind kind;
#endif

        QVariant val; // set elements and call args are ExpList embedded in val
        Expression* lhs; // for unary and binary ops
        Expression* rhs; // for binary ops
        Expression* next; // for args, set elems, and caselabellist

        bool isConst() const;
        bool isLvalue() const; // true if result of expression is usually a ref to type; can be changed with byVal
        void setByVal();
        bool isCharLiteral();
        qint64 getCaseValue(bool* ok = 0) const;
        void appendRhs(Expression*);
        static int getCount(const Expression* list);
        static void append(Expression* list, Expression* elem);
        static QList<Expression*> getList(Expression* exp);
        static Expression* createFromToken(quint16, const RowCol& rc);

        Expression(Kind k = Invalid, const RowCol& rc = RowCol()):
            lhs(0),rhs(0),next(0) {meta = E; kind = k; pos = rc; }
        ~Expression();
    };

    typedef QList<Expression*> ExpList;

    struct Import {
        QByteArray moduleName;
        Declaration* importer;
        RowCol importedAt;
        Declaration* resolved; // module
        Import():resolved(0),importer(0){}
        bool equals( const Import& other) const;
    };

    class Statement : public Node
    {
    public:
        enum Kind { Invalid,
                    StatBlock, // head of statseq, and also possible member of statseq
            Assig, Call, If, Elsif, Else, Case, CaseLabel, With,
            Loop, While, Repeat, Exit, Return, ForAssig, ForToBy, End, Assembler
        };
#ifdef _DEBUG
        Kind kind;
#endif
        bool active;
        bool exclusive;
        RowCol pos;
        Expression* lhs; // owns: proc, assig lhs
        Expression* rhs; // owns: rhs, args, cond, case, label, return
        Statement* body; // owns: then

        Statement(Kind k = Invalid, const RowCol& p = RowCol()):active(0),exclusive(0),pos(p),lhs(0),rhs(0),
            next(0),body(0) { kind = k; }
        Statement* getLast() const;
        Statement* getNext() const { return next; }
        void append(Statement*s);
        static void deleteAll(Statement*);
    private:
        ~Statement();

        Statement* next; // owns: list of statements
    };

    typedef QList<Declaration*> MetaParamList;

    struct ModuleData {
        QString sourcePath;
        QByteArray fullName;
        RowCol end;
    };

    typedef QPair<QByteArray,QByteArray> Qualident;

    class Symbol
    {
    public:
        enum Kind { Invalid, Module, Decl, DeclRef, Lval };
        quint8 kind;
        quint8 len;
        RowCol pos;
        Declaration* decl;
        Symbol* next;
        Symbol():kind(Invalid),len(0),decl(0),next(0){}
        static void deleteAll(Symbol*);
    };

    typedef QList<Symbol*> SymList;

    struct Xref {
        Symbol* syms;
        QHash<Declaration*,SymList> uses;
        QHash<Declaration*,DeclList> subs;
        Xref():syms(0){}
    };

    class Importer {
    public:
        virtual Declaration* loadModule( const Ast::Import& imp ) = 0;
    };

    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&);
        Declaration* findDecl(const QByteArray&, bool recursive = true) const;
        Declaration* findDeclInImport(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const;
        Declaration* getTopModule() const;

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
        static DeclList toList(Declaration* d);
        static Declaration* getSystem();
        static Declaration* getGlobalScope() { return globalScope; }
    protected:
        Type* newType(Type::Kind form, int size);
        Type* addType(const QByteArray& name, Type::Kind form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Kind);
        void addConst(const QByteArray& name, quint8, const QVariant& = QVariant());
    private:
        QList<Declaration*> scopes;
        static Declaration* globalScope;
        static Type* types[Type::MaxBasicType];

    };
}
}

Q_DECLARE_METATYPE(Ao::Ast::Import)
Q_DECLARE_METATYPE(Ao::Ast::Declaration*)
Q_DECLARE_METATYPE(Ao::Ast::Expression*)
Q_DECLARE_METATYPE(Ao::Ast::Symbol*)
Q_DECLARE_METATYPE(Ao::Ast::ModuleData)


#endif // AOAST_H
