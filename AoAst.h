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
            // system functions
            SYSTEM_ADR, SYSTEM_BIT, SYSTEM_CC, SYSTEM_LSH, SYSTEM_ROT, SYSTEM_VAL,
            // system procs
            SYSTEM_GET, SYSTEM_PUT, SYSTEM_MOVE, SYSTEM_NEW,
            SYSTEM_PORTOUT, SYSTEM_PORTIN,
            SYSTEM_CLI, SYSTEM_STI,
            SYSTEM_GET8, SYSTEM_GET16, SYSTEM_GET32, SYSTEM_GET64,
            SYSTEM_PUT8, SYSTEM_PUT16, SYSTEM_PUT32, SYSTEM_PUT64,
            Max
        };
        static const char* name[];
    };

    class Node
    {
    public:
        enum Meta { T, D, E, S }; // Type, Declaration, Expression, Statement
        uint meta : 2;
        uint kind : 5;

        uint validated : 1;
        uint inList : 1; // private

        // Type:
        uint deferred : 1;
        uint delegate : 1;
        uint anonymous : 1;
        uint allocated : 1;
        uint receiver : 1;

        // Declaration:
        uint varParam : 1; // var param
        uint constructor : 1;
        uint begin : 1;
        uint ownstype : 1;
        uint hasErrors : 1;
        uint hasSubs : 1; // class/method: has overrides; module: has clients

        // Expression:
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index
        uint needsLval : 1;

        // Statement:
        uint active : 1;
        uint exclusive : 1;

        RowCol pos;

        Node():meta(0),kind(0),validated(0),deferred(0),delegate(0),anonymous(0),allocated(0),receiver(0),
            varParam(0),constructor(0),begin(0),ownstype(0),inList(0),hasErrors(0),hasSubs(0),
            byVal(0),needsLval(0),active(0),exclusive(0){}
    };

    class Expression;
    class Statement;

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
            SHORTINT,   // 1 byte
            BYTE,       // 1 byte, SYSTEM.BYTE
            INTEGER,    // 2 bytes
            LONGINT,    // 4 bytes
            REAL,       // 4 bytes
            LONGREAL,   // 8 bytes
            SET,        // 4 bytes
            MaxBasicType,
            Pointer,
            Procedure,
            Array,
            Record,
            Object,
            NameRef
        };
        quint32 len; // array length
        Type* base; // array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl; // if NameRef includes pos and name
        Expression* expr; // array len
        Statement* body;  // active object

        bool isNumber() const { return kind == INTEGER || kind == REAL || kind == BYTE; }
        bool isReal() const { return kind == REAL; }
        bool isInteger() const { return kind == INTEGER || kind == BYTE;  }
        bool isSet() const { return kind == SET; }
        bool isBoolean() const { return kind == BOOLEAN; }
        bool isStructured() const { return kind == Array || kind == Record; }
        QPair<int,int> countAllocRecordMembers(bool recursive = false);
        static bool isSubtype(Type* super, Type* sub);

        bool isDerefCharArray() const;
        bool isDerefByteArray() const;
        Type* deref() const;

        Declaration* find(const QByteArray& name, bool recursive = true) const;
        QList<Declaration*> fieldList() const;
        QList<Declaration*> methodList(bool recursive = false) const;

        static QVariant getMax(quint8 form);
        static QVariant getMin(quint8 form);
        static const char* name[];

        Type():base(0),expr(0),body(0),len(0),decl(0){meta = T;}
        ~Type();
    };

    class Declaration : public Node
    {
    public:
        enum Kind { Invalid, Helper, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field,
                    VarDecl, LocalDecl,
                    Procedure, ParamDecl,
                    Max };
        enum { IdWidth = 16, NoSlot = (1 << IdWidth) - 1 };
        Declaration* link; // member list or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Declaration* super; // super class or overridden method
        Statement* body; // procs, owned
        Type* type;
        QByteArray name;
        enum Visi { NA, Private, ReadOnly, ReadWrite };
        uint visi : 2;
        uint id : IdWidth; // used for built-in code and local/param number
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        Expression* expr; // const decl, enum, meta actuals

        Declaration():next(0),link(0),outer(0),super(0),type(0),body(0),id(NoSlot),expr(0),visi(0) { meta = D; }

        QList<Declaration*> getParams(bool skipReceiver = false) const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return kind == VarDecl || kind == LocalDecl || kind == ParamDecl; }
        bool isPublic() const { return visi >= ReadOnly; }
        Declaration* getNext() const { return next; }
        Declaration* getLast() const;
        Declaration* find(const QByteArray& name, bool recursive = true);
        Declaration* getModule();
        void appendMember(Declaration*);
        RowCol getEndPos() const;
        QByteArray scopedName(bool withModule = false, bool withPath = false) const;
        QByteArray getModuleFullName(bool dots = false) const;
        static void deleteAll(Declaration*);

    private:
        ~Declaration();
        Declaration* next; // list of all declarations in outer scope
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
        Type* type;
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
        static Expression* createFromToken(quint16,const RowCol&);

        Expression(Kind k = Invalid, const RowCol& rc = RowCol()):
            type(0),lhs(0),rhs(0),next(0) {meta = E; kind = k; pos = rc; }
        ~Expression();
    };

    typedef QList<Expression*> ExpList;

    struct Import {
        QByteArray path;    // full path incl. name
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
            Assig, Call, If, Elsif, Else, Case, TypeCase, CaseLabel, With,
            Loop, While, Repeat, Exit, Return, ForAssig, ForToBy, End, Assembler
        };
        Expression* lhs; // owns: proc, assig lhs
        Expression* rhs; // owns: rhs, args, cond, case, label, return
        Statement* body; // owns: then

        Statement(Kind k = Invalid, const RowCol& p = RowCol()):lhs(0),rhs(0),
            next(0),body(0) { meta = S; kind = k; pos = p;}
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
        QByteArrayList path;
        QString source;
        QByteArray suffix;
        QByteArray fullName; // path.join('/') + suffix as symbol
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


    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&);
        Declaration* addHelper();
        Declaration* findDecl(const QByteArray&, bool recursive = true) const;
        Declaration* findDecl(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const;
        QByteArray getTempName();
        Declaration* getTopModule() const;

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
        static DeclList toList(Declaration* d);
    protected:
        Type* newType(int form, int size);
        Type* addType(const QByteArray& name, int form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Kind);
    private:
        QList<Declaration*> scopes;
        Declaration* helper;
        quint32 helperId;
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
