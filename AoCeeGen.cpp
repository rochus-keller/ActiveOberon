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
#include "AoAsmToIntelXpiler.h"
#include "AoCeeGen.h"
#include "AoToken.h"
#include <QCoreApplication>
#include <QDateTime>
#include <QIODevice>
#include <QtDebug>
using namespace Ao;
using namespace Ast;

// result can be simply built with `gcc *.c -lm -lgc -O2`

static inline void dummy() {}

CeeGen::CeeGen()
{
    static const char* kw[] = {
        "auto", "break", "case", "char", "const", "continue", "default", "do", "double",
        "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
        "register", "restrict", "return", "short", "signed", "sizeof", "static",
        "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", 0
    };

    for( int i = 0; kw[i] != 0; i++)
        keywords.insert(Token::getSymbol(kw[i]).constData());
}

static inline QByteArray typePrefix(Type* t)
{
    // if( t == 0 )
        return "";
    switch(t->kind)
    {
    case Type::Record:
    case Type::Object:
        return "struct ";
    case Type::Array:
        if( t->expr != 0 )
            return "struct ";
        break;
    }
    return QByteArray();
}

static inline QByteArray typePrefix2(Type* t)
{
    if( t == 0 || t->kind != Type::Pointer )
        return "";

    t = t->type();
    if( t ) t = t->deref();
    switch(t->kind)
    {
    case Type::Record:
    case Type::Object:
        return "struct ";
    case Type::Array:
        if( t->expr != 0 )
            return "struct ";
        break;
    }
    return QByteArray();
}

static inline QByteArray typePrefix3(Type* t, bool isPtr)
{
    if( t == 0 || !isPtr )
        return "";
    switch(t->kind)
    {
    case Type::Record:
    case Type::Object:
        return "struct ";
    case Type::Array:
        if( t->expr != 0 )
            return "struct ";
        break;
    }
    return QByteArray();
}

static void renderIntLit(Type* t, const QVariant& val, QTextStream &out)
{
    if (t->kind == Type::HUGEINT) {
        quint64 u = val.toULongLong();
        if (u > 9223372036854775807ULL) {
            out << "((long long)" << u << "ULL)";
        } else {
            out << u << "LL";
        }
    } else if (t->kind == Type::LONGINT) {
        quint64 u = (quint32)val.toULongLong();
        if (u > 2147483647ULL) {
            out << "((int)" << u << "U)";
        } else {
            out << u;
        }
    } else {
        // SHORTINT and INTEGER fit into C's signed 'int'.
        // If they are part of a negative declaration (like MinSInt = -80H),
        // the AST provides a UnaryMinus node that correctly negates them.
        out << val.toLongLong();
    }
}


enum ArrayKind {
    Invalid,
    NoArray,
    FixedSize,        // typedef struct { $[]; }
    PointerToArray,   // MIC$DA, a pointer which points to an array (fixed or open)
    DerefedArray,     // MIC$DA, dereferenced pointer to array (fixed or open)
    ReferenceToArray, // MIC$AP, a VAR parameter of open or fixed array type
    OpenArrayValue,   // MIC$AP, a value parameter of open array type (needs a copy)
};

static inline Type* deref(Type* t)
{
    if( t )
        return t->deref(false);
    else
        return t;
}

static ArrayKind deriveArrayKind(Expression* e)
{
    if( e == 0 )
        return NoArray;
    Type* t = deref(e->type());
    if( t && t->kind == Type::Reference )
    {
        // this happens when e directly designates a parameter
        t = deref(t->type());
        if( t && t->kind == Type::Array )
            return ReferenceToArray; // can be fixed or open
        else
            return NoArray;
    }
    if( t && t->kind == Type::Pointer )
    {
        t = deref(t->type());
        if( t && t->kind == Type::Array )
            return PointerToArray;
        else
            return NoArray;
    }
    if( t == 0 || t->kind != Type::Array )
        return NoArray;
    switch(e->kind)
    {
    case Expression::DeclRef: {
            Declaration* d = e->val.value<Declaration*>();
            switch(d->kind)
            {
            case Declaration::LocalDecl:
            case Declaration::VarDecl:
                if( t->expr != 0 )
                    return FixedSize;
                else
                    return Invalid;
            case Declaration::ParamDecl:
                if( t->expr == 0 )
                    return OpenArrayValue; // value open array parameter
                else
                    return FixedSize;
            default:
                return Invalid;
            }
        } break;
    case Expression::Select:
    case Expression::Index:
        // a.b where b is a fixed size array
        // a[i] where the element is a fixed size array
        if( t->expr != 0 )
            return FixedSize;
        else
            return Invalid;
    case Expression::Deref:
        return DerefedArray; // can be open or fixed
    default:
        break;
    }
    return NoArray;
}

static inline QByteArray basicType(Type* t)
{
    switch(t->kind)
    {
    case Type::StrLit:
        return "const char*";
    case Type::NIL:
        return "NULL";
    case Type::BOOLEAN:
        return "unsigned char";
    case Type::CHAR:
        return "char";
    case Type::BYTE:
        return "unsigned char";
    case Type::SHORTINT:
        return "char";
    case Type::INTEGER:
        return "short";
    case Type::LONGINT:
        return "int";
    case Type::SET:
        return "int";
    case Type::HUGEINT:
        return "long long";
    case Type::REAL:
        return "float";
    case Type::LONGREAL:
        return "double";
    case Type::PTR:
        return "void*";
    case Type::ANY:
        return "void";
    }
    return "?";
}

QByteArray CeeGen::qualident(Declaration* d)
{
    if( d == 0 )
        return "???";
    if( d->outer && d->kind != Declaration::LocalDecl && d->kind != Declaration::ParamDecl )
        return qualident(d->outer) + "$" + escape(d->name);
    else
    {
        const bool nonlocal = d->outer != curProc && ( d->kind == Declaration::LocalDecl || d->kind == Declaration::ParamDecl );
        if(nonlocal) // TODO handle nonlocal properly
        {
            QByteArray res = "(*";
            res += escape(d->name);
            res += ")";
            return res;
        }else
        {
            return escape(d->name);
        }
    }
}

QByteArray CeeGen::qualident(Ast::Type *t)
{
    Q_ASSERT(t);
    if( t->kind == Type::Reference )
        t = t->type();
    Type* td = deref(t);
    if( td->kind < Type::MaxBasicType )
        return basicType(td);
    QByteArray res;
    if( t->kind == Type::NameRef )
    {
        // TODO: fails for namerefs to builtin types
        Quali* q = t->quali;
        if( q->first.isEmpty() )
        {
            if( t->decl )
            {
                Q_ASSERT(t->decl->kind == Declaration::Module);
                res = t->decl->name;
            }else
                res = curMod->name;
        }else
            res = q->first;
        res += "$" + q->second;
    }else
    {
        Q_ASSERT( t->decl );
        res = qualident(t->decl);
    }
    return res;
}

CeeGen::ArrayType CeeGen::arrayType(Ast::Type * t)
{
    Q_ASSERT( t && t->kind == Type::Array );
    ArrayType a;
    a.first << t->expr;
    t = deref(t->type());
    Q_ASSERT(t->kind != Type::Reference);
    while( t && t->kind == Type::Array )
    {
        a.first << t->expr;
        t = deref(t->type());
    }
    a.second = t;
    return a;
}

bool CeeGen::generate(Ast::Declaration* module, QIODevice *header, QIODevice *body, bool generateMain) {
    errors.clear();

    curMod = module;
    curLevel = 0;
    localId = 0;
    curPlan = 0;
    curProc = 0;
    hout.setDevice(header);
    QString dummy;
    if( body )
        bout.setDevice(body);
    else
        bout.setString(&dummy, QIODevice::WriteOnly);

    const QByteArray guard = "__" + module->name.toUpper() + "_INCLUDED__";
    const QString dedication = genDedication();

    hout << "#ifndef " << guard << endl;
    hout << "#define " << guard << endl << endl;
    hout << "// " << module->name << ".h" << endl;
    hout << dedication << endl << endl;

    bout << "// " << module->name << ".c" << endl;
    bout << dedication << endl << endl;

    bout << "#include \"" << module->name << ".h\"" << endl;
    bout << "#include <gc.h>" << endl;
    bout << "#include <stdlib.h>" << endl;
    bout << "#include <stdio.h>" << endl;
    bout << "#include <string.h>" << endl;
    bout << "#include <assert.h>" << endl;
    bout << "#include <math.h>" << endl;
    bout << "#include <ctype.h>" << endl;
    bout << "#include <stddef.h>" << endl << endl;

    hout << "#include <stdint.h>" << endl;

    Module(module);

    if( generateMain )
    {
        bout << "int main(int argc, char* argv[]) {" << endl;
        bout << "    setvbuf(stdout, NULL, _IONBF, 0);" << endl;
        bout << "    GC_INIT();" << endl;
        bout << "    " << module->name << "$init$();" << endl;
        bout << "    return 0;" << endl;
        bout << "}" << endl;
    }

    hout << endl;
    hout << "#endif // " << guard;

    return errors.isEmpty();
}

bool CeeGen::invalid(const char* what, const RowCol& pos) {
    errors << Error(QString("invalid %1").arg(what),pos, curMod->name);
    return false;
}

void CeeGen::typeDecl(Ast::Declaration *d)
{
    Ast::Type* t = d->type();
    if( t == 0 )
    {
        hout << "// undeclared type " << d->name << endl;
        return;
    }

#if 0
    if( t->kind == Type::NameRef )
        return; // don't create name aliasses
#endif

    if( t->kind == Type::Object )
    {
        // forward declaration for class objects
        hout << "struct " << qualident(d) << "$Class$ " << qualident(d) << "$Class$;" << endl;
    }

    if( t->kind == Type::Array && t->expr == 0 )
    {
        hout << "// no typedef for open array " << qualident(d) << " (" << typeRef(t->type()) << "*)" << endl;
        return; // we need not typedef for open arrays, instead they are referenced by element_type*
    }

    bool makeInit = false;
    if( deref(t)->kind < Type::MaxBasicType )
        return; // don't make typedefs for basic types: hout << "typedef " << typeRef(t); //  << " " << qualident(d);
    else
    {
        switch( t->kind )
        {
        case Type::Pointer: {
                Type* to = deref(t->type());
                if( to->kind == Type::Array )
                {
                    hout << "// no typedef for pointer to array " << qualident(d) << " (using MIC$DA instead)" << endl;
                    return;
                }
                hout << "typedef ";
                if( to->isSO() )
                    hout << "struct ";
                hout << qualident(t->type()) << "*";
            } break;
        case Type::Procedure:
#if 0
            if( t->typebound )
            {
                hout << "struct " << qualident(d) << " {" << endl;
                hout << ws(0) << "void* self;" << endl;
                hout << ws(0) << typeRef(t->getType()) << " (*proc)(void* self";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( t->typebound || i != 0 )
                        hout << ", ";
                    parameter(hout, params[i]);
                }
                hout << ");" << endl;
                hout << "}";
            }else
#endif
            {
                hout << "typedef ";
                hout << typeRef(t->type()) << " (*";
                hout << qualident(d);
                hout << ")(";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( i != 0 )
                        hout << ", ";
                    parameter(hout, params[i]);
                }
                hout << "); " << endl;
                return;
            }
            break;
        case Type::Array:
            if( t->expr != 0 )
            {
                // declare a fixed size array of the form struct { $[N]; }
                ArrayType at = arrayType(t);
                hout << "typedef ";
                hout << "struct " << qualident(d) << " { " << typeRef(at.second) << " $[";
                ConstExpr(t->expr, hout);
                for( int i = 1; i < at.first.size(); i++ )
                {
                    t = deref(t->type());
                    hout << " * ";
                    ConstExpr(t->expr, hout);
                }
                hout << "];" << " }";
                // for fixlen arrays whose fields or elements need object initialization, create an initializer
                if( t->objectInit )
                    makeInit = true;
            }else
                Q_ASSERT(false); // hout << typeRef(t->getType()) << "* ";
            break;

        case Type::Record:
        case Type::Object: {
                QList<Declaration*> fields = t->fieldList();
                foreach( Declaration* field, fields )
                {
                    if( field->kind == Declaration::Field )
                        printHelper(field->helper);
                }
                hout << "typedef ";
                hout << "struct " << qualident(d) << " {" << endl;
                hout << ws(1) << "struct " << qualident(d) << "$Class$* class$;" << endl;
                foreach( Declaration* field, fields )
                {
                    if( field->kind == Declaration::Field )
                        hout << ws(1) << typeRef(field->type()) << " " << escape(field->name) << ";" << endl;
                }
                hout << "}";
                makeInit = true;
            } break;
        case Type::NameRef: {
                Type* td = deref(t);
                if( td->kind < Type::MaxBasicType || (td->kind == Type::Array && td->expr == 0) )
                    return;
                hout << "typedef ";
                hout << typeRef(td);
            } break;
        }
    }
    // typedef what name
    hout << " " << qualident(d) << ";" << endl;
    if( makeInit )
        emitInitializer(t);
}

void CeeGen::emitInitializer(Type* t)
{
    t = deref(t);
    hout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n);" << endl;
    bout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n) {" << endl;
    // obj is a pointer to the object to be initialized; we can interpret this pointer as a single or an array of this object
    bout << ws(1) << "int i;" << endl;
    bout << ws(1) << "for( i = 0; i < n; i++ ) {" << endl; // this code is generalized to an array of this object of length n

    if( t->kind == Type::Object || t->kind == Type::Record )
        bout << ws(2) << "obj[i].class$ = &" << qualident(t->decl) << "$class$;" << endl; // set the vptr to its class object
    else if( t->kind == Type::Array && t->expr != 0 && t->objectInit )
    {
        Type* et = deref(t->type());
        bout << ws(2) << qualident(et->decl) << "$init$(obj, ";
        ArrayType at = arrayType(t);
        Type* tt = t;
        Expr(tt->expr, bout);
        for( int i = 1; i < at.first.size(); i++ )
        {
            tt = deref(tt->type());
            bout << " * ";
            Expr(tt->expr, bout);
        }
        bout << ");" << endl;
    }

    foreach( Declaration* field, t->subs )
    {
        if( field->kind != Declaration::Field )
            continue;
        Type* tt = deref(field->type());
        if( tt->objectInit && tt->isSO() )
        {
            bout << ws(2) << qualident(tt->decl) << "$init$(&obj[i]." << field->name << ", 1);" << endl;
        }else if( tt->objectInit && tt->kind == Type::Array && tt->expr != 0 )
        {
            Type* et = deref(tt->type());
            if( et->isSO() )
            {
                bout << ws(2) << qualident(tt->decl) << "$init$(obj[i]." << field->name << ", ";
                ArrayType at = arrayType(tt);
                Expr(tt->expr, bout);
                for( int i = 1; i < at.first.size(); i++ )
                {
                    tt = deref(tt->type());
                    bout << " * ";
                    Expr(tt->expr, bout);
                }
                bout << ");" << endl;
            }
        }
    }

    bout << ws(1) << "}" << endl;
    bout << "}" << endl << endl;
}

void CeeGen::emitVariableInit(Ast::Declaration * d, int level)
{
    Type* t = deref(d->type());
    if( t->kind == Type::Pointer && t->pointerInit )
        bout << ws(level) << qualident(d) << " = NULL;" << endl;
    else if( t->isSOA() )
    {
        if( t->pointerInit ) // it's cheaper to directly zero the whole thing
            bout << ws(level) << "memset(&" << qualident(d) << ", 0, sizeof(" << typeRef(t) << "));" << endl;
    }

    if( t->isSO() )
        bout << ws(level) << qualident(t) << "$init$(&" << qualident(d) << ", 1);" << endl;
    else if( t->kind == Type::Array && t->expr != 0 && t->objectInit )
    {
        ArrayType at = arrayType(t);
        bout << ws(level) << qualident(at.second) << "$init$(" << qualident(d) << ".$,";
        Expr(t->expr, bout);
        for( int i = 1; i < at.first.size(); i++ )
        {
            t = deref(t->type());
            bout << " * ";
            Expr(t->expr, bout);
        }
        bout << ");" << endl;
    }
}

bool CeeGen::checkOpenArray(Ast::Type *t, const RowCol& pos)
{
    t = deref(t);
    if( t == 0 || t->kind != Type::Array )
        return true;
    ArrayType a = arrayType(t);
    int openDims = 0;
    for( int i = 0; i < a.first.size(); i++ )
        if( a.first[i] == 0 ) openDims++;
    if( openDims > 1 )
        return invalid("multi-dimensional open array parameter not yet supported", pos);
    return true;
}

void CeeGen::Module(Ast::Declaration *module) {

    cl.analyze(module);

    Ast::Declaration* d = module->link;
    if( d && d->kind == Ast::Declaration::Import ) {
        d = ImportList(d);
    }

    hout << endl;

    hout << "#define ASH$(x, n) (((n) >= 0) ? ((x) << ((n) >= 0 ? (n) : 0)) : ((x) >> ((n) < 0  ? -(n) : 0)))" << endl;
    hout << "#define MOD$(x, y) (((x) % (y) < 0) ? ((x) % (y) + (y)) : ((x) % (y)))" << endl;
       hout << "#define DIV$(x, y) ((((x) % (y) != 0) && (((x) < 0) != ((y) < 0))) ? ((x) / (y) - 1) : ((x) / (y)))" << endl;
    hout << "#ifndef __MIC_DEFINE__" << endl << "#define __MIC_DEFINE__" << endl;
    hout << "static inline long long OBERON$MOD(long long x, long long y) { long long r = x % y; return r < 0 ? r + y : r; }" << endl;
    hout << "static inline long long OBERON$DIV(long long x, long long y) { long long q = x / y; long long r = x % y; " <<
            "return (r != 0 && ((x < 0) != (y < 0))) ? q - 1 : q; }" << endl;

    // static analysis revealed that in ETH Oberon System v2.3.7 only the first dim of all n-dim arrays is open
    // array kinds:
    // fixed size by value: struct { array[n]; } on fields/var/param FixedSize
    //            by pointer: MIC$DA   dynamic=1 DerefedArray/PointerToArray
    //            by reference: MIC$AP dynamic=0 ReferenceToArray
    // open       by pointer: MIC$DA dynamic=1   DerefedArray/PointerToArray
    //            by reference: MIC$AP dynamic=0 ReferenceToArray
    //            by value: MIC$AP dynamic=0     ArrayParameter

    hout << "typedef struct MIC$AP { uint32_t $1; void* $; } MIC$AP;" << endl; // n-dim open array parameter (var or val) or fixed size var parameter; $1 is **the first dimension**
    hout << "typedef struct MIC$DA { uint32_t $1; void* $[]; } MIC$DA;" << endl; // n-dim dynamic array, pointer points to $, not $1 (for compat with SYSTEM calls); $1 is **the first dimension**
    hout << "#endif" << endl;

    bout << "typedef struct $Class { struct $Class* super; } $Class;" << endl;
    bout << "static int $isinst(void* super, void* sub) {" << endl;
    bout << "    $Class* cls = ($Class*)sub;" << endl;
    bout << "    while(cls) {" << endl;
    bout << "        if( cls == super ) return 1;" << endl;
    bout << "        cls = cls->super;" << endl;
    bout << "    }" << endl;
    bout << "    return 0;" << endl;
    bout << "}" << endl;
    bout << "static MIC$DA* $toda(void** ptr) {" << endl;
    bout << "    return (MIC$DA*)((char*)ptr - offsetof(MIC$DA, $));" << endl;
    bout << "}" << endl;
    bout << "static void** $allocda(uint32_t count, int elemSize) {" << endl;
    bout << "    MIC$DA* ptr = (MIC$DA*)GC_MALLOC(sizeof(MIC$DA) + count * elemSize);" << endl;
    bout << "    if( ptr == 0 ) return 0;" << endl;
    bout << "    ptr->$1 = count;" << endl;
    bout << "    return ptr->$;" << endl;
    bout << "}" << endl << endl;

    d = DeclSeq(d, true, true);

    if( !module->extern_ )
    {
        hout << "extern void " << module->name << "$init$(void);" << endl;
        bout << "static int " << module->name << "$initialized = 0;" << endl;
        bout << "void " << module->name << "$init$(void) {" << endl;
        bout << "    if( " << module->name << "$initialized ) return;" << endl;
        bout << "    " << module->name << "$initialized = 1;" << endl;

        Ast::Declaration* d = module->link;
        while( d )
        {
            if( d->kind == Ast::Declaration::Import )
            {
                Import import = d->data.value<Import>();
                if( import.moduleName != "SYSTEM" && !import.resolved->extern_ )
                    bout << "    " << import.moduleName << "$init$();" << endl;
            }else if( d->objectInit || d->pointerInit )
                emitVariableInit(d, 1);
            d = d->next;
        }

        d = module->link;
        while(d)
        {
            if( d->kind == Declaration::Procedure && d->begin )
            {
                bout << "    " << module->name << "$BEGIN$();" << endl;
                break;
            }
            d = d->next;
        }

        bout << "}" << endl << endl;
    }
}

void CeeGen::ImportDecl(Ast::Declaration* i) {
    Import import = i->data.value<Import>();
    if( import.moduleName != "SYSTEM" )
        hout << "#include \"" << import.moduleName << ".h\"" << endl;
}

Ast::Declaration* CeeGen::ImportList(Ast::Declaration* import) {
    ImportDecl(import);
    import = import->next;
    while( import && import->kind == Ast::Declaration::Import ) {
        ImportDecl(import);
        import = import->next;
    }
    return import;
}

Ast::Declaration* CeeGen::DeclSeq(Ast::Declaration* d, bool doProcs, bool doOthers) {
    while( d && (d->kind == Ast::Declaration::ConstDecl || d->kind == Ast::Declaration::TypeDecl ||
           d->kind == Ast::Declaration::VarDecl || d->kind == Ast::Declaration::LocalDecl ||
                 d->kind == Ast::Declaration::Procedure ) ) {
        if( doOthers && d->kind == Ast::Declaration::ConstDecl ) {
            while( d && d->kind == Ast::Declaration::ConstDecl ) {
                ConstDecl(d);
                d = d->next;
            }
        } else if( doOthers && d->kind == Ast::Declaration::TypeDecl ) {
            while( d && d->kind == Ast::Declaration::TypeDecl ) {
                TypeDecl(d);
                d = d->next;
            }
        } else if( doOthers && (d->kind == Ast::Declaration::VarDecl || d->kind == Ast::Declaration::LocalDecl) ) {
            while( d && (d->kind == Ast::Declaration::VarDecl || d->kind == Ast::Declaration::LocalDecl) ) {
                VarDecl(d);
                d = d->next;
            }
        } else if( doProcs && d->kind == Ast::Declaration::Procedure ) {
            while( d && d->kind == Ast::Declaration::Procedure ) {
                ProcDecl(d);
                d = d->next;
            }
        } else
            d = d->next;
    }
    return d;
}

void CeeGen::ConstDecl(Ast::Declaration* d) {
    hout << "#define " << qualident(d) << " ";
    ConstExpr(d->expr, hout);
    if( d->expr == 0 ) {
        Type* t = deref(d->type());
        if (t && t->isInteger()) {
            renderIntLit(t, d->data, hout);
        } else {
            hout << d->data.toULongLong();
        }
    }
    hout << endl;
}

void CeeGen::TypeDecl(Ast::Declaration* d) {
    if( d->type()->kind != Type::Pointer )
        printHelper(d->helper);
    if( d->visi > Declaration::Private )
        hout << "/* public */ ";
    typeDecl(d);
    if( d->type()->kind == Type::Pointer )
        printHelper(d->helper);
    metaDecl(d);
    foreach( Declaration* p, d->type()->subs )
    {
        if( p->kind == Declaration::Procedure )
            ProcDecl(p);
    }
}

void CeeGen::VarDecl(Ast::Declaration* d) {
    printHelper(d->helper);
    if( d->outer->kind == Declaration::Module )
    {
        if( d->visi > Declaration::Private )
            bout << "/* public */ ";
        variable(bout, d, true);
        bout << ";" << endl << endl;
        if( d->visi > Declaration::Private )
            hout << "/* public */ ";
        hout << "extern ";
        variable(hout, d, false);
        hout << ";" << endl;
    }else
    {
        bout << ws();
        if( d->kind == Declaration::ParamDecl )
            parameter(bout, d);
        else
            variable(bout, d, true);
        bout << ";" << endl;
        // TODO emitSoapInit(bout, sub->name, sub->getType(), 0 );
    }
}

void CeeGen::Assembler(Ast::Declaration* proc) {
#if 1
    bout << "#if 0" << endl;
    bout << proc->data.toString().mid(5);
    bout << "#endif" << endl;
    bout << "  printf(\"TODO: calling unimplemented CODE section in " << curMod->name << "." << proc->name << "\\n\");" << endl;
    // bout << "  assert(0); // not yet implemented" << endl;
    if( proc->type() && deref(proc->type())->kind != Type::NoType )
        bout << "  return 0;" << endl;
#else
    QString err;
    TranspileOptions opt;
    bout << AsmToIntelXpiler::transform(proc->data.toString().mid(5), opt, &err) << endl;
#endif
}

void CeeGen::ProcDecl(Ast::Declaration * proc) {
    // SysFlag();

    Ast::Declaration* d = proc->link;
    while(d && d->kind == Declaration::ParamDecl )
        d = d->next;
    DeclSeq(d, true, false); // make sure nested procedures are declared before the procedure using them

    Q_ASSERT(curPlan == 0);
    curPlan = cl.plan(proc);

    Q_ASSERT(curProc == 0);
    curProc = proc;

    if( proc->outer && proc->outer->kind == Declaration::Module )
    {
        // no header file declarations for nested procedures
        procHeader(proc, true);
        hout << ";" << endl;
    }

    if( !proc->extern_ )
    {
        procHeader(proc, false);
        bout << " {" << endl;

        if( curPlan )
        {
            bout << "    // added params: ";
            foreach( const ClosureLifter::LiftParam& p, curPlan->addedParams )
            {
                bout << p.name;
                if( p.renamed )
                    bout << " (" << p.originalName << ")";
                bout << " ";
            }
            bout << endl;
        }
        d = proc->link;
        while(d && d->kind == Declaration::ParamDecl )
            d = d->next;
        // TODO: make a copy for OpenArrayValue
        curLevel++;
        DeclSeq(d, false, true);
        d = proc->link;
        while(d)
        {
            if( d->objectInit || d->pointerInit )
                emitVariableInit(d, curLevel);
            d = d->next;
        }
        if( proc->body ) {
            if( proc->body->kind == Ast::Statement::Assembler )
                Assembler(proc);
            else
                StatBlock(proc->body);
        }
        curLevel--;
        bout << "}" << endl << endl;
    }
    curPlan = 0;
    curProc = 0;
}

void CeeGen::SysFlag() {
    // TODO
}

bool CeeGen::Type_(Ast::Type* t) {
    typeRef(t);
    return true;
}

void CeeGen::Attributes() {
    // TODO
}

void CeeGen::StatBlock(Ast::Statement* s) {
    // Attributes();
    StatSeq(s);
}

void CeeGen::StatSeq(Ast::Statement*s) {
    s = Statement(s);
    while( s ) {
        s = Statement(s);
    }
}

void CeeGen::assig(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Assig);
    Type* lt = deref(s->lhs->type());
    Type* rt = deref(s->rhs->type());
    if( lt && (lt->kind == Type::Record || lt->kind == Type::Object) )
    {
        bout << "memcpy(&";
        Expr(s->lhs, bout);
        bout << ", &";
        Expr(s->rhs, bout);
        bout << ", sizeof(" << typeRef(lt) << "));";
    }else if( lt && lt->kind == Type::Array )
    {
        ArrayKind lak = deriveArrayKind(s->lhs);
        ArrayKind rak = deriveArrayKind(s->rhs);

        if( rt->kind == Type::StrLit )
        {
            bout << "strcpy((char*)";
            renderArrayPtr(lak, s->lhs, bout);
            bout << ", ";
            Expr(s->rhs, bout);
            bout << ");";
        }else
        {
            ArrayType at = arrayType(lt);
            bout << "memcpy(";
            renderArrayPtr(lak, s->lhs, bout);
            bout << ", ";
            renderArrayPtr(rak, s->rhs, bout);
            bout << ",";
            renderArrayLen(lak, s->lhs, bout); // RISK: just use the size of the left side
            // we assume the validator would have rejected invalid assignments
            bout << " * ";
            bout << "sizeof(" << typeRef(at.second) << ")";
            for( int i = 1; i < at.first.size(); i++ )
            {
                bout << " * ";
                Expr(at.first[i],bout);
            }
            bout << ");";
        }
    }else
    {
        Expr(s->lhs, bout);
        bout << " = ";
        if( lt && rt && lt->kind == Type::Pointer && rt->kind == Type::Pointer && lt != rt )
            bout << "(" << typeRef(lt) << ")";
        Expr(s->rhs, bout);
        bout << ";";
    }
}

Ast::Statement *CeeGen::IfStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::If);
    bout << "if( ";
    Expr(s->rhs, bout);
    bout << " ) { " << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    bout << ws() << "}";
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::Elsif ) {
        s = s->getNext();
        bout << " else if( ";
        Expr(s->rhs, bout);
        bout << " ) { " << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
        bout << ws() << "}";
    }
    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
        s = s->getNext();
        bout << " else { " << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
        bout << ws() << "}";
    }
    return s;
}

Ast::Statement *CeeGen::CaseStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Case);

    Ast::Statement* c = s->getNext();
    bool hasRange = false;
    while( !hasRange && c && c->kind == Ast::Statement::CaseLabel )
    {
        Ast::Expression* label = c->rhs;
        while( label )
        {
            if( label->kind == Expression::Range )
            {
                hasRange = true;
                break;
            }
            label = label->next;
        }
        c = c->getNext();
    }

    if( hasRange )
    {
        Expression* e = s->rhs;
        while( s && s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
        {
            s = s->getNext();
            if( !hasRange )
                bout << " else ";
            hasRange = false;
            bout << "if( ";
            Ast::Expression* label = s->rhs;
            while( label )
            {
                if( label->kind == Expression::Range )
                {
                    bout << "(";
                    Expr(e, bout);
                    bout << " >= "; // TODO: is lhs alway <= rhs?
                    Expr(label->lhs, bout);
                    bout << " && ";
                    Expr(e, bout);
                    bout << " <= ";
                    Expr(label->rhs, bout);
                    bout << ")";
                }else
                {
                    bout << "(";
                    Expr(e, bout);
                    bout << " == ";
                    Expr(label, bout);
                    bout << ")";
                }
                if( label->next )
                    bout << " || ";
                label = label->next;
            }
            bout << " ) {" << endl;
            curLevel++;
            StatSeq(s->body);
            curLevel--;
            bout << ws() << "}";
        }
        if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
            s = s->getNext();
            bout << " else {" << endl;
            curLevel++;
            StatSeq(s->body);
            curLevel--;
            bout << ws() << "}" << endl;
        }
    }else
    {
        bout << "switch( ";
        Expr(s->rhs, bout); // case
        bout << " ) { " << endl;
        while( s && s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
        {
            s = s->getNext();
            Ast::Expression* label = s->rhs;
            while( label )
            {
                bout << ws() << "case ";
                Q_ASSERT(label->kind != Expression::Range);
                Expr(label, bout, true);
                bout << ":" << endl;
                label = label->next;
            }
            curLevel++;
            StatSeq(s->body);
            bout << ws() << "break;" << endl;
            curLevel--;
        }
        if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
            s = s->getNext();
            bout << ws() << "default:" << endl;
            curLevel++;
            StatSeq(s->body);
            bout << ws() << "break;" << endl;
            curLevel--;
        }
        bout << ws() << "}";
    }
    return s;
}

void CeeGen::WhileStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::While);
    bout << "while( ";
    Expr(s->rhs, bout);
    bout << " ) {" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    bout << ws() << "}";
}

void CeeGen::RepeatStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Repeat);
    bout << "do {" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    bout << ws() << "} while( !";
    Expr(s->rhs, bout);
    bout << " );";
}

Ast::Statement *CeeGen::ForStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::ForAssig);

    Expression* i = s->lhs;
    Expression* from = s->rhs;
    Ast::Statement* body = s->body;
    Q_ASSERT( s->getNext() && s->getNext()->kind == Ast::Statement::ForToBy );
    s = s->getNext();
    Expression* to = s->lhs;
    Expression* by = s->rhs;

    const QByteArray temp = "_temp" + QByteArray::number(localId++);
    bout << "int " << temp << " = ";
    Expr(to, bout);
    bout << "; ";
    Expr(i, bout); // i := val
    bout << " = ";
    Expr(from, bout); // val
    bout << "; ";

    if( by == 0 )
    {
        bout << endl;
        bout << ws() << "while( ";
        Expr(i, bout);
        bout << " <= " << temp << " ) {" << endl;
        curLevel++;
        StatSeq(body);
        bout << ws();
        Expr(i, bout);
        bout << "++;" << endl;
        curLevel--;
        bout << ws() << "}";
    }else
    {
        // TODO: by is actually known at compile time, but we have no evaluator yet
        const QByteArray step = "_step" + QByteArray::number(localId++);
        bout << "const int " << step << " = ";
        ConstExpr(by, bout);
        bout << "; ";
        bout << endl;
        bout << ws() << "if( " << step << " > 0 ) {" << endl;
        // WHILE v <= temp DO statements; v := v + step END
        bout << ws() << "while( ";
        Expr(i, bout);
        bout << " <= " << temp << " ) {" << endl;
        curLevel++;
        StatSeq(body);
        bout << ws();
        Expr(i, bout);
        bout << " += " << step << ";" << endl;
        curLevel--;
        bout << ws() << "}" << endl;
        bout << ws() << "} else {" << endl;
        // WHILE v >= temp DO statements; v := v + step END
        bout << ws() << "while( ";
        Expr(i, bout);
        bout << " >= " << temp << " ) {" << endl;
        curLevel++;
        StatSeq(body);
        bout << ws();
        Expr(i, bout);
        bout << " += " << step << ";" << endl;
        curLevel--;
        bout << ws() << "}" << endl;
        bout << ws() << "}";
    }
    return s;
}

void CeeGen::LoopStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Loop);
    loopStack.push_back(s);
    bout << "while(1) {" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    loopStack.pop_back();
    bout << ws() << "}";
    bout << ws() << "__" << s->pos.d_row << ":" << endl;
}

Ast::Statement *CeeGen::WithStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::With);
    Type* guardType = deref(s->rhs->type());
    Type* guardBase = guardType;
    if( guardBase->kind == Type::Pointer )
        guardBase = deref(guardBase->type());
    Type* lhsType = deref(s->lhs->type());
    bout << "{ " << endl;
    curLevel++;
    bout << ws() << "if( ";
    if( lhsType->kind == Type::Pointer || lhsType->kind == Type::PTR )
    {
        bout << "(";
        Expr(s->lhs, bout);
        bout << " ? $isinst(&" << qualident(guardBase->decl) << "$class$, (void*)";
        Expr(s->lhs, bout);
        bout << "->class$) : 0)";
    }else
    {
        bout << "$isinst(&" << qualident(guardBase->decl) << "$class$, (void*)";
        Expr(s->lhs, bout);
        bout << ".class$)";
    }
    bout << " ) {" << endl;
    curLevel++;
    Declaration* d = s->lhs->val.value<Declaration*>();
    bout << ws() << "void* _with_tmp = (void*)";
    Expr(s->lhs, bout);
    bout << ";" << endl;
    bout << ws() << typeRef(guardBase) << "* " << escape(d->name) << " = ("
         << typeRef(guardBase) << "*)_with_tmp;" << endl;
    StatSeq(s->body);
    curLevel--;
    bout << ws() << "}" << endl;
    curLevel--;
    bout << ws() << "}";
    return s;
}

void CeeGen::ReturnStat(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Return);
    bout << "return";
    if( s->rhs ) {
        bout << " ";
        if( curProc )
        {
            Type* retT = deref(curProc->type());
            Type* exprT = deref(s->rhs->type());
            if( retT && exprT && retT->kind == Type::Pointer && exprT->kind == Type::Pointer && retT != exprT )
                bout << "(" << typeRef(retT) << ")";
        }
        Expr(s->rhs, bout);
    }
    bout << ";";
}

Ast::Statement* CeeGen::Statement(Ast::Statement* s) {
    if( s == 0 )
        return 0;
    if( s->kind != Statement::End )
        bout << ws();
    switch( s->kind )
    {
    case Ast::Statement::Assig:
        assig(s);
        break;
    case Ast::Statement::Call:
        call(s);
        break;
    case Ast::Statement::If:
        s = IfStat(s);
        break;
    case Ast::Statement::Case:
        s = CaseStat(s);
        break;
    case Ast::Statement::With:
        WithStat(s);
        break;
    case Ast::Statement::Loop:
        LoopStat(s);
        break;
    case Ast::Statement::While:
        WhileStat(s);
        break;
    case Ast::Statement::Repeat:
        RepeatStat(s);
        break;
    case Ast::Statement::Exit:
        if( !loopStack.isEmpty() )
            bout << "goto " << "__" << loopStack.back()->pos.d_row << ";";
        break;
    case Ast::Statement::Return:
        ReturnStat(s);
        break;
    case Ast::Statement::ForAssig:
        s = ForStat(s);
        break;
    case Ast::Statement::StatBlock:
        StatBlock(s);
        break;
    case Ast::Statement::End:
        break; // just marks the end of a non-empty statement sequence
    default:
        invalid("Statement", s->pos);
    }
    if( s->kind != Statement::End )
        bout << endl;
    if( s )
        s = s->getNext();
    return s;
}

bool CeeGen::ConstExpr(Ast::Expression* e, QTextStream &out) {
    return Expr(e, out, true);
}

bool CeeGen::Expr(Ast::Expression* e, QTextStream &out, bool isConst) {
    // Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    switch (e->kind) {
    case Ast::Expression::Lt:
    case Ast::Expression::Leq:
    case Ast::Expression::Geq:
    case Ast::Expression::Neq:
    case Ast::Expression::Eq:
    case Ast::Expression::Gt:
        if( !relation(e, out, isConst) )
            return false;
        break;
    case Ast::Expression::Is:
        if( !isOp(e, out) )
            return false;
        break;
    case Ast::Expression::In:
        if( !inOp(e, out,isConst) )
            return false;
        break;
    case Ast::Expression::Plus:
    case Ast::Expression::Minus:
    case Ast::Expression::Not:
        if( !unaryOp(e, out, isConst) )
            return false;
        break;
    case Ast::Expression::Add:
    case Ast::Expression::Sub:
    case Ast::Expression::Mul:
    case Ast::Expression::Fdiv:
    case Ast::Expression::Div:
    case Ast::Expression::Mod:
        if( !arithOp(e, out, isConst) )
            return false;
        break;
    case Ast::Expression::Or:
    case Ast::Expression::And:
        if( !logicOp(e, out) )
            return false;
        break;
    case Ast::Expression::DeclRef:
        if( !declRef(e, out,isConst) )
            return false;
        break;
    case Ast::Expression::Select:
        if( !select(e, out) )
            return false;
        break;
    case Ast::Expression::Index:
        if( !index(e, out) )
            return false;
        break;
    case Ast::Expression::Deref:
        if( !depointer(e, out) )
            return false;
        break;
    case Ast::Expression::Cast:
        if( !cast(e, out,isConst) )
            return false;
        break;
    case Ast::Expression::Call:
        if( !call(e, out, isConst) )
            return false;
        break;
    case Ast::Expression::Literal:
        if( !literal(e, out) )
            return false;
        break;
    case Ast::Expression::Constructor:
        if( !constructor(e, out, isConst) )
            return false;
        break;
    case Ast::Expression::Range:
    case Ast::Expression::NameRef:
    default:
        Q_ASSERT(false);
        break;
    }
    return true;
}

bool CeeGen::relation(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Type* lt = deref(e->lhs->type());
    Type* rt = deref(e->rhs->type());
    bool isChar = (lt && lt->kind == Type::CHAR) || (rt && rt->kind == Type::CHAR); // NOTE: validator assured that there is no StrLit here
    bool isStr = !isChar &&
                 ((lt && (lt->kind == Type::StrLit || (lt->kind == Type::Array && deref(lt->type()) &&
                   deref(lt->type())->kind == Type::CHAR))) ||
                  (rt && (rt->kind == Type::StrLit || (rt->kind == Type::Array && deref(rt->type()) &&
                   deref(rt->type())->kind == Type::CHAR))));
    if( isStr )
    {
        const ArrayKind lak = deriveArrayKind(e->lhs);
        const ArrayKind rak = deriveArrayKind(e->rhs);

        out << "(strcmp((const char*)";
        Expr(e->lhs, out,isConst);
        if( lak == FixedSize || lak == ReferenceToArray || lak == OpenArrayValue )
            out << ".$";
        out << ", (const char*)";
        Expr(e->rhs, out, isConst);
        if( rak == FixedSize || rak == ReferenceToArray || rak == OpenArrayValue )
            out << ".$";
        out << ")";

        switch(e->kind)
        {
        case Ast::Expression::Lt: out << " < 0)"; break;
        case Ast::Expression::Leq: out << " <= 0)"; break;
        case Ast::Expression::Geq: out << " >= 0)"; break;
        case Ast::Expression::Neq: out << " != 0)"; break;
        case Ast::Expression::Eq: out << " == 0)"; break;
        case Ast::Expression::Gt: out << " > 0)"; break;
        default: Q_ASSERT(false);
        }
    }else
    {
        out << "(";
        Expr(e->lhs, out, isConst);
        switch(e->kind)
        {
        case Ast::Expression::Lt:
            out << " < ";
            break;
        case Ast::Expression::Leq:
            out << " <= ";
            break;
        case Ast::Expression::Geq:
            out << " >= ";
            break;
        case Ast::Expression::Neq:
            out << " != ";
            break;
        case Ast::Expression::Eq:
            out << " == ";
            break;
        case Ast::Expression::Gt:
            out << " > ";
            break;
        case Ast::Expression::Is:
        case Ast::Expression::In:
        default:
            Q_ASSERT(false);
        }
        Expr(e->rhs, out, isConst);
        out << ")";
    }
    return true;
}

bool CeeGen::inOp(Ast::Expression *e, QTextStream &out, bool isConst)
{
    out << "(((1 << ";
    Expr(e->lhs, out,isConst);
    out << ") & ";
    Expr(e->rhs,out,isConst);
    out << ") != 0)";
    return true;
}

bool CeeGen::isOp(Ast::Expression *e, QTextStream &out)
{
    if( !Type::isA(e->lhs->type(), e->rhs->type()) )
        out << "0";
    else
    {
        Type* guardType = deref(e->rhs->type());
        if( guardType->kind == Type::Pointer )
            guardType = deref(guardType->type());
        Type* lhsType = deref(e->lhs->type());
        if( lhsType->kind == Type::Pointer || lhsType->kind == Type::PTR )
        {
            out << "(";
            Expr(e->lhs, out);
            out << " ? $isinst(&" << qualident(guardType->decl) << "$class$, (void*)";
            Expr(e->lhs, out);
            out << "->class$) : 0)";
        }else
        {
            out << "$isinst(&" << qualident(guardType->decl) << "$class$, (void*)";
            Expr(e->lhs, out);
            out << ".class$)";
        }
    }
    return true;
}

bool CeeGen::unaryOp(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Type* t = deref(e->lhs->type());
    bool isSet = t && t->kind == Type::SET;

    out << "(";
    switch(e->kind)
    {
    case Ast::Expression::Plus:
        out << "+";
        break;
    case Ast::Expression::Minus:
        out << (isSet ? "~" : "-");
        break;
    case Ast::Expression::Not:
        out << "!";
        break;
    default:
        Q_ASSERT(false);
    }
    Expr(e->lhs, out,isConst);
    out << ")";
    return true;
}

bool CeeGen::arithOp(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Type* lt = deref(e->lhs->type());
    bool isSet = lt && lt->kind == Type::SET;

    if( isSet && e->kind == Ast::Expression::Sub )
    {
        out << "(";
        Expr(e->lhs, out,isConst);
        out << " & ~(";
        Expr(e->rhs, out,isConst);
        out << "))";
        return true;
    }

    if (!isSet && e->kind == Ast::Expression::Div) {
        if( isConst )
            out << "DIV$(";
        else
            out << "OBERON$DIV(";
        Expr(e->lhs, out,isConst);
        out << ", ";
        Expr(e->rhs, out,isConst);
        out << ")";
        return true;
    }
    if (!isSet && e->kind == Ast::Expression::Mod) {
        if( isConst )
            out << "MOD$(";
        else
            out << "OBERON$MOD(";
        Expr(e->lhs, out,isConst);
        out << ", ";
        Expr(e->rhs, out,isConst);
        out << ")";
        return true;
    }

    out << "(";
    Expr(e->lhs, out,isConst);
    switch(e->kind)
    {
    case Ast::Expression::Add:
        out << (isSet ? " | " : " + ");
        break;
    case Ast::Expression::Sub:
        out << " - ";
        break;
    case Ast::Expression::Mul:
        out << (isSet ? " & " : " * ");
        break;
    case Ast::Expression::Fdiv:
        out << (isSet ? " ^ " : " / ");
        break;
    case Ast::Expression::Div:
        out << " / ";
        break;
    case Ast::Expression::Mod:
        out << " % ";
        break;
    default:
        Q_ASSERT(false);
    }
    Expr(e->rhs, out,isConst);
    out << ")";
    return true;
}

bool CeeGen::logicOp(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    out << "(";
    Expr(e->lhs, out);
    switch( e->kind )
    {
    case Ast::Expression::Or:
        out << " || ";
        break;
    case Ast::Expression::And:
        out << " && ";
        break;
    default:
        Q_ASSERT(false);
    }
    Expr(e->rhs, out);
    out << ")";
    return true;
}

bool CeeGen::declRef(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Declaration* d = e->val.value<Declaration*>();
    if( d && d->kind == Declaration::ConstDecl )
    {
        if( d->expr )
            Expr(d->expr, out, isConst);
        else
        {
            Type* t = deref(d->type());
            if (t && t->isInteger()) {
                renderIntLit(t, d->data, out);
            } else {
                out << d->data.toULongLong();
            }
        }
        return true;
    }
    Type* t = deref(d->type());
#if 0
    bool depointer = d && d->isVarParam() && t->kind != Type::Array; // don't deref if it is a MIC$AP value
    if( t && t->kind == Type::Array && t->expr == 0 )
        depointer = false;
    bool nonlocal = d->outer != curProc && ( d->kind == Declaration::LocalDecl || d->kind == Declaration::ParamDecl );
    if( nonlocal && depointer )
        depointer = false;
    if( depointer )
        out << "(*";
    out << qualident(d);
    if( depointer )
        out << ")";
#else
    const bool needsDeref = d && d->isVarParam() &&
                      t->kind != Type::Array &&
                      !(t && t->kind == Type::Array && t->expr == 0) &&
                      (d->outer == curProc || (d->kind != Declaration::LocalDecl && d->kind != Declaration::ParamDecl));
    if( needsDeref )
        out << "(*" << qualident(d) << ")";
    else
        out << qualident(d);
#endif
#if 0
    if( t && t->kind == Type::Array && t->expr == 0 )
        out << "$";
#endif
    return true;
}

bool CeeGen::select(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Expr(e->lhs, out);
    Type* t = deref(e->lhs->type());
    if( t->kind == Type::Pointer || t->kind == Type::Reference )
        out << "->";
    else
        out << ".";
    Q_ASSERT(e->rhs == 0);
    Declaration* d = e->val.value<Declaration*>();
    Q_ASSERT(d);
    out << escape(d->name);
    return true;
}

bool CeeGen::index(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;

    // if we meet an index, first check whether it is the last of a series of indices
    // of the same n-dim array
    QList<Expression*> indices;
    indices.push_front(e);
    Expression* i = e->lhs;
    while( i && i->kind == Expression::Index )
    {
        indices.push_front(i);
        i = i->lhs;
    }
    Q_ASSERT( !indices.isEmpty() );

    Type* at = deref(indices[0]->lhs->type());
    const ArrayKind lak = deriveArrayKind(indices[0]->lhs);
    ArrayType art = arrayType(at);

    // this transpiler represents n-dim array of T as 1-dim array of T
    if( lak == FixedSize || lak == ReferenceToArray || lak == OpenArrayValue )
    {
        out << "((" << typeRef(art.second) << "*)";
        Expr(indices[0]->lhs, out);
        out << ".$)";
    }else
    {
        Expr(indices[0]->lhs, out);
    }

    out << "[";
    if( indices.size() > 1 )
        out << QByteArray(indices.size()-1,'(');
    Expr(indices[0]->rhs, out);
    for( int n = 1; n < indices.size(); n++ )
    {
        // calculate offset with Horner's rule
        // ((idx[0] * len[1] + idx[1]) * len[2] + idx[2]...)
        // we're lucky that we don't need len[0]!
        out << " * ";
        Type* dt = deref(indices[n]->lhs->type());
        Q_ASSERT(dt->kind == Type::Array);
        if( dt->expr )
            Expr(dt->expr, out);
        else
        {
            invalid("multi-dimensional open/dynamic array indexing not yet supported", e->pos);
            out << "0 /* unsupported multi-dim open array */";
        }
        out << " + ";
        Expr(indices[n]->rhs, out);
        out << ")";
    }
    out << "]";
    return false;
}

bool CeeGen::depointer(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Type* pt = deref(e->lhs->type());
    if( pt && pt->kind == Type::Pointer )
    {
        Type* base = deref(pt->type());
        if( base && base->kind == Type::Array )
        {
            Expr(e->lhs, out);
            return false;
        }
    }
    out << "(*";
    Expr(e->lhs, out);
    out << ")";
    return false;
}

bool CeeGen::cast(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Type* t = deref(e->type());
    if( t )
    {
        out << "((" << typeRef(t) << ")";
        Expr(e->lhs, out, isConst);
        out << ")";
    }else
        Expr(e->lhs, out, isConst);
    return true;
}

bool CeeGen::call(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;

    Expression* lhs = e->lhs;
    if( lhs && lhs->kind == Expression::Super )
        lhs = lhs->lhs;

    Declaration* proc = lhs->val.value<Declaration*>();
    if( proc && proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
        proc = 0;
    Ast::Type* procType = deref(lhs->type());
    if( procType && procType->kind != Type::Procedure )
        procType = 0;
    const DeclList formals = proc ? proc->getParams(true) : procType ? procType->subs : DeclList();

    if( proc && proc->kind == Declaration::Builtin && builtin(proc->id, e->rhs, out,isConst) )
        return true;

    Expr(e->lhs, out, isConst);

    out << "(";
    Expression* arg = e->rhs;
    int i = 0;
    while(arg)
    {
        if( i != 0 )
            out << ", ";
        Type* ft = deref( i < formals.size() ? formals[i]->type() : 0);
        const bool varParam = i < formals.size() ? formals[i]->type()->kind == Type::Reference : false;
        Type* at = deref(arg->type());
        if( ft->kind == Type::Array )
        {
            ArrayKind aak = deriveArrayKind(arg);
            if( arg->kind == Expression::DeclRef && arg->val.value<Declaration*>()->outer != curProc )
            {
                // the argument references a non-local variable or constant; this is not a genaral work-around, but good enough for OP2

                // seen in OP2: NoArray, FixedSize, ReferenceToArray
                if( arg->type()->kind == Type::Reference )
                    ; // don't care, the Expr machinery dereferences it
                if(at->kind == Type::StrLit)
                    ; // don't care, just a constant from outer scope
                else if( at->kind == Type::Array )
                {
                    if( at->expr == 0 )
                        invalid("non-local open arrays cannot be properly represented by this transpiler", arg->pos);
                    else
                        aak = FixedSize;
                }else
                    ; // don't care, not an array
            }

            // formal type is an Array, either by value or VAR param, either fixed or open
            if( varParam && arg->varArrOfByte )
            {
                // formal uses MIC$AP, irregular actual
                switch(aak)
                {
                case FixedSize:
                case DerefedArray:
                case ReferenceToArray:
                case OpenArrayValue: {
                    // reinterpret an incompatible array as an array of bytes
                    ArrayType a = arrayType(at);
                    out << "(MIC$AP){ sizeof(";
                    out << typeRef(a.second);
                    out << ") * ";
                    renderArrayLen(aak, arg, out);
                    for( int i = 1; i < a.first.size(); i++ )
                    {
                        out << " * ";
                        Expr(a.first[i], out,isConst);
                    }
                    out << ",";
                    renderArrayPtr(aak, arg, out);
                    out << "}";
                    } break;
                case NoArray:
                    // reinterpret any non-array value as array of bytes
                    out << "(MIC$AP){ sizeof(";
                    out << typeRef(at);
                    out << "), &"; // non-array var params come derefed from Expr
                    Expr(arg,out,isConst);
                    out << "}";
                    break;
                default:
                    Q_ASSERT(false);
                    break;
                }
            }else if( varParam || ft->expr == 0 )
            {
                // formal uses MIC$AP, regular actual
                if( at->kind == Type::StrLit )
                {
                    out << "(MIC$AP){strlen(";
                    Expr(arg, out,isConst);
                    out << ") + 1,";
                    Expr(arg, out,isConst);
                    out << "}";
                }else
                    switch(aak)
                    {
                    case FixedSize:
                        out << "(MIC$AP){";
                        Expr(at->expr, out,isConst);
                        out << ", ";
                        renderArrayPtr(aak, arg, out);
                        out << "}";
                        break;
                    case DerefedArray:
                        out << "(MIC$AP){";
                        renderArrayLen(aak, arg, out);
                        out << ",";
                        renderArrayPtr(aak, arg, out);
                        out << "}";
                        break;
                    case ReferenceToArray:
                    case OpenArrayValue:
                        Expr(arg, out,isConst); // just render the arg
                        break;
                    case NoArray:
                    default:
                        Q_ASSERT(false);
                        break;
                    }
            }else
            {
                // formal uses struct { $[]; }
                if( at->kind == Type::StrLit )
                {
                    out << "(" << typeRef(ft) << "){.$ = ";
                    Expr(arg, out,isConst);
                    out << "}";
                }else
                    switch(aak)
                    {
                    case FixedSize:
                        Expr(arg, out,isConst); // just render the arg
                        break;
                    case DerefedArray:
                    case ReferenceToArray:
                        out << "*(" << typeRef(ft) << "*)";
                        renderArrayPtr(aak, arg, out);
                        break;
                    case OpenArrayValue:
                    default:
                        Q_ASSERT(false);
                        break;
                    }
            }
        }else if( varParam )
        {
            // non-array VAR parameter
            out << "&"; // Expr derefs var parameters for non-array arg
            Expr(arg, out,isConst);
        }else
        {
            // non-array value parameter
            Expr(arg, out,isConst);
        }
        i++;
        arg = arg->next;
    }
    if( proc && proc->kind == Declaration::Procedure )
    {
        const ClosureLifter::ProcPlan* plan = cl.plan(proc);
        if( plan && !plan->addedParams.isEmpty() )
        {
            foreach( const ClosureLifter::LiftParam& p, plan->addedParams )
            {
                if( i != 0 )
                    out << ", ";
                i++;
                if( p.sourceDecl->outer == curProc )
                {
                    // pass a local object to the present lifted argument
                    if( p.sourceDecl->kind == Declaration::ParamDecl )
                    {
                        // if it's a parameter, it might be a special case
                        Type* t = deref(p.sourceDecl->type());
                        if( t->kind == Type::Array && (t->expr == 0 || p.sourceDecl->isVarParam()) )
                        {
                            // open arrays or fixed VAR array params need special treatment
                            ArrayType at = arrayType(t);
                            // convert the MIC$AP back to pointer to element type
                            bout << "(" << typeRef(at.second) << "*)" << escape(p.sourceDecl->name) << ".$";
                        }else if( p.sourceDecl->isVarParam() )
                            bout << escape(p.sourceDecl->name);
                        else
                            bout << "&" << escape(p.sourceDecl->name);
                    }else
                        // this is unproblematic, just take the address
                        bout << "&" << escape(p.sourceDecl->name);
                }
                else
                {
                    // just pass through a lifted argument to the present lifted argument
                    Q_ASSERT(curPlan);
                    const ClosureLifter::LiftParam* pp = curPlan->findFromSourceDecl(p.sourceDecl);
                    Q_ASSERT(pp);
                    bout << escape(pp->name);
                }
            }
        }
    }
    out << ")";
    return true;
}

bool CeeGen::literal(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    QVariant val;
    if( e->kind == Expression::Literal )
    {
        val = e->val;
        Type* t = deref(e->type());
        if( t->kind == Type::NIL )
            out << "NULL";
        else if( t->isInteger() )
            renderIntLit(t, val, out);
        else if( t->kind == Type::BYTE || t->kind == Type::SET )
            out << val.toULongLong();
        else if( t->kind == Type::BOOLEAN )
            out << (int)val.toBool();
        else if( t->kind == Type::CHAR )
            out << val.toULongLong();
        else if( t->isReal() )
        {
            QByteArray tmp = QByteArray::number(val.toDouble());
            // NOTE: if we don't do that, Qt writes 1/2 instead of 1.0/2.0 which renders a different result!
            if( !tmp.contains('.') && !tmp.contains('e') )
                tmp += ".0";
            out << tmp;
        }
        else if( t->kind == Type::StrLit )
        {
            QByteArray str = val.toByteArray();
            str.replace("\"", "\\\"");
            out << "\"" << str << "\"";
        }else
            Q_ASSERT(false);
        return true;
    }
    return false;
}

bool CeeGen::constructor(Ast::Expression *e, QTextStream &out, bool isConst)
{
    if( e == 0 )
        return false;
    Ast::Expression* elem = e->rhs;
    out << "(0";
    while( elem )
    {
        out << " | ";
        if( elem->kind == Expression::Range )
        {
            // TODO: assuming lhs <= rhs
            out << "(uint32_t)((((uint64_t)1 << (";
            Expr(elem->rhs, out, isConst); // high
            out << " - ";
            Expr(elem->lhs, out, isConst); // low
            out << " + 1)) - 1) << ";
            Expr(elem->lhs, out, isConst); // low
            out << ")";
        }else
        {
            out << "1u << ";
            Expr(elem, out, isConst);
        }
        elem = elem->next;
    }
    out << ")";
    return true;
}

void CeeGen::call(Ast::Statement *s, bool isConst)
{
    if( s == 0 )
        return;
    Q_ASSERT(s->lhs && s->lhs->kind == Expression::Call);
    Expr(s->lhs, bout, isConst);
    bout << ";";
}

void CeeGen::metaDecl(Ast::Declaration *d)
{
    Type* t = d->type();
    if( t && t->kind != Type::Object && t->kind != Type::Record )
        return;
    t = deref(t);
    const QByteArray className = qualident(d);
    hout << "struct " << className << "$Class$ {" << endl;
    bout << "struct " << className << "$Class$ " << className << "$class$ = { " << endl;

    if( t->type() )
    {
        Type* super = deref(t->type());
        if( super->kind == Type::Pointer )
            super = deref(super->type());
        hout << ws(1) << "struct " << qualident(super->decl) << "$Class$* super$;" << endl;
        bout << ws(1) << "&" << qualident(super->decl) << "$class$," << endl;
    }else
    {
        hout << ws(1) << "void* super$;" << endl;
        bout << ws(1) << "0," << endl;
    }

    DeclList methods = t->methodList(true);
    foreach( Declaration* p, methods )
    {
        Q_ASSERT( p->kind == Declaration::Procedure && !p->extern_ );
        bout << ws(1) << qualident(p) << ", " << endl;

        hout << ws(1) << typeRef(p->type()) << " (*" << p->name << ")";
        hout << "(";
        DeclList params = p->getParams();
        for( int i = 0; i < params.size(); i++ )
        {
            if( i != 0 )
                hout << ", ";
            parameter(hout, params[i]);
        }
        hout << ");" << endl;
    }

    bout << "};" << endl << endl;
    hout << "};" << endl;
    hout << "extern struct " << className << "$Class$ " << className << "$class$;" << endl;
}

bool CeeGen::builtin(int bi, Ast::Expression *args, QTextStream &out, bool isConst)
{
    ExpList a = Expression::getList(args);

    switch( bi )
    {
    case Builtin::ABS:
        if( a.size() != 1 )
            return false;
        {
            Type* t = deref(a[0]->type());
            if( t && t->isReal() )
                out << "fabs(";
            else
                out << "abs(";
            Expr(a[0], out, isConst);
            out << ")";
        }
        return true;
    case Builtin::ODD:
        if( a.size() != 1 )
            return false;
        out << "((";
        Expr(a[0], out, isConst);
        out << ") & 1)";
        return true;
    case Builtin::CAP:
        if( a.size() != 1 )
            return false;
        out << "(char)toupper((unsigned char)";
        Expr(a[0], out, isConst);
        out << ")";
        return true;
    case Builtin::ASH:
        if( a.size() != 2 )
            return false;
        out << "ASH$(";
        Expr(a[0], out, isConst);
        out << ", ";
        Expr(a[1], out, isConst);
        out << ")";
        return true;
    case Builtin::LEN:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        else
        {
            if( a.size() == 2 )
                invalid("LEN with two arguments not supported by CeeGen; ignored", a[1]->pos);
            Type* t = deref(a[0]->type());
            if( t && t->kind == Type::StrLit )
            {
                out << "(strlen(";
                Expr(a[0], out, isConst);
                out << ") + 1)";
            }else
            {
                ArrayKind ak = deriveArrayKind(a[0]);
                renderArrayLen(ak, a[0], out);
            }
        }
        return true;
    case Builtin::MAX:
        if( a.size() == 1 )
        {
            Type* t = deref(a[0]->type());
            if( t && t->kind == Type::SET )
            {
                out << "31";
            }else if( t )
            {
                switch( t->kind )
                {
                case Type::SHORTINT: out << "127"; break;
                case Type::INTEGER: out << "32767"; break;
                case Type::LONGINT: out << "2147483647L"; break;
                case Type::HUGEINT: out << "9223372036854775807LL"; break;
                case Type::REAL: out << "3.40282347e+38f"; break;
                case Type::LONGREAL: out << "1.7976931348623157e+308"; break;
                case Type::CHAR: out << "255"; break;
                case Type::BOOLEAN: out << "1"; break;
                case Type::SET: out << "31"; break;
                default: out << "0"; break;
                }
            }else
                out << "0";
            return true;
        }else if( a.size() == 2 )
        {
            out << "((";
            Expr(a[0], out,isConst);
            out << ") > (";
            Expr(a[1], out,isConst);
            out << ") ? (";
            Expr(a[0], out,isConst);
            out << ") : (";
            Expr(a[1], out,isConst);
            out << "))";
            return true;
        }
        return false;
    case Builtin::MIN:
        if( a.size() == 1 )
        {
            Type* t = deref(a[0]->type());
            if( t && t->kind == Type::SET )
            {
                out << "0";
            }else if( t )
            {
                switch( t->kind )
                {
                case Type::SHORTINT: out << "-128"; break;
                case Type::INTEGER: out << "-32768"; break;
                case Type::LONGINT: out << "(-2147483647L - 1)"; break;
                case Type::HUGEINT: out << "(-9223372036854775807LL - 1)"; break;
                case Type::REAL: out << "1.17549435e-38f"; break;
                case Type::LONGREAL: out << "2.2250738585072014e-308"; break;
                case Type::CHAR: out << "0"; break;
                case Type::BOOLEAN: out << "0"; break;
                case Type::SET: out << "0"; break;
                default: out << "0"; break;
                }
            }else
                out << "0";
            return true;
        }else if( a.size() == 2 )
        {
            out << "((";
            Expr(a[0], out,isConst);
            out << ") < (";
            Expr(a[1], out,isConst);
            out << ") ? (";
            Expr(a[0], out,isConst);
            out << ") : (";
            Expr(a[1], out,isConst);
            out << "))";
            return true;
        }
        return false;
    case Builtin::SIZE:
        if( a.size() != 1 )
            return false;
        out << "sizeof(" << typeRef(a[0]->type()) << ")";
        return true;
    case Builtin::ORD:
        if( a.size() != 1 )
            return false;
        out << "(int)(unsigned char)(";
        Expr(a[0], out,isConst);
        out << ")";
        return true;
    case Builtin::CHR:
        if( a.size() != 1 )
            return false;
        out << "(char)(";
        Expr(a[0], out,isConst);
        out << ")";
        return true;
    case Builtin::SHORT:
        if( a.size() != 1 )
            return false;
        {
            Type* t = deref(a[0]->type());
            if( t )
            {
                switch( t->kind )
                {
                case Type::HUGEINT: out << "(int)("; break;
                case Type::LONGINT: out << "(short)("; break;
                case Type::INTEGER: out << "(char)("; break;
                case Type::LONGREAL: out << "(float)("; break;
                default: out << "("; break;
                }
            }else
                out << "(";
            Expr(a[0], out,isConst);
            out << ")";
        }
        return true;
    case Builtin::LONG:
        if( a.size() != 1 )
            return false;
        {
            Type* t = deref(a[0]->type());
            if( t )
            {
                switch( t->kind )
                {
                case Type::LONGINT:
                    out << "(long long)("; break;
                case Type::INTEGER:
                    out << "(int)("; break;
                case Type::SHORTINT:
                case Type::CHAR:
                    out << "(short)("; break;
                case Type::REAL:
                    out << "(double)("; break;
                default:
                    out << "("; break;
                }
            }else
                out << "(";
            Expr(a[0], out,isConst);
            out << ")";
        }
        return true;
    case Builtin::ENTIER:
        if( a.size() != 1 )
            return false;
        if( deref(a[0]->type())->kind == Type::LONGREAL )
            out << "(long long)floor((double)";
        else
            out << "(int)floor((float)";
        Expr(a[0], out,isConst);
        out << ")";
        return true;
    case Builtin::INC:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        if( a.size() == 1 )
        {
            Expr(a[0], out,isConst);
            out << "++";
        }else
        {
            Expr(a[0], out,isConst);
            out << " += ";
            Expr(a[1], out,isConst);
        }
        return true;
    case Builtin::DEC:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        if( a.size() == 1 )
        {
            Expr(a[0], out,isConst);
            out << "--";
        }else
        {
            Expr(a[0], out,isConst);
            out << " -= ";
            Expr(a[1], out,isConst);
        }
        return true;
    case Builtin::INCL:
        if( a.size() != 2 )
            return false;
        Expr(a[0], out,isConst);
        out << " |= (1u << ";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::EXCL:
        if( a.size() != 2 )
            return false;
        Expr(a[0], out,isConst);
        out << " &= ~(1u << ";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::COPY:
        if( a.size() != 2 )
            return false;
        {
#if 1
            ArrayKind srcAk = deriveArrayKind(a[0]);
            ArrayKind dstAk = deriveArrayKind(a[1]);
            out << "strcpy((char*)";

            renderArrayPtr(dstAk, a[1], out);
            out << ", (const char*)";

            if( srcAk == NoArray )
                Expr(a[0], out,isConst);
            else
                renderArrayPtr(srcAk, a[0], out);
            out << ")";
#else
            Type* srcT = deref(a[0]->type());
            Type* dstT = deref(a[1]->type());
            if( dstT && dstT->kind == Type::Array && dstT->expr )
            {
                out << "strncpy((char*)";
                Expr(a[1], out);
                if( dstT->isSOA() || (dstT->kind == Type::Array && dstT->expr == 0 && !dstT->dynamic) )
                    out << ".$";
                out << ", (const char*)";
                if( a[0]->kind == Expression::Literal && a[0]->val.type() == QVariant::ByteArray
                        && a[0]->val.toByteArray().size() == 1 )
                {
                    QByteArray str = a[0]->val.toByteArray();
                    out << "\"";
                    char c = str[0];
                    if( c == '"' ) out << "\\\"";
                    else if( c == '\\' ) out << "\\\\";
                    else out << c;
                    out << "\"";
                }else
                {
                    Expr(a[0], out);
                    if( srcT && (srcT->isSOA() || (srcT->kind == Type::Array && srcT->expr == 0 && !srcT->dynamic)) )
                        out << ".$";
                }
                out << ", ";
                Expr(dstT->expr, out);
                out << ")";
            }else
            {
                out << "strcpy((char*)";
                Expr(a[1], out);
                if( dstT && (dstT->isSOA() || (dstT->kind == Type::Array && dstT->expr == 0 && !dstT->dynamic)) )
                    out << ".$";
                out << ", (const char*)";
                if( a[0]->kind == Expression::Literal && a[0]->val.type() == QVariant::ByteArray
                        && a[0]->val.toByteArray().size() == 1 )
                {
                    QByteArray str = a[0]->val.toByteArray();
                    out << "\"";
                    char c = str[0];
                    if( c == '"' ) out << "\\\"";
                    else if( c == '\\' ) out << "\\\\";
                    else out << c;
                    out << "\"";
                }else
                {
                    Expr(a[0], out);
                    if( srcT && (srcT->isSOA() || (srcT->kind == Type::Array && srcT->expr == 0 && !srcT->dynamic)) )
                        out << ".$";
                }
                out << ")";
            }
#endif
        }
        return true;
    case Builtin::NEW:
        if( a.size() < 1 )
            return false;
        else
        {
            Type* ptrType = deref(a[0]->type());
            if( ptrType && ptrType->kind == Type::Pointer )
            {
                Type* base = deref(ptrType->type());
                if( base && (base->kind == Type::Record || base->kind == Type::Object) )
                {
                    Expr(a[0], out,isConst);
                    out << " = (" << typeRef(base) << "*)GC_MALLOC(sizeof(" << typeRef(ptrType->type()) << "))";
                    if( base->decl )
                    {
                        out << "; ";
                        Expr(a[0], out,isConst);
                        out << "->class$ = &" << qualident(ptrType->type()) << "$class$";
                    }
                }else if( base && base->kind == Type::Array )
                {
                    if( base->expr == 0 && !checkOpenArray(base, a[0]->pos) )
                        return false;
                    ArrayType at = arrayType(base);
                    Expr(a[0], out,isConst);
                    out << " = (" << typeRef(at.second) << "*)$allocda(";
                    if( base->expr == 0 )
                    {
                        if( a.size() > 1 )
                            Expr(a[1], out,isConst); // use the second argument of NEW as dynamic size
                        else
                            return invalid("calling NEW for an open array with no size parameter", a[0]->pos);
                    }else
                        Expr(at.first[0], out,isConst);
                    for( int i = 1; i < at.first.size(); i++ )
                    {
                        out << " * ";
                        Expr(at.first[i], out,isConst);
                    }
                    out << ", sizeof(" << typeRef(at.second) << "))";
                }else
                    Q_ASSERT(false);
                // TODO: call initializer if needed
            }else
                Q_ASSERT(false);
        }
        return true;
    case Builtin::HALT:
        if( a.size() != 1 )
            return false;
        out << "exit(";
        Expr(a[0], out,isConst);
        out << ")";
        return true;
    case Builtin::AWAIT:
        if( a.size() != 1 )
            return false;
        out << "/* AWAIT("; // stub for concurrency
        Expr(a[0], out,isConst);
        out << ") */";
        return true;
    case Builtin::ASSERT:
        if( a.size() < 1 || a.size() > 2 )
            return false;
#if _DEBUG
        out << "if(!";
        Expr(a[0], out,isConst);
        out << "){fprintf(stderr,\"assertion FAILED in %s:%d\\n\", __FILE__, __LINE__);fflush(stderr);}";
#else
        out << "assert(";
        Expr(a[0], out,isConst);
        out << ")";
#endif
        return true;
    case Builtin::SYSTEM_ADR:
        if( a.size() != 1 )
            return false;
        out << "(void*)&(";
        Expr(a[0], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_VAL: {
        if( a.size() != 2 )
            return false;
        Type* fromT = deref(a[1]->type());
        Type* toT = deref(a[0]->type());
        const QByteArray from = typeRef(fromT);
        const QByteArray to = typeRef(toT);
        if( from == to )
            Expr(a[1], out,isConst);
        else if( toT->kind == Type::LONGINT && fromT->kind == Type::Pointer )
        {
            out << "(void*)";
            Expr(a[1], out,isConst);
        }else
        {
            out << "((" << to << ")(";
            Expr(a[1], out,isConst);
            out << "))";
        }
        return true;
    }
    case Builtin::SYSTEM_BIT:
        if( a.size() != 2 )
            return false;
        out << "((*(int*)(";
        Expr(a[0], out,isConst);
        out << ") >> (";
        Expr(a[1], out,isConst);
        out << ")) & 1)";
        return true;
    case Builtin::SYSTEM_CC:
        if( a.size() != 1 )
            return false;
        out << "0 /* SYSTEM.CC not supported in C */";
        return true;
    case Builtin::SYSTEM_LSH:
        if( a.size() != 2 )
            return false;
        out << "ASH$(";
        Expr(a[0], out,isConst);
        out << ", ";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_ROT:
        if( a.size() != 2 )
            return false;
        out << "((";
        Expr(a[0], out,isConst);
        out << ") << ((";
        Expr(a[1], out,isConst);
        out << ") & 31) | (uint32_t)(";
        Expr(a[0], out,isConst);
        out << ") >> (32 - ((";
        Expr(a[1], out,isConst);
        out << ") & 31)))";
        return true;
    case Builtin::SYSTEM_TYPECODE:
        if( a.size() != 1 )
            return false;
        out << "(int)(";
        Expr(a[0], out,isConst);
        out << ")->class$";
        return true;
    case Builtin::SYSTEM_GET:
        if( a.size() != 2 )
            return false;
        Expr(a[1], out,isConst);
        out << " = *((" << typeRef(a[1]->type()) << "*)(";
        Expr(a[0], out,isConst);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET8:
        if( a.size() != 1 )
            return false;
        out << "(*(char*)(";
        Expr(a[0], out,isConst);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET16:
        if( a.size() != 1 )
            return false;
        out << "(*(short*)(";
        Expr(a[0], out,isConst);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET32:
        if( a.size() != 1 )
            return false;
        out << "(*(int*)(";
        Expr(a[0], out,isConst);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET64:
        if( a.size() != 1 )
            return false;
        out << "(*(long long*)(";
        Expr(a[0], out,isConst);
        out << "))";
        return true;
    case Builtin::SYSTEM_PUT:
        if( a.size() != 2 )
            return false;
        out << "*((" << typeRef(a[1]->type()) << "*)(";
        Expr(a[0], out,isConst);
        out << ")) = ";
        Expr(a[1], out,isConst);
        return true;
    case Builtin::SYSTEM_PUT8:
        if( a.size() != 2 )
            return false;
        out << "(*(char*)(";
        Expr(a[0], out,isConst);
        out << ")) = (char)(";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT16:
        if( a.size() != 2 )
            return false;
        out << "(*(short*)(";
        Expr(a[0], out,isConst);
        out << ")) = (short)(";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT32:
        if( a.size() != 2 )
            return false;
        out << "(*(int*)(";
        Expr(a[0], out,isConst);
        out << ")) = (int)(";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT64:
        if( a.size() != 2 )
            return false;
        out << "(*(long long*)(";
        Expr(a[0], out,isConst);
        out << ")) = (long long)(";
        Expr(a[1], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_MOVE:
        if( a.size() != 3 )
            return false;
        out << "memcpy(";
        Expr(a[1], out,isConst);
        out << ", ";
        Expr(a[0], out,isConst);
        out << ", ";
        Expr(a[2], out,isConst);
        out << ")";
        return true;
    case Builtin::SYSTEM_NEW:
        if( a.size() < 2 )
            return false;
        Expr(a[0], out);
        out << " = (";
        out << typeRef(a[0]->type());
        out << ")GC_MALLOC(";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_GETREG:
        if( a.size() != 2 )
            return false;
        out << "/* SYSTEM.GETREG(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ") not supported in C */";
        return true;
    case Builtin::SYSTEM_PUTREG:
        if( a.size() != 2 )
            return false;
        out << "/* SYSTEM.PUTREG(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ") not supported in C */";
        return true;
    case Builtin::SYSTEM_PORTIN:
        if( a.size() != 2 )
            return false;
        out << "/* SYSTEM.PORTIN(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ") not supported in C */";
        return true;
    case Builtin::SYSTEM_PORTOUT:
        if( a.size() != 2 )
            return false;
        out << "/* SYSTEM.PORTOUT(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ") not supported in C */";
        return true;
    case Builtin::SYSTEM_CLI:
        out << "/* SYSTEM.CLI() not supported in C */";
        return true;
    case Builtin::SYSTEM_STI:
        out << "/* SYSTEM.STI() not supported in C */";
        return true;
    case Builtin::SYSTEM_ENABLEINTERRUPTS:
        out << "/* SYSTEM.ENABLEINTERRUPTS() not supported in C */";
        return true;
    case Builtin::SYSTEM_DISABLEINTERRUPTS:
        out << "/* SYSTEM.DISABLEINTERRUPTS() not supported in C */";
        return true;
    case Builtin::SYSTEM_RESTOREINTERRUPTS:
        out << "/* SYSTEM.RESTOREINTERRUPTS() not supported in C */";
        return true;
    }
    return false;
}

QString CeeGen::genDedication()
{
    return "// this file was generated by " + QCoreApplication::applicationName() + " "
            + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

Type *CeeGen::deref(Ast::Type * t)
{
    if( t && (t->kind == Type::NameRef || t->kind == Type::Reference) )
        return deref(t->type());
    return t;
}

QByteArray CeeGen::typeRef(Type* orig)
{
    // Render a type for a "storage slot", i.e. a variable (module or local), a parameter, a field, an array element, a pointer target
    // this is not about procedure return types (which don't allow arrays nor records)
    // this is not (yet) about casting
    // "orig" is the true type of the "storage slot", not deref'ed
    // NOTE that all types have been "deanonymized", i.e. even if types are constructed in place, they have a name and a typedecl

    if( orig == 0 || orig->kind == Type::NoType )
        return "void";
    const bool isVarParam = orig->kind == Type::Reference;

    QByteArray res;
    Type* t = deref(orig);
    switch(t->kind)
    {
    case Type::BOOLEAN:
    case Type::CHAR:
    case Type::BYTE:
    case Type::SHORTINT:
    case Type::INTEGER:
    case Type::LONGINT:
    case Type::SET:
    case Type::HUGEINT:
    case Type::REAL:
    case Type::LONGREAL:
    case Type::PTR:
    case Type::ANY:
        res = basicType(t);
        if( isVarParam )
            res = res + "*";
        break;

    case Type::StrLit:
    case Type::NIL:
    case Type::NoType:
        Q_ASSERT(false);
        break;

    case Type::Pointer: {
        Type* to = deref(t->type());
        if( to->kind == Type::Array )
            res = typePrefix(to) + qualident(to->type()) + "*";
        else
            res = typePrefix(to) + qualident(orig);
        if( isVarParam )
            res = res + "*";
        } break;
    case Type::Procedure:
    case Type::Record:
    case Type::Object:
        res = typePrefix(t) + qualident(orig);
        if( isVarParam )
            res = res + "*";
        break;
    case Type::Array:
        if( t->expr == 0 )
            res = "MIC$AP";
        else
        {
            res = typePrefix(t) + qualident(orig);
            if( isVarParam )
                res = res + "*";
        }
        break;
    case Type::Reference:
    case Type::NameRef:
        Q_ASSERT(false);
        break;
    }
    return res;
}

void CeeGen::printHelper(Ast::Declaration * d)
{
    if( d == 0 || d->getModule() != curMod )
        return;
    Q_ASSERT(d->helper == 0);
    typeDecl(d);
    metaDecl(d);
    foreach( Declaration* p, d->type()->subs )
    {
        if( p->kind == Declaration::Procedure )
            ProcDecl(p);
    }
    printHelper(d->next);
}

void CeeGen::parameter(QTextStream &out, Ast::Declaration *param)
{
    Type* orig = param->type();
    parameter(out, orig, param->name, param->pos);
}

void CeeGen::parameter(QTextStream &out, Ast::Type *orig, const QByteArray &name, const RowCol& pos)
{
    Type* t = deref(orig);
    if( orig == 0 )
        out << "? " << escape(name);
    else if( t && t->kind == Type::Array )
    {
        // VAR open or fixed array, value open or fixed array
        checkOpenArray(t, pos);
        if( orig->kind == Type::Reference || t->expr == 0 )
            // VAR (fixed or open) or open
            out << "MIC$AP " << escape(name);
        else
            // not VAR and not open:
            out << typeRef(orig) << " " << escape(name);
    }else
        out << typePrefix2(orig) << typeRef(orig) << " " << escape(name);
}

void CeeGen::liftedParam(QTextStream &out, Ast::Declaration *what, const QByteArray& name)
{
#if 1 // TODO
    out << typePrefix3(what->type(),!what->isVarParam()) << typeRef(what->type());
    if( !what->isVarParam() )
        out << "* "; // only make it a pointer if it is not already a reference
    out << escape(name);
#else
    // NOTE: this is actually the correct approach, but designators could no longer just be *derefed
    if( what->isVarParam() )
        parameter(out, what->type(), name, what->pos);
    else
    {
        Type ref;
        ref.kind = Type::Reference;
        ref.pos = what->pos;
        ref.setType(what->type()); // simulate a var parameter if not already the case
        parameter(out, &ref, name, what->pos);
        ref.setType(0);
    }
#endif
}

void CeeGen::variable(QTextStream &out, Ast::Declaration *var, bool body)
{
    out << typePrefix2(var->type()) << typeRef(var->type()) << " " << qualident(var);
    if( body && deref(var->type())->kind == Type::Pointer )
        out << " = NULL";
}

void CeeGen::procHeader(Ast::Declaration *proc, bool header)
{
    DeclList params = proc->getParams();

    QTextStream& out = header ? hout : bout;

    if( header )
        for( int i = 0; i < params.size(); i++ )
            printHelper(params[i]->helper);

    if( proc->visi > Declaration::Private )
        out << "/* public */ ";
    if( header )
        out << "extern ";
    out << typeRef(proc->type()) << " ";
    out << qualident(proc);
    out << "(";
    bool hasPars = false;
    for( int i = 0; i < params.size(); i++ )
    {
        if( hasPars )
            out << ", ";
        hasPars = true;
        parameter(out, params[i]);
    }
    if( curPlan )
    {
        foreach( const ClosureLifter::LiftParam& ap, curPlan->addedParams )
        {
            if( hasPars )
                out << ", ";
            hasPars = true;
            liftedParam(out, ap.sourceDecl, ap.name);
        }
    }
    out << ")";
}

QByteArray CeeGen::escape(const QByteArray & name)
{
    if( keywords.contains(name.constData()) )
        return name + "_";
    else
        return name;
}

void CeeGen::renderArrayPtr(int ak, Expression* e, QTextStream &out )
{
    Expr(e, out);
    switch(ak)
    {
    case FixedSize:
        out << ".$";
        break;
    case PointerToArray:
        break;
    case DerefedArray:
        // NOTE: Expr(e) doesn't deref the pointer, so we still have a pointer here
        break;
    case ReferenceToArray:
    case OpenArrayValue:
        out << ".$";
        break;
    default:
        Q_ASSERT(false);

    }
}

void CeeGen::renderArrayLen(int ak, Ast::Expression *e, QTextStream &out)
{
    // this is only the number of elements in the first array dimension!
    switch( ak )
    {
    case FixedSize:
        Expr(deref(e->type())->expr, out);
        break;
    case PointerToArray:
        out << "$toda((void**)";
        Expr(e, out);
        out << ")->$1";
        break;
    case DerefedArray:
        out << "$toda((void**)";
        // NOTE: Expr(e) doesn't deref the pointer, so we still have a pointer here
        Expr(e, out);
        out << ")->$1";
        break;
    case ReferenceToArray:
    case OpenArrayValue:
        Expr(e, out);
        out << ".$1";
        break;
    default:
        Q_ASSERT(false);
    }
}
