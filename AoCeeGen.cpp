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

QByteArray CeeGen::qualident(Declaration* d)
{
    if( d == 0 )
        return "???";
    if( d->outer && d->kind != Declaration::LocalDecl && d->kind != Declaration::ParamDecl )
        return qualident(d->outer) + "$" + escape(d->name);
    else
    {
        const bool nonlocal = d->outer != curProc && ( d->kind == Declaration::LocalDecl || d->kind == Declaration::ParamDecl );
        if(nonlocal)
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
        bout << "    GC_INIT();" << endl;
        bout << "    " << module->name << "$init$();" << endl;
        bout << "    return 0;" << endl;
        bout << "}" << endl;
    }

    hout << endl;
    hout << "#endif // " << guard;

    return errors.isEmpty();
}

void CeeGen::invalid(const char* what, const RowCol& pos) {
    errors << Error(QString("invalid %1").arg(what),pos, curMod->name);
}

void CeeGen::typeDecl(Ast::Declaration *d)
{
    Ast::Type* t = d->type();
    if( t == 0 )
    {
        hout << "// undeclared type " << d->name;
        return;
    }

    if( t->kind == Type::NameRef )
        return; // don't create name aliasses

    if( t->kind == Type::Object )
    {
        // forward declaration for class objects
        hout << "struct " << qualident(d) << "$Class$ " << qualident(d) << "$Class$;" << endl;
    }

    if( t->kind == Type::Array && t->expr == 0 )
    {
        hout << "// no typedef for open array " << qualident(d) << " (" << typeRef(t->type()) << "*)";
        return; // we need not typedef for open arrays, instead they are referenced by element_type*
    }

    if( t->kind < Type::MaxBasicType )
        hout << "typedef " << typeRef(t); //  << " " << qualident(d);
    else
    {
        switch( t->kind )
        {
        case Type::Pointer:
            hout << "typedef ";
            pointerTo(hout, t);
            break;
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
                hout << ")";
                return;
            }
            break;
        case Type::Array:
            if( t->expr != 0 )
            {
                ArrayType at = arrayType(t);
                hout << "typedef ";
                hout << "struct " << qualident(d) << " { " << typeRef(at.second) << " $[";
                Expr(t->expr, hout);
                for( int i = 1; i < at.first.size(); i++ )
                {
                    t = deref(t->type());
                    hout << " * ";
                    Expr(t->expr, hout);
                }
                hout << "];" << " }";
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
            } break;
        case Type::NameRef:
            hout << "typedef ";
            hout << typeRef(t->type());
            break;
        }
    }
    // typedef what name
    hout << " " << qualident(d);
}

void CeeGen::Module(Ast::Declaration *module) {

    cl.analyze(module);

    Ast::Declaration* d = module->link;
    if( d && d->kind == Ast::Declaration::Import ) {
        d = ImportList(d);
    }

    hout << endl;

    hout << "#define ASH(x, n) (((n) >= 0) ? ((x) << ((n) >= 0 ? (n) : 0)) : ((x) >> ((n) < 0  ? -(n) : 0)))" << endl;
    hout << "#ifndef __MIC_DEFINE__" << endl << "#define __MIC_DEFINE__" << endl;
    // static analysis revealed that in ETH Oberon System v2.3.7 only the first dim of all n-dim arrays is open
    hout << "typedef struct MIC$AP { uint32_t $1; void* $; } MIC$AP;" << endl; // n-dim open array parameter (var or val)
    hout << "typedef struct MIC$DA { uint32_t $1; void* $[]; } MIC$DA;" << endl; // n-dim dynamic array, pointer points to $, not $1 (for compat with SYSTEM calls)
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

        Ast::Declaration* imp = module->link;
        while( imp && imp->kind == Ast::Declaration::Import )
        {
            Import import = imp->data.value<Import>();
            if( import.moduleName != "SYSTEM" && import.resolved && !import.resolved->extern_ )
                bout << "    " << import.moduleName << "$init$();" << endl;
            imp = imp->next;
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
    hout << endl;
}

void CeeGen::TypeDecl(Ast::Declaration* d) {
    printHelper(d->helper);
    if( d->visi > Declaration::Private )
        hout << "/* public */ ";
    typeDecl(d);
    hout << ";" << endl;
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
        variable(bout, d);
        bout << ";" << endl << endl;
        if( d->visi > Declaration::Private )
            hout << "/* public */ ";
        hout << "extern ";
        variable(hout, d);
        hout << ";" << endl;
    }else
    {
        bout << ws();
        parameter(bout, d);
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
    DeclSeq(d, true, false);

    Q_ASSERT(curPlan == 0);
    curPlan = cl.plan(proc);

    Q_ASSERT(curProc == 0);
    curProc = proc;

    procHeader(proc, true);
    hout << ";" << endl;

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
        curLevel++;
        d = DeclSeq(d, false, true);
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
    }else if( lt && lt->kind == Type::Array && lt->expr != 0 )
    {
        Type* elemT = deref(lt->type());
        if( elemT && elemT->kind == Type::CHAR && rt && rt->kind == Type::StrLit )
        {
            bout << "strncpy((char*)";
            Expr(s->lhs, bout);
            if( lt->isSOA() )
                bout << ".$";
            bout << ", ";
            Expr(s->rhs, bout);
            bout << ", ";
            Expr(lt->expr, bout);
            bout << ");";
        }else
        {
            bout << "memcpy(&";
            Expr(s->lhs, bout);
            bout << ", &";
            Expr(s->rhs, bout);
            bout << ", sizeof(" << typeRef(lt) << "));";
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
                Expr(label, bout);
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
    return Expr(e, out);
}

bool CeeGen::Expr(Ast::Expression* e, QTextStream &out) {
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
        if( !relation(e, out) )
            return false;
        break;
    case Ast::Expression::Is:
        if( !isOp(e, out) )
            return false;
        break;
    case Ast::Expression::In:
        if( !inOp(e, out) )
            return false;
        break;
    case Ast::Expression::Plus:
    case Ast::Expression::Minus:
    case Ast::Expression::Not:
        if( !unaryOp(e, out) )
            return false;
        break;
    case Ast::Expression::Add:
    case Ast::Expression::Sub:
    case Ast::Expression::Mul:
    case Ast::Expression::Fdiv:
    case Ast::Expression::Div:
    case Ast::Expression::Mod:
        if( !arithOp(e, out) )
            return false;
        break;
    case Ast::Expression::Or:
    case Ast::Expression::And:
        if( !logicOp(e, out) )
            return false;
        break;
    case Ast::Expression::DeclRef:
        if( !declRef(e, out) )
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
        if( !cast(e, out) )
            return false;
        break;
    case Ast::Expression::Call:
        if( !call(e, out) )
            return false;
        break;
    case Ast::Expression::Literal:
        if( !literal(e, out) )
            return false;
        break;
    case Ast::Expression::Constructor:
        if( !constructor(e, out) )
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

bool CeeGen::relation(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Type* lt = deref(e->lhs->type());
    Type* rt = deref(e->rhs->type());
    bool isChar = (lt && lt->kind == Type::CHAR) || (rt && rt->kind == Type::CHAR);
    bool isStr = !isChar &&
                 ((lt && (lt->kind == Type::StrLit || (lt->kind == Type::Array && deref(lt->type()) &&
                   deref(lt->type())->kind == Type::CHAR))) ||
                  (rt && (rt->kind == Type::StrLit || (rt->kind == Type::Array && deref(rt->type()) &&
                   deref(rt->type())->kind == Type::CHAR))));
    if( isStr )
    {
        out << "(strcmp((const char*)";
        Expr(e->lhs, out);
        if( lt && lt->kind == Type::Array && (lt->isSOA() || (lt->expr == 0 && !lt->dynamic)) )
            out << ".$";
        out << ", (const char*)";
        Expr(e->rhs, out);
        if( rt && rt->kind == Type::Array && (rt->isSOA() || (rt->expr == 0 && !rt->dynamic)) )
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
        Expr(e->lhs, out);
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
        Expr(e->rhs, out);
        out << ")";
    }
    return true;
}

bool CeeGen::inOp(Ast::Expression *e, QTextStream &out)
{
    out << "((1 << ";
    Expr(e->lhs, out);
    out << ") & ";
    Expr(e->rhs,out);
    out << ")";
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

bool CeeGen::unaryOp(Ast::Expression *e, QTextStream &out)
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
    Expr(e->lhs, out);
    out << ")";
    return true;
}

bool CeeGen::arithOp(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Type* lt = deref(e->lhs->type());
    bool isSet = lt && lt->kind == Type::SET;

    if( isSet && e->kind == Ast::Expression::Sub )
    {
        out << "(";
        Expr(e->lhs, out);
        out << " & ~(";
        Expr(e->rhs, out);
        out << "))";
        return true;
    }

    out << "(";
    Expr(e->lhs, out);
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
    Expr(e->rhs, out);
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

bool CeeGen::declRef(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Declaration* d = e->val.value<Declaration*>();
    if( d && d->kind == Declaration::ConstDecl && d->outer == 0 && (d->name == "FALSE" || d->name == "TRUE") )
    {
        if( d->name == "TRUE" )
            out << "1";
        else
            out << "0";
        return true;
    }
    Type* t = deref(d->type());
    bool depointer = d && d->kind == Declaration::ParamDecl && d->varParam;
    if( t && t->kind == Type::Array && t->expr == 0 )
        depointer = false;
    if( depointer )
        out << "(*";
    out << qualident(d);
    if( depointer )
        out << ")";
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
    out << d->name;
    return true;
}

bool CeeGen::index(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
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
    bool isOpenParam = (at->kind == Type::Array && at->expr == 0 && !at->dynamic);

    if( isOpenParam )
    {
        ArrayType art = arrayType(at);
        out << "((" << typeRef(art.second) << "*)";
        Expr(indices[0]->lhs, out);
        out << ".$)";
    }else
    {
        Expr(indices[0]->lhs, out);
        if( at->kind == Type::Array && at->expr != 0 )
            out << ".$";
    }

    out << "[";
    if( indices.size() > 1 )
        out << QByteArray(indices.size()-1,'(');
    Expr(indices[0]->rhs, out);
    for( int n = 1; n < indices.size(); n++ )
    {
        out << " * ";
        at = deref(indices[n]->lhs->type());
        Q_ASSERT(at->kind == Type::Array);
        if( at->expr )
            Expr(at->expr, out);
        else if( at->dynamic )
        {
            invalid("multi-dimensional open/dynamic array indexing not yet supported", e->pos);
            out << "0 /* unsupported multi-dim open array */";
        }else
        {
            invalid("multi-dimensional open array param indexing not yet supported", e->pos);
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
        if( base && base->kind == Type::Array && base->expr == 0 )
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

bool CeeGen::cast(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    Type* t = deref(e->type());
    if( t )
    {
        out << "((" << typeRef(t) << ")";
        Expr(e->lhs, out);
        out << ")";
    }else
        Expr(e->lhs, out);
    return true;
}

bool CeeGen::call(Ast::Expression *e, QTextStream &out)
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

    if( proc && proc->kind == Declaration::Builtin && builtin(proc->id, e->rhs, out) )
        return true;

    Expr(e->lhs, out);

    out << "(";
    Expression* arg = e->rhs;
    int i = 0;
    while(arg)
    {
        if( i != 0 )
            out << ", ";
        Type* formT = deref( i < formals.size() ? formals[i]->type() : 0);
        if( formT && formT->kind == Type::Array && formT->expr == 0 )
        {
            ArrayType formArr = arrayType(formT);
            int openDims = 0;
            for( int j = 0; j < formArr.first.size(); j++ )
                if( formArr.first[j] == 0 ) openDims++;
            if( openDims > 1 )
                invalid("multi-dimensional open array argument not yet supported", arg->pos);
            Type* actT = deref(arg->type());
            if( actT && actT->kind == Type::StrLit )
            {
                out << "(MIC$AP){strlen(";
                Expr(arg, out);
                out << ") + 1, (void*)";
                Expr(arg, out);
                out << "}";
            }else if( actT && actT->kind == Type::Array && actT->expr != 0 )
            {
                out << "(MIC$AP){";
                Expr(actT->expr, out);
                out << ", (void*)";
                Expr(arg, out);
                if( actT->isSOA() )
                    out << ".$";
                out << "}";
            }else if( actT && actT->kind == Type::Array && actT->expr == 0 )
            {
                Expr(arg, out);
            }else if( actT && actT->kind == Type::Pointer )
            {
                Type* base = deref(actT->type());
                if( base && base->kind == Type::Array && base->expr == 0 )
                {
                    out << "(MIC$AP){$toda((void**)";
                    Expr(arg, out);
                    out << ")->$1, (void*)";
                    Expr(arg, out);
                    out << "}";
                }else
                    Expr(arg, out);
            }else if( actT && actT->kind == Type::Reference )
            {
                Expr(arg, out);
            }else
                Expr(arg, out);
        }else if( i < formals.size() && formals[i]->varParam )
        {
            Type* actT = deref(arg->type());
            if( actT && actT->kind == Type::Array && actT->expr == 0 )
                Expr(arg, out);
            else
            {
                out << "&";
                Expr(arg, out);
            }
        }else
        {
            Type* actT = deref(arg->type());
            if( formT && actT && formT->kind == Type::Pointer && actT->kind == Type::Pointer && formT != actT )
                out << "(" << typeRef(formT) << ")";
            Expr(arg, out);
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
                    bout << "&" << escape(p.sourceDecl->name);
                else
                {
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
    switch( e->val.type() )
    {
    case QVariant::Invalid:
        out << "NULL";
        break;
    case QVariant::Bool:
    case QVariant::Int:
    case QVariant::UInt:
    case QVariant::LongLong:
    case QVariant::ULongLong:
        out << e->val.toULongLong();
        break;
    case QVariant::Double:
        out << e->val.toDouble();
        break;
    case QVariant::ByteArray: {
        QByteArray str = e->val.toByteArray();
        if( str.size() == 1 )
        {
            char c = str[0];
            if( c == '\'' )
                out << "'\\\''";
            else if( c == '\\' )
                out << "'\\\\'"; 
            else if( c >= 32 && c < 127 )
                out << "'" << str << "'";
            else
                out << "(char)" << (int)(unsigned char)c;
        }else
        {
            str.replace("\"", "\\\"");
            out << "\"" << str << "\"";
        }
        } break;
    default:
        Q_ASSERT(false);
    }

    return true;
}

bool CeeGen::constructor(Ast::Expression *e, QTextStream &out)
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
            Expr(elem->rhs, out); // high
            out << " - ";
            Expr(elem->lhs, out); // low
            out << " + 1)) - 1) << ";
            Expr(elem->lhs, out); // low
            out << ")";
        }else
        {
            out << "1u << ";
            Expr(elem, out);
        }
        elem = elem->next;
    }
    out << ")";
    return true;
}

void CeeGen::call(Ast::Statement *s)
{
    if( s == 0 )
        return;
    Q_ASSERT(s->lhs && s->lhs->kind == Expression::Call);
    Expr(s->lhs, bout);
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

bool CeeGen::builtin(int bi, Ast::Expression *args, QTextStream &out)
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
            Expr(a[0], out);
            out << ")";
        }
        return true;
    case Builtin::ODD:
        if( a.size() != 1 )
            return false;
        out << "((";
        Expr(a[0], out);
        out << ") & 1)";
        return true;
    case Builtin::CAP:
        if( a.size() != 1 )
            return false;
        out << "(char)toupper((unsigned char)";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::ASH:
        if( a.size() != 2 )
            return false;
        out << "ASH(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::LEN:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        {
            Type* t = deref(a[0]->type());
            if( t && t->kind == Type::StrLit )
            {
                out << "(strlen(";
                Expr(a[0], out);
                out << ") + 1)";
            }else if( t && t->kind == Type::Array )
            {
                if( t->expr )
                {
                    Expr(t->expr, out);
                }else if( t->dynamic )
                {
                    out << "$toda((void**)";
                    Expr(a[0], out);
                    out << ")->$1";
                }else
                {
                    Expr(a[0], out);
                    out << ".$1";
                }
            }else if( t && t->kind == Type::Pointer )
            {
                Type* base = deref(t->type());
                if( base && base->kind == Type::Array && base->expr == 0 )
                {
                    out << "$toda((void**)";
                    Expr(a[0], out);
                    out << ")->$1";
                }else
                {
                    out << "0";
                }
            }else
                out << "0";
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
            Expr(a[0], out);
            out << ") > (";
            Expr(a[1], out);
            out << ") ? (";
            Expr(a[0], out);
            out << ") : (";
            Expr(a[1], out);
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
            Expr(a[0], out);
            out << ") < (";
            Expr(a[1], out);
            out << ") ? (";
            Expr(a[0], out);
            out << ") : (";
            Expr(a[1], out);
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
        out << "(long)(unsigned char)(";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::CHR:
        if( a.size() != 1 )
            return false;
        out << "(char)(";
        Expr(a[0], out);
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
                case Type::HUGEINT: out << "(long)("; break;
                case Type::LONGINT: out << "(short)("; break;
                case Type::INTEGER: out << "(char)("; break;
                case Type::LONGREAL: out << "(float)("; break;
                default: out << "("; break;
                }
            }else
                out << "(";
            Expr(a[0], out);
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
                case Type::LONGINT: out << "(long long)("; break;
                case Type::INTEGER: out << "(long)("; break;
                case Type::SHORTINT:
                case Type::CHAR: out << "(short)("; break;
                case Type::REAL: out << "(double)("; break;
                default: out << "("; break;
                }
            }else
                out << "(";
            Expr(a[0], out);
            out << ")";
        }
        return true;
    case Builtin::ENTIER:
        if( a.size() != 1 )
            return false;
        out << "(long)floor((double)";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::INC:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        if( a.size() == 1 )
        {
            Expr(a[0], out);
            out << "++";
        }else
        {
            Expr(a[0], out);
            out << " += ";
            Expr(a[1], out);
        }
        return true;
    case Builtin::DEC:
        if( a.size() < 1 || a.size() > 2 )
            return false;
        if( a.size() == 1 )
        {
            Expr(a[0], out);
            out << "--";
        }else
        {
            Expr(a[0], out);
            out << " -= ";
            Expr(a[1], out);
        }
        return true;
    case Builtin::INCL:
        if( a.size() != 2 )
            return false;
        Expr(a[0], out);
        out << " |= (1u << ";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::EXCL:
        if( a.size() != 2 )
            return false;
        Expr(a[0], out);
        out << " &= ~(1u << ";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::COPY:
        if( a.size() != 2 )
            return false;
        {
            Type* srcT = deref(a[0]->type());
            Type* dstT = deref(a[1]->type());
            if( dstT && dstT->kind == Type::Array && dstT->expr )
            {
                out << "strncpy((char*)";
                Expr(a[1], out);
                if( dstT->isSOA() || (dstT->kind == Type::Array && dstT->expr == 0 && !dstT->dynamic) )
                    out << ".$";
                out << ", (const char*)";
                Expr(a[0], out);
                if( srcT && (srcT->isSOA() || (srcT->kind == Type::Array && srcT->expr == 0 && !srcT->dynamic)) )
                    out << ".$";
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
                Expr(a[0], out);
                if( srcT && (srcT->isSOA() || (srcT->kind == Type::Array && srcT->expr == 0 && !srcT->dynamic)) )
                    out << ".$";
                out << ")";
            }
        }
        return true;
    case Builtin::NEW:
        if( a.size() < 1 )
            return false;
        {
            Type* ptrType = deref(a[0]->type());
            if( ptrType && ptrType->kind == Type::Pointer )
            {
                Type* base = deref(ptrType->type());
                if( base && (base->kind == Type::Record || base->kind == Type::Object) )
                {
                    Expr(a[0], out);
                    out << " = (" << typeRef(base) << "*)GC_MALLOC(sizeof(" << typeRef(base) << "))";
                    if( base->decl )
                    {
                        out << "; ";
                        Expr(a[0], out);
                        out << "->class$ = &" << qualident(base->decl) << "$class$";
                    }
                }else if( base && base->kind == Type::Array && base->expr == 0 )
                {
                    ArrayType at = arrayType(base);
                    Expr(a[0], out);
                    out << " = (" << typeRef(at.second) << "*)$allocda(";
                    if( a.size() > 1 )
                        Expr(a[1], out);
                    else
                        out << "0";
                    out << ", sizeof(" << typeRef(at.second) << "))";
                }else
                {
                    Expr(a[0], out);
                    out << " = (" << typeRef(ptrType) << ")GC_MALLOC(sizeof(" << typeRef(base) << "))";
                }
            }else
            {
                Expr(a[0], out);
                out << " = GC_MALLOC(sizeof(*";
                Expr(a[0], out);
                out << "))";
            }
        }
        return true;
    case Builtin::HALT:
        if( a.size() != 1 )
            return false;
        out << "exit(";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::AWAIT:
        if( a.size() != 1 )
            return false;
        out << "/* AWAIT("; // stub for concurrency
        Expr(a[0], out);
        out << ") */";
        return true;
    case Builtin::ASSERT:
        if( a.size() < 1 || a.size() > 2 )
            return false;
#if _DEBUG
        out << "if(!";
        Expr(a[0], out);
        out << "){fprintf(stderr,\"assertion FAILED in %s:%d\\n\", __FILE__, __LINE__);fflush(stderr);}";
#else
        out << "assert(";
        Expr(a[0], out);
        out << ")";
#endif
        return true;
    case Builtin::SYSTEM_ADR:
        if( a.size() != 1 )
            return false;
        out << "(void*)&(";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_VAL:
        if( a.size() != 2 )
            return false;
        out << "((" << typeRef(a[0]->type()) << ")(";
        Expr(a[1], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_BIT:
        if( a.size() != 2 )
            return false;
        out << "((*(long*)(";
        Expr(a[0], out);
        out << ") >> (";
        Expr(a[1], out);
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
        out << "ASH(";
        Expr(a[0], out);
        out << ", ";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_ROT:
        if( a.size() != 2 )
            return false;
        out << "((";
        Expr(a[0], out);
        out << ") << ((";
        Expr(a[1], out);
        out << ") & 31) | (unsigned long)(";
        Expr(a[0], out);
        out << ") >> (32 - ((";
        Expr(a[1], out);
        out << ") & 31)))";
        return true;
    case Builtin::SYSTEM_TYPECODE:
        if( a.size() != 1 )
            return false;
        out << "(long)(";
        Expr(a[0], out);
        out << ")->class$";
        return true;
    case Builtin::SYSTEM_GET:
        if( a.size() != 2 )
            return false;
        Expr(a[1], out);
        out << " = *((" << typeRef(a[1]->type()) << "*)(";
        Expr(a[0], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET8:
        if( a.size() != 1 )
            return false;
        out << "(*(char*)(";
        Expr(a[0], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET16:
        if( a.size() != 1 )
            return false;
        out << "(*(short*)(";
        Expr(a[0], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET32:
        if( a.size() != 1 )
            return false;
        out << "(*(long*)(";
        Expr(a[0], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_GET64:
        if( a.size() != 1 )
            return false;
        out << "(*(long long*)(";
        Expr(a[0], out);
        out << "))";
        return true;
    case Builtin::SYSTEM_PUT:
        if( a.size() != 2 )
            return false;
        out << "*((" << typeRef(a[1]->type()) << "*)(";
        Expr(a[0], out);
        out << ")) = ";
        Expr(a[1], out);
        return true;
    case Builtin::SYSTEM_PUT8:
        if( a.size() != 2 )
            return false;
        out << "(*(char*)(";
        Expr(a[0], out);
        out << ")) = (char)(";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT16:
        if( a.size() != 2 )
            return false;
        out << "(*(short*)(";
        Expr(a[0], out);
        out << ")) = (short)(";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT32:
        if( a.size() != 2 )
            return false;
        out << "(*(long*)(";
        Expr(a[0], out);
        out << ")) = (long)(";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_PUT64:
        if( a.size() != 2 )
            return false;
        out << "(*(long long*)(";
        Expr(a[0], out);
        out << ")) = (long long)(";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::SYSTEM_MOVE:
        if( a.size() != 3 )
            return false;
        out << "memcpy((void*)(";
        Expr(a[1], out);
        out << "), (void*)(";
        Expr(a[0], out);
        out << "), ";
        Expr(a[2], out);
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

QByteArray CeeGen::typeRef(Type * t)
{
    if( t == 0 || t->kind == Type::NoType )
        return "void";
    t = deref(t);
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
        return "long"; // using long so it is 32 or 64 bits like the target system, useful for e.g. SYSTEM.GET)
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
    }

    // Arrays:
    // fixed size: struct { a[1*2*3..]}
    // dynamic open: [..sizes ] <ptr> [1*2*3..]
    // param open: ptr, d1, d2, ...

    if( t->kind == Type::Pointer )
    {

        Type* to = deref(t->type());
        if( to->kind == Type::Array )
        {
            // pointer to open arrays have no extra typedef, instead we use element_type*
            ArrayType at = arrayType(to);
            to = at.second;
        }
        return typeRef(to) + "*";
    }else if( t->kind == Type::Array && t->expr == 0 )
    {
        ArrayType at = arrayType(t);
        return typeRef(at.second) + "*";
    }else if( t->decl )
    {
        if( t->isSOA() )
            return "struct " + qualident(t->decl);
        else
            return qualident(t->decl);
    }else
        return "?TYPE";
}

void CeeGen::pointerTo(QTextStream &out, Ast::Type *ptr)
{
    Type* to = ptr->type();
    Type* to2 = deref(to);
    if( to2 && to2->kind == Type::Array && to2->expr == 0 )
    {
        // Pointer to array is translated to pointer to array element
        ptr = to2;
        to = ptr->type();
        to2 = deref(to);
    }
    out << typeRef(ptr->type()) << "*";

}

void CeeGen::printHelper(Ast::Declaration * d)
{
    if( d == 0 )
        return;
    Q_ASSERT(d->helper == 0);
    typeDecl(d);
    hout << ";" << endl;
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
    Type* t = deref(param->type());
    if( t && t->kind == Type::Array && t->expr == 0 )
    {
        ArrayType a = arrayType(t);
        int openDims = 0;
        for( int i = 0; i < a.first.size(); i++ )
            if( a.first[i] == 0 ) openDims++;
        if( openDims > 1 )
            invalid("multi-dimensional open array parameter not yet supported", param->pos);
        out << "MIC$AP " << escape(param->name);
    }else if( param->varParam )
        out << typeRef(param->type()) << "* " << escape(param->name);
    else
        out << typeRef(param->type()) << " " << escape(param->name);
}

void CeeGen::variable(QTextStream &out, Ast::Declaration *var)
{
    out << typeRef(var->type()) << " " << qualident(var);
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
            out << typeRef(ap.sourceDecl->type()) << "* " << escape(ap.name);
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
