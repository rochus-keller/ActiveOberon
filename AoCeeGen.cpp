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

bool CeeGen::generate(Ast::Declaration* module, QIODevice *header, QIODevice *body) {
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
    bout << "#include <stdlib.h>" << endl;
    bout << "#include <string.h>" << endl;
    bout << "#include <assert.h>" << endl;
    bout << "#include <math.h>" << endl << endl;

    hout << "#include <stdint.h>" << endl;

    Module(module);

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
    bout << "}" << endl;
    bout << "static MIC$DA* $toda(void** ptr) {" << endl;
    bout << "    return (MIC$DA*)((char*)ptr - offsetof(MIC$DA, $));" << endl;
    bout << "}" << endl;
    bout << "static void** $allocda(uint32_t count, int elemSize) {" << endl;
    bout << "    MIC$DA* ptr = malloc(sizeof(MIC$DA) + count * elemSize);" << endl;
    bout << "    if( ptr == 0 ) return 0;" << endl;
    bout << "    ptr->$1 = count;" << endl;
    bout << "    return ptr->$;" << endl;
    bout << "}" << endl << endl;

    d = DeclSeq(d, true, true);
    if( d && d->body ) {
        StatBlock(d->body);
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

    if( !proc->forward )
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
    Expr(s->lhs, bout);
    bout << " = ";
    Expr(s->rhs, bout);
    bout << ";";
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
    bout << ws() << "__" << s->pos.d_row << ":" << endl;
    bout << ws() << "}";
}

Ast::Statement *CeeGen::WithStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::With);
    bout << ws() << "if( ";
    bout << "$isinst(&" << typeRef(s->rhs->type()) << "$class$, ";
    Type* t = deref(s->lhs->type());
    Q_ASSERT(t->kind == Type::Pointer || t->kind == Type::PTR || s->lhs->type()->kind == Type::Reference);
    Expr(s->lhs, bout);
    bout << ") ) {" << endl;
    // TODO: cast
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    bout << ws() << "}";
    return s;
}

void CeeGen::ReturnStat(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Return);
    bout << "return";
    if( s->rhs ) {
        bout << " ";
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
        out << "$isinst(&" << typeRef(e->rhs->type()) << "$class$, ";
        Expr(e->lhs, out );
        Type* t = deref(e->lhs->type());
        // TODO Q_ASSERT(t->kind == Type::Pointer || e->lhs->type()->kind == Type::Reference);
        // we pass the pointer to the instance, not the class pointer, because the former can be NULL
        out << ")";
    }
    return true;
}

bool CeeGen::unaryOp(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;

    out << "(";
    switch(e->kind)
    {
    case Ast::Expression::Plus:
        out << "+";
        break;
    case Ast::Expression::Minus:
        out << "-";
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
    out << "(";
    Expr(e->lhs, out);
    switch(e->kind)
    {
    case Ast::Expression::Add:
        out << " + ";
        break;
    case Ast::Expression::Sub:
        out << " - ";
        break;
    case Ast::Expression::Mul:
        out << " * ";
        break;
    case Ast::Expression::Fdiv:
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

    Expr(indices[0]->lhs, out);

    Type* at = deref(indices[0]->lhs->type());

    if( at->expr != 0 )
        out << ".$";

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
            // pointer
            out << "*(((uint32_t*)";
            Expr(indices[0]->lhs->lhs, out);
            out << ") - 4*" << n << ")"; // TODO
        }else
        {
            // open array param
            Expr(indices[0]->lhs, out);
            out << n; // TODO
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
    out << "(*";
    Expr(e->lhs, out);
    out << ")";
    return false;
}

bool CeeGen::cast(Ast::Expression *e, QTextStream &out)
{
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs, out);
    return false;
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
#if 0
        // TODO
        Type* formT = deref( i < formals.size() ? formals[i]->type() : 0);
        if( formT && formT->kind == Type::Array )
        {
            Type* actT = deref(arg->type());
            Q_ASSERT( actT && (actT->kind == Type::Array || actT->kind == Type::StrLit || actT->kind == Type::Reference));

            // this is a formal array parameter; it can be fix size or open by var or by val
            // the actual arg must also be an array; it can be fix, a derefed pointer to arr, an open array, or a string lit

            // TODO: implement all possible combinations (~30)
            ArrayType arrT = arrayType(formT);
            Expr(arg, out);
            if( formT->dynamic )
            {
                // pointer
                for(int i = 0; i < arrT.first.size() && arrT.first[i] == 0; i++)
                {
                    out << ", ";
                    out << "*(((uint32_t*)";
                    Expr(arg, out);
                    out << ") - 4*" << i << ")";
                }
            }else
            {
                // open array param
                for(int i = 0; i < arrT.first.size() && arrT.first[i] == 0; i++)
                {
                    out << ", ";
                    Expr(arg, out);
                    out << i;
                }
            }
        }else if( i < formals.size() && formals[i]->varParam )
        {
            out << "&";
            Expr(arg, out);
        }else
#endif
            Expr(arg, out);
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
        str.replace("\"", "\\\"");
        out << "\"" << str << "\"";
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
        Q_ASSERT( p->kind == Declaration::Procedure && !p->forward );
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
    case Builtin::SYSTEM_VAL:
        if( a.size() != 2 )
            return false;
        out << "((" << typeRef(a[0]->type()) << ")";
        Expr(a[1], out);
        out << ")";
        return true;
    case Builtin::ASSERT:
        if( a.size() != 1 )
            return false;
        out << "assert(";
        Expr(a[0], out);
        out << ")";
        return true;
    case Builtin::INC:
        if( a.size() == 1 )
        {
            Expr(a[0], out);
            out << "++";
            return true;
        }else if( a.size() == 2 )
        {
            Expr(a[0], out);
            out << " += ";
            Expr(a[1], out);
            return true;
        }else
            return false;
    case Builtin::DEC:
        if( a.size() == 1 )
        {
            Expr(a[0], out);
            out << "--";
            return true;
        }else if( a.size() == 2 )
        {
            Expr(a[0], out);
            out << " -= ";
            Expr(a[1], out);
            return true;
        }else
            return false;
    case Builtin::ASH:
        break; // implemented by macro
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
#if 0
        // an open Array is just a pointer to the memory, even in n-dim or var param case
        out << typeRef(a.second) << "* " << escape(param->name) << "$";
        for( int i = 0; i < a.first.size(); i++ )
        {
            if(a.first[i] != 0)
                break; // from this dim size is fix
            out << ", uint32_t " << escape(param->name) << "$" << i;
        }
#else
        out << "MIC$AP " << escape(param->name);
#endif
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
