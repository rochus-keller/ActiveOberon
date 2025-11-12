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
#include <QCoreApplication>
#include <QDateTime>
#include <QIODevice>
using namespace Ao;
using namespace Ast;

static QByteArray qualident(Declaration* d)
{
    if( d->outer )
        return qualident(d->outer) + "$" + d->name;
    else
        return d->name;
}

bool CeeGen::generate(Ast::Declaration* module, QIODevice *header, QIODevice *body) {
    errors.clear();

    curMod = module;
    curLevel = 0;
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
    bout << "#include <math.h>" << endl << endl;

    Module(module);
    return errors.isEmpty();
}

void CeeGen::invalid(const char* what, const RowCol& pos) {
    errors << Error(QString("invalid %1").arg(what),pos, curMod->name);
}

void CeeGen::typeDecl(Ast::Declaration *d)
{
    Ast::Type* t = deref(d->type());
    if( t == 0 )
    {
        hout << "// undeclared type " << d->name;
        return;
    }

    if( t->kind == Type::Object )
    {
        // forward declaration for class objects
        hout << "typedef struct " << qualident(d) << "$Class$ " << qualident(d) << "$Class$;" << endl;
    }

    if( t->kind == Type::Array && t->expr == 0 )
    {
        hout << "// no typedef for open array " << qualident(d) << " (" << typeRef(t->type()) << "*)";
        return; // we need not typedef for open arrays, instead they are referenced by element_type*
    }

    hout << "typedef ";
    if( t->kind < Type::MaxBasicType )
        hout << typeRef(t); //  << " " << qualident(d);
    else
    {
        switch( t->kind )
        {
        case Type::Pointer:
            pointerTo(hout, t);
            // hout << " " << qualident(d);
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
                hout << "struct " << qualident(d) << " { " << typeRef(t->type()) << " _[" << t->len << "];" << " }"; // TODO: calc len
            else
                Q_ASSERT(false); // hout << typeRef(t->getType()) << "* ";
            break;

        case Type::Record:
            hout << "struct " << qualident(d) << " {" << endl;
            foreach( Declaration* field, t->subs )
            {
                if( field->kind == Declaration::Field )
                    hout << ws(1) << typeRef(field->type()) << " " << field->name << ";" << endl;
            }
            hout << "}";
            break;
#if 0
        case Type::Object: {
                hout << "struct " << qualident(d) << " {" << endl;
                hout << ws(0) << qualident(d) << "$Class$* class$;" << endl;
                QList<Declaration*> fields = t->getFieldList(true);
                foreach( Declaration* field, fields ) // TODO: was t->subs, but this cannot be correct
                {
                     hout << ws(0) << typeRef(field->type()) << " " << field->name << ";" << endl;
                }
                hout << "}";
            } break;
#endif
        case Type::NameRef:
            hout << typeRef(t->type());
            break;
        }
    }
    // typedef what name
    hout << " " << qualident(d);
}

static inline void dummy() {}

void CeeGen::Module(Ast::Declaration *module) {

    Ast::Declaration* d = module->link;
    if( d && d->kind == Ast::Declaration::Import ) {
        d = ImportList(d);
    }
    d = DeclSeq(d);
    if( d && d->body ) {
        StatBlock(d->body);
    }
}

void CeeGen::ImportDecl(Ast::Declaration* i) {
    Import import = i->data.value<Import>();
    if( import.moduleName != "SYSTEM" )
        bout << "#include \"" << import.moduleName << ".h\"" << endl;
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

Ast::Declaration* CeeGen::DeclSeq(Ast::Declaration* d) {
    while( d && (d->kind == Ast::Declaration::ConstDecl || d->kind == Ast::Declaration::TypeDecl ||
           d->kind == Ast::Declaration::VarDecl || d->kind == Ast::Declaration::Procedure ) ) {
        if( d->kind == Ast::Declaration::ConstDecl ) {
            while( d && d->kind == Ast::Declaration::ConstDecl ) {
                ConstDecl(d);
                d = d->next;
            }
        } else if( d->kind == Ast::Declaration::TypeDecl ) {
            while( d && d->kind == Ast::Declaration::TypeDecl ) {
                TypeDecl(d);
                d = d->next;
            }
        } else if( d->kind == Ast::Declaration::VarDecl ) {
            while( d && d->kind == Ast::Declaration::VarDecl ) {
                VarDecl(d);
                d = d->next;
            }
        } else if( d->kind == Ast::Declaration::Procedure ) {
            while( d && d->kind == Ast::Declaration::Procedure ) {
                ProcDecl(d);
                d = d->next;
            }
        } else
            invalid("DeclSeq", d->pos);
    }
    return d;
}

void CeeGen::ConstDecl(Ast::Declaration* d) {
    hout << "#define " << qualident(d);
    ConstExpr(d->expr);
    hout << endl;
}

void CeeGen::TypeDecl(Ast::Declaration* d) {
    printHelper(d->helper);
    if( d->visi > Declaration::Private )
        hout << "/* public */ ";
    typeDecl(d);
    hout << ";" << endl;
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
    bout << proc->data.toString().mid(5); // TODO
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

    procHeader(proc, true);
    hout << ";" << endl;

    procHeader(proc, false);
    bout << " {" << endl;
    Ast::Declaration* d = proc->link;
    curLevel++;
    d = DeclSeq(d);
    if( proc->body ) {
        if( proc->body->kind == Ast::Statement::Assembler )
            Assembler(proc);
        else
            StatBlock(proc->body);
    }
    curLevel--;
    bout << "}" << endl << endl;
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
    // TODO
    // Attributes();
    StatSeq(s->body);
}

void CeeGen::StatSeq(Ast::Statement*s) {
    s = Statement(s);
    while( s ) {
        s = Statement(s);
    }
}

void CeeGen::assig(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Assig);
    Expr(s->lhs);
    bout << " = ";
    Expr(s->rhs);
    bout << ";";
}

Ast::Statement *CeeGen::IfStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::If);
    bout << "if( ";
    Expr(s->rhs);
    bout << " ) { " << endl;
    StatSeq(s->body);
    bout << ws() << " }";
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::Elsif ) {
        s = s->getNext();
        bout << "else if( ";
        Expr(s->rhs);
        bout << " ) { " << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
        bout << ws() << " }";
    }
    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
        s = s->getNext();
        bout << "else { " << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
        bout << ws() << " }";
    }
    return s;
}

Ast::Statement *CeeGen::CaseStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Case);
    // TODO

    bout << "switch( ";
    Expr(s->rhs); // case
    bout << " ) { " << endl;
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
    {
        s = s->getNext();
        Ast::Expression* label = s->rhs;
        while( label )
        {
            bout << ws() << "case ";
            Expr(label); // TODO: Range
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
    return s;
}

void CeeGen::WhileStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::While);
    bout << "while( ";
    Expr(s->rhs);
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
    Expr(s->rhs);
    bout << " );";
}

Ast::Statement *CeeGen::ForStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::ForAssig);

    bout << "for( ";
    Ast::Statement* i = s;
    Expr(i->lhs); // i := val
    bout << " = ";
    Expr(i->rhs); // val
    bout << " ; ";
    Ast::Statement* body = s->body;

    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::ForToBy )
    {
        s = s->getNext();
        Expr(i->lhs);
        bout << " <= ";
        Expr(s->lhs); // to
        bout << "; ";
        if( s->rhs )
        {
            // TODO: i < 0
            Expr(i->lhs);
            bout << " += ";
            ConstExpr(s->rhs); // by
        }else
        {
            Expr(i->lhs);
            bout << "++";
        }
    }else
        invalid("For Statement", s->pos);
    bout << " ){" << endl;
    curLevel++;
    StatSeq(body);
    curLevel--;
    bout << ws() << "}";
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
    // TODO
    Expr(s->lhs);
    Expr(s->rhs);
    StatSeq(s->body);
    return s;
}

void CeeGen::ReturnStat(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Return);
    bout << "return";
    if( s->rhs ) {
        bout << " ";
        Expr(s->rhs);
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

bool CeeGen::ConstExpr(Ast::Expression* e) {
    return Expr(e);
}

bool CeeGen::Expr(Ast::Expression* e) {
    // Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    switch (e->kind) {
    case Ast::Expression::Lt:
    case Ast::Expression::Leq:
    case Ast::Expression::Geq:
    case Ast::Expression::In:
    case Ast::Expression::Neq:
    case Ast::Expression::Eq:
    case Ast::Expression::Gt:
    case Ast::Expression::Is:
        if( !relation(e) )
            return false;
        break;
    case Ast::Expression::Plus:
    case Ast::Expression::Minus:
    case Ast::Expression::Not:
        if( !unaryOp(e) )
            return false;
        break;
    case Ast::Expression::Add:
    case Ast::Expression::Sub:
    case Ast::Expression::Mul:
    case Ast::Expression::Fdiv:
    case Ast::Expression::Div:
    case Ast::Expression::Mod:
        if( !arithOp(e) )
            return false;
        break;
    case Ast::Expression::Or:
    case Ast::Expression::And:
        if( !logicOp(e) )
            return false;
        break;
    case Ast::Expression::DeclRef:
        if( !declRef(e) )
            return false;
        break;
    case Ast::Expression::Select:
        if( !select(e) )
            return false;
        break;
    case Ast::Expression::Index:
        if( !index(e) )
            return false;
        break;
    case Ast::Expression::Deref:
        if( !depointer(e) )
            return false;
        break;
    case Ast::Expression::Cast:
        if( !cast(e) )
            return false;
        break;
    case Ast::Expression::Call:
        if( !call(e) )
            return false;
        break;
    case Ast::Expression::Literal:
        if( !literal(e) )
            return false;
        break;
    case Ast::Expression::Constructor:
        if( !constructor(e) )
            return false;
        break;
    case Ast::Expression::Range:
        if( !range(e) )
            return false;
        break;
    case Ast::Expression::NameRef:
        if( !nameRef(e) )
            return false;
        break;
    default:
        Q_ASSERT(false);
        break;
    }
    return true;
}

bool CeeGen::relation(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool CeeGen::unaryOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    // TODO
    return false;
}

bool CeeGen::arithOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool CeeGen::logicOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool CeeGen::declRef(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

bool CeeGen::select(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool CeeGen::index(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool CeeGen::depointer(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    return false;
}

bool CeeGen::cast(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    return false;
}

bool CeeGen::call(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool CeeGen::literal(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

bool CeeGen::constructor(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Ast::Expression* elem = e->rhs;
    while( elem )
    {
        Expr(elem);
        elem = elem->next;
    }
    // TODO
    return false;
}

bool CeeGen::range(Ast::Expression *e)
{
    if( e->lhs == 0 || e->rhs == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return true;
}


bool CeeGen::nameRef(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

void CeeGen::call(Ast::Statement *s)
{
    Q_ASSERT(s); // TEST
    if( s == 0 )
        return;
    Expr(s->lhs);
    if( s->rhs )
        Expr(s->rhs);
    // TODO
}

QString CeeGen::genDedication()
{
    return "// this file was generated by " + QCoreApplication::applicationName() + " "
            + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

Type *CeeGen::deref(Ast::Type * t)
{
    if( t && t->kind == Type::NameRef )
        return deref(t->type());
    return t;
}

QByteArray CeeGen::typeRef(Type * t)
{
    if( t == 0 || t->kind == Type::NoType )
        return "void";
    t = t->deref();
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

    if( t->kind == Type::Pointer )
    {

        Type* to = deref(t->type());
        if( t->isPtrToOpenArray())
            // pointer to open arrays have no extra typedef, instead we use element_type*
            to = to->type();
        QByteArray prefix;
        if( to->isSOA() ) // array types are embedded in a struct
            prefix = "struct ";
        return prefix + typeRef(to) + "*";
    }else if( t->kind == Type::Array && t->expr == 0 )
        return typeRef(t->type()) + "*";
    else if( t->decl )
        return qualident(t->decl);
    else
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
    if( to2 && to2->isSOA() )
            out << "struct ";
    out << typeRef(ptr->type()) << "*";

}

void CeeGen::printHelper(Ast::Declaration * d)
{
    if( d == 0 )
        return;
    printHelper(d->next);
    Q_ASSERT(d->helper == 0);
    typeDecl(d);
    hout << ";" << endl;
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
    for( int i = 0; i < params.size(); i++ )
    {
        if( i != 0 )
            out << ", ";
        parameter(out, params[i]);
    }
    out << ")";
}
