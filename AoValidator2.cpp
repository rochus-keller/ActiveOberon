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

#include "AoValidator2.h"
using namespace Ao;

bool Validator2::validate(Ast::Declaration* module) {
	errors.clear();
    Ast::ModuleData md = module->data.value<Ast::ModuleData>();
    sourcePath = md.sourcePath;
    Module(module);
    return errors.isEmpty();
}

void Validator2::invalid(const char* what, const RowCol& pos) {
    errors << Error(QString("invalid %1").arg(what),pos, sourcePath);
}

static inline void dummy() {}

void Validator2::Module(Ast::Declaration *module) {

    Ast::Declaration* d = module->link;
    if( d && d->kind == Ast::Declaration::Import ) {
        d = ImportList(d);
	}
    d = DeclSeq(d);
    if( d && d->body ) {
        Body(d->body);
	}
}

void Validator2::ImportDecl(Ast::Declaration* import) {
    // TODO
}

Ast::Declaration* Validator2::ImportList(Ast::Declaration* import) {
    ImportDecl(import);
    while( import->next && import->next->kind == Ast::Declaration::Import ) {
        import = import->next;
        ImportDecl(import);
	}
    return import;
}

Ast::Declaration* Validator2::DeclSeq(Ast::Declaration* d) {
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

void Validator2::ConstDecl(Ast::Declaration* d) {
    // TODO
    if( !ConstExpr(d->expr) )
        return;
}

void Validator2::TypeDecl(Ast::Declaration* d) {
    // TODO
    Type(d->type());
}

void Validator2::VarDecl(Ast::Declaration* d) {
    // TODO
    Type(d->type());
}

void Validator2::Assembler(Ast::Declaration* proc) {
    //expect(Tok_CODE, false, "Assembler");
    // TODO
}

void Validator2::ProcDecl(Ast::Declaration * proc) {
    // TODO
    // SysFlag();

    Ast::Declaration* d = proc->link;
    while( d && d->kind == Ast::Declaration::ParamDecl )
    {
        if( !d->receiver )
            Type(d->type());
        d = d->next;
    }
    d = DeclSeq(d);
    if( proc->body ) {
        if( proc->body->kind == Ast::Statement::Assembler )
            Assembler(proc);
        else
            Body(proc->body);
    }
}

void Validator2::SysFlag() {
    // TODO
}

bool Validator2::ArrayType(Ast::Type* t) {
    // TODO
    // SysFlag();
    if( t->expr )
        ConstExpr(t->expr);
    return Type(t->type());
}

bool Validator2::RecordType(Ast::Type* t) {
    // TODO
    // SysFlag();
    if( t->type() )
        Type(t->type());  // optional base class
    return FieldList(t);
}

bool Validator2::PointerType(Ast::Type* t) {
    // TODO
    // SysFlag();
    return Type(t->type());
}

bool Validator2::ObjectType(Ast::Type* t) {
    // TODO
    // SysFlag();
    if( t->type() )
        Type(t->type());  // optional base class

    foreach( Ast::Declaration* member, t->subs )
    {
        switch( member->kind )
        {
        case Ast::Declaration::Field:
            VarDecl(member);
            break;
        case Ast::Declaration::Procedure:
            ProcDecl(member);
            break;
        default:
            invalid("ObjectDeclSeq", t->pos);
            break;
        }
    }
    return true;
}

bool Validator2::ProcedureType(Ast::Type* t) {
    // TODO
    // SysFlag();
    // Attributes();
    int ok = 1;
    foreach( Ast::Declaration* param, t->subs )
    {
        if( param->kind == Ast::Declaration::ParamDecl )
            ok &= Type(param->type());
    }
    if( t->type() && t->type()->kind != Ast::Type::NoType )
        ok &= Type(t->type());
    return ok;
}

bool Validator2::AliasType(Ast::Type* t) {
    // TODO Qualident();
    return true;
}

bool Validator2::Type(Ast::Type* t) {
    switch( t->kind )
    {
    case Ast::Type::NameRef:
        return AliasType(t);
    case Ast::Type::Pointer:
        return PointerType(t);
    case Ast::Type::Procedure:
        return ProcedureType(t);
    case Ast::Type::Array:
        return ArrayType(t);
    case Ast::Type::Record:
        return RecordType(t);
    case Ast::Type::Object:
        return ObjectType(t);
    default:
        invalid("Type", t->pos);
    }
    return true;
}

bool Validator2::FieldList(Ast::Type* t) {
    foreach( Ast::Declaration* f, t->subs )
    {
        if( !Type(f->type()) )
            return false;
    }
    return true;
}

void Validator2::Body(Ast::Statement* s) {
    StatBlock(s);
}

void Validator2::Attributes() {
    // TODO
}

void Validator2::StatBlock(Ast::Statement* s) {
    // TODO
    // Attributes();
    StatSeq(s->body);
}

void Validator2::StatSeq(Ast::Statement*s) {
    s = Statement(s);
    while( s ) {
        s = Statement(s);
	}
}

void Validator2::assig(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Assig);
    // TODO
    Expr(s->lhs);
    Expr(s->rhs);
}

Ast::Statement *Validator2::IfStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::If);
    // TODO
    Expr(s->rhs); // if
    StatSeq(s->body);
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::Elsif ) {
        s = s->getNext();
        Expr(s->rhs);
        StatSeq(s->body);
    }
    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
        s = s->getNext();
        StatSeq(s->body);
    }
    return s;
}

Ast::Statement *Validator2::CaseStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Case);
    // TODO

    Expr(s->rhs); // case

    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
    {
        s = s->getNext();
        Ast::Expression* label = s->rhs;
        while( label )
        {
            Expr(label);
            label = label->next;
        }
        StatSeq(s->body);
    }
    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::Else ) {
        s = s->getNext();
        StatSeq(s->body);
    }
    return s;
}

void Validator2::WhileStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::While);
    // TODO
    Expr(s->rhs);
    StatSeq(s->body);
}

void Validator2::RepeatStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Repeat);
    // TODO
    StatSeq(s->body);
    Expr(s->rhs);
}

Ast::Statement *Validator2::ForStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::ForAssig);
    // TODO
    Expr(s->lhs); // i := val
    Expr(s->rhs); // val
    Ast::Statement* body = s->body;

    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::ForToBy )
    {
        s = s->getNext();
        Expr(s->lhs); // to
        if( s->rhs )
            ConstExpr(s->rhs); // by
    }else
        invalid("For Statement", s->pos);

    StatSeq(body);
    return s;
}

void Validator2::LoopStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Loop);
    StatSeq(s->body);
}

Ast::Statement *Validator2::WithStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::With);
    Expr(s->lhs);
    Expr(s->rhs);
    StatSeq(s->body);
    return s;
}

void Validator2::ReturnStat(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Return);
    // TODO
    if( s->rhs ) {
        Expr(s->rhs);
	}
}

Ast::Statement* Validator2::Statement(Ast::Statement* s) {
    if( s == 0 )
        return 0;
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
    if( s )
        s = s->getNext();
    return s;
}

bool Validator2::ConstExpr(Ast::Expression* e) {
    // TODO
    return Expr(e);
}

bool Validator2::Expr(Ast::Expression* e) {
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
        if( !deref(e) )
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

bool Validator2::relation(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool Validator2::unaryOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    // TODO
    return false;
}

bool Validator2::arithOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool Validator2::logicOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    // TODO
    return false;
}

bool Validator2::declRef(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

bool Validator2::select(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool Validator2::index(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool Validator2::deref(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    return false;
}

bool Validator2::cast(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    return false;
}

bool Validator2::call(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    Expr(e->rhs);
    return false;
}

bool Validator2::literal(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

bool Validator2::constructor(Ast::Expression *e)
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

bool Validator2::nameRef(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;
    // TODO
    return false;
}

void Validator2::call(Ast::Statement *s)
{
    Q_ASSERT(s); // TEST
    if( s == 0 )
        return;
    Expr(s->lhs);
    if( s->rhs )
        Expr(s->rhs);
    // TODO
}


