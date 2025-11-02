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
#include "AoToken.h"
#include "AoBuilins.h"
#include <QtDebug>
using namespace Ao;
using namespace Ast;

#define _ALLOW_POINTER_BASE_TYPE
#define _ALLOW_RETURN_STRUCTURED_TYPES

static QByteArray SELF;

static inline void dummy() {}

Validator2::Validator2(Ast::AstModel *mdl, Ast::Importer *imp, bool haveXref):module(0),mdl(mdl),imp(imp),
    first(0),last(0),curObjectTypeDecl(0)
{
    Q_ASSERT(mdl);
    if( haveXref )
        first = last = new Symbol();
    SELF = Token::getSymbol("SELF");
}

Validator2::~Validator2()
{
    if( first )
        Symbol::deleteAll(first);
}

bool Validator2::validate(Ast::Declaration* module, const Ast::Import &import) {
    Q_ASSERT(module);

    if( module->validated )
        return true;

    errors.clear();
    if( first )
    {
        first->decl = module;
        first->kind = Symbol::Module;
        first->pos = module->pos;
        first->len = module->name.size();
    }

    markDecl(module);

    Ast::ModuleData md = module->data.value<Ast::ModuleData>();

    sourcePath = md.sourcePath;
    this->module = module;
    try
    {

        md.fullName = import.moduleName;
        module->data = QVariant::fromValue(md);
        Module(module);
    }catch(...)
    {
    }

    if( first )
        last->next = first; // close the circle

    module->validated = true;
    module->hasErrors = !errors.isEmpty();

    return errors.isEmpty();
}

void Validator2::invalid(const char* what, const RowCol& pos) {
    errors << Error(QString("invalid %1").arg(what),pos, sourcePath);
}

void Validator2::Module(Ast::Declaration *module) {
    Ast::Declaration* d = module->link;
    scopeStack.push_back(module);
    if( d && d->kind == Ast::Declaration::Import ) {
        d = ImportList(d);
	}
    d = DeclSeq(d);
    if( d && d->body ) {
        Body(d->body);
	}
    scopeStack.pop_back();

}

void Validator2::ImportDecl(Ast::Declaration* import) {
    if( import->outer->kind != Declaration::Module )
        error(import->pos,"imports are only supported on module level");

    Declaration* mod = 0;
    Import i = import->data.value<Import>();
    if( imp )
        mod = imp->loadModule(i);
    if( mod )
    {
        // loadModule returns the module decl; we just need the list of module elements:
        mod->hasSubs = true; // the module is used by at least this one
        import->link = mod->link;
        i.resolved = mod;
        import->data = QVariant::fromValue(i);
        markRef(mod, i.importedAt);
    }else
    {
        error(import->pos,QString("cannot import module '%1'").arg(i.moduleName.constData()));
        throw "";
    }
    import->validated = true;
}

Ast::Declaration* Validator2::ImportList(Ast::Declaration* import) {
    ImportDecl(import);
    import = import->next;
    while( import && import->kind == Ast::Declaration::Import ) {
        ImportDecl(import);
        import = import->next;
    }
    return import;
}

Ast::Declaration* Validator2::DeclSeq(Ast::Declaration* d) {

    while( d && (d->kind == Ast::Declaration::ConstDecl || d->kind == Ast::Declaration::TypeDecl ||
           d->kind == Ast::Declaration::VarDecl || d->kind == Ast::Declaration::LocalDecl || d->kind == Ast::Declaration::Procedure ) )
    {
        if( d->validated )
            continue;
        markDecl(d);
        d->validated = true;
        switch( d->kind )
        {
        case Ast::Declaration::ConstDecl:
            ConstDecl(d);
            break;
        case Ast::Declaration::TypeDecl:
            TypeDecl(d);
            break;
        case Ast::Declaration::VarDecl:
        case Ast::Declaration::LocalDecl:
            VarDecl(d);
            break;
        case Ast::Declaration::Procedure:
            ProcDecl(d);
            break;
        default:
            Q_ASSERT(false);
        }
        d = d->next;
	}
    return d;
}

void Validator2::ConstDecl(Ast::Declaration* d) {
    ConstExpr(d->expr);
    d->setType(d->expr->type());
    d->data = d->expr->val;
}

void Validator2::TypeDecl(Ast::Declaration* d) {
    if( d->type() && d->type()->kind == Type::Object )
        curObjectTypeDecl = d;

    Type(d->type());

    if( d->type() && d->type()->kind == Type::Object )
    {
        QList<Declaration*> bounds_ = boundProcs;
        boundProcs.clear();
        foreach(Declaration* proc, bounds_)
        {
            Q_ASSERT(proc->link && proc->link->kind == Declaration::ParamDecl &&
                     proc->link->receiver && proc->link->type() );
            Ast::Type* objectType = proc->link->type()->deref();
            Q_ASSERT(objectType == d->type());
            if( objectType->type() )
            {
                Declaration* super = findInType(objectType->type()->deref(),proc->name);
                if( super )
                {
                    super->hasSubs = true;
                    proc->super = super;
                    if( first )
                        subs[super].append(proc);
                }
            }
            // TODO visitScope(proc);
        }
        curObjectTypeDecl = 0;
    }
    if( d->type() && (d->type()->kind == Type::Object || d->type()->kind == Type::Record) )
    {
        if( d->type()->type() )
        {
            Ast::Type* baseType = d->type()->type();
            Q_ASSERT( baseType->kind == Type::NameRef );
            if( !baseType->validated || baseType->type() == 0 )
                return;
            Declaration* super = baseType->type()->deref()->decl;
            super->hasSubs = true;
            d->super = super;
            if( first )
                subs[super].append(d);
        }
    }
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
    // SysFlag();
    scopeStack.push_back(proc);

    if( curObjectTypeDecl )
    {
        proc->receiver = true;
        proc->outer = curObjectTypeDecl;
        // TODO boundProcs << proc; // do body later
        Declaration* self = new Declaration();
        self->kind = Declaration::ParamDecl;
        self->name = SELF;
        self->setType(curObjectTypeDecl->type());
        self->pos = curObjectTypeDecl->pos;
        self->receiver = true;
        self->next = proc->link;
        self->outer = proc;
        proc->link = self;
    }

    Ast::Declaration* d = proc->link;
    while( d && d->kind == Ast::Declaration::ParamDecl )
    {
        markDecl(d);
        d->validated = true;
        if( !d->receiver )
            Type(d->type());
        d = d->next;
    }
    if( proc->type() )
        Type(proc->type());
    d = DeclSeq(d);
    if( proc->body ) {
        if( proc->body->kind == Ast::Statement::Assembler )
            Assembler(proc);
        else
            Body(proc->body);
    }
    scopeStack.pop_back();
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
    if( t->type() && !t->type()->validated ) // optional base class
    {
        // resolve base objects if present
        // we have to disable curObjectType here because otherwise findInType is called with
        // curObjectType->type()->base which leads to infinite loop
        Declaration* tmp = curObjectTypeDecl;
        Q_ASSERT(curObjectTypeDecl != 0);
        curObjectTypeDecl = 0;
        resolveIfNamedType(t->type(), t->pos);
        curObjectTypeDecl = tmp;
    }

    foreach( Ast::Declaration* member, t->subs )
    {
        if( member->validated )
            continue;
        member->validated = true;
        markDecl(member);

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
    t->validated = false; // it was false when entering this proc, and resolveIfNamedType does nothing otherwise
    resolveIfNamedType(t, t->pos);
    return true;
}

bool Validator2::Type(Ast::Type* t) {
    if( t == 0 || t->validated )
        return true;
    t->validated = true;
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
    case Ast::Type::NoType:
        return true;
    default:
        invalid("Type", t->pos);
    }
    return true;
}

bool Validator2::FieldList(Ast::Type* t) {
    foreach( Ast::Declaration* f, t->subs )
    {
        if( f->validated )
            continue;
        f->validated = true;
        markDecl(f);
        Type(f->type());
    }
    return true;
}

void Validator2::Body(Ast::Statement* s) {
    if( s && s->kind == Ast::Statement::StatBlock )
        StatBlock(s);
    else
        StatSeq(s);
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

    Q_ASSERT( s->lhs->kind == Expression::DeclRef );
    Declaration* d = s->lhs->val.value<Declaration*>();
    Ast::Type* t = d->overrideType(s->rhs->type());
    StatSeq(s->body);
    d->overrideType(t);

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
        if( !depoint(e) )
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
        Expr(e->lhs);
        Expr(e->rhs);
        e->setType(e->lhs->type());
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
    e->setType(mdl->getType(Type::BOOLEAN));
    return false;
}

bool Validator2::unaryOp(Ast::Expression *e)
{
    Q_ASSERT(e); // TEST
    if( e == 0 )
        return false;

    Expr(e->lhs);

    if( e->lhs->type() == 0 )
        return true; // already reported

    Ast::Type* lhsT = deref(e->lhs->type());
    if( e->kind == Expression::Plus )
    {
        if( !lhsT->isNumber() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Minus )
    {
        if( !lhsT->isNumber() && !lhsT->isSet() )
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Not )
    {
        if( !lhsT->isBoolean()  )
            return error(e->pos, "unary '~' or 'NOT' not applicable to this type");
    }
    if( e->lhs == 0 )
        return true; // already reported
    e->setType(lhsT);
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
    e->setType(deref(e->lhs->type()));
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
    e->setType(mdl->getType(Type::BOOLEAN));
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
    if( e == 0 )
        return false;

    Expr(e->lhs);
    Expr(e->rhs);

    if( e->lhs == 0 || e->lhs->type() == 0 )
        return true;

    Ast::Type* lhsT = deref(e->lhs->type());
    if( lhsT->kind == Type::Pointer )
    {
        Expression* tmp = new Expression(Expression::Deref, e->lhs->pos );
        tmp->lhs = e->lhs;
        tmp->setType(lhsT->type());
        e->lhs = tmp;
        lhsT = deref(e->lhs->type());
    }
    if( lhsT->kind == Type::Record || lhsT->kind == Type::Object )
    {
        Declaration* ld = e->lhs->val.value<Declaration*>();
        if( ld && ld->kind == Declaration::TypeDecl )
            return error(e->lhs->pos,"selector expects a variable on the left side");
        Declaration* field = findInType(lhsT,e->val.toByteArray());
        if( field == 0 )
        {
            error(e->pos,QString("the record doesn't have a field named '%1'"). arg(e->val.toString()) );
            markUnref(e->val.toString().size(), e->pos);
            return false;
        }else
        {
            Symbol* s = markRef(field, e->pos);
            if( e->needsLval )
                s->kind = Symbol::Lval;
            e->val = QVariant::fromValue(field); // Field or bound proc
            e->setType(field->type());
        }
    }else
        return error(e->pos,"cannot select a field in given type");
    return true;
}

bool Validator2::index(Ast::Expression *e)
{
    if( e == 0 )
        return false;

    Expr(e->lhs);
    Expr(e->rhs);

    if( e->lhs == 0 || e->lhs->type() == 0 || e->rhs == 0 ||e->rhs->type() == 0 )
        return true;

    Ast::Type* lhsT = deref(e->lhs->type());
    Ast::Type* rhsT = deref(e->rhs->type());

    if( lhsT->kind == Type::Pointer )
    {
        Expression* tmp = new Expression(Expression::Deref, e->lhs->pos );
        tmp->lhs = e->lhs;
        tmp->setType(lhsT->type());
        e->lhs = tmp;
        lhsT = deref(e->lhs->type());
    }

    e->setType(lhsT->type());
    if( lhsT->kind == Type::Array )
    {
        if( !rhsT->isInteger() )
            return error(e->rhs->pos,"expecting an array index of integer type");
    }else
        return error(e->pos,"cannot index an element in given type");
    return true;
}

bool Validator2::depoint(Ast::Expression *e)
{
    if( e == 0 )
        return false;

    Expr(e->lhs);

    if( e->lhs == 0 || e->lhs->type() == 0 )
        return true;

    if( (e->lhs->kind == Expression::Select || e->lhs->kind == Expression::DeclRef) &&
            e->lhs->val.value<Declaration*>() &&
            e->lhs->val.value<Declaration*>()->kind == Declaration::Procedure )
    {
        Declaration* d = e->lhs->val.value<Declaration*>();
        if( !d->receiver )
            return error(e->pos,"super calls only supported for type-bound procedures");
        else
            e->kind = Expression::Super;
        return true;
    }
    // else
    Ast::Type* lhsT = deref(e->lhs->type());
    if( lhsT->kind == Type::Pointer
            || lhsT->kind == Type::Object // this happens in some places and is likely an error
            )
        e->setType(lhsT->type());
    else
        return error(e->pos,"can only dereference a pointer");
    return true;
}

bool Validator2::cast(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    // TODO
    Expr(e->lhs);
    return false;
}

bool Validator2::call(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    if( e->lhs && e->lhs->kind == Expression::Super )
        Expr(e->lhs->lhs);
    else
        Expr(e->lhs);

    bool supercall = false;
    Expression* lhs = e->lhs;
    if( lhs && lhs->kind == Expression::Super )
    {
        supercall = true;
        lhs = lhs->lhs;
    }
    if( lhs == 0 || lhs->type() == 0 ) // e->rhs is null in case there are no args
        return true;

    Declaration* proc = lhs->val.value<Declaration*>();
    if( proc && proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
        proc = 0;
    Ast::Type* procType = deref(lhs->type());
    if( procType && procType->kind != Type::Procedure )
        procType = 0;

    const DeclList formals = proc ? proc->getParams(true) : procType ? procType->subs : DeclList();
    Expression* arg = e->rhs;
    for(int i = 0; arg != 0; i++, arg = arg->next )
        Expr(arg);

    const bool isTypeCast = (proc == 0 || proc->kind != Declaration::Builtin) &&
            e->rhs &&
            e->rhs->kind == Expression::DeclRef &&
            e->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl;

    Ast::Type* lhsT = deref(lhs->type());
    if( isTypeCast )
    {
        if( supercall )
            return error(e->pos,"super call operator cannot be used here");
        e->kind = Expression::Cast;
        e->setType(deref(e->rhs->type()));
        if( e->rhs->next )
            return error(e->rhs->next->pos,"type guard requires a single argument");
        if( lhsT->kind == Type::Pointer )
            lhsT = deref(lhsT->type());
        if( lhsT->kind != Type::Record && lhsT->kind != Type::Object
                && lhsT->kind != Type::PTR // this is used allover in AOS/Sys
                && lhsT->kind != Type::ANY // this also happens
                )
            return error(e->rhs->pos,"a type guard is not supported for this type");
    }else
    {
        Ast::Type* ret = 0;
        if( proc )
        {
            if( proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
                return error(lhs->pos,"this expression cannot be called");
            ret = proc->type();
        }else if( lhsT->kind != Type::Procedure )
            return error(lhs->pos,"this expression cannot be called");
        else
            ret = lhsT->type();

        if( supercall && (proc == 0 || !proc->receiver || proc->super == 0) )
            return error(e->pos,"super call operator cannot be used here");

        ExpList actuals = Expression::getList(e->rhs);

        if( proc && proc->kind == Declaration::Builtin )
        {
            Builins bi(mdl);
            if( bi.checkArgs(proc->id, actuals, &ret, e->pos) )
            {
                // NOTE: no eval done here
            }else
                error(bi.errPos, bi.error);
        }else
        {
            if( actuals.size() != formals.size() )
                error(e->pos,"number of actual doesn't fit number of formal arguments");
            // NOTE: no param type checking here
        }
        if( ret )
            e->setType(ret);
    }
    return true;
}

bool Validator2::literal(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    // TODO
    return true;
}

bool Validator2::constructor(Ast::Expression *e)
{
    if( e == 0 )
        return false;

    e->setType(mdl->getType(Type::SET));
    Expression* comp = e->rhs;
    while( comp )
    {
        if( comp->kind == Expression::Constructor )
            return error(comp->pos,"component type not supported for SET constructors");
        Expr(comp);
        if( comp->type() && !deref(comp->type())->isInteger() )
            return error(comp->pos,"expecting integer compontents for SET constructors");
        comp = comp->next;
    }
    return true;
}

bool Validator2::nameRef(Ast::Expression * nameRef)
{
    if( nameRef == 0 )
        return false;

    Qualident q = nameRef->val.value<Qualident>();

    ResolvedQ r = find(q,nameRef->pos);
    RowCol pos = nameRef->pos;
    if( r.second == 0 )
    {
        if( r.first )
        {
            markRef(r.first, pos);
            pos.d_col += q.first.size() + 1;
        }
        markUnref(q.second.size(), pos);
        return true;
    }

    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    Symbol* s = markRef(r.second, pos);
    if( nameRef->needsLval )
        s->kind = Symbol::Lval;
    resolveIfNamedType(r.second->type(), nameRef->pos);

    nameRef->kind = Expression::DeclRef;
    nameRef->val = QVariant::fromValue(r.second);
    nameRef->setType(r.second->type());

    if( r.second->kind == Declaration::LocalDecl || r.second->kind == Declaration::ParamDecl )
    {
        if( !scopeStack.isEmpty() && r.second->outer && r.second->outer != scopeStack.back() )
        {
            nameRef->nonlocal = true;
            Q_ASSERT(r.second->outer->kind == Declaration::Procedure);
            r.second->outer->nonlocal = true;
            // accessing parameter or local variable of outer procedures
        }
    }
    return true;
}

void Validator2::call(Ast::Statement *s)
{
    if( s == 0 )
        return;
    if( s->lhs->kind != Expression::Call )
    {
        // calling without args; so add an explicit call expression for consistency
        Expression* call = new Expression(Expression::Call, s->lhs->pos);
        call->lhs = s->lhs;
        s->lhs = call;
    }
    Expr(s->lhs);
    Q_ASSERT( s->rhs == 0 );
}

Xref Validator2::takeXref()
{
    Xref res;
    if( first == 0 )
        return res;
    res.uses = xref;
    res.syms = first;
    res.subs = subs;
    xref.clear();
    subs.clear();
    first = last = new Symbol();
    return res;
}

bool Validator2::error(const RowCol& pos, const QString& msg) const
{
    errors << Error(msg, pos, sourcePath);
    return false;
}

void Validator2::markDecl(Ast::Declaration* d)
{
    if( first == 0 )
        return;
    Symbol* s = new Symbol();
    s->kind = Symbol::Decl;
    s->decl = d;
    s->pos = d->pos;
    s->len = d->name.size();
    xref[d].append(s);
    last->next = s;
    last = last->next;
}

Symbol* Validator2::markRef(Declaration* d, const RowCol& pos)
{
    if( first == 0 )
        return 0;
    Symbol* s = new Symbol();
    s->kind = Symbol::DeclRef;
    s->decl = d;
    s->pos = pos;
    s->len = d->name.size();
    xref[d].append(s);
    last->next = s;
    last = last->next;
    return s;
}

Symbol*Validator2::markUnref(int len, const RowCol& pos)
{
    if( first == 0 )
        return 0;
    Symbol* s = new Symbol();
    s->kind = Symbol::DeclRef;
    s->pos = pos;
    s->len = len;
    last->next = s;
    last = last->next;
    return s;
}

Type *Validator2::deref(Ast::Type *t)
{
    // never returns zero
    if( t == 0 )
        return mdl->getType(Type::NoType);
    if( t->kind == Type::NameRef )
    {
        if( !t->validated )
        {
            // This is necessary because declarations can follow their use, but all so far unvalidated NameRefs
            // must be local, assuming that all NameRefs in imported modules must already have been validated at
            // this point.
#ifdef _DEBUG_ // TODO
            Qualident q = t->quali;
            if( q.first && q.first.constData() != module->name.constData() )
                qWarning() << "Validator::deref: unvalidated quali" << q.first << "." << q.second << "in"
                           << module->name;
#endif
            resolveIfNamedType(t, t->pos);
        }
        return deref(t->type());
    }else
        return t;
}

void Validator2::resolveIfNamedType(Ast::Type *nameRef, const RowCol& where)
{
    if( nameRef == 0 || nameRef->kind != Type::NameRef)
        return;
    if( nameRef->validated )
        return;
    Q_ASSERT(nameRef->quali);
    Q_ASSERT(nameRef->expr == 0);
    Qualident q = *nameRef->quali;
    ResolvedQ r = find(q, where);
    if(r.second == 0)
        return;
    RowCol pos = nameRef->pos;
    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    markRef(r.second, pos);
    nameRef->validated = true;
    nameRef->setType(r.second->type());
    if( r.second->kind != Declaration::TypeDecl )
    {
        error(where,"identifier doesn't refer to a type declaration");
        return;
    }

    resolveIfNamedType(r.second->type(), where);
}

Validator2::ResolvedQ Validator2::find(const Ast::Qualident &q, RowCol pos)
{
    if( scopeStack.isEmpty() )
        return ResolvedQ();

    ResolvedQ res;
    if( !q.first.isEmpty() )
    {
        Declaration* import = scopeStack.back()->find(q.first);

        if( import == 0 || import->kind != Declaration::Import )
        {
            error(pos,"identifier doesn't refer to an imported module");
            markUnref(q.first.size(), pos);
            return ResolvedQ();
        }
        if( !import->validated )
            ImportDecl(import);
        res.first = import;
        Declaration* member = mdl->findDeclInImport(import,q.second);
        pos.d_col += q.first.size() + 1;
        if( member == 0 )
        {
            error(pos,QString("declaration '%1' not found in imported module '%2'").
                  arg(q.second.constData()).arg(q.first.constData()) );
            markUnref(q.second.size(), pos);
        }else
        {
            if( member->visi == Declaration::Private )
                error(pos,QString("cannot access private declaration '%1' from module '%2'").
                      arg(q.second.constData()).arg(q.first.constData()) );
            res.second = member;
        }
    }else
    {
        Declaration* d = 0;

        Declaration* nested = scopeStack.back();
        while( d == 0 && nested && nested->kind != Declaration::Module )
        {
            d = nested->find(q.second, false); // check for local vars
            nested = nested->outer;
        }

        if( d == 0 && curObjectTypeDecl )
            d = findInType(deref(curObjectTypeDecl->type()),q.second);
        if( d == 0 )
            d = module->find(q.second,false);
        if( d == 0 )
            d = mdl->findDecl(q.second); // built-ins
        if( d == 0 )
        {
            error(pos,QString("declaration '%1' not found").arg(q.second.constData()));
            markUnref(q.second.size(), pos);
        }
        res.second = d;
    }
    return res;
}

Declaration *Validator2::findInType(Ast::Type * t, const QByteArray &field)
{
    if( t == 0 )
        return 0;
#if 0
    Type* base = deref(t->base);
    while( base && base->base && base->base->kind == Type::NameRef && !base->base->validated )
        base = deref(base->base);
    return t->find(field);
#else
    Declaration* res = t->find(field, false);
    QList<Ast::Type*> done;
    while( res == 0 && (t->kind == Type::Record || t->kind == Type::Object) && t->type() )
    {
        Ast::Type* super = deref(t->type());
        if( super->kind == Type::Pointer && super->type() )
            super = deref(super->type());
        res = super->find(field, false);
        t = super;
        if( done.contains(t) ) // in OberonSystem3_Native_2.2_1997 there is a cycle over TextGadgets.FrameDesc, TextGadgets0.FrameDesc and Gadgets.ViewDesc
            break;
        done.append(t);
    }
    return res;
#endif
}

