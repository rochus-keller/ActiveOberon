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

#include "AoValidator.h"
#include "AoToken.h"
#include <limits>
#include <QtDebug>
using namespace Ao;
using namespace Ast;

static QByteArray SELF;

Validator::Validator(AstModel* mdl, Importer* imp, bool haveXref):module(0),mdl(mdl),imp(imp),
    first(0),last(0),curObjectTypeDecl(0)
{
    Q_ASSERT(mdl);
    if( haveXref )
        first = last = new Symbol();
    SELF = Token::getSymbol("SELF");
}

Validator::~Validator()
{
    if( first )
        Symbol::deleteAll(first);
}

bool Validator::validate(Declaration* module, const Import& import)
{
    Q_ASSERT(module);
    this->module = module;
    if( first )
    {
        first->decl = module;
        first->kind = Symbol::Module;
        first->pos = module->pos;
        first->len = module->name.size();
    }

    markDecl(module);

    Ast::ModuleData md = module->data.value<Ast::ModuleData>();

    try
    {

        md.fullName = import.moduleName;
        module->data = QVariant::fromValue(md);

        visitScope(module);
    }catch(...)
    {
    }

    if( first )
        last->next = first; // close the circle

    return errors.isEmpty();
}

Xref Validator::takeXref()
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

void Validator::error(const RowCol& pos, const QString& msg) const
{
    ModuleData md = module->data.value<ModuleData>();
    errors << Error(msg, pos, md.sourcePath);
}

void Validator::markDecl(Ast::Declaration* d)
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

Symbol* Validator::markRef(Declaration* d, const RowCol& pos)
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

Symbol*Validator::markUnref(int len, const RowCol& pos)
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

void Validator::visitScope(Declaration* scope)
{
    scopeStack.push_back(scope);
    Declaration* cur = scope->link;
    while( cur )
    {
        // all decls including procedure headers
        visitDecl(cur);
        cur = cur->getNext();
    }

    cur = scope->link;
    while( cur )
    {
        if( cur->kind == Declaration::Procedure )
            visitScope(cur);
        cur = cur->getNext();
    }

    visitBody(scope->body);
    scopeStack.pop_back();
}

void Validator::visitDecl(Declaration* d)
{
    if( d->validated )
        return;
    d->validated = true;
    markDecl(d);
    switch( d->kind )
    {
    case Declaration::TypeDecl:
        if( d->type && d->type->kind == Type::Object )
            curObjectTypeDecl = d;
        visitType(d->type);
        if( d->type && d->type->kind == Type::Object )
        {
            QList<Declaration*> bounds_ = boundProcs;
            boundProcs.clear();
            foreach(Declaration* proc, bounds_)
            {
                Q_ASSERT(proc->link && proc->link->kind == Declaration::ParamDecl &&
                         proc->link->receiver && proc->link->type );
                Type* objectType = proc->link->type->deref();
                Q_ASSERT(objectType == d->type);
                if( objectType->base )
                {
                    Declaration* super = findInType(objectType->base->deref(),proc->name);
                    if( super )
                    {
                        super->hasSubs = true;
                        proc->super = super;
                        if( first )
                            subs[super].append(proc);
                    }
                }
                visitScope(proc);
            }
            curObjectTypeDecl = 0;
        }
        if( d->type && (d->type->kind == Type::Object || d->type->kind == Type::Record) )
        {
            if( d->type->base )
            {
                Type* baseType = d->type->base;
                Q_ASSERT( baseType->kind == Type::NameRef );
                if( !baseType->validated || baseType->base == 0 )
                    break;
                Declaration* super = baseType->base->deref()->decl;
                super->hasSubs = true;
                d->super = super;
                if( first )
                    subs[super].append(d);
            }
        }
        break;
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
    case Declaration::ParamDecl:
    case Declaration::Field:
        visitType(d->type);
        break;
    case Declaration::ConstDecl:
        if(d->expr)
        {
            visitExpr(d->expr);
            d->type = d->expr->type;
            d->data = d->expr->val;
        }else if( d->type == 0 )
            d->type = mdl->getType(Type::NoType);
        else
            visitType(d->type);
        break;
    case Declaration::Import:
        visitImport(d);
        break;
    case Declaration::Procedure: {
        // only header is evaluated here
        visitType(d->type);
        const QList<Declaration*> params = d->getParams(true);
        for( int i = 0; i < params.size(); i++ )
            visitDecl(params[i]);
        break;
        }
    default:
        break;
    }
}

void Validator::visitImport(Declaration* import)
{

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

void Validator::visitBody(Statement* s)
{
    // already in the right scope when called

    while( s )
    {
        if( s->kind == Statement::Call && s->lhs->kind != Expression::Call )
        {
            Expression* call = new Expression(Expression::Call, s->lhs->pos);
            call->lhs = s->lhs;
            s->lhs = call;
        }

        visitExpr(s->lhs);
        visitExpr(s->rhs);
        if( s->kind == Statement::With )
        {
            Q_ASSERT( s->lhs->kind == Expression::DeclRef );
            Declaration* d = s->lhs->val.value<Declaration*>();
            Type* t = d->type;
            d->type = s->rhs->type;
            visitBody(s->body);
            d->type = t;
        }else
            visitBody(s->body);

        s = s->getNext();
    }
}

void Validator::visitExpr(Expression* e, bool followNext)
{
    if( e == 0 )
        return;

    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not: // Unary
        visitExpr(e->lhs);
        unaryOp(e);
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In: // Relation
    case Expression::Is: // Relation
    case Expression::Add:
    case Expression::Sub:
    case Expression::Or: // AddOp
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::And: // MulOp
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        binaryOp(e);
        break;
    case Expression::Literal:
    case Expression::DeclRef:
    case Expression::Cast:
        resolveIfNamedType(e->type);
        break;
    case Expression::NameRef:
        resolveDesig(e);
        break;
    case Expression::Select:
        visitExpr(e->lhs);
        selectOp(e);
        break;
    case Expression::Deref:
        visitExpr(e->lhs);
        derefOp(e);
        break;
    case Expression::Index:
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        indexOp(e);
        break;
    case Expression::Call:
        if( e->lhs && e->lhs->kind == Expression::Super )
            visitExpr(e->lhs->lhs);
        else
            visitExpr(e->lhs); // proc
        callOp(e);
        break;
    case Expression::Super:
        error(e->pos,"super call cannot be used here");
        break;
    case Expression::Constructor:
        constructor(e);
        break;
    case Expression::Range: // used in Constructor and LabelRange (as ConstExpr)
        visitExpr(e->lhs);
        visitExpr(e->rhs);
        e->type = e->lhs->type;
        break;
    case Expression::Invalid:
    default:
        Q_ASSERT(false);
        break;
    }
    if( followNext && e->next )
        visitExpr(e->next);
}

void Validator::unaryOp(Expression* e)
{
    if( e->lhs->type == 0 )
        return; // already reported
    Type* lhsT = deref(e->lhs->type);
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
        return; // already reported
    e->type = lhsT;
}

void Validator::binaryOp(Expression* e)
{
    if( e->lhs == 0 || e->rhs == 0 || e->lhs->type == 0 || e->rhs->type == 0 )
        return; // already reported

    // NOTE: we don't do real type checking here because we assume that the code already passed the original compiler
    switch( e->kind )
    {
    // Arith
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::Add:
    case Expression::Sub:
    case Expression::And:
    case Expression::Or:
        e->type = deref(e->lhs->type);
        break;
    // Relation
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In:
    case Expression::Is:
        e->type = mdl->getType(Type::BOOLEAN);
        break;
    default:
        Q_ASSERT(false);
    }
}

void Validator::selectOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 )
        return;

    Type* lhsT = deref(e->lhs->type);
    if( lhsT->kind == Type::Pointer )
    {
        Expression* tmp = new Expression(Expression::Deref, e->lhs->pos );
        tmp->lhs = e->lhs;
        tmp->type = lhsT->base;
        e->lhs = tmp;
        lhsT = deref(e->lhs->type);
    }
    if( lhsT->kind == Type::Record || lhsT->kind == Type::Object )
    {
        Declaration* ld = e->lhs->val.value<Declaration*>();
        if( ld && ld->kind == Declaration::TypeDecl )
            error(e->lhs->pos,"selector expects a variable on the left side");
        Declaration* field = findInType(lhsT,e->val.toByteArray());
        if( field == 0 )
        {
            error(e->pos,QString("the record doesn't have a field named '%1'"). arg(e->val.toString()) );
            markUnref(e->val.toString().size(), e->pos);
        }else
        {
            Symbol* s = markRef(field, e->pos);
            if( e->needsLval )
                s->kind = Symbol::Lval;
            e->val = QVariant::fromValue(field); // Field or bound proc
            e->type = field->type;
        }
    }else
        error(e->pos,"cannot select a field in given type");
}

void Validator::derefOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 )
        return;

    if( (e->lhs->kind == Expression::Select || e->lhs->kind == Expression::DeclRef) &&
            e->lhs->val.value<Declaration*>() &&
            e->lhs->val.value<Declaration*>()->kind == Declaration::Procedure )
    {
        Declaration* d = e->lhs->val.value<Declaration*>();
        if( !d->receiver )
            error(e->pos,"super calls only supported for type-bound procedures");
        else
            e->kind = Expression::Super;
        return;
    }
    // else
    Type* lhsT = deref(e->lhs->type);
    if( lhsT->kind == Type::Pointer
            || lhsT->kind == Type::Object // this happens in some places and is likely an error
            )
        e->type = lhsT->base;
    else
        error(e->pos,"can only dereference a pointer");
}

void Validator::indexOp(Expression* e)
{
    if( e->lhs == 0 || e->lhs->type == 0 || e->rhs == 0 ||e->rhs->type == 0 )
        return;

    Type* lhsT = deref(e->lhs->type);
    Type* rhsT = deref(e->rhs->type);

    if( lhsT->kind == Type::Pointer )
    {
        Expression* tmp = new Expression(Expression::Deref, e->lhs->pos );
        tmp->lhs = e->lhs;
        tmp->type = lhsT->base;
        e->lhs = tmp;
        lhsT = deref(e->lhs->type);
    }

    e->type = lhsT->base;
    if( lhsT->kind == Type::Array )
    {
        if( !rhsT->isInteger() )
            error(e->rhs->pos,"expecting an array index of integer type");
    }else
        error(e->pos,"cannot index an element in given type");
}

void Validator::callOp(Expression* e)
{
    bool supercall = false;
    Expression* lhs = e->lhs;
    if( lhs && lhs->kind == Expression::Super )
    {
        supercall = true;
        lhs = lhs->lhs;
    }
    if( lhs == 0 || lhs->type == 0 ) // e->rhs is null in case there are no args
        return;

    Declaration* proc = lhs->val.value<Declaration*>();
    if( proc && proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
        proc = 0;
    Type* procType = deref(lhs->type);
    if( procType && procType->kind != Type::Procedure )
        procType = 0;

    const DeclList formals = proc ? proc->getParams(true) : procType ? procType->subs : DeclList();
    Expression* arg = e->rhs;
    for(int i = 0; arg != 0; i++, arg = arg->next )
        visitExpr(arg, false);

    const bool isTypeCast = (proc == 0 || proc->kind != Declaration::Builtin) &&
            e->rhs &&
            e->rhs->kind == Expression::DeclRef &&
            e->rhs->val.value<Declaration*>()->kind == Declaration::TypeDecl;

    Type* lhsT = deref(lhs->type);
    if( isTypeCast )
    {
        if( supercall )
            return error(e->pos,"super call operator cannot be used here");
        e->kind = Expression::Cast;
        e->type = deref(e->rhs->type);
        if( e->rhs->next )
            return error(e->rhs->next->pos,"type guard requires a single argument");
        if( lhsT->kind == Type::Pointer )
            lhsT = deref(lhsT->base);
        if( lhsT->kind != Type::Record && lhsT->kind != Type::Object
                && lhsT->kind != Type::PTR // this is used allover in AOS/Sys
                && lhsT->kind != Type::ANY // this also happens
                )
            return error(e->rhs->pos,"a type guard is not supported for this type");
    }else
    {
        if( proc )
        {
            if( proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
                return error(lhs->pos,"this expression cannot be called");
            e->type = proc->type;
        }else if( lhsT->kind != Type::Procedure )
            return error(lhs->pos,"this expression cannot be called");
        else
            e->type = lhsT->base;

        if( supercall && (proc == 0 || !proc->receiver || proc->super == 0) )
            return error(e->pos,"super call operator cannot be used here");

        ExpList actuals = Expression::getList(e->rhs);

        if( proc && proc->kind == Declaration::Builtin )
        {
            if( checkBuiltinArgs(proc->id, actuals, &e->type, e->pos) )
            {
                // NOTE: no eval done here
            }
        }else
        {
            if( actuals.size() != formals.size() )
                error(e->pos,"number of actual doesn't fit number of formal arguments");

#if 0
            // NOTE: no type checking here
            for( int i = 0; i < formals.size() && i < actuals.size(); i++ )
            {
                if( !paramCompat(formals[i],actuals[i]) )
                {
                    paramCompat(formals[i],actuals[i]); // TEST
                    error(actuals[i]->pos, "actual argument not compatible with formal parameter");
                }
            }
#endif
        }
    }
}

void Validator::constructor(Expression* e)
{
    e->type = mdl->getType(Type::SET);
    Expression* comp = e->rhs;
    while( comp )
    {
        if( comp->kind == Expression::Constructor )
            return error(comp->pos,"component type not supported for SET constructors");
        visitExpr(comp, false);
        if( comp->type && !deref(comp->type)->isInteger() )
            return error(comp->pos,"expecting integer compontents for SET constructors");
        comp = comp->next;
    }
}

Type* Validator::deref(Type* t)
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
#ifdef _DEBUG
            Qualident q = t->decl->data.value<Qualident>();
            if( t->decl->getModule() != module )
                qWarning() << "Validator::deref: unvalidated quali" << q.first << "." << q.second << "in"
                           << module->name;
#endif
            resolveIfNamedType(t);
        }
        return deref(t->base);
    }else
        return t;
}

void Validator::resolveIfNamedType(Type* nameRef)
{
    if( nameRef == 0 || nameRef->kind != Type::NameRef)
        return;
    if( nameRef->validated )
        return;
    Q_ASSERT(nameRef->decl);
    Q_ASSERT(nameRef->expr == 0);
    Qualident q = nameRef->decl->data.value<Qualident>();
    ResolvedQ r = find(q, nameRef->decl->pos);
    if(r.second == 0)
        return;
    RowCol pos = nameRef->decl->pos;
    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    markRef(r.second, pos);
    nameRef->validated = true;
    nameRef->base = r.second->type;
    if( r.second->kind != Declaration::TypeDecl )
        return error(nameRef->decl->pos,"identifier doesn't refer to a type declaration");

    resolveIfNamedType(r.second->type);
}

void Validator::resolveDesig(Expression* nameRef)
{
    Q_ASSERT( nameRef && nameRef->kind == Expression::NameRef );
    Q_ASSERT( !scopeStack.isEmpty() );

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
        return;
    }

    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }
    Symbol* s = markRef(r.second, pos);
    if( nameRef->needsLval )
        s->kind = Symbol::Lval;
    resolveIfNamedType(r.second->type);

    nameRef->kind = Expression::DeclRef;
    nameRef->val = QVariant::fromValue(r.second);
    nameRef->type = r.second->type;

    if( r.second->kind == Declaration::LocalDecl || r.second->kind == Declaration::ParamDecl )
    {
        if( r.second->outer != scopeStack.back() )
            nameRef->nonlocal = true;
#if 0
            qWarning() << "accessing parameter or local variable of outer procedures in"
                       << module->name << nameRef->pos.d_row;
#endif
    }
}

Validator::ResolvedQ Validator::find(const Qualident& q, RowCol pos)
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
            visitImport(import);
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
            d = findInType(deref(curObjectTypeDecl->type),q.second);
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

Declaration*Validator::findInType(Type* t, const QByteArray& field)
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
    while( res == 0 && (t->kind == Type::Record || t->kind == Type::Object) && t->base )
    {
        Type* super = deref(t->base);
        if( super->kind == Type::Pointer && super->base )
            super = deref(super->base);
        res = super->find(field, false);
        t = super;
    }
    return res;
#endif
}

void Validator::visitType(Type* type)
{
    if( type == 0 || type->validated )
        return;
    type->validated = true;
    switch( type->kind )
    {
    case Type::Pointer:
        visitType(type->base);
        break;
    case Type::Record:
    case Type::Object:
    case Type::Procedure:
        if( type->kind == Type::Object && type->base && !type->base->validated )
        {
            // resolve base objects if present
            // we have to disable curObjectType here because otherwise findInType is called with
            // curObjectType->type->base which leads to infinite loop
            Declaration* tmp = curObjectTypeDecl;
            Q_ASSERT(curObjectTypeDecl != 0);
            curObjectTypeDecl = 0;
            resolveIfNamedType(type->base);
            curObjectTypeDecl = tmp;
        }else
            visitType(type->base);
        foreach( Declaration* d, type->subs )
        {
            if( d->kind == Declaration::Procedure && curObjectTypeDecl )
            {
                d->receiver = true;
                d->outer = curObjectTypeDecl;
                boundProcs << d; // do body later
                Declaration* self = new Declaration();
                self->kind = Declaration::ParamDecl;
                self->name = SELF;
                self->type = curObjectTypeDecl->type;
                self->pos = curObjectTypeDecl->pos;
                self->receiver = true;
                self->next = d->link;
                d->link = self;
            }
            visitDecl(d);
        }
        if( type->kind == Type::Record && type->base )
        {
            Type* base = deref(type->base);
            if( base->kind == Type::Pointer )
                base = deref(base->base);
            if( base->kind != Type::Record &&
                    base->kind != Type::Object ) // this happens indeed
                error(type->base->decl->pos,"invalid base record");
        }
        if( type->kind == Type::Object && type->base )
        {
            Type* base = deref(type->base);
            if( base->kind == Type::Pointer )
                base = deref(base->base);
            if( base->kind == Type::NameRef )
                base = deref(base);
            if( base->kind != Type::Object &&
                    base->kind != Type::Record ) // this happens indeed
                error(type->base->decl->pos,"invalid base object");
            // TODO: connect to super methods
        }
        break;
    case Type::Array:
        visitExpr(type->expr );
        if( type->kind == Type::Array )
        {
            if( type->expr )
            {
                if( !type->expr->isConst() || !deref(type->expr->type)->isInteger() )
                    error(type->expr->pos,"expecting constant integer expression");
                else
                {
                    const qint64 tmp = type->expr->val.toLongLong();
                    if( tmp < 0 || tmp > std::numeric_limits<quint32>::max() )
                        error(type->expr->pos,"invalid array length");
                    else
                        type->len = tmp;
                }
            }
        }else if( type->expr->kind == Expression::DeclRef )
        {
            if( type->expr->val.value<Declaration*>()->kind != Declaration::TypeDecl )
                error(type->expr->pos,"expecting a type name");
        }
        visitType(type->base);
        break;
    case Type::NameRef:
        type->validated = false; // it was false when entering this proc, and resolveIfNamedType does nothing otherwise
        resolveIfNamedType(type);
        break;
    default:
        break;
    }
}

static inline bool expectingNArgs(const ExpList& args,int n)
{
    if( args.size() != n )
        throw QString("expecting %1 arguments").arg(n);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

static inline bool expectingNMArgs(const ExpList& args,int n, int m)
{
    if( args.size() < n || args.size() > m)
        throw QString("expecting %1 to %2 arguments").arg(n).arg(m);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type == 0 )
            return false;
    return true;
}

bool Validator::checkBuiltinArgs(quint8 builtin, const ExpList& args, Type** ret, const RowCol& pos)
{
    // NOTE: args are already visited at this point

    Q_ASSERT(ret);

    *ret = mdl->getType(Type::NoType);

    try
    {
    switch(builtin)
    {
    // functions:
    case Builtin::ABS:
        if( !expectingNArgs(args,1) )
            break;
        *ret = args.first()->type;
        break;
    case Builtin::CHR:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(Type::CHAR);
        break;
    case Builtin::ENTIER:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(Type::INTEGER);
        break;
    case Builtin::LEN:
        if( !expectingNMArgs(args,1,2) )
            break;
        // second argument only appears in Gad/AosProfiler.Mod, i.e. 5 lines of ~400k
        *ret = mdl->getType(Type::INTEGER);
        break;
    case Builtin::MAX:
    case Builtin::MIN:
        if(!expectingNMArgs(args,1,2))
            break;
        if( deref(args[0]->type)->kind == Type::SET )
            *ret = mdl->getType(Type::BYTE);
        else
            *ret = args[0]->type;
        break;
    case Builtin::ODD:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(Type::BOOLEAN);
       break;
    case Builtin::ORD:
        if( !expectingNArgs(args,1) )
            break;
        *ret = mdl->getType(Type::INTEGER);
        break;
    case Builtin::ASSERT:
        if( !expectingNMArgs(args,1,2) )
            break; // bool, line, file
        break;
    case Builtin::INC:
    case Builtin::DEC:
        if( !expectingNMArgs(args,1,2) )
            break;
        break;
    case Builtin::EXCL:
    case Builtin::INCL:
        if( !expectingNArgs(args,2) )
            break;
        break;
    case Builtin::NEW:
        if( !expectingNMArgs(args,1,99) )
            break;
        break;
    case Builtin::COPY:
        if( !expectingNArgs(args,2) )
            break;
        break;

    case Builtin::HALT:
        expectingNArgs(args,1);
        break;
    case Builtin::CAP:
        expectingNArgs(args,1);
        break;
    case Builtin::ASH:
        expectingNArgs(args,2);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SHORT:
    case Builtin::LONG:
        expectingNArgs(args,1);
        *ret = args[0]->type; // TODO
        break;
    case Builtin::SIZE:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::AWAIT:
        expectingNArgs(args,1);
        break;

    case Builtin::SYSTEM_ADR:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::LONGINT);
        // TODO: should be PTR, but there is a lot of code storing the result of ADR in LONGINT vars
        // and doing arithmetic operations assuming LONGINT
        break;
    case Builtin::SYSTEM_BIT:
        expectingNArgs(args,2);
        *ret = mdl->getType(Type::BOOLEAN);
        break;
    case Builtin::SYSTEM_CC:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::BOOLEAN);
        break;
    case Builtin::SYSTEM_LSH:
    case Builtin::SYSTEM_ROT:
    case Builtin::SYSTEM_VAL:
        expectingNArgs(args,2);
        *ret = args[0]->type;
        break;
    case Builtin::SYSTEM_TYPECODE:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SYSTEM_GET8:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::SHORTINT);
        break;
    case Builtin::SYSTEM_GET16:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::INTEGER);
        break;
    case Builtin::SYSTEM_GET32:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SYSTEM_GET64:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::HUGEINT);
        break;

        // system procs
    case Builtin::SYSTEM_GET:
    case Builtin::SYSTEM_PUT:
    case Builtin::SYSTEM_MOVE:
    case Builtin::SYSTEM_NEW:
    case Builtin::SYSTEM_PORTOUT:
    case Builtin::SYSTEM_PORTIN:
    case Builtin::SYSTEM_CLI:
    case Builtin::SYSTEM_STI:
    case Builtin::SYSTEM_PUT8:
    case Builtin::SYSTEM_PUT16:
    case Builtin::SYSTEM_PUT32:
    case Builtin::SYSTEM_PUT64:
    case Builtin::SYSTEM_GETREG:
    case Builtin::SYSTEM_PUTREG:
        break;
    default:
        Q_ASSERT(false);
    }
    }catch( const QString& err )
    {
        error(pos, err);
        return false;
    }catch( const char* str)
    {
        error(pos, str);
        return false;
    }

    return true;
}
