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

#include "AoMicronGen.h"
#include "AoToken.h"
#include <QCoreApplication>
#include <QDateTime>
#include <QFile>
#include <QIODevice>
#include <QtDebug>
using namespace Ao;
using namespace Ast;

// the idea is to generate readable and maintainable Micron code with comments
// it's even more complicated as CeeGen; see design notes for more info

Type* MicronGen::deref(Type* t)
{
    if( t && (t->kind == Type::NameRef || t->kind == Type::Reference) )
        return deref(t->type());
    return t;
}

static inline Type* dr(Type* t)
{
    return MicronGen::deref(t);
}

static inline bool isArray(Type* t)
{
    t = dr(t);
    return t && t->kind == Type::Array;
}

static Type* open2dElem(Type* t)
{
    t = dr(t);
    if( t == 0 )
        return 0;
    if( t->kind == Type::Pointer )
        t = dr(t->type());
    if( t == 0 || t->kind != Type::Array || t->expr != 0 )
        return 0;
    Type* inner = dr(t->type());
    if( inner == 0 || inner->kind != Type::Array || inner->expr != 0 )
        return 0;
    Type* elem = dr(inner->type());
    if( elem && elem->kind == Type::Array && elem->expr == 0 )
        return 0; // three or more open dimensions are not supported
    return elem;
}

static int openDims(Type* t)
{
    t = dr(t);
    if( t && t->kind == Type::Pointer )
        t = dr(t->type());
    int res = 0;
    while( t && t->kind == Type::Array && t->expr == 0 )
    {
        res++;
        t = dr(t->type());
    }
    return res;
}

static inline bool isOpenArray(Type* t)
{
    t = dr(t);
    return t && t->kind == Type::Array && t->expr == 0;
}

static bool isPointerKind(Type* t)
{
    t = dr(t);
    return t && ( t->kind == Type::Pointer || t->kind == Type::PTR ||
                  t->kind == Type::ANY || t->kind == Type::ANYOBJ );
}

static bool isVarParamDecl(Declaration* d)
{
    return d && d->kind == Declaration::ParamDecl && d->type() && d->type()->kind == Type::Reference;
}

static bool isOpenArrayParam(Declaration* d)
{
    return d && d->kind == Declaration::ParamDecl && isOpenArray(d->type());
}

static bool isPointerForm(Declaration* d)
{
    if( d == 0 || d->kind != Declaration::ParamDecl )
        return false;
    return isVarParamDecl(d) || isArray(d->type());
}

static void addBoundProcs(Declaration* d, QList<Declaration*>& scopes)
{
    // the procedures bound to an ao object are not part of the link chain of the scope but of the subs of the object type
    if( d->kind != Declaration::TypeDecl )
        return;
    Type* t = dr(d->type());
    if( t && t->kind == Type::Pointer )
        t = dr(t->type());
    if( t == 0 || !t->isSO() )
        return;
    foreach( Declaration* p, t->subs )
    {
        if( p->kind == Declaration::Procedure )
            scopes << p;
    }
}

MicronModel::MicronModel():curMod(0),banPass(false),varPass(false),dirty(false),noBan(false)
{
}

void MicronModel::markAnyPtr(Type* t, int level, bool nested)
{
    t = dr(t);
    if( t == 0 || curMod == 0 || level > 8 )
        return;
    switch( t->kind )
    {
    case Type::PTR:
    case Type::ANY:
    case Type::ANYOBJ:
        anyPtr.insert(curMod);
        break;
    case Type::Pointer:
    case Type::Array: {
            if( !nested )
            {
                Type* elem = open2dElem(t);
                if( elem )
                    array2d[curMod].insert(elem);
            }
            markAnyPtr(t->type(), level+1, t->kind == Type::Array && t->expr == 0);
        } break;
    case Type::Procedure:
        markAnyPtr(t->type(), level+1);
        foreach( Declaration* p, t->subs )
            markAnyPtr(p->type(), level+1);
        break;
    case Type::Record:
    case Type::Object:
        foreach( Declaration* f, t->subs )
        {
            if( f->kind == Declaration::Field )
                markAnyPtr(f->type(), level+1);
        }
        break;
    }
}

bool MicronModel::needsAnyPtr(Declaration* module) const
{
    return anyPtr.contains(module);
}

bool MicronModel::needsSys(Declaration* module) const
{
    return sys.contains(module);
}

bool MicronModel::needsBytePtr(Declaration* module) const
{
    return bytePtr.contains(module);
}

bool MicronModel::needsBytesPtr(Declaration* module) const
{
    return bytesPtr.contains(module);
}

QList<Type*> MicronModel::array2dElems(Declaration* module) const
{
    return array2d.value(module).toList();
}

QList<int> MicronModel::castPtrKinds(Declaration* module) const
{
    QList<int> res = castPtrs.value(module).toList();
    qSort(res);
    return res;
}

QList<int> MicronModel::arrPtrKinds(Declaration* module) const
{
    QList<int> res = arrPtrs.value(module).toList();
    qSort(res);
    return res;
}

bool MicronModel::isByteRef(Declaration* formal, Expression* actual)
{
    if( formal == 0 || actual == 0 || formal->type() == 0 ||
            formal->type()->kind != Type::Reference )
        return false;
    Type* ft = dr(formal->type());
    Type* at = dr(actual->type());
    return ft && ft->kind == Type::BYTE && at && at->kind != Type::BYTE;
}

bool MicronModel::isAnyRef(Declaration* formal, Expression* actual)
{
    if( formal == 0 || actual == 0 || formal->type() == 0 )
        return false;
    Type* ft = dr(formal->type());
    Type* at = dr(actual->type());
    if( ft == 0 || at == 0 || at->kind != Type::Pointer )
        return false;
    if( ft->kind == Type::PTR )
        return true;
    return ft->kind == Type::Pointer && dr(ft->type()) && dr(ft->type())->kind == Type::BYTE &&
            dr(at->type()) && dr(at->type())->kind != Type::BYTE;
}

bool MicronModel::isBytesRef(Declaration* formal, Expression* actual)
{
    if( formal == 0 || actual == 0 || formal->type() == 0 )
        return false;
    Type* ft = dr(formal->type());
    if( ft == 0 || ft->kind != Type::Array || ft->expr != 0 ||
            dr(ft->type()) == 0 || dr(ft->type())->kind != Type::BYTE )
        return false;
    Type* at = dr(actual->type());
    if( at == 0 || at->kind == Type::StrLit )
        return false;
    Type* ae = at->kind == Type::Array ? dr(at->type()) : 0;
    return ae == 0 || ae->kind != Type::BYTE;
}

void MicronModel::markBytePtr(Expression* call)
{
    if( call->lhs == 0 )
        return;
    Declaration* callee = call->lhs->val.value<Declaration*>();
    if( callee && callee->kind == Declaration::Builtin )
        return;
    Type* pt = dr(call->lhs->type());
    DeclList formals;
    if( callee && callee->kind == Declaration::Procedure )
        formals = callee->getParams(true);
    else if( pt && pt->kind == Type::Procedure )
        formals = pt->subs;
    const ExpList a = Expression::getList(call->rhs);
    for( int i = 0; i < a.size() && i < formals.size(); i++ )
    {
        if( isByteRef(formals[i], a[i]) )
            bytePtr.insert(curMod);
        else if( isBytesRef(formals[i], a[i]) )
            bytesPtr.insert(curMod);
        else if( isAnyRef(formals[i], a[i]) )
            anyPtr.insert(curMod);
        else if( varValRef(formals[i], a[i]) )
            castPtrs[curMod].insert(dr(formals[i]->type())->kind);
    }
}

static Type* addrBase(Type* t)
{
    t = dr(t);
    while( t && ( t->kind == Type::Array || ( t->kind == Type::Pointer && dr(t->type()) &&
                    dr(t->type())->kind == Type::Array ) ) )
        t = dr(t->kind == Type::Array ? t->type() : dr(t->type())->type());
    return t;
}

static bool isBuiltinCall(Expression* e, int id)
{
    if( e == 0 || e->kind != Expression::Call || e->lhs == 0 )
        return false;
    Declaration* d = e->lhs->val.value<Declaration*>();
    return d && d->kind == Declaration::Builtin && d->id == id;
}

Expression* MicronModel::varValRef(Declaration* formal, Expression* actual)
{
    if( formal == 0 || actual == 0 || formal->type() == 0 ||
            formal->type()->kind != Type::Reference )
        return 0;
    if( !isBuiltinCall(actual, Builtin::SYSTEM_VAL) )
        return 0;
    Expression* inner = actual->rhs ? actual->rhs->next : 0;
    Type* ft = dr(formal->type());
    Type* it = inner ? dr(inner->type()) : 0;
    if( ft == 0 || it == 0 || ft->kind == it->kind || ft->kind >= Type::MaxBasicType )
        return 0;
    return inner;
}

Declaration* MicronModel::designated(Expression* e)
{
    while( e )
    {
        switch( e->kind )
        {
        case Expression::DeclRef:
        case Expression::Select:
            return e->val.value<Declaration*>();
        case Expression::Index:
        case Expression::Deref:
        case Expression::Cast:
        case Expression::Super:
            e = e->lhs;
            break;
        default:
            return 0;
        }
    }
    return 0;
}

bool MicronModel::isAddress(Declaration* d) const
{
    return d && addrDecls.contains(d);
}

bool MicronModel::isAddrExpr(Expression* e) const
{
    if( e == 0 )
        return false;
    if( isBuiltinCall(e, Builtin::SYSTEM_ADR) )
        return true;
    if( e->kind == Expression::Call )
        return isAddress(designated(e->lhs));
    return isAddress(designated(e));
}

Expression* MicronModel::ptrMove(Expression* e)
{
    if( isBuiltinCall(e, Builtin::SYSTEM_VAL) && e->rhs && e->rhs->next )
        e = e->rhs->next;
    if( e && ( e->kind == Expression::Add || e->kind == Expression::Sub ) &&
            e->lhs && e->rhs && dr(e->rhs->type()) && dr(e->rhs->type())->isInteger() )
        return e;
    return 0;
}

static bool isArith(Expression* e)
{
    if( e == 0 )
        return false;
    switch( e->kind )
    {
    case Expression::Add:
    case Expression::Sub:
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
        return true;
    }
    return false;
}

bool MicronModel::hasAddrBase(Expression* e) const
{
    if( e == 0 )
        return false;
    if( isBuiltinCall(e, Builtin::SYSTEM_VAL) && e->rhs && e->rhs->next )
        e = e->rhs->next;
    if( e->kind == Expression::Add )
        return hasAddrBase(e->lhs) || hasAddrBase(e->rhs);
    if( e->kind == Expression::Sub )
        return hasAddrBase(e->lhs);
    return isAddrExpr(e);
}

Expression* MicronModel::moveChain(Expression* e, QList<Expression*>* ops, QList<Expression*>* offs) const
{
    while( e )
    {
        Expression* x = e;
        if( isBuiltinCall(x, Builtin::SYSTEM_VAL) && x->rhs && x->rhs->next )
            x = x->rhs->next; // the arithmetic is wrapped in a SYSTEM.VAL to the pointer
        if( x == 0 || ( x->kind != Expression::Add && x->kind != Expression::Sub ) ||
                x->lhs == 0 || x->rhs == 0 )
            return x;
        Type* lt = dr(x->lhs->type());
        Type* rt = dr(x->rhs->type());
        bool left = x->kind != Expression::Add || !isArith(x->lhs) || isArith(x->rhs) ||
                !( lt && lt->isInteger() );
        if( x->kind == Expression::Add && hasAddrBase(x->lhs) != hasAddrBase(x->rhs) )
            left = hasAddrBase(x->lhs);
        if( left && !( rt && rt->isInteger() ) )
            return x;
        if( ops )
            ops->prepend(x);
        if( offs )
            offs->prepend(left ? x->rhs : x->lhs);
        e = left ? x->lhs : x->rhs;
    }
    return 0;
}

void MicronModel::banAddr(Expression* e)
{
    if( !banPass || noBan )
        return;
    Declaration* d = designated(e);
    if( d && !noAddr.contains(d) )
    {
        noAddr.insert(d);
        dirty = true;
    }
}

void MicronModel::markAddrDecl(Declaration* d, bool strong)
{
    if( banPass || varPass || d == 0 || ( noAddr.contains(d) && !strong ) ||
            ( addrDecls.contains(d) && ( !strong || strongAddr.contains(d) ) ) )
        return;
    Type* t = addrBase(d->type());
    if( t == 0 || !t->isInteger() )
        return;
    Type* dt = dr(d->type());
    if( dt && dt->kind >= Type::MaxBasicType && !dt->anonymous && dt->decl && dt->decl != d )
        return;
    addrDecls.insert(d);
    if( strong )
        strongAddr.insert(d);
    if( d->getModule() )
        anyPtr.insert(d->getModule());
    dirty = true;
}

void MicronModel::markAddr(Expression* e, bool strong)
{
    markAddrDecl(designated(e), strong);
}

bool MicronModel::isStrongAddr(Expression* e) const
{
    if( isBuiltinCall(e, Builtin::SYSTEM_ADR) )
        return true;
    Declaration* d = designated(e);
    return d && strongAddr.contains(d);
}

void MicronModel::collectAddr(Declaration* scope)
{
    visitStatForAddr(scope->body, scope);
    QList<Declaration*> subScopes;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure )
            subScopes << d;
        addBoundProcs(d, subScopes);
        d = d->next;
    }
    foreach( Declaration* s, subScopes )
        collectAddr(s);
}

void MicronModel::demoteAddr(Expression* e)
{
    Declaration* d = designated(e);
    if( d == 0 || strongAddr.contains(d) )
        return;
    if( !noAddr.contains(d) )
    {
        noAddr.insert(d);
        dirty = true;
    }
    if( addrDecls.contains(d) )
    {
        addrDecls.remove(d);
        dirty = true;
    }
}

bool MicronModel::isAddrSub(Expression* e) const
{
    return e && e->kind == Expression::Sub && isAddrExpr(e->lhs) && isAddrExpr(e->rhs);
}

void MicronModel::visitStatForAddr(Statement* s, Declaration* scope)
{
    while( s )
    {
        if( s->kind == Statement::Assig && s->lhs && s->rhs )
        {
            Type* lt = dr(s->lhs->type());
            if( lt && lt->kind == Type::Procedure )
                procAddr(s->rhs, lt);
        }
        if( s->kind == Statement::Assig && s->lhs && s->rhs && isAddrSub(s->rhs) )
        {
            demoteAddr(s->lhs);
        }else if( s->kind == Statement::Assig && s->lhs && s->rhs )
        {
            Expression* base = ptrMove(s->rhs) ? moveChain(s->rhs, 0, 0) : 0;
            if( base )
            {
                if( isAddrExpr(s->lhs) )
                    markAddr(base, isStrongAddr(s->lhs));
                else if( isAddrExpr(base) )
                    markAddr(s->lhs, isStrongAddr(base));
            }
            else if( isAddrExpr(s->rhs) )
                markAddr(s->lhs, isStrongAddr(s->rhs));
            else if( isAddrExpr(s->lhs) )
                markAddr(s->rhs, isStrongAddr(s->lhs));
        }else if( s->kind == Statement::ForAssig && s->lhs && s->rhs )
        {
            if( isAddrExpr(s->rhs) )
                markAddr(s->lhs);
            else if( isAddrExpr(s->lhs) )
                markAddr(moveChain(s->rhs, 0, 0));
        }else if( s->kind == Statement::Return && s->rhs )
        {
            if( isAddrExpr(s->rhs) )
                markAddrDecl(scope);
            else if( isAddress(scope) )
                markAddr(s->rhs);
        }
        noBan = s->kind == Statement::ForAssig || s->kind == Statement::ForToBy;
        visitExprForAddr(s->lhs);
        noBanExprs.clear();
        if( s->kind == Statement::Assig )
        {
            QList<Expression*> ops;
            Expression* base = moveChain(s->rhs, &ops, 0);
            foreach( Expression* op, ops )
                noBanExprs.insert(op);
            if( ops.isEmpty() && isArith(s->rhs) )
                banAddr(s->lhs);
            else if( !ops.isEmpty() && isArith(base) )
                banAddr(s->lhs);
        }
        visitExprForAddr(s->rhs);
        noBanExprs.clear();
        noBan = false;
        visitStatForAddr(s->body, scope);
        s = s->getNext();
    }
}

static bool isIntVar(Expression* e)
{
    Declaration* d = MicronModel::designated(e);
    if( d == 0 || ( d->kind != Declaration::VarDecl && d->kind != Declaration::LocalDecl &&
                    d->kind != Declaration::ParamDecl ) )
        return false;
    Type* t = dr(d->type());
    return t && t->isInteger();
}

bool MicronModel::isAddrDiff(Expression* e)
{
    return e && e->kind == Expression::Sub && isIntVar(e->lhs) && isIntVar(e->rhs);
}

void MicronModel::visitExprForAddr(Expression* e)
{
    if( e == 0 )
        return;
    switch( e->kind )
    {
    case Expression::Add:
    case Expression::Sub:
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
        if( !noBanExprs.contains(e) && !isAddrDiff(e) )
        {
            banAddr(e->lhs);
            banAddr(e->rhs);
        }
        break;
    case Expression::Call: {
            Declaration* callee = e->lhs ? designated(e->lhs) : 0;
            Type* pt = dr(e->lhs ? e->lhs->type() : 0);
            if( isBuiltinCall(e, Builtin::SYSTEM_VAL) && e->rhs && e->rhs->next &&
                    isPointerKind(e->rhs->type()) )
                markAddr(e->rhs->next);
            else if( ( isBuiltinCall(e, Builtin::SYSTEM_GET) ||
                       isBuiltinCall(e, Builtin::SYSTEM_PUT) ) && e->rhs && e->rhs->next )
            {
                QList<Expression*> ops;
                Expression* base = moveChain(e->rhs, &ops, 0);
                foreach( Expression* op, ops )
                    noBanExprs.insert(op);
                markAddr(base, true);
                Type* vt = dr(e->rhs->next->type());
                const bool valIsPtr = isAddrExpr(e->rhs->next) || isPointerKind(vt);
                if( !banPass && vt && ( vt->kind < Type::MaxBasicType || valIsPtr ) )
                {
                    const int kind = valIsPtr ? (int)Type::PTR : vt->kind;
                    castPtrs[curMod].insert(kind);
                    arrPtrs[curMod].insert(kind);
                    anyPtr.insert(curMod);
                    if( !ops.isEmpty() )
                        bytesPtr.insert(curMod);
                }
            }
            else if( ( pt && pt->kind == Type::Procedure ) ||
                     ( callee && callee->kind == Declaration::Procedure ) )
            {
                // the parameters of a procedure declaration are its subdeclarations,
                // whereas a procedure type carries them in subs
                const DeclList params = callee && callee->kind == Declaration::Procedure ?
                            callee->getParams(true) : pt->subs;
                Expression* arg = e->rhs;
                for( int i = 0; arg != 0 && i < params.size(); i++, arg = arg->next )
                {
                    if( varPass )
                    {
                        Declaration* a = designated(arg);
                        Type* ft = dr(params[i]->type());
                        const bool bridgeable = ft && ( ft->isInteger() ||
                                                        ft->kind == Type::Pointer );
                        if( isVarParamDecl(params[i]) && a && !bridgeable &&
                                addrDecls.contains(params[i]) != addrDecls.contains(a) )
                        {
                            addrDecls.remove(params[i]);
                            addrDecls.remove(a);
                            strongAddr.remove(params[i]);
                            strongAddr.remove(a);
                            noAddr.insert(params[i]);
                            noAddr.insert(a);
                            dirty = true;
                        }
                    }
                    else if( banPass )
                    {
                        Declaration* a = designated(arg);
                        if( isVarParamDecl(params[i]) && noAddr.contains(params[i]) )
                            banAddr(arg);
                        else if( a && noAddr.contains(a) && !noAddr.contains(params[i]) )
                        {
                            noAddr.insert(params[i]);
                            dirty = true;
                        }
                    }
                    else if( isAddress(params[i]) )
                        markAddr(arg);
                    else if( isAddrExpr(arg) )
                        markAddrDecl(params[i]);
                }
            }
        } break;
    }
    visitExprForAddr(e->lhs);
    visitExprForAddr(e->rhs);
    visitExprForAddr(e->next);
}

void MicronModel::markObject(Type* record)
{
    record = dr(record);
    if( record == 0 || !record->isSO() || objects.contains(record) )
        return;
    objects.insert(record);
    Type* base = dr(record->type());
    if( base && base->kind == Type::Pointer )
        base = dr(base->type());
    markObject(base);
}

void MicronModel::collectObjects(Declaration* module)
{
    curMod = module;
    QList<Declaration*> scopes;
    scopes << module;
    while( !scopes.isEmpty() )
    {
        Declaration* scope = scopes.takeFirst();
        Declaration* d = scope->link;
        while( d )
        {
            if( d->kind == Declaration::Procedure )
                scopes << d;
            addBoundProcs(d, scopes);
            markAnyPtr(d->type());
            Type* t = dr(d->type());
            if( t && t->kind == Type::Pointer )
                t = dr(t->type());
            if( t && ( t->kind == Type::Object || ( t->isSO() && t->type() ) ) )
                markObject(t);
            d = d->next;
        }
        visitStatForObjects(scope->body);
    }
}

void MicronModel::visitStatForObjects(Statement* s)
{
    while( s )
    {
        if( s->kind == Statement::With && s->rhs )
        {
            Type* t = dr(s->rhs->type());
            if( t && t->kind == Type::Pointer )
                t = dr(t->type());
            markObject(t);
        }
        visitExprForObjects(s->lhs);
        visitExprForObjects(s->rhs);
        visitStatForObjects(s->body);
        s = s->getNext();
    }
}

void MicronModel::visitExprForObjects(Expression* e)
{
    if( e == 0 )
        return;
    markAnyPtr(e->type());
    Type* to = 0;
    Expression* arg = 0;
    if( MicronGen::isStructuredVal(e, to, arg) && to->decl &&
            to->decl->kind == Declaration::TypeDecl )
        castTargets.insert(to);
    if( e->kind == Expression::Call )
        markBytePtr(e);
    if( isBuiltinCall(e, Builtin::SYSTEM_VAL) || isBuiltinCall(e, Builtin::SYSTEM_ADR) )
        anyPtr.insert(curMod);
    if( isBuiltinCall(e, Builtin::SYSTEM_MOVE) || isBuiltinCall(e, Builtin::SYSTEM_ROT) )
    {
        sys.insert(curMod);
        anyPtr.insert(curMod);
    }
    if( e->varArrOfByte )
        bytesPtr.insert(curMod);
    if( e->kind == Expression::Is || e->kind == Expression::Cast )
    {
        Type* t = dr(e->kind == Expression::Is ? e->rhs->type() : e->type());
        if( t && t->kind == Type::Pointer )
            t = dr(t->type());
        markObject(t);
    }
    visitExprForObjects(e->lhs);
    visitExprForObjects(e->rhs);
    visitExprForObjects(e->next);
}

void MicronModel::collectPointers(Declaration* scope)
{
    QList<Declaration*> subScopes;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure )
            subScopes << d; // types can also be declared local to a procedure
        addBoundProcs(d, subScopes);
        if( d->kind == Declaration::TypeDecl )
        {
            Type* t = dr(d->type());
            if( t && t->kind == Type::Pointer )
            {
                Type* base = dr(t->type());
                if( base && base->isSO() && !ptrName.contains(base) &&
                        base->decl && base->decl->getModule() == d->getModule() )
                    ptrName.insert(base, d->name);
            }
        }
        d = d->next;
    }

    d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
        {
            Type* t = dr(d->type());
            if( t && ( ( t->isSO() && objects.contains(t) ) || castTargets.contains(t) )
                    && !ptrName.contains(t) )
            {
                ptrName.insert(t, "P" + d->name);
                synthesized[t] = d;
            }
        }
        d = d->next;
    }

    foreach( Declaration* s, subScopes )
        collectPointers(s);
}

bool MicronModel::isObject(Type* record) const
{
    return objects.contains(dr(record));
}

QByteArray MicronModel::pointerName(Type* record) const
{
    record = dr(record);
    QHash<Type*,QByteArray>::const_iterator i = ptrName.find(record);
    if( i != ptrName.end() )
        return i.value();
    if( record && record->decl )
        return "P" + record->decl->name;
    return "???";
}

bool MicronModel::hasSynthesizedPointer(Declaration* typeDecl) const
{
    Type* t = dr(typeDecl->type());
    return synthesized.value(t) == typeDecl;
}

QByteArray MicronModel::lenName(Declaration* param)
{
    return param->name + "_len";
}

bool MicronModel::needsLen(Declaration* param) const
{
    if( param && open2dElem(param->type()) )
        return false;
    return lenParams.contains(param);
}

void MicronModel::requireLen(Expression* e)
{
    if( e == 0 || e->kind != Expression::DeclRef )
        return;
    Declaration* d = e->val.value<Declaration*>();
    if( isOpenArrayParam(d) && !lenParams.contains(d) )
    {
        lenParams.insert(d);
        dirty = true;
    }
}

void MicronModel::unifyLen(const DeclList& a, const DeclList& b)
{
    for( int i = 0; i < a.size() && i < b.size(); i++ )
    {
        if( lenParams.contains(a[i]) == lenParams.contains(b[i]) )
            continue;
        Declaration* d = lenParams.contains(a[i]) ? b[i] : a[i];
        if( isOpenArrayParam(d) )
        {
            lenParams.insert(d);
            dirty = true;
        }
    }
}

void MicronModel::procValue(Expression* actual, Type* formalType)
{
    Type* pt = dr(formalType);
    if( pt == 0 || pt->kind != Type::Procedure || actual == 0 )
        return;
    Declaration* d = designated(actual);
    if( d && d->kind == Declaration::Procedure )
        unifyLen(pt->subs, d->getParams(true));
    else
    {
        Type* at = dr(actual->type());
        if( at && at->kind == Type::Procedure && at != pt )
            unifyLen(pt->subs, at->subs);
    }
}

void MicronModel::unifyAddr(const DeclList& a, const DeclList& b)
{
    for( int i = 0; i < a.size() && i < b.size(); i++ )
    {
        if( isAddress(a[i]) == isAddress(b[i]) )
            continue;
        markAddrDecl(isAddress(a[i]) ? b[i] : a[i], true);
    }
}

void MicronModel::procAddr(Expression* actual, Type* formalType)
{
    Type* pt = dr(formalType);
    if( pt == 0 || pt->kind != Type::Procedure || actual == 0 )
        return;
    Declaration* d = designated(actual);
    if( d && d->kind == Declaration::Procedure )
    {
        unifyAddr(pt->subs, d->getParams(true));
    }else
    {
        Type* at = dr(actual->type());
        if( at && at->kind == Type::Procedure && at != pt )
            unifyAddr(pt->subs, at->subs);
    }
}

void MicronModel::visitExprForLen(Expression* e)
{
    if( e == 0 )
        return;
    if( e->kind == Expression::Call && e->lhs )
    {
        Declaration* callee = e->lhs->val.value<Declaration*>();
        const ExpList a = Expression::getList(e->rhs);
        if( callee && callee->kind == Declaration::Builtin )
        {
            switch( callee->id )
            {
            case Builtin::LEN:
                if( !a.isEmpty() )
                    requireLen(a.first());
                break;
            case Builtin::COPY:
                if( a.size() == 2 )
                    requireLen(a[1]);
                break;
            }
        }else
        {
            DeclList formals;
            if( callee && callee->kind == Declaration::Procedure )
                formals = callee->getParams(true);
            else
            {
                Type* pt = dr(e->lhs->type());
                if( pt && pt->kind == Type::Procedure )
                    formals = pt->subs;
            }
            for( int i = 0; i < a.size() && i < formals.size(); i++ )
            {
                if( needsLen(formals[i]) )
                    requireLen(a[i]);
                procValue(a[i], formals[i]->type());
            }
        }
    }
    visitExprForLen(e->lhs);
    visitExprForLen(e->rhs);
    visitExprForLen(e->next);
}

void MicronModel::visitStatForLen(Statement* s)
{
    while( s )
    {
        visitExprForLen(s->lhs);
        visitExprForLen(s->rhs);
        if( s->kind == Statement::Assig && s->lhs && s->rhs )
        {
            Type* lt = dr(s->lhs->type());
            Type* rt = dr(s->rhs->type());
            if( lt && lt->kind == Type::Array && rt &&
                    (rt->kind == Type::StrLit || rt->kind == Type::Array) )
                requireLen(s->lhs);
            if( lt && lt->kind == Type::Procedure )
                procValue(s->rhs, lt);
        }
        visitStatForLen(s->body);
        s = s->getNext();
    }
}

void MicronModel::collectRecords(Declaration* module)
{
    QSet<QByteArray> used;
    Declaration* d = module->link;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
            used.insert(d->name);
        d = d->next;
    }
    d = module->link;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
        {
            Type* t = dr(d->type());
            if( t && t->kind == Type::Pointer )
            {
                Type* base = dr(t->type());
                // the record type of a POINTER TO RECORD has no declaration of its own
                const bool anonymous = base && ( base->anonymous || base->decl == 0 || base->decl == d ||
                                                 base->decl->kind != Declaration::TypeDecl );
                if( base && base->isSO() && anonymous && !recName.contains(base) )
                {
                    QByteArray name = d->name + "Desc";
                    while( used.contains(name) )
                        name += "_";
                    used.insert(name);
                    recName.insert(base, name);
                    recOwner.insert(base, d);
                }
            }
        }
        d = d->next;
    }
}

QByteArray MicronModel::recordName(Type* record) const
{
    return recName.value(dr(record));
}

Declaration* MicronModel::recordOwner(Type* record) const
{
    return recOwner.value(dr(record));
}

void MicronModel::collectLen(Declaration* scope)
{
    visitStatForLen(scope->body);
    QList<Declaration*> subScopes;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure )
            subScopes << d;
        addBoundProcs(d, subScopes);
        d = d->next;
    }
    foreach( Declaration* s, subScopes )
        collectLen(s);
}

void MicronModel::collectRefs(Declaration* scope)
{
    visitStatForRefs(scope->body, scope);
    QList<Declaration*> subScopes;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure )
            subScopes << d;
        addBoundProcs(d, subScopes);
        d = d->next;
    }
    foreach( Declaration* s, subScopes )
        collectRefs(s);
}

void MicronModel::visitStatForRefs(Statement* s, Declaration* scope)
{
    while( s )
    {
        visitExprForRefs(s->lhs, scope);
        visitExprForRefs(s->rhs, scope);
        visitStatForRefs(s->body, scope);
        s = s->getNext();
    }
}

void MicronModel::visitExprForRefs(Expression* e, Declaration* scope)
{
    if( e == 0 )
        return;
    if( e->kind == Expression::DeclRef )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( d && d->kind == Declaration::Procedure && d != scope )
            extRefs.insert(d);
    }
    visitExprForRefs(e->lhs, scope);
    visitExprForRefs(e->rhs, scope);
    visitExprForRefs(e->next, scope);
}

bool MicronModel::needsForward(Declaration* proc) const
{
    return extRefs.contains(proc);
}

void MicronModel::analyze(const QList<Declaration*>& modules)
{
    foreach( Declaration* m, modules )
    {
        if( m )
            collectObjects(m);
    }
    foreach( Declaration* m, modules )
    {
        if( m )
            collectPointers(m);
    }
    foreach( Declaration* m, modules )
    {
        if( m )
            collectRecords(m);
    }
    foreach( Declaration* m, modules )
    {
        if( m )
            collectRefs(m);
    }
    int guard = 0;
    do
    {
        dirty = false;
        foreach( Declaration* m, modules )
        {
            if( m )
                collectLen(m);
        }
    }while( dirty && ++guard < 32 );

    banPass = true;
    guard = 0;
    do
    {
        dirty = false;
        foreach( Declaration* m, modules )
        {
            if( m )
            {
                curMod = m;
                collectAddr(m);
            }
        }
    }while( dirty && ++guard < 32 );
    banPass = false;
    guard = 0;
    do
    {
        dirty = false;
        foreach( Declaration* m, modules )
        {
            if( m )
            {
                curMod = m;
                collectAddr(m);
            }
        }
    }while( dirty && ++guard < 32 );

    varPass = true;
    guard = 0;
    do
    {
        dirty = false;
        foreach( Declaration* m, modules )
        {
            if( m )
            {
                curMod = m;
                collectAddr(m);
            }
        }
    }while( dirty && ++guard < 32 );
    varPass = false;
}

static QSet<QByteArray>& keywords()
{
    static QSet<QByteArray> kw;
    if( kw.isEmpty() )
    {
        static const char* words[] = {
            // reserved words
            "and", "array", "begin", "bits", "by", "case", "const", "div", "do", "else",
            "elsif", "end", "exit", "extern", "false", "finally", "for", "goto", "if",
            "import", "in", "inline", "interface", "invar", "is", "loop", "mod", "module",
            "nil", "not", "object", "of", "or", "pointer", "proc", "procedure", "record",
            "repeat", "return", "then", "to", "true", "type", "until", "var", "where", "while",
            // predeclared identifiers
            "abs", "any", "asr", "assert", "band", "bnot", "bool", "boolean", "bor", "bset",
            "bxor", "byte", "cap", "cast", "char", "chr", "cli", "copy", "dec", "default",
            "dispose", "excl", "floor", "flt", "flt32", "flt64", "getenv", "getreg", "halt",
            "inc", "incl", "int16", "int32", "int64", "int8", "integer", "len", "long",
            "longint", "longreal", "max", "min", "new", "newgc", "newinit", "nop", "odd",
            "ord", "pcall", "print", "println", "ptroff", "putreg", "raise", "real", "set", "setenv",
            "shl", "short", "shortint", "shr", "sigc", "sigl", "size", "sti", "strlen", "uint16",
            "uint32", "uint64", "uint8", "usig", "val",
            "i1", "i2", "i4", "i8", "u1", "u2", "u4", "u8", "r4", "r8",
            0
        };
        for( int i = 0; words[i] != 0; i++ )
            kw.insert(words[i]);
    }
    return kw;
}

bool MicronGen::isKeyword(const QByteArray& name)
{
    if( name.isEmpty() )
        return false;
    if( name != name.toLower() && name != name.toUpper() )
        return false;
    return keywords().contains(name.toLower());
}

QByteArray MicronGen::escape(const QByteArray& name)
{
    if( isKeyword(name) )
        return name + "_";
    return name;
}

MicronGen::MicronGen(MicronModel* m):mdl(m),curMod(0),curProc(0),cmts(0),curLevel(0),level(4),
    obDiv(false),genCmds(false),lastRow(0),curPlan(0),no2d(false),constCtx(0),noPromote(0),addSys(false)
{
    if( mdl == 0 )
        mdl = &ownModel;
    out.setString(&buffer, QIODevice::WriteOnly);
}

QString MicronGen::genDedication()
{
    return "// this file was generated by " + QCoreApplication::applicationName() + " "
            + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

void MicronGen::error(const QString& msg, const RowCol& pos)
{
    errors << Error(msg, pos, curMod ? QString::fromUtf8(curMod->name) : QString());
}

QString MicronGen::sourceLine(const RowCol& pos) const
{
    if( pos.d_row < 1 || pos.d_row > (quint32)srcLines.size() )
        return QString();
    QString line = srcLines[pos.d_row-1].trimmed();
    // the comment delimiters of the Oberon code would nest in the generated comment
    line.replace("(*", "( *");
    line.replace("*)", "* )");
    return line;
}

void MicronGen::todo(const QString& msg, const RowCol& pos)
{
    const QString line = sourceLine(pos);
    manual << QString("%1:%2:%3: %4%5").arg(curMod ? QString::fromUtf8(curMod->name) : QString())
              .arg(pos.d_row).arg(pos.d_col).arg(msg)
              .arg(line.isEmpty() ? QString() : QString("\n\tOberon: %1").arg(line));
    out << " (* TODO o2m: " << msg;
    if( !line.isEmpty() )
        out << "; Oberon: " << line;
    out << " *)";
}

QByteArray MicronGen::ws(int l) const
{
    if( l < 0 )
        l = curLevel;
    return QByteArray(l * 2, ' ');
}

QByteArray MicronGen::toStr(Expression* e, bool desig, bool addr, Type* to)
{
    out.flush();
    const int mark = buffer.size();
    if( addr )
        addrExpr(e, to);
    else if( desig )
        designator(e);
    else
        Expr(e);
    out.flush();
    const QByteArray res = buffer.mid(mark).toLatin1();
    buffer.truncate(mark);
    out.seek(mark);
    return res;
}

void MicronGen::condition(Expression* e)
{
    Expr(e, NoPrec);
}

void MicronGen::emitComment(Comment* c, bool ownLine)
{
    QByteArray text = c->text;
    text.replace('\r', "");
    QList<QByteArray> lines = text.split('\n');
    if( lines.size() == 1 && lines.first().startsWith("(*") && lines.first().endsWith("*)") )
    {
        // a comment which reaches to the end of the line is rendered as a line comment
        QByteArray line = lines.first();
        line = line.mid(2, line.size() - 4).trimmed();
        if( line.startsWith('*') )
            line = line.mid(1).trimmed(); // this was an Oberon documentation comment
        lines.clear();
        lines << "// " + line;
    }
    if( !ownLine )
        out << " ";
    for( int i = 0; i < lines.size(); i++ )
    {
        if( i != 0 )
            out << endl;
        if( i != 0 || ownLine )
            out << ws();
        out << QString::fromLatin1(lines[i].trimmed());
    }
    if( ownLine )
        out << endl;
}

void MicronGen::blankLine(const RowCol& pos)
{
    if( lastRow != 0 && pos.d_row > lastRow + 1 )
        out << endl;
}

void MicronGen::leading(const RowCol& pos)
{
    if( cmts )
    {
        const QList<Comment*> list = cmts->takeBefore(pos);
        foreach( Comment* c, list )
        {
            blankLine(c->pos);
            emitComment(c, true);
            lastRow = c->endRow;
        }
    }
    blankLine(pos);
    lastRow = pos.d_row;
}

void MicronGen::trailing(quint32 row)
{
    if( cmts == 0 )
        return;
    Comment* c = cmts->takeTrailing(row);
    if( c )
    {
        emitComment(c, false);
        lastRow = c->endRow;
    }
}

void MicronGen::flushUntil(quint32 row)
{
    if( cmts == 0 )
        return;
    const QList<Comment*> list = cmts->takeUntil(row);
    foreach( Comment* c, list )
    {
        blankLine(c->pos);
        emitComment(c, true);
        lastRow = c->endRow;
    }
}

QByteArray MicronGen::basicType(Type* t)
{
    switch( t->kind )
    {
    case Type::BOOLEAN:
        return "boolean";
    case Type::CHAR:
        return "char";
    case Type::BYTE:
        return "byte";
    case Type::SHORTINT:
        return "int8";  // Oberon SHORTINT is one byte wide
    case Type::INTEGER:
        return "int16"; // Oberon INTEGER is two bytes wide
    case Type::LONGINT:
        return "int32"; // Oberon LONGINT is four bytes wide
    case Type::HUGEINT:
        return "int64";
    case Type::REAL:
        return "real";
    case Type::LONGREAL:
        return "longreal";
    case Type::SET:
        return "set";
    case Type::PTR:
    case Type::ANY:
    case Type::ANYOBJ:
        return anyPtr.isEmpty() ? QByteArray("pointer to any") : anyPtr;
    case Type::StrLit:
        return "pointer to array of char";
    }
    return "???";
}

qint64 MicronGen::constVal(Expression* e, bool* ok)
{
    if( ok )
        *ok = true;
    if( e == 0 )
    {
        if( ok )
            *ok = false;
        return 0;
    }
    bool ok1 = true, ok2 = true;
    switch( e->kind )
    {
    case Expression::Literal:
    case Expression::ConstVal:
        return e->getCaseValue(ok);
    case Expression::DeclRef: {
            Declaration* d = e->val.value<Declaration*>();
            if( d && d->kind == Declaration::ConstDecl && d->expr )
                return constVal(d->expr, ok);
            if( d && d->kind == Declaration::ConstDecl && d->getModule() == 0 )
                return d->data.toLongLong();
        } break;
    case Expression::Plus:
        return constVal(e->lhs, ok);
    case Expression::Minus:
        return -constVal(e->lhs, ok);
    case Expression::Add:
    case Expression::Sub:
    case Expression::Mul:
    case Expression::Div:
    case Expression::Mod: {
            const qint64 lhs = constVal(e->lhs, &ok1);
            const qint64 rhs = constVal(e->rhs, &ok2);
            if( !ok1 || !ok2 )
                break;
            switch( e->kind )
            {
            case Expression::Add:
                return lhs + rhs;
            case Expression::Sub:
                return lhs - rhs;
            case Expression::Mul:
                return lhs * rhs;
            case Expression::Div:
                return rhs != 0 ? lhs / rhs : 0;
            case Expression::Mod:
                return rhs != 0 ? lhs % rhs : 0;
            }
        } break;
    case Expression::Call: {
            Declaration* callee = e->lhs ? e->lhs->val.value<Declaration*>() : 0;
            const ExpList a = Expression::getList(e->rhs);
            if( callee && callee->kind == Declaration::Builtin && a.size() == 1 &&
                    ( callee->id == Builtin::MAX || callee->id == Builtin::MIN ) )
            {
                Type* t = deref(a[0]->type());
                if( t == 0 || !t->isInteger() )
                    break;
                 const bool max = callee->id == Builtin::MAX;
                switch( t->kind )
                {
                case Type::SHORTINT:
                    return max ? 127 : -128;
                case Type::INTEGER:
                    return max ? 32767 : -32768;
                case Type::LONGINT:
                    return max ? Q_INT64_C(2147483647) : Q_INT64_C(-2147483648);
                }
                break;
            }
            if( callee == 0 || callee->kind != Declaration::Builtin || a.size() != 2 ||
                    (callee->id != Builtin::ASH && callee->id != Builtin::SYSTEM_LSH) )
                break;
            const qint64 lhs = constVal(a[0], &ok1);
            const qint64 rhs = constVal(a[1], &ok2);
            if( !ok1 || !ok2 )
                break;
            return rhs >= 0 ? lhs << rhs : lhs >> -rhs;
        } break;
    }
    if( ok )
        *ok = false;
    return 0;
}

QByteArray MicronGen::hexChar(quint32 ch)
{
    QByteArray res = QByteArray::number(ch, 16).toUpper() + "X";
    if( res[0] >= 'A' && res[0] <= 'F' )
        res = "0" + res; // a hex literal must start with a digit
    return res;
}

QByteArray MicronGen::charLit(quint32 ch)
{
    if( ch >= ' ' && ch < 127 && ch != '"' )
        return "\"" + QByteArray(1, char(ch)) + "\"";
    QByteArray res = QByteArray::number(ch, 16).toUpper() + "X";
    if( res[0] >= 'A' && res[0] <= 'F' )
        res = "0" + res; // a hex literal must start with a digit
    return res;
}

QByteArray MicronGen::hexNum(quint64 n)
{
    QByteArray res = QByteArray::number(n, 16).toUpper() + "H";
    if( res[0] >= 'A' && res[0] <= 'F' )
        res = "0" + res; // a hex literal must start with a digit
    return res;
}

QByteArray MicronGen::limit(Type* t, bool max)
{
    if( t == 0 )
        return "0";
    switch( t->kind )
    {
    case Type::SHORTINT:
        return max ? "127" + suffix(t, 127) : "(-128" + suffix(t, -128) + ")";
    case Type::INTEGER:
        return max ? "32767" + suffix(t, 32767) : "(-32768" + suffix(t, -32768) + ")";
    case Type::LONGINT:
        return max ? "2147483647" + suffix(t, 2147483647) :
                     "(-2147483647" + suffix(t, -2147483647) + "-1)";
    case Type::HUGEINT:
        return max ? "9223372036854775807" : "(-9223372036854775807-1)";
    case Type::BYTE:
        return max ? "255" : "0";
    case Type::CHAR:
        return max ? "0FFX" : "00X";
    case Type::SET:
        return max ? "31" : "0";
    case Type::BOOLEAN:
        return max ? "true" : "false";
    case Type::REAL:
        return max ? "3.40282347E38" : "1.17549435E-38";
    case Type::LONGREAL:
        return max ? "1.7976931348623157E308" : "2.2250738585072014E-308";
    }
    return "0";
}

QByteArray MicronGen::zeroValue(Type* t)
{
    t = deref(t);
    if( t == 0 )
        return "0";
    switch( t->kind )
    {
    case Type::CHAR:
        return "00X";
    case Type::SET:
        return "{}";
    case Type::BOOLEAN:
        return "false";
    case Type::REAL:
    case Type::LONGREAL:
        return "0.0";
    case Type::Pointer:
    case Type::Object:
    case Type::Procedure:
    case Type::PTR:
    case Type::ANY:
    case Type::ANYOBJ:
        return "nil";
    }
    if( t->isInteger() )
        return "0";
    return "0";
}

QByteArray MicronGen::moduleRef(Declaration* module)
{
    if( module == 0 )
        return "???";
    return importAlias.value(module, escape(module->name));
}

QByteArray MicronGen::qualident(Declaration* d)
{
    if( d == 0 )
        return "???";
    if( d->receiver && d->kind == Declaration::ParamDecl )
        return "self"; // the receiver is lower case in Micron
    QByteArray res = escape(d->name);
    Declaration* m = d->getModule();
    if( m && m != curMod && d->outer && d->outer->kind == Declaration::Module )
        res = moduleRef(m) + "." + res;
    return res;
}

QByteArray MicronGen::pointerTo(Type* record)
{
    record = deref(record);
    QByteArray res = escape(mdl->pointerName(record));
    Declaration* d = record ? record->decl : 0;
    Declaration* m = d ? d->getModule() : 0;
    if( m && m != curMod )
        res = moduleRef(m) + "." + res;
    return res;
}

QByteArray MicronGen::typeRef(Type* t)
{
    if( t == 0 || t->kind == Type::NoType )
        return QByteArray();

    if( anonPtrName.contains(t) )
        return anonPtrName.value(t);

    if( t->kind == Type::Reference )
        return typeRef(t->type());

    if( t->kind == Type::NameRef )
    {
        Type* d = deref(t);
        if( d && d->kind < Type::MaxBasicType )
            return basicType(d);
        if( d && d->kind == Type::Object )
            return pointerTo(d);
        Quali* q = t->quali;
        QByteArray res;
        if( q && !q->first.isEmpty() )
            res = escape(q->first) + ".";
        res += escape(q ? q->second : QByteArray("???"));
        return res;
    }

    if( t->kind < Type::MaxBasicType )
        return basicType(t);

    if( t->kind == Type::Object )
        return pointerTo(t);

    if( t->isSO() && !mdl->recordName(t).isEmpty() )
        return recordRef(t);

    if( t->decl && t->decl->kind == Declaration::TypeDecl && !t->anonymous )
        return qualident(t->decl);

    return typeExpr(t);
}

QByteArray MicronGen::typeExpr(Type* t, bool declaring)
{
    if( t == 0 )
        return QByteArray();
    Type* elem2d = no2d ? 0 : open2dElem(t);
    if( elem2d && arr2dName.contains(elem2d) )
        return arr2dName.value(elem2d);
    if( !no2d && openDims(t) > 2 )
    {
        const bool old = no2d;
        no2d = true;
        const QByteArray res = typeExpr(t, declaring);
        no2d = old;
        return res + " // TODO o2m: open arrays with more than two dimensions are not supported";
    }
    switch( t->kind )
    {
    case Type::Pointer: {
            Type* base = deref(t->type());
            if( base && base->kind == Type::Array && base->expr == 0 )
            {
                Type* elem = deref(base->type());
                if( elem && elem->isSO() && elem->decl && !elem->anonymous &&
                        elem->decl->kind == Declaration::TypeDecl &&
                        !elem->decl->name.isEmpty() &&
                        elem->decl->getModule() == curMod &&
                        !emittedTypes.contains(elem->decl) )
                    return "pointer to " + openArrOf(elem);
            }
            if( base && !mdl->recordName(base).isEmpty() )
                return "pointer to " + typeRef(base);
            if( !declaring && base && base->isSO() && mdl->isObject(base) )
                return pointerTo(base);
            return "pointer to " + typeRef(t->type());
        }
    case Type::Array: {
            QByteArray res = "array ";
            if( t->expr )
            {
                bool ok = false;
                const qint64 len = constVal(t->expr, &ok);
                const QByteArray expr = toStr(t->expr);
                if( ok && len >= 0 )
                {
                    res += QByteArray::number(len);
                    if( t->expr->kind != Expression::Literal )
                        res += " (* " + expr + " *)";
                }else
                    res += "usig(" + expr + ")";
                res += " ";
            }
            res += "of " + typeRef(t->type());
            return res;
        }
    case Type::Procedure: {
            QByteArray res = "procedure(";
            for( int i = 0; i < t->subs.size(); i++ )
            {
                if( i != 0 )
                    res += "; ";
            if( !isVarParamDecl(t->subs[i]) && isArray(t->subs[i]->type()) )
                res += "const ";
            res += escape(t->subs[i]->name) + ": " + formalType(t->subs[i]);
                if( mdl->needsLen(t->subs[i]) )
                    res += "; " + MicronModel::lenName(t->subs[i]) + ": int32";
            }
            res += ")";
            Type* ret = deref(t->type());
            if( ret && ret->kind != Type::NoType )
                res += ": " + typeRef(t->type());
            return res;
        }
    case Type::Record:
    case Type::Object: {
            out.flush();
            const int mark = buffer.size();
            RecordBody(t);
            out.flush();
            const QByteArray res = buffer.mid(mark).toLatin1();
            buffer.truncate(mark);
            out.seek(mark);
            return res;
        }
    }
    return "???";
}

QByteArray MicronGen::arr2dDecl(Type* elem)
{
    const QByteArray e = typeRef(elem);
    return arr2dName.value(elem) + "* = record len0*, len1*: int32; data*: pointer to array of "
            + e + " end // descriptor for ARRAY OF ARRAY OF " + e;
}

static QByteArray visibility(Declaration* d);

QByteArray MicronGen::openArrOf(Type* elem)
{
    QByteArray name = openArrName.value(elem);
    if( name.isEmpty() )
    {
        name = escape(elem->decl->name) + "Array";
        openArrName.insert(elem, name);
    }
    openArrPending.insert(elem);
    return name;
}

void MicronGen::arr2dSection()
{
    foreach( Type* elem, openArrPending.toList() )
    {
        if( elem->decl == 0 || !emittedTypes.contains(elem->decl) )
            continue;
        out << endl << ws() << openArrName.value(elem) << visibility(elem->decl)
            << " = array of " << typeRef(elem) << endl;
        openArrPending.remove(elem);
    }

    foreach( Type* elem, arr2dPending.toList() )
    {
        if( elem->decl == 0 || !emittedTypes.contains(elem->decl) )
            continue;
        out << endl << ws() << arr2dDecl(elem) << endl;
        arr2dPending.remove(elem);
    }
}

QByteArray MicronGen::toMicron(Expression* e)
{
    out.flush();
    const int mark = buffer.size();
    designator(e, false);
    out.flush();
    const QByteArray res = buffer.mid(mark).toLatin1();
    buffer.truncate(mark);
    out.seek(mark);
    return res;
}

Expression* MicronGen::arr2dOf(Expression* e) const
{
    if( e == 0 || e->kind != Expression::Index )
        return 0;
    Expression* base = e->lhs;
    while( base && base->kind == Expression::Deref )
        base = base->lhs;
    Type* elem = base ? open2dElem(base->type()) : 0;
    if( elem && arr2dName.contains(elem) )
        return base;
    return 0;
}

bool MicronGen::isArr2d(Type* t) const
{
    Type* elem = open2dElem(t);
    return elem && arr2dName.contains(elem);
}

QByteArray MicronGen::addressType(Type* t)
{
    Type* d = deref(t);
    if( d && d->kind == Type::Pointer )
        return "pointer to " + addressType(d->type());
    if( d && d->kind == Type::Array )
    {
        QByteArray res = typeExpr(d);
        const int pos = res.lastIndexOf("of ");
        if( pos != -1 )
            return res.left(pos+3) + anyPtr;
    }
    return anyPtr;
}

QByteArray MicronGen::declType(Declaration* d)
{
    if( mdl->isAddress(d) )
        return addressType(d->type());
    return typeRef(d->type());
}

QByteArray MicronGen::formalType(Declaration* param)
{
    Type* t = param->type();
    Type* d = deref(t);
    if( d == 0 )
        return QByteArray();
    if( mdl->isAddress(param) )
        return t->kind == Type::Reference ? "pointer to " + addressType(t) : addressType(t);
    if( isArr2d(t) )
        return t->kind == Type::Reference ? "pointer to " + typeRef(t) : typeRef(t);
    if( d->kind == Type::Array && d->expr != 0 &&
            deref(d->type()) && deref(d->type())->kind == Type::CHAR )
        return "pointer to array of char";
    if( d->kind == Type::Array )
        return "pointer to " + typeRef(t);
    if( t->kind == Type::Reference )
        return "pointer to " + typeRef(t);
    return typeRef(t);
}

bool MicronGen::isVarParam(Declaration* d) const
{
    return isVarParamDecl(d);
}

const ClosureLifter::LiftParam* MicronGen::lifted(Declaration* d) const
{
    if( curPlan == 0 )
        return 0;
    return curPlan->findFromSourceDecl(d);
}

static QByteArray visibility(Declaration* d)
{
    switch( d->visi )
    {
    case Declaration::ReadWrite:
        return "*";
    case Declaration::ReadOnly:
        return "-";
    }
    return QByteArray();
}

static void constRefs(Expression* e, QSet<Declaration*>& refs)
{
    if( e == 0 )
        return;
    if( e->kind == Expression::DeclRef || e->kind == Expression::ConstVal )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( d && d->kind == Declaration::ConstDecl )
            refs.insert(d);
    }
    constRefs(e->lhs, refs);
    constRefs(e->rhs, refs);
    constRefs(e->next, refs);
}

void MicronGen::collectConstFeeds(Declaration* d)
{
    while( d )
    {
        if( d->kind == Declaration::ConstDecl )
            constRefs(d->expr, constFeeds);
        collectConstFeeds(d->link);
        d = d->next;
    }
}

bool MicronGen::generate(Declaration* module, QIODevice* dev, CommentTable* comments)
{
    errors.clear();
    curMod = module;
    cmts = comments;
    curLevel = 0;
    lastRow = 0;
    curProc = 0;
    curPlan = 0;
    wantAddr = false;
    wantType = 0;
    srcLines.clear();
    QFile src(module->data.value<ModuleData>().sourcePath);
    if( src.open(QIODevice::ReadOnly) )
        srcLines = QString::fromLatin1(src.readAll()).split('\n');
    buffer.clear();
    out.seek(0);

    if( mdl == &ownModel )
        mdl->analyze(QList<Declaration*>() << module);

    constFeeds.clear();
    collectConstFeeds(module->link);

    anyPtr.clear();
    if( mdl->needsAnyPtr(module) )
        anyPtr = uniqueName(module, "AnyPtr");
    bytePtr.clear();
    if( mdl->needsBytePtr(module) )
        bytePtr = uniqueName(module, "BytePtr");
    bytesPtr.clear();
    if( mdl->needsBytesPtr(module) )
        bytesPtr = uniqueName(module, "BytesPtr");
    arr2dName.clear();
    foreach( Type* elem, mdl->array2dElems(module) )
    {
        QByteArray name = typeRef(elem);
        for( int i = 0; i < name.size(); i++ )
        {
            if( !::isalnum(name[i]) )
                name[i] = '_';
        }
        name[0] = QChar::fromLatin1(name[0]).toUpper().toLatin1();
        arr2dName.insert(elem, uniqueName(module, "Array2dOf" + name));
    }
    castPtr.clear();
    foreach( int kind, mdl->castPtrKinds(module) )
    {
        Type t;
        t.kind = (quint8)kind;
        QByteArray name = basicType(&t);
        name[0] = QChar::fromLatin1(name[0]).toUpper().toLatin1();
        castPtr.insert(kind, uniqueName(module, name + "Ptr"));
    }
    arrPtr.clear();
    foreach( int kind, mdl->arrPtrKinds(module) )
    {
        Type t;
        t.kind = (quint8)kind;
        QByteArray name = basicType(&t);
        name[0] = QChar::fromLatin1(name[0]).toUpper().toLatin1();
        arrPtr.insert(kind, uniqueName(module, name + "ArrPtr"));
    }

    importAlias.clear();
    sysModule.clear();
    Declaration* imp = module->link;
    while( imp && imp->kind == Declaration::Import )
    {
        const Import i = imp->data.value<Import>();
        if( i.resolved && imp->name != i.moduleName )
            importAlias.insert(i.resolved, escape(imp->name));
        if( i.moduleName == "SYS" )
            sysModule = escape(imp->name);
        imp = imp->next;
    }
    addSys = sysModule.isEmpty() && mdl->needsSys(module);
    if( addSys )
        sysModule = "SYS";

    Module(module);

    out.flush();
    if( dev )
    {
        buffer.replace(QRegExp("[ \t]+\n"), "\n");
        // the section and comment logic can produce more than one blank line in a row
        while( buffer.contains("\n\n\n") )
            buffer.replace("\n\n\n", "\n\n");
        dev->write(buffer.toLatin1());
    }
    return errors.isEmpty();
}

QByteArray MicronGen::uniqueName(Declaration* module, const QByteArray& name)
{
    QByteArray res = name;
    Declaration* d = module->link;
    while( d )
    {
        if( d->name == res )
        {
            res += "_";
            d = module->link;
        }else
            d = d->next;
    }
    return res;
}

static Type* anonRecordOf(Type* t)
{
    if( t == 0 || t->kind != Type::Pointer || !t->anonymous )
        return 0;
    Type* base = dr(t->type());
    while( base && base->kind == Type::Array )
        base = dr(base->type());
    if( base && base->isSO() && base->anonymous )
        return base;
    return 0;
}

bool MicronGen::depsEmitted(Type* t, QSet<Type*>& seen)
{
    if( t == 0 || seen.contains(t) )
        return true;
    seen.insert(t);
    if( t->kind == Type::NameRef )
    {
        Type* d = dr(t);
        if( d && d->decl && d->decl->kind == Declaration::TypeDecl &&
                d->decl->getModule() == curMod && !emittedTypes.contains(d->decl) )
            return false;
        return true;
    }
    if( !depsEmitted(t->type(), seen) )
        return false;
    foreach( Declaration* sub, t->subs )
    {
        if( !depsEmitted(sub->type(), seen) )
            return false;
    }
    return true;
}

void MicronGen::anonPtrSection(Declaration* scope)
{
    if( scope == 0 || scope->kind == Declaration::Module )
        return;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
            return;
        d = d->next;
    }
    QList<Type*> mine;
    foreach( Type* t, anonPtrPending )
    {
        if( anonPtrScope.value(t) == scope )
            mine << t;
    }
    if( mine.isEmpty() )
        return;
    out << endl << ws() << "type" << endl;
    curLevel++;
    CommentTable* const old = cmts;
    const quint32 oldRow = lastRow;
    cmts = 0;
    lastRow = 0;
    foreach( Type* t, mine )
    {
        out << ws() << anonPtrName.value(t) << " = " << typeExpr(t) << endl;
        anonPtrPending.removeAll(t);
    }
    cmts = old;
    lastRow = oldRow;
    curLevel--;
}

void MicronGen::collectAnonPtrs(Declaration* scope, QSet<QByteArray>& used)
{
    bool hasTypeSection = false;
    Declaration* d = scope->link;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
        {
            hasTypeSection = true;
            used.insert(d->name);
        }
        d = d->next;
    }
    d = scope->link;
    while( d )
    {
        if( ( d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl ||
              d->kind == Declaration::ParamDecl ) && anonRecordOf(d->type()) && !anonPtrName.contains(d->type()) )
        {
            QByteArray name = d->name;
            if( !name.isEmpty() )
                name[0] = toupper(name[0]);
            name += "Type";
            while( used.contains(name) )
                name += "_";
            used.insert(name);
            anonPtrName.insert(d->type(), name);
            anonPtrScope.insert(d->type(), scope);
            anonPtrPending << d->type();
        }
        if( d->kind == Declaration::Procedure )
            collectAnonPtrs(d, used);
        d = d->next;
    }
}

void MicronGen::Module(Declaration* module)
{
    cl.analyze(module);

    out << genDedication() << endl;
    out << "// transpiled from " << module->data.value<ModuleData>().sourcePath << endl << endl;

    leading(module->pos);
    out << "module " << escape(module->name) << " [level=" << level << "]" << endl;

    Declaration* d = module->link;
    if( d && d->kind == Declaration::Import )
    {
        out << endl;
        d = ImportList(d);
    }

    if( !anyPtr.isEmpty() )
    {
        out << endl << "type " << anyPtr
            << " = pointer to byte // helper type for Oberon addresses and SYSTEM.PTR" << endl;
    }

    if( !bytePtr.isEmpty() )
    {
        out << endl << "type " << bytePtr
            << " = pointer to byte // helper type for VAR SYSTEM.BYTE parameters" << endl;
    }

    if( !bytesPtr.isEmpty() )
    {
        out << endl << "type " << bytesPtr
            << " = pointer to array of byte // helper type for VAR ARRAY OF SYSTEM.BYTE parameters"
            << endl;
    }

    arr2dPending.clear();
    anonPtrName.clear();
    anonPtrScope.clear();
    anonPtrPending.clear();
    openArrPending.clear();
    openArrName.clear();
    emittedTypes.clear();
    {
        QSet<QByteArray> used;
        collectAnonPtrs(module, used);
    }
    for( QHash<Type*,QByteArray>::const_iterator i = arr2dName.begin(); i != arr2dName.end(); ++i )
    {
        if( i.key() && i.key()->decl && i.key()->decl->getModule() == module )
        {
            arr2dPending.insert(i.key());
            continue;
        }
        out << endl << "type " << arr2dDecl(i.key()) << endl;
    }

    for( QMap<int,QByteArray>::const_iterator i = castPtr.begin(); i != castPtr.end(); ++i )
    {
        Type t;
        t.kind = (quint8)i.key();
        out << endl << "type " << i.value() << " = pointer to " << basicType(&t)
            << " // helper type for a variable reinterpreted by SYSTEM.VAL" << endl;
    }

    for( QMap<int,QByteArray>::const_iterator i = arrPtr.begin(); i != arrPtr.end(); ++i )
    {
        Type t;
        t.kind = (quint8)i.key();
        out << endl << "type " << i.value() << " = pointer to array of " << basicType(&t)
            << " // helper type for an address moved by an offset" << endl;
    }

    DeclSeq(d);

    if( genCmds )
        CmdProc(module);

    Declaration* body = 0;
    d = module->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure && d->begin )
        {
            body = d;
            break;
        }
        d = d->next;
    }

    out << endl;
    if( body && body->body )
    {
        curProc = body;
        curPlan = 0;
        out << "begin" << endl;
        lastRow = 0; // no blank line directly after begin
        curLevel++;
        StatSeq(body->body);
        curLevel--;
        curProc = 0;
    }
    flushUntil(module->data.value<ModuleData>().end.d_row + 1);
    out << "end " << escape(module->name) << "." << endl;
}

void MicronGen::CmdProc(Declaration* module)
{
    DeclList cmds;
    Declaration* d = module->link;
    while( d )
    {
        Type* ret = deref(d->type());
        if( d->kind == Declaration::Procedure && d->isPublic() && !d->begin && !d->receiver &&
                d->getParams(true).isEmpty() && ( ret == 0 || ret->kind == Type::NoType ) )
            cmds << d;
        d = d->next;
    }
    if( cmds.isEmpty() )
        return;

    const QByteArray type = uniqueName(module, "CMD_");
    const QByteArray name = uniqueName(module, "cmd_");
    out << endl << "type " << type << " = procedure" << endl;
    out << endl << "procedure " << name
        << "*(i: int32; name: ^array " << cmdNameLen << " of char): " << type << endl;
    out << "begin" << endl;
    curLevel++;
    for( int i = 0; i < cmds.size(); i++ )
    {
        out << ws() << "if i = " << i << " then if name # nil then name^ := \"" << cmds[i]->name
            << "\" end return " << escape(cmds[i]->name) << " end" << endl;
    }
    out << ws() << "return nil" << endl;
    curLevel--;
    out << "end " << name << endl;
}

Declaration* MicronGen::ImportList(Declaration* import)
{
    QByteArrayList imports;
    while( import && import->kind == Declaration::Import )
    {
        const Import i = import->data.value<Import>();
        if( i.moduleName != "SYSTEM" )
        {
            QByteArray str;
            if( import->name != i.moduleName )
                str = escape(import->name) + " := ";
            str += escape(i.moduleName);
            imports << str;
        }
        lastRow = import->pos.d_row;
        import = import->next;
    }
    if( addSys )
        imports << sysModule;
    if( !imports.isEmpty() )
        out << "import " << imports.join(", ") << endl;
    return import;
}

void MicronGen::typeDeps(Type* t, const DeclList& section, DeclList& deps, Declaration* owner)
{
    if( t == 0 )
        return;
    // t->decl == owner means that this is the declaration itself, not a reference to it
    if( t->decl != owner )
    {
        if( t->kind == Type::NameRef || t->kind == Type::Reference )
        {
            Type* d = deref(t);
            if( d && d->decl && section.contains(d->decl) )
                deps << d->decl;
            return;
        }
        if( t->decl && section.contains(t->decl) && !t->anonymous )
        {
            deps << t->decl;
            return;
        }
    }
    if( t->kind == Type::Pointer )
    {
        Type* base = deref(t->type());
        if( base && base->isSO() && ( !mdl->recordName(base).isEmpty() ||
                                      ( base->decl && section.contains(base->decl) ) ) )
            return;
    }
    typeDeps(t->type(), section, deps, 0);
    foreach( Declaration* sub, t->subs )
    {
        if( sub->kind == Declaration::Field || sub->kind == Declaration::ParamDecl )
            typeDeps(sub->type(), section, deps, 0);
    }
}

DeclList MicronGen::sortTypes(const DeclList& section)
{
    DeclList res;
    QSet<Declaration*> done, pending;
    for( int i = 0; i < section.size(); i++ )
    {
        DeclList stack;
        stack << section[i];
        while( !stack.isEmpty() )
        {
            Declaration* cur = stack.back();
            if( done.contains(cur) )
            {
                stack.pop_back();
                continue;
            }
            DeclList deps;
            typeDeps(cur->type(), section, deps, cur);
            bool ready = true;
            foreach( Declaration* dep, deps )
            {
                if( done.contains(dep) || dep == cur || pending.contains(dep) )
                    continue; // a cycle cannot be resolved by ordering
                ready = false;
                pending.insert(dep);
                stack << dep;
            }
            if( ready )
            {
                done.insert(cur);
                pending.remove(cur);
                res << cur;
                stack.pop_back();
            }
        }
    }
    return res;
}

void MicronGen::ConstSection(const DeclList& section)
{
    if( section.isEmpty() )
        return;
    leading(section.first()->pos); // the comments before the section belong to the section
    if( curProc == 0 )
        out << endl;
    out << ws() << "const" << endl;
    curLevel++;
    foreach( Declaration* d, section )
        ConstDecl(d);
    curLevel--;
}

void MicronGen::TypeSection(const DeclList& section)
{
    if( section.isEmpty() )
        return;
    leading(section.first()->pos);
    if( curProc == 0 )
        out << endl;
    out << ws() << "type" << endl;
    curLevel++;
    hoisted.clear();
    foreach( Declaration* t, sortTypes(section) )
        TypeDecl(t);
    for( int i = 0; i < hoisted.size(); i++ )
    {
        Declaration* ptr = hoisted[i].second;
        out << endl << ws() << escape(mdl->recordName(hoisted[i].first))
            << visibility(ptr) << " = ";
        RecordBody(hoisted[i].first);
        out << endl;
    }
    hoisted.clear();
    arr2dSection();
    foreach( Type* t, anonPtrPending )
    {
        QSet<Type*> seen;
        if( anonPtrScope.value(t) != section.first()->outer || !depsEmitted(t, seen) )
            continue;
        out << endl << ws() << anonPtrName.value(t) << " = " << typeExpr(t) << endl;
        anonPtrPending.removeAll(t);
    }
    curLevel--;
    foreach( Declaration* p, boundProcs )
    {
        if( !mdl->needsForward(p) )
            continue;
        ProcHeader(p, true);
        out << endl;
    }
}

void MicronGen::VarSection(const DeclList& section)
{
    if( section.isEmpty() )
        return;
    leading(section.first()->pos);
    if( curProc == 0 )
        out << endl;
    out << ws() << "var" << endl;
    curLevel++;
    for( int i = 0; i < section.size(); i++ )
    {
        Declaration* d = section[i];
        DeclList group;
        group << d;
        while( i + 1 < section.size() && !( cmts && cmts->hasTrailing(d->pos.d_row) ) )
        {
            Declaration* q = section[i+1];
            if( declType(q) != declType(d) || visibility(q) != visibility(d) ||
                    ( cmts && cmts->hasBefore(q->pos) ) )
                break;
            group << q;
            d = q;
            i++;
        }
        VarDecl(group);
    }
    curLevel--;
}

bool MicronGen::typesFirst(Declaration* d)
{
    DeclList types, vars;
    Declaration* i = d;
    while( i && ( i->kind == Declaration::ConstDecl || i->kind == Declaration::TypeDecl ||
                  i->kind == Declaration::VarDecl || i->kind == Declaration::LocalDecl ) )
    {
        if( i->kind == Declaration::TypeDecl )
            types << i;
        else if( i->kind != Declaration::ConstDecl )
            vars << i;
        i = i->next;
    }
    QSet<Declaration*> seen;
    i = d;
    while( i )
    {
        if( i->kind == Declaration::TypeDecl )
            seen.insert(i);
        else if( i->kind == Declaration::VarDecl || i->kind == Declaration::LocalDecl )
        {
            DeclList deps;
            typeDeps(i->type(), types, deps, i);
            foreach( Declaration* dep, deps )
            {
                if( !seen.contains(dep) )
                    return true;
            }
        }else if( i->kind != Declaration::ConstDecl )
            break;
        i = i->next;
    }
    return false;
}

static void refsOfExpr(Expression* e, QSet<Declaration*>& refs)
{
    if( e == 0 )
        return;
    if( e->kind == Expression::DeclRef )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( d && d->kind == Declaration::Procedure )
            refs.insert(d);
    }
    refsOfExpr(e->lhs, refs);
    refsOfExpr(e->rhs, refs);
    refsOfExpr(e->next, refs);
}

static void refsOfStat(Statement* s, QSet<Declaration*>& refs)
{
    while( s )
    {
        refsOfExpr(s->lhs, refs);
        refsOfExpr(s->rhs, refs);
        refsOfStat(s->body, refs);
        s = s->getNext();
    }
}

static void refsOfProc(Declaration* proc, QSet<Declaration*>& refs)
{
    refsOfStat(proc->body, refs);
    Declaration* d = proc->link;
    while( d )
    {
        if( d->kind == Declaration::Procedure )
            refsOfProc(d, refs);
        d = d->next;
    }
}

DeclList MicronGen::forwardDecls(const DeclList& procs)
{
    QSet<Declaration*> needed;
    for( int i = 0; i < procs.size(); i++ )
    {
        QSet<Declaration*> refs;
        refsOfProc(procs[i], refs);
        for( int j = i + 1; j < procs.size(); j++ )
        {
            if( refs.contains(procs[j]) )
                needed.insert(procs[j]);
        }
    }
    DeclList res;
    foreach( Declaration* p, procs )
    {
        if( needed.contains(p) )
            res << p;
    }
    return res;
}

Declaration* MicronGen::DeclSeq(Declaration* d)
{
    const DeclList outerBound = boundProcs;
    boundProcs.clear();

    anonPtrSection(d ? d->outer : 0);

    if( typesFirst(d) )
    {
        DeclList consts, types, vars;
        while( d && ( d->kind == Declaration::ConstDecl || d->kind == Declaration::TypeDecl ||
                      d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl ) )
        {
            if( d->kind == Declaration::ConstDecl )
                consts << d;
            else if( d->kind == Declaration::TypeDecl )
                types << d;
            else
                vars << d;
            d = d->next;
        }
        ConstSection(consts);
        TypeSection(types);
        VarSection(vars);
    }

    while( d && ( d->kind == Declaration::ConstDecl || d->kind == Declaration::TypeDecl ||
                  d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl ||
                  d->kind == Declaration::Procedure ) )
    {
        if( d->kind == Declaration::ConstDecl )
        {
            DeclList section;
            while( d && d->kind == Declaration::ConstDecl )
            {
                section << d;
                d = d->next;
            }
            ConstSection(section);
        }else if( d->kind == Declaration::TypeDecl )
        {
            DeclList section;
            while( d && d->kind == Declaration::TypeDecl )
            {
                section << d;
                d = d->next;
            }
            TypeSection(section);
        }else if( d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl )
        {
            DeclList section;
            while( d && (d->kind == Declaration::VarDecl || d->kind == Declaration::LocalDecl) )
            {
                section << d;
                d = d->next;
            }
            VarSection(section);
        }else if( d->kind == Declaration::Procedure )
        {
            DeclList procs;
            Declaration* p = d;
            while( p && p->kind == Declaration::Procedure )
            {
                if( !p->begin && !p->extern_ ) // the module body is generated separately
                    procs << p;
                p = p->next;
            }
            const DeclList fwd = forwardDecls(procs);
            if( !fwd.isEmpty() )
            {
                out << endl;
                foreach( Declaration* p, fwd )
                {
                    ProcHeader(p, true);
                    out << endl;
                }
            }
            while( d && d->kind == Declaration::Procedure )
            {
                if( !d->begin )
                    ProcDecl(d);
                d = d->next;
            }
        }
    }

    const DeclList bound = boundProcs;
    boundProcs.clear();
    foreach( Declaration* p, bound )
        ProcDecl(p);
    boundProcs = outerBound;

    return d;
}

void MicronGen::ConstDecl(Declaration* d)
{
    leading(d->pos);
    out << ws() << escape(d->name) << visibility(d) << " = ";
    bool ok = false;
    const qint64 v = d->expr ? constVal(d->expr, &ok) : d->data.toLongLong();
    if( mdl->isAddress(d) && v != 0 )
        out << "0" << QByteArray::number((quint32)v, 16).toUpper() << "H";
    else if( d->expr )
    {
        const bool feeds = constFeeds.contains(d);
        if( feeds )
            constCtx++;
        Expr(d->expr);
        if( feeds )
            constCtx--;
    }
    else
        out << d->data.toLongLong();
    trailing(d->pos.d_row);
    out << endl;
}

QByteArray MicronGen::recordRef(Type* record)
{
    QByteArray res = escape(mdl->recordName(record));
    Declaration* owner = mdl->recordOwner(record);
    Declaration* m = owner ? owner->getModule() : 0;
    if( m && m != curMod )
        res = moduleRef(m) + "." + res;
    return res;
}

void MicronGen::RecordBody(Type* t)
{
    const bool isObj = mdl->isObject(t);
    out << (isObj ? "object" : "record");
    Type* base = deref(t->type());
    if( base )
    {
        if( base->kind == Type::Pointer )
            base = deref(base->type());
        out << " (" << ( base->decl && !base->anonymous ? qualident(base->decl) :
                         recordRef(base) ) << ")";
    }
    out << endl;
    curLevel++;
    DeclList fields;
    foreach( Declaration* f, t->subs )
    {
        if( f->kind == Declaration::Field )
            fields << f;
    }
    for( int i = 0; i < fields.size(); i++ )
    {
        Declaration* f = fields[i];
        leading(f->pos);
        out << ws() << escape(f->name) << visibility(f);
        while( i + 1 < fields.size() && !( cmts && cmts->hasTrailing(f->pos.d_row) ) )
        {
            Declaration* g = fields[i+1];
            if( declType(g) != declType(f) || ( cmts && cmts->hasBefore(g->pos) ) )
                break;
            out << ", " << escape(g->name) << visibility(g);
            f = g;
            i++;
        }
        out << ": " << declType(f);
        trailing(f->pos.d_row);
        out << endl;
    }
    curLevel--;
    out << ws() << "end";
}

void MicronGen::TypeDecl(Declaration* d)
{
    leading(d->pos);
    Type* t = d->type();
    out << ws() << escape(d->name) << visibility(d) << " = ";
    Type* td = deref(t);
    Type* hoist = 0;
    if( td && td->kind == Type::Pointer )
    {
        Type* base = deref(td->type());
        if( base && !mdl->recordName(base).isEmpty() && mdl->recordOwner(base) == d )
            hoist = base;
    }
    if( hoist )
    {
        out << "pointer to " << escape(mdl->recordName(hoist));
        hoisted << qMakePair(hoist, d);
    }
    else if( td && td->isSO() && td->decl == d )
        RecordBody(td);
    else if( t->kind != Type::NameRef && t->decl == d )
        out << typeExpr(t, true);
    else
        out << typeRef(t);
    trailing(d->pos.d_row);
    out << endl;

    if( td && mdl->hasSynthesizedPointer(d) )
    {
        out << ws() << escape(mdl->pointerName(td)) << visibility(d) << " = pointer to "
            << escape(d->name) << endl;
    }

    emittedTypes.insert(d);

    foreach( Declaration* p, td ? td->subs : DeclList() )
    {
        if( p->kind == Declaration::Procedure )
            boundProcs << p;
    }
}

void MicronGen::VarDecl(const DeclList& group)
{
    Declaration* d = group.first();
    leading(d->pos);
    out << ws();
    for( int i = 0; i < group.size(); i++ )
        out << ( i ? ", " : "" ) << escape(group[i]->name) << visibility(group[i]);
    out << ": " << declType(d);
    trailing(group.last()->pos.d_row);
    out << endl;
}

void MicronGen::ProcHeader(Declaration* proc, bool forward)
{
    out << ws() << "procedure ";
    if( forward )
        out << "^ ";
    if( proc->receiver )
    {
        Type* obj = deref(proc->outer ? proc->outer->type() : 0);
        out << "(self: " << (obj ? pointerTo(obj) : QByteArray("???")) << ") ";
    }
    out << escape(proc->name) << visibility(proc) << "(";
    const DeclList params = proc->getParams(true);
    int n = 0;
    for( int i = 0; i < params.size(); i++ )
    {
        Declaration* p = params[i];
        if( n != 0 )
            out << "; ";
        n++;
        const bool ro = !isVarParamDecl(p) && isArray(p->type());
        if( ro )
            out << "const ";
        const QByteArray type = formalType(p);
        out << escape(p->name);
        while( !mdl->needsLen(p) && i + 1 < params.size() )
        {
            Declaration* q = params[i+1];
            const bool qro = !isVarParamDecl(q) && isArray(q->type());
            if( qro != ro || mdl->needsLen(q) || formalType(q) != type )
                break;
            out << ", " << escape(q->name);
            p = q;
            i++;
        }
        out << ": " << type;
        if( mdl->needsLen(p) )
            out << "; " << MicronModel::lenName(p) << ": int32";
    }
    const ClosureLifter::ProcPlan* plan = cl.plan(proc);
    if( plan )
    {
        foreach( const ClosureLifter::LiftParam& lp, plan->addedParams )
        {
            if( n != 0 )
                out << "; ";
            n++;
            if( lp.sourceDecl->kind == Declaration::ParamDecl &&
                    deref(lp.sourceDecl->type()) &&
                    deref(lp.sourceDecl->type())->kind == Type::Array )
                out << escape(lp.name) << ": " << formalType(lp.sourceDecl);
            else
                out << escape(lp.name) << ": " << "pointer to " << declType(lp.sourceDecl);
        }
    }
    out << ")";
    Type* ret = deref(proc->type());
    if( ret && ret->kind != Type::NoType )
        out << ": " << declType(proc);
}

void MicronGen::Assembler(Declaration* proc)
{
    manual << QString("%1:%2: inline assembler in %3 needs manual translation")
              .arg(QString::fromUtf8(curMod->name)).arg(proc->pos.d_row).arg(proc->name.constData());
    out << ws() << "// TODO o2m: the following inline assembler needs manual translation" << endl;
    const QStringList lines = proc->data.toString().split('\n');
    foreach( const QString& line, lines )
        out << ws() << "// " << line.trimmed() << endl;
    Type* ret = deref(proc->type());
    if( ret && ret->kind != Type::NoType )
    {
        out << ws() << "return " << zeroValue(ret)
            << " // TODO o2m: the result is delivered by the assembler code above" << endl;
    }
}

void MicronGen::ProcDecl(Declaration* proc)
{
    Declaration* oldProc = curProc;
    const ClosureLifter::ProcPlan* oldPlan = curPlan;
    curProc = proc;
    curPlan = cl.plan(proc);

    out << endl;
    leading(proc->pos);
    ProcHeader(proc);
    out << endl;

    if( proc->extern_ )
    {
        out << ws() << "extern" << endl;
        curProc = oldProc;
        curPlan = oldPlan;
        return;
    }

    Declaration* d = proc->link;
    while( d && d->kind == Declaration::ParamDecl )
        d = d->next;
    curLevel++;
    DeclSeq(d);
    curLevel--;

    out << ws() << "begin" << endl;
    lastRow = 0; // no blank line directly after begin
    curLevel++;
    if( proc->body )
    {
        if( proc->body->kind == Statement::Assembler )
            Assembler(proc);
        else
        {
            StatSeq(proc->body);
            Type* ret = deref(proc->type());
            if( ret && ret->kind != Type::NoType )
            {
                Ast::Statement* last = proc->body;
                Ast::Statement* cur = last;
                while( cur )
                {
                    if( cur->kind != Ast::Statement::End )
                        last = cur;
                    cur = cur->getNext();
                }
                if( last == 0 || last->kind != Ast::Statement::Return )
                    out << ws() << "return "
                        << ( mdl->isAddress(proc) ? QByteArray("nil") : zeroValue(ret) )
                        << " // TODO o2m: Micron requires a result on all control paths" << endl;
            }
        }
    }
    curLevel--;
    out << ws() << "end " << escape(proc->name) << endl;

    curProc = oldProc;
    curPlan = oldPlan;
}

void MicronGen::StatSeq(Ast::Statement* s)
{
    while( s )
        s = Statement(s);
}

Ast::Statement* MicronGen::Statement(Ast::Statement* s)
{
    if( s == 0 )
        return 0;
    if( s->kind == Ast::Statement::End )
    {
        // just marks the end of a non-empty statement sequence
        flushUntil(s->pos.d_row);
        return s->getNext();
    }

    leading(s->pos);
    out << ws();
    switch( s->kind )
    {
    case Ast::Statement::Assig:
        Assig(s);
        break;
    case Ast::Statement::Call:
        CallStat(s);
        break;
    case Ast::Statement::If:
        s = IfStat(s);
        break;
    case Ast::Statement::Case:
        s = CaseStat(s);
        break;
    case Ast::Statement::With:
        s = WithStat(s);
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
        out << "exit";
        break;
    case Ast::Statement::Return:
        ReturnStat(s);
        break;
    case Ast::Statement::ForAssig:
        s = ForStat(s);
        break;
    case Ast::Statement::StatBlock:
        StatSeq(s->body);
        break;
    default:
        error("statement not supported", s->pos);
        break;
    }
    trailing(s->pos.d_row);
    out << endl;
    return s->getNext();
}

static bool valArgs(Expression* e, Expression*& to, Expression*& from)
{
    if( !isBuiltinCall(e, Builtin::SYSTEM_VAL) || e->rhs == 0 || e->rhs->next == 0 )
        return false;
    to = e->rhs;
    from = e->rhs->next;
    return true;
}

bool MicronGen::isSetOf(Expression* e)
{
    Type* t = deref(e ? e->type() : 0);
    return t && t->kind == Type::SET;
}

bool MicronGen::isPtrOrAddr(Expression* e)
{
    if( isBuiltinCall(e, Builtin::SYSTEM_VAL) && e->rhs && e->rhs->next )
    {
        Type* to = deref(e->rhs->type());
        return to && ( to->isInteger() || isPointerKind(to) ) && isPtrOrAddr(e->rhs->next);
    }
    if( isBuiltinCall(e, Builtin::SYSTEM_ADR) )
        return true;
    Type* t = deref(e ? e->type() : 0);
    return e && ( isPointerKind(e->type()) || ( t && t->kind == Type::NIL ) ||
                  mdl->isAddrExpr(e) );
}

bool MicronGen::isNil(Expression* e)
{
    Type* t = deref(e ? e->type() : 0);
    return t && t->kind == Type::NIL;
}

bool MicronGen::isPtrForm(Expression* e)
{
    if( e == 0 )
        return false;
    switch( e->kind )
    {
    case Expression::Add:
    case Expression::Sub:
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
        return false;
    }
    return isPtrOrAddr(e) && !isBuiltinCall(e, Builtin::SYSTEM_ADR);
}

static int byteWidth(Ast::Type*);

bool MicronGen::ptrOffsetForm(Expression* e, Expression** base, Expression** off)
{
    if( e == 0 || ( e->kind != Expression::Add && e->kind != Expression::Sub ) )
        return false;
    if( !isPtrForm(e->lhs) || isPtrForm(e->rhs) )
        return false;
    Type* t = deref(e->lhs->type());
    if( t && isPointerKind(t) && byteWidth(deref(t->type())) != 1 )
        return false;
    if( base )
        *base = e->lhs;
    if( off )
        *off = e->rhs;
    return true;
}

bool MicronGen::ptrBitOp(Ast::Statement* s)
{
    Expression* to = 0;
    Expression* from = 0;
    Expression* target = s->lhs;
    if( valArgs(s->lhs, to, from) && isSetOf(to) )
        target = from;
    Expression* rhs = s->rhs;
    if( valArgs(rhs, to, from) && isPtrOrAddr(to) )
        rhs = from;
    if( rhs == 0 || ( rhs->kind != Expression::Add && rhs->kind != Expression::Sub ) )
        return false;
    if( !valArgs(rhs->lhs, to, from) || !isSetOf(to) || !isPtrOrAddr(from) ||
            !isPtrOrAddr(target) )
        return false;

    const bool add = rhs->kind == Expression::Add;
    const QByteArray tgt = toStr(target, true);
    const QByteArray src = toStr(from, false, true, target->type());
    if( tgt != src )
        out << tgt << " := " << src << endl << ws();
    const QByteArray bits = "bset(val(uint32, ord(" + tgt + ")))";
    out << ( add ? "inc(" : "dec(" ) << tgt << ", ord(";
    if( add )
        out << toStr(rhs->rhs) << " - " << bits; // only the bits not yet set
    else
        out << bits << " * " << toStr(rhs->rhs); // only the bits actually set
    out << "))";
    return true;
}

Expression* MicronGen::addrOperand(Expression* e)
{
    if( e == 0 )
        return 0;
    if( isPtrForm(e) )
        return e;
    switch( e->kind )
    {
    case Expression::Add:
    case Expression::Sub:
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::Plus:
    case Expression::Minus: {
            Expression* res = addrOperand(e->lhs);
            return res ? res : addrOperand(e->rhs);
        }
    case Expression::Call:
        if( isBuiltinCall(e, Builtin::SYSTEM_VAL) )
            return addrOperand(e->rhs); // the arithmetic is wrapped in a SYSTEM.VAL
        break;
    }
    return 0;
}

bool MicronGen::ptrOffAssign(const QByteArray& tgt, Expression* e, Type* to)
{
    Expression* base = addrOperand(e);
    if( base == 0 || base == e )
        return false;
    const QByteArray b = toStr(base, false, true, to);
    if( b.contains("(*") ) // don't duplicate a TODO comment
        return false;
    out << tgt << " := ";
    const QByteArray pt = to && isPointerKind(to) ? QByteArray() : anyPtr;
    if( !pt.isEmpty() )
        out << "cast(" << pt << ", ";
    out << "ptroff(" << b << ", " << numStr(e) << " - val(int32, ord(" << b << ")))";
    if( !pt.isEmpty() )
        out << ")";
    return true;
}

bool MicronGen::ptrAssign(const QByteArray& tgt, Expression* e, Type* to)
{
    if( MicronModel::ptrMove(e) == 0 )
        return ptrOffAssign(tgt, e, to);
    QList<Expression*> ops;
    QList<Expression*> offs;
    Expression* base = mdl->moveChain(e, &ops, &offs);
    bool isConst = false;
    const qint64 c = constVal(base, &isConst);
    if( !isConst && !isPtrOrAddr(base) )
        return false;
    const QByteArray src = isConst ?
                anyPtr + "{" + constAddr(base, c) + "}" :
                toStr(base, false, true, to);
    if( tgt != src )
        out << tgt << " := " << src << endl << ws();
    for( int i = 0; i < ops.size(); i++ )
    {
        if( i != 0 )
            out << endl << ws();
        out << ( ops[i]->kind == Expression::Add ? "inc(" : "dec(" ) << tgt << ", ";
        if( isPtrOrAddr(offs[i]) )
            ordExpr(offs[i]);
        else
            Expr(offs[i]);
        out << ")";
    }
    return true;
}

bool MicronGen::ptrMove(Ast::Statement* s)
{
    if( !isPtrForm(s->lhs) )
        return false;
    Type* to = s->lhs->type();
    if( isBuiltinCall(s->rhs, Builtin::SYSTEM_VAL) && s->rhs->rhs )
        to = s->rhs->rhs->type(); // the arithmetic is wrapped in a SYSTEM.VAL
    return ptrAssign(toStr(s->lhs, true), s->rhs, to);
}

bool MicronGen::setAssig(Ast::Statement* s)
{
    Expression* to = 0;
    Expression* from = 0;
    if( !valArgs(s->lhs, to, from) || !isSetOf(to) || isPtrOrAddr(from) )
        return false;
    Type* t = deref(from->type());
    if( t == 0 || !t->isInteger() )
        return false;
    designator(from);
    out << " := val(" << basicType(t) << ", ord(";
    Expr(s->rhs);
    out << "))";
    return true;
}

void MicronGen::recordDesig(Expression* e)
{
    designator(e);
    if( e->kind == Expression::Cast )
    {
        Type* t = deref(e->type());
        Declaration* d = e->lhs->kind == Expression::DeclRef ?
                    e->lhs->val.value<Declaration*>() : 0;
        if( t && t->isSO() && d && ( lifted(d) || isPointerForm(d) ) )
            out << "^";
        return;
    }
    if( e->kind != Expression::DeclRef )
        return;
    Declaration* d = e->val.value<Declaration*>();
    if( d && ( lifted(d) || isPointerForm(d) ) && deref(d->type()) && deref(d->type())->isSO() )
        out << "^";
}

void MicronGen::Assig(Ast::Statement* s)
{
    if( ptrBitOp(s) || ptrMove(s) || setAssig(s) )
        return;
    if( isPtrForm(s->lhs) )
    {
        bool isConst = false;
        const qint64 c = constVal(s->rhs, &isConst);
        if( isConst )
        {
            out << toStr(s->lhs, true) << " := ";
            if( c == 0 )
                out << "nil";
            else
                out << anyPtr << "{" << constAddr(s->rhs, c) << "}";
            return;
        }
    }
    Type* lt = deref(s->lhs->type());
    Type* rt = deref(s->rhs->type());
    if( isArr2d(s->lhs->type()) && isNil(s->rhs) )
    {
        const QByteArray d = toMicron(s->lhs);
        out << d << ".data := nil;" << endl << ws()
            << d << ".len0 := 0;" << endl << ws() << d << ".len1 := 0";
        return;
    }
    if( lt && lt->kind == Type::Array && deref(lt->type()) && deref(lt->type())->kind == Type::CHAR &&
            rt && (rt->kind == Type::StrLit || rt->kind == Type::Array) )
    {
        out << "copy(";
        addressOf(s->lhs);
        out << ", ";
        if( rt->kind == Type::StrLit )
            Expr(s->rhs);
        else
            addressOf(s->rhs);
        out << ", ";
        lenOf(s->lhs);
        out << ")";
        return;
    }
    if( lt && lt->isSO() && rt && rt->isSO() )
    {
        recordDesig(s->lhs);
        out << " := ";
        recordDesig(s->rhs);
        return;
    }
    Declaration* guarded = s->lhs->kind == Expression::DeclRef ?
                s->lhs->val.value<Declaration*>() : 0;
    const bool unguard = guarded && withCast.contains(guarded);
    QByteArray guard;
    if( unguard )
    {
        guard = withCast.value(guarded);
        withCast.remove(guarded);
    }
    designator(s->lhs);
    if( unguard )
        withCast.insert(guarded, guard);
    out << " := ";
    if( isPtrOrAddr(s->lhs) )
    {
        const QByteArray rhs = toStr(s->rhs, false, true, s->lhs->type());
        out << rhs;
        if( !isPtrOrAddr(s->rhs) && !isZeroLit(s->rhs) && !rhs.startsWith("ptroff(") &&
                !rhs.startsWith("cast(") &&
                !isBuiltinCall(s->rhs, Builtin::SYSTEM_VAL) )
            todo("Micron has no conversion from a dynamic address to a pointer", s->pos);
    }
    else
        coerce(s->rhs, s->lhs->type());
}

void MicronGen::CallStat(Ast::Statement* s)
{
    Expr(s->lhs);
}

QPair<Ast::Declaration*,QByteArray> MicronGen::typeTest(Ast::Expression* e)
{
    QPair<Declaration*,QByteArray> res(0,QByteArray());
    if( e == 0 || e->kind != Expression::Is || e->lhs->kind != Expression::DeclRef )
        return res;
    Type* guard = deref(e->rhs->type());
    if( guard && guard->kind == Type::Pointer )
        guard = deref(guard->type());
    res.first = e->lhs->val.value<Declaration*>();
    res.second = guard && guard->isSO() ? pointerTo(guard) : typeRef(e->rhs->type());
    return res;
}

static bool modifiesExpr(Expression* e, Declaration* d)
{
    if( e == 0 )
        return false;
    if( isBuiltinCall(e, Builtin::SYSTEM_ADR) )
    {
        Expression* a = Expression::getList(e->rhs).first();
        if( a && a->kind == Expression::DeclRef && a->val.value<Declaration*>() == d )
            return true;
    }
    return modifiesExpr(e->lhs, d) || modifiesExpr(e->rhs, d) || modifiesExpr(e->next, d);
}

static bool modifies(Statement* s, Declaration* d)
{
    while( s )
    {
        if( s->kind == Statement::Assig && s->lhs && s->lhs->kind == Expression::DeclRef &&
                s->lhs->val.value<Declaration*>() == d )
            return true;
        if( modifiesExpr(s->lhs, d) || modifiesExpr(s->rhs, d) || modifies(s->body, d) )
            return true;
        s = s->getNext();
    }
    return false;
}

bool MicronGen::typeCaseChain(Ast::Statement* s, Declaration*& var, QList<Branch>& branches)
{
    var = 0;
    branches.clear();
    if( level < 4 )
        return false; // the type case requires the Micron language level 4
    Ast::Statement* cur = s;
    while( cur )
    {
        const QPair<Declaration*,QByteArray> test = typeTest(cur->rhs);
        if( test.first == 0 )
            return false;
        Type* t = deref(test.first->type());
        if( t == 0 || !( ( t->kind == Type::Pointer && !isPointerForm(test.first) ) ||
                         ( t->isSO() && isVarParamDecl(test.first) ) ) )
            return false;
        if( var == 0 )
            var = test.first;
        else if( var != test.first )
            return false;
        for( int i = 0; i < branches.size(); i++ )
            if( branches[i].second == test.second )
                return false;
        branches << Branch(cur, test.second);
        if( modifies(cur->body, var) )
            return false;
        cur = cur->getNext();
        if( cur == 0 || cur->kind != Statement::Elsif )
            break;
    }
    return !branches.isEmpty();
}

Ast::Statement* MicronGen::TypeCaseStat(Ast::Statement* s, Declaration* var, const QList<Branch>& branches)
{
    out << "case ";
    designator(branches.first().first->rhs->lhs);
    out << " of" << endl;
    Ast::Statement* last = s;
    for( int i = 0; i < branches.size(); i++ )
    {
        Ast::Statement* b = branches[i].first;
        last = b;
        if( i )
            leading(b->pos);
        out << ws() << "| " << branches[i].second << ":" << endl;
        curLevel++;
        const QByteArray outerCast = withCast.value(var);
        const bool outerNarrow = narrowVars.contains(var);
        withCast.insert(var, branches[i].second);
        narrowVars.insert(var);
        knownTests << qMakePair(var, branches[i].second);
        StatSeq(b->body);
        knownTests.removeLast();
        if( !outerNarrow )
            narrowVars.remove(var);
        if( outerCast.isEmpty() )
            withCast.remove(var);
        else
            withCast.insert(var, outerCast);
        curLevel--;
    }
    if( last->getNext() && last->getNext()->kind == Statement::Else )
    {
        last = last->getNext();
        leading(last->pos);
        out << ws() << "else" << endl;
        curLevel++;
        StatSeq(last->body);
        curLevel--;
    }
    out << ws() << "end";
    return last;
}

Ast::Statement* MicronGen::IfStat(Ast::Statement* s)
{
    Declaration* var = 0;
    QList<Branch> branches;
    if( typeCaseChain(s, var, branches) )
        return TypeCaseStat(s, var, branches);

    out << "if ";
    condition(s->rhs);
    out << " then" << endl;
    curLevel++;
    const QPair<Declaration*,QByteArray> test = typeTest(s->rhs);
    if( test.first )
        knownTests << test;
    StatSeq(s->body);
    if( test.first )
        knownTests.removeLast();
    curLevel--;
    while( s->getNext() && s->getNext()->kind == Ast::Statement::Elsif )
    {
        s = s->getNext();
        leading(s->pos);
        out << ws() << "elsif ";
        condition(s->rhs);
        out << " then" << endl;
        curLevel++;
        const QPair<Declaration*,QByteArray> test = typeTest(s->rhs);
        if( test.first )
            knownTests << test;
        StatSeq(s->body);
        if( test.first )
            knownTests.removeLast();
        curLevel--;
    }
    if( s->getNext() && s->getNext()->kind == Ast::Statement::Else )
    {
        s = s->getNext();
        leading(s->pos);
        out << ws() << "else" << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
    }
    out << ws() << "end";
    return s;
}

Type* MicronGen::narrowed(Expression* arg, Type* ft)
{
    Declaration* d = arg->kind == Expression::DeclRef ? arg->val.value<Declaration*>() : 0;
    if( d == 0 || withCast.contains(d) || ft == 0 )
        return 0;
    Type* to = ft->kind == Type::Pointer ? deref(ft->type()) : ft;
    Type* dt = deref(d->type());
    if( dt && dt->kind == Type::Pointer )
        dt = deref(dt->type());
    if( to == 0 || dt == 0 || !to->isSO() || !dt->isSO() || to == dt || Type::isA(to, dt) )
        return 0;
    return to;
}

Type* MicronGen::fieldOwner(Declaration* field)
{
    if( fieldOwners.contains(field) )
        return fieldOwners.value(field);
    Type* res = 0;
    Declaration* d = field->outer ? field->outer->link : 0;
    while( d )
    {
        if( d->kind == Declaration::TypeDecl )
        {
            Type* t = deref(d->type());
            if( t && t->kind == Type::Pointer )
                t = deref(t->type());
            if( t && t->isSO() && t->find(field->name, false) == field )
            {
                res = t;
                break;
            }
        }
        d = d->next;
    }
    fieldOwners.insert(field, res);
    return res;
}

void MicronGen::caseLabel(Expression* label, bool charCase, bool numLabels, Type* labelType)
{
    Type* t = deref(label->type());
    if( labelType && t && t->isInteger() && t->kind != labelType->kind &&
            label->kind != Expression::Literal && label->kind != Expression::ConstVal )
    {
        bool ok = false;
        const qint64 v = constVal(label, &ok);
        if( ok )
        {
            out << v << " (* " << toStr(label) << " *)";
            return;
        }
    }
    if( t && t->kind == Type::StrLit )
    {
        const QByteArray str = label->val.toByteArray();
        if( charCase && str.size() == 1 )
        {
            if( numLabels )
                out << quint8(str[0]);
            else
                out << hexChar(quint8(str[0]));
            return;
        }
    }else if( t && t->kind == Type::CHAR )
    {
        bool ok = false;
        const qint64 ch = constVal(label, &ok);
        if( ok && ch >= 0 && ch < 256 )
        {
            if( numLabels )
                out << ch;
            else
                out << hexChar(quint8(ch));
            return;
        }
    }
    Expr(label);
}

Ast::Statement* MicronGen::CaseStat(Ast::Statement* s)
{
    out << "case ";
    Expr(s->rhs);
    out << " of" << endl;
    Type* caseType = deref(s->rhs->type());
    const bool charCase = caseType && caseType->kind == Type::CHAR;
    Type* labelType = caseType && caseType->isInteger() ? caseType : 0;
    const bool numLabels = charCase && ( isBuiltinCall(s->rhs, Builtin::CAP) ||
                                         isBuiltinCall(s->rhs, Builtin::CHR) ||
                                         isBuiltinCall(s->rhs, Builtin::SYSTEM_VAL) );
    bool first = true;
    while( s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
    {
        s = s->getNext();
        leading(s->pos);
        out << ws() << (first ? "  " : "| ");
        first = false;
        Expression* label = s->rhs;
        while( label )
        {
            if( label != s->rhs )
                out << ", ";
            if( label->kind == Expression::Range )
            {
                caseLabel(label->lhs, charCase, numLabels, labelType);
                out << "..";
                caseLabel(label->rhs, charCase, numLabels, labelType);
            }else
                caseLabel(label, charCase, numLabels, labelType);
            label = label->next;
        }
        out << ":" << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
    }
    if( s->getNext() && s->getNext()->kind == Ast::Statement::Else )
    {
        s = s->getNext();
        leading(s->pos);
        out << ws() << "else" << endl;
        curLevel++;
        StatSeq(s->body);
        curLevel--;
    }
    out << ws() << "end";
    return s;
}

Ast::Statement* MicronGen::WithStat(Ast::Statement* s)
{
    Type* lhsT = deref(s->lhs->type());
    Type* guard = deref(s->rhs->type());
    if( guard && guard->kind == Type::Pointer )
        guard = deref(guard->type());

    Declaration* d = s->lhs->kind == Expression::DeclRef ?
                s->lhs->val.value<Declaration*>() : 0;
    const bool untyped = lhsT && ( lhsT->kind == Type::PTR || lhsT->kind == Type::ANY ||
                                   ( lhsT->kind == Type::Pointer && deref(lhsT->type()) &&
                                     deref(lhsT->type())->kind == Type::ANY ) );
    if( untyped && d && guard )
    {
        const QByteArray to = guard->isSO() ? pointerTo(guard) : typeRef(s->rhs->type());
        todo("the type test of the untyped pointer is not possible in Micron", s->pos);
        out << endl;
        withCast.insert(d, to);
        castOnly.insert(d);
        StatSeq(s->body);
        withCast.remove(d);
        castOnly.remove(d);
        return s;
    }

    const QByteArray to = guard && guard->isSO() ? pointerTo(guard) : typeRef(s->rhs->type());
    if( d && knownTests.contains(qMakePair(d,to)) )
    {
        out.flush();
        while( buffer.endsWith(QLatin1Char(' ')) )
            buffer.chop(1);
        out.seek(buffer.size());
        const bool cast = !withCast.contains(d);
        if( cast )
            withCast.insert(d, to);
        StatSeq(s->body);
        if( cast )
            withCast.remove(d);
        return s;
    }
    const bool typeCase = level >= 4 && lhsT && guard && guard->isSO() &&
            ( lhsT->kind == Type::Pointer || lhsT->kind == Type::PTR ||
              ( lhsT->isSO() && d && isVarParamDecl(d) ) );
    const bool narrow = typeCase && d && !modifies(s->body, d) &&
            ( !isPointerForm(d) || isVarParamDecl(d) );
    if( typeCase )
    {
        out << "case ";
        designator(s->lhs);
        out << " of" << endl;
        out << ws() << "| " << to << ":" << endl;
    }else
    {
        out << "if ";
        designator(s->lhs);
        out << " is " << to << " then" << endl;
    }
    const QByteArray outerCast = d ? withCast.value(d) : QByteArray();
    const bool outerNarrow = d && narrowVars.contains(d);
    if( d && guard )
        withCast.insert(d, to);
    if( narrow )
        narrowVars.insert(d);
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    if( narrow && !outerNarrow )
        narrowVars.remove(d);
    if( d )
    {
        if( outerCast.isEmpty() )
            withCast.remove(d);
        else
            withCast.insert(d, outerCast);
    }
    out << ws() << "end";
    return s;
}

void MicronGen::WhileStat(Ast::Statement* s)
{
    out << "while ";
    condition(s->rhs);
    out << " do" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    out << ws() << "end";
}

void MicronGen::RepeatStat(Ast::Statement* s)
{
    out << "repeat" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    out << ws() << "until ";
    condition(s->rhs);
}

void MicronGen::LoopStat(Ast::Statement* s)
{
    loopStack.push_back(s);
    out << "loop" << endl;
    curLevel++;
    StatSeq(s->body);
    curLevel--;
    loopStack.pop_back();
    out << ws() << "end";
}

Ast::Statement* MicronGen::ForStat(Ast::Statement* s)
{
    Expression* i = s->lhs;
    Expression* from = s->rhs;
    Ast::Statement* body = s->body;
    if( s->getNext() == 0 || s->getNext()->kind != Ast::Statement::ForToBy )
    {
        error("invalid for statement", s->pos);
        return s;
    }
    s = s->getNext();
    Expression* to = s->lhs;
    Expression* by = s->rhs;

    const QByteArray var = toMicron(i);
    if( isPtrForm(i) )
    {
        bool ok = false;
        const qint64 step = by ? constVal(by, &ok) : 1;
        if( !ptrAssign(var, from, i->type()) )
        {
            out << var << " := ";
            addrExpr(from);
        }
        const bool bothPtr = isPtrForm(to) || ptrOffsetForm(to);
        out << ";" << endl << ws() << "while " << ( bothPtr ? "ord(" : "val(int32, ord(" )
            << var << ( bothPtr ? ")" : "))" )
            << ( by && ok && step < 0 ? " >= " : " <= " );
        if( bothPtr )
            ordExpr(to, false);
        else
            Expr(to);
        out << " do" << endl;
        curLevel++;
        StatSeq(body);
        out << ws() << ( by && ok && step < 0 ? "dec(" : "inc(" ) << var << ", "
            << ( ok ? qAbs(step) : 1 ) << ")" << endl;
        curLevel--;
        out << ws() << "end";
        return s;
    }
    if( var.endsWith("^") )
    {
        bool ok = false;
        const qint64 step = by ? constVal(by, &ok) : 1;
        out << var << " := ";
        Expr(from);
        out << ";" << endl << ws() << "while " << var
            << ( by && ok && step < 0 ? " >= " : " <= " );
        Expr(to);
        out << " do" << endl;
        curLevel++;
        StatSeq(body);
        out << ws() << ( by && ok && step < 0 ? "dec(" : "inc(" ) << var << ", "
            << ( ok ? qAbs(step) : 1 ) << ")" << endl;
        curLevel--;
        out << ws() << "end";
        return s;
    }
    out << "for ";
    designator(i);
    out << " := ";
    Expr(from);
    out << " to ";
    Expr(to);
    if( by )
    {
        out << " by ";
        Expr(by);
    }
    out << " do" << endl;
    curLevel++;
    StatSeq(body);
    curLevel--;
    out << ws() << "end";
    return s;
}

void MicronGen::ReturnStat(Ast::Statement* s)
{
    out << "return";
    if( s->rhs )
    {
        out << " ";
        if( mdl->isAddress(curProc) )
            addrExpr(s->rhs, curProc->type());
        else
            coerce(s->rhs, curProc ? curProc->type() : 0);
    }
}

int MicronGen::precOf(Expression* e)
{
    if( e == 0 )
        return Atom;
    switch( e->kind )
    {
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In:
    case Expression::Is:
        return Relation;
    case Expression::Add:
    case Expression::Sub:
    case Expression::Or:
        return AddOp;
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::And:
        return MulOp;
    case Expression::Plus:
    case Expression::Minus:
        return AddOp;
    case Expression::Not:
        return Atom;
    }
    return Atom;
}

static bool simpleByte(Expression* e);

static inline const char* exprKindStr(int kind)
{
    switch( kind )
    {
    case Expression::Eq:
        return " = ";
    case Expression::Neq:
        return " # ";
    case Expression::Lt:
        return" < ";
    case Expression::Leq:
        return " <= ";
    case Expression::Gt:
        return" > ";
    case Expression::Geq:
        return " >= ";
    case Expression::In:
        return " in ";
    case Expression::Add:
        return " + ";
    case Expression::Sub:
        return " - ";
    case Expression::Or:
        return " or ";
    case Expression::Mul:
        return " * ";
    case Expression::Fdiv:
        return " / ";
    case Expression::Div:
        return " div ";
    case Expression::Mod:
        return " mod ";
    case Expression::And:
        return " & ";
    }
    return "??";
}
void MicronGen::Expr(Expression* e, int prec)
{
    if( e == 0 )
        return;
    const bool paren = precOf(e) < prec;
    switch( e->kind )
    {
    case Expression::Plus:
    case Expression::Minus:
    case Expression::Not:
        if( paren )
            out << "(";
        switch( e->kind )
        {
        case Expression::Plus:
            out << "+";
            break;
        case Expression::Minus:
            out << "-";
            break;
        case Expression::Not:
            out << "~";
            break;
        }
        if( e->kind != Expression::Not && isPtrForm(e->lhs) )
            ordExpr(e->lhs);
        else
            Expr(e->lhs, Unary);
        if( paren )
            out << ")";
        break;
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
    case Expression::In:
    case Expression::Add:
     case Expression::Sub:
    case Expression::Or:
    case Expression::Mul:
    case Expression::Fdiv:
    case Expression::Div:
    case Expression::Mod:
    case Expression::And: {
        Type* to = e->kind == Expression::In ? 0 : commonType(deref(e->lhs->type()), deref(e->rhs->type()));
        bool isConst = false;
        constVal(e, &isConst);
        const ConstScope cs(this, isConst);
        const bool eq = e->kind == Expression::Eq || e->kind == Expression::Neq;
        const bool lp = isPtrOrAddr(e->lhs);
        const bool rp = isPtrOrAddr(e->rhs);
        const bool nil = isNil(e->lhs) || isNil(e->rhs);
        const bool addr = eq && ( nil || ( lp && rp ) || ( lp && isZeroLit(e->rhs) ) || ( rp && isZeroLit(e->lhs) ) );
        const bool ord = !addr && ( isPtrForm(e->lhs) || isPtrForm(e->rhs) );
        const bool bothPtr = ord && isRelation(e->kind) &&
                ( isPtrForm(e->lhs) || ptrOffsetForm(e->lhs) ) &&
                ( isPtrForm(e->rhs) || ptrOffsetForm(e->rhs) );
        const bool chr = isRelation(e->kind) && isCharExpr(e->lhs) && isCharExpr(e->rhs) && ( isChrCall(e->lhs) || isChrCall(e->rhs) );
        if( chr )
        {
            if( paren )
                out << "(";
            charAsNum(e->lhs);
            out << ( e->kind == Expression::Eq ? " = " : e->kind == Expression::Neq ? " # " :
                     e->kind == Expression::Lt ? " < " : e->kind == Expression::Leq ? " <= " :
                     e->kind == Expression::Gt ? " > " : " >= " );
            charAsNum(e->rhs);
            if( paren )
                out << ")";
            break;
        }
        const int oldPromote = noPromote;
        if( !isRelation(e->kind) )
            noPromote = 0;
        Type* lhsT = deref(e->lhs->type());
        Type* rhsT = deref(e->rhs->type());
        const bool fdiv = e->kind == Expression::Fdiv &&
                lhsT && lhsT->isInteger() && rhsT && rhsT->isInteger();
        const bool oldWant = wantAddr;
        if( !addr )
            wantAddr = false;
        if( obDiv && ( e->kind == Expression::Div || e->kind == Expression::Mod ) &&
                needsObDiv(e) )
        {
            bool isPow = false;
            const qint64 div = constVal(e->rhs, &isPow);
            int shiftBy = 0;
            if( isPow )
            {
                isPow = div > 0 && ( div & ( div - 1 ) ) == 0;
                while( ( Q_INT64_C(1) << shiftBy ) < div )
                    shiftBy++;
            }
            if( isPow && !ord )
            {
                if( e->kind == Expression::Div )
                    out << "asr(" << toStr(e->lhs) << ", " << shiftBy << ")";
                else
                    out << "band(" << toStr(e->lhs) << ", " << (div-1) << ")";
                wantAddr = oldWant;
                noPromote = oldPromote;
                break;
            }
            out << ( e->kind == Expression::Div ? "obdiv(" : "obmod(" );
            if( ord )
                ordExpr(e->lhs);
            else
                coerce(e->lhs, to, NoPrec);
            out << ", ";
            if( ord )
                ordExpr(e->rhs);
            else
                coerce(e->rhs, to, NoPrec);
            out << ")";
            wantAddr = oldWant;
            noPromote = oldPromote;
            break;
        }
        const int my = precOf(e);
        const bool plainLhs = isRelation(e->kind) && plainConst(e->rhs, to) && simpleByte(e->lhs);
        const bool plainRhs = isRelation(e->kind) && plainConst(e->lhs, to) && simpleByte(e->rhs);
        if( paren )
            out << "(";
        if( plainLhs )
            noPromote++;
        if( addr )
            addrExpr(e->lhs);
        else if( ord )
            ordExpr(e->lhs, !bothPtr);
        else if( fdiv )
        {
            out << "val(real, ";
            Expr(e->lhs);
            out << ")";
        }else
            coerce(e->lhs, to, my == Relation ? my + 1 : my);
        if( plainLhs )
            noPromote--;
        out << exprKindStr(e->kind);
        if( plainRhs )
            noPromote++;
        if( addr )
            addrExpr(e->rhs);
        else if( ord )
            ordExpr(e->rhs, !bothPtr);
        else if( fdiv )
        {
            out << "val(real, ";
            Expr(e->rhs);
            out << ")";
        }else
            coerce(e->rhs, to, my + 1);
        if( plainRhs )
            noPromote--;
        if( paren )
            out << ")";
        wantAddr = oldWant;
        noPromote = oldPromote;
        } break;
    case Expression::Is: {
            Type* guard = deref(e->rhs->type());
            if( guard && guard->kind == Type::Pointer )
                guard = deref(guard->type());
            if( paren )
                out << "(";
            Expr(e->lhs, Relation + 1);
            out << " is " << (guard && guard->isSO() ? pointerTo(guard) : typeRef(e->rhs->type()));
            if( paren )
                out << ")";
        } break;
    case Expression::Literal:
    case Expression::ConstVal:
        literal(e);
        break;
    case Expression::Constructor:
        constructor(e);
        break;
    case Expression::Call:
        call(e);
        break;
    case Expression::DeclRef:
    case Expression::Select:
    case Expression::Index:
    case Expression::Deref:
    case Expression::Cast:
        designator(e);
        break;
    case Expression::Range:
        Expr(e->lhs);
        out << "..";
        Expr(e->rhs);
        break;
    default:
        error("expression not supported", e->pos);
        break;
    }
}

void MicronGen::designator(Expression* e, bool wantValue)
{
    if( e == 0 )
        return;
    switch( e->kind )
    {
    case Expression::DeclRef: {
            Declaration* d = e->val.value<Declaration*>();
            if( d && d->kind == Declaration::ConstDecl && d->getModule() == 0 )
            {
                Type* t = deref(d->type());
                if( t && t->kind == Type::BOOLEAN )
                    out << (d->data.toBool() ? "true" : "false");
                else
                    out << d->data.toLongLong();
                return;
            }
            if( d && ( d->kind == Declaration::Field || ( d->kind == Declaration::Procedure && d->receiver ) ) )
            {
                out << "self." << escape(d->name);
                return;
            }
            const bool guarded = withCast.contains(d) && !narrowVars.contains(d);
            const bool byCast = guarded && castOnly.contains(d);
            if( byCast )
                out << "cast(" << withCast.value(d) << ", ";
            const ClosureLifter::LiftParam* lp = lifted(d);
            out << (lp ? escape(lp->name) : qualident(d));
            if( (lp || isPointerForm(d)) &&
                    ( wantValue || !isArray(d->type()) ) &&
                    !(deref(d->type()) && deref(d->type())->isSO()) )
                out << "^";
            if( guarded && !byCast )
                out << "(" << withCast.value(d) << ")";
            if( byCast )
                out << ")";
        } break;
    case Expression::Select: {
            Declaration* field = e->val.value<Declaration*>();
            Declaration* base = e->lhs->kind == Expression::DeclRef ? e->lhs->val.value<Declaration*>() : 0;
            Type* lt = deref( base && !withCast.contains(base) ? base->type() : e->lhs->type() );
            if( lt && lt->kind == Type::Pointer )
                lt = deref(lt->type());
            Type* owner = lt && lt->isSO() && lt->find(field->name, true) != field ? fieldOwner(field) : 0;
            if( owner && owner != lt )
                out << toMicron(e->lhs) << "(" << pointerTo(owner) << ")";
            else
                designator(e->lhs, false);
            out << "." << escape(field->name);
            if( mdl->isAddress(field) && field->getModule() != curMod )
                todo("'" + QString::fromUtf8(field->name) +
                     "' is an address filled by the Oberon linker/loader", e->pos);
        } break;
    case Expression::Index: {
            Expression* desc = arr2dOf(e->lhs);
            if( desc )
            {
                const QByteArray d = toMicron(desc);
                out << d << ".data[(" << numStr(e->lhs->rhs)
                    << ") * " << d << ".len1 + (" << numStr(e->rhs) << ")]";
                break;
            }
            if( arr2dOf(e) )
                todo("a single row of a two dimensional open array cannot be designated in Micron", e->pos);
            designator(e->lhs, false);
            noPromote++;
            const QByteArray idx = numStr(e->rhs);
            noPromote--;
            out << "[" << idx << "]";
        } break;
    case Expression::Deref:
        designator(e->lhs, false);
        if( wantValue && !isArr2d(e->lhs->type()) )
            out << "^";
        break;
    case Expression::Cast: {
            Type* t = deref(e->type());
            if( t && t->kind == Type::Pointer )
                t = deref(t->type());
            designator(e->lhs, false);
            out << "(" << (t && t->isSO() ? pointerTo(t) : typeRef(e->type())) << ")";
        } break;
    case Expression::Super:
        designator(e->lhs, wantValue);
        break;
    default:
        Expr(e);
        break;
    }
}

static bool fitsInt(Type* t, qint64 v);

void MicronGen::literal(Expression* e)
{
    Type* t = deref(e->type());
    const QVariant val = e->val;
    if( t == 0 )
    {
        out << "0";
        return;
    }
    switch( t->kind )
    {
    case Type::NIL:
        out << "nil";
        break;
    case Type::BOOLEAN:
        out << (val.toBool() ? "true" : "false");
        break;
    case Type::CHAR:
        out << charLit(val.toULongLong());
        break;
    case Type::StrLit: {
            QByteArray str = val.toByteArray();
            str.replace('"', "\" + 022X + \"");
            out << "\"" << QString::fromLatin1(str) << "\"";
        } break;
    case Type::REAL:
    case Type::LONGREAL: {
            QByteArray tmp = QByteArray::number(val.toDouble(), 'g', t->kind == Type::REAL ? 9 : 17);
            const int e = tmp.indexOf('e');
            QByteArray mantissa = e < 0 ? tmp : tmp.left(e);
            QByteArray exp = e < 0 ? QByteArray() : tmp.mid(e+1);
            if( !mantissa.contains('.') )
                mantissa += ".0";
            if( !exp.isEmpty() )
            {
                const bool neg = exp.startsWith('-');
                while( exp.size() && ( exp[0] == '+' || exp[0] == '-' || exp[0] == '0' ) )
                    exp = exp.mid(1);
                if( exp.isEmpty() )
                    exp = "0";
                mantissa += "E" + QByteArray(neg ? "-" : "") + exp;
            }
            out << mantissa;
        } break;
    case Type::SET: {
            const quint32 bits = val.toUInt();
            out << "{";
            bool first = true;
            for( int i = 0; i < 32; i++ )
            {
                if( bits & (1u << i) )
                {
                    if( !first )
                        out << ",";
                    first = false;
                    out << i;
                }
            }
            out << "}";
        } break;
    default:
    {
        const qint64 v = val.toLongLong();
        if( t->kind == Type::LONGINT && v > Q_INT64_C(0x7fffffff) && v <= Q_INT64_C(0xffffffff) )
            out << "val(int32, " << v << ")";
        else
            out << v << suffix(t, v);
        } break;
    }
}

QByteArray MicronGen::suffix(Type* t, qint64 val)
{
    if( t == 0 )
        return QByteArray();
    if( constCtx == 0 &&
            ( fitsInt(t, val) || ( t->kind == Type::BYTE && val >= 0 && val <= 255 ) ) )
        return QByteArray();
    int width = 4;
    switch( t->kind )
    {
    case Type::SHORTINT:
        width = 1;
        break;
    case Type::INTEGER:
        width = 2;
        break;
    case Type::LONGINT:
        width = 4;
        break;
    case Type::HUGEINT:
        width = 8;
        break;
    default:
        return QByteArray();
    }
    while( width < 8 && ( val < -(Q_INT64_C(1) << (width * 8 - 1)) ||
                          val > (Q_INT64_C(1) << (width * 8 - 1)) - 1 ) )
        width *= 2;
    switch( width )
    {
    case 1:
        return "i1";
    case 2:
        return "i2";
    case 4:
        return "i4";
    case 8:
        if( val >= Q_INT64_C(-2147483648) && val <= Q_INT64_C(2147483647) )
            return "i8";
        break;
    }
    if( val >= 0 && val <= Q_INT64_C(4294967295) )
        return "u4";
    return QByteArray();
}

static int byteWidth(Type* t)
{
    if( t == 0 )
        return 0;
    switch( t->kind )
    {
    case Type::BYTE:
    case Type::CHAR:
    case Type::BOOLEAN:
    case Type::SHORTINT:
        return 1;
    case Type::INTEGER:
        return 2;
    case Type::LONGINT:
    case Type::SET:
    case Type::REAL:
        return 4;
    case Type::HUGEINT:
    case Type::LONGREAL:
        return 8;
    }
    return 0;
}

static int rank(Type* t)
{
    if( t == 0 )
        return 0;
    switch( t->kind )
    {
    case Type::BYTE:
    case Type::CHAR:
    case Type::SHORTINT:
        return 1;
    case Type::INTEGER:
        return 2;
    case Type::LONGINT:
        return 3;
    case Type::HUGEINT:
        return 4;
    case Type::REAL:
        return 5;
    case Type::LONGREAL:
        return 6;
    }
    return 0;
}

Type* MicronGen::commonType(Type* lhs, Type* rhs)
{
    if( rank(lhs) == 0 || rank(rhs) == 0 )
        return 0;
    if( lhs->kind == rhs->kind )
        return 0;
    if( lhs->kind == Type::CHAR )
        return rhs;
    if( rhs->kind == Type::CHAR )
        return lhs;
    return rank(lhs) >= rank(rhs) ? lhs : rhs;
}

bool MicronGen::isZeroLit(Expression* e)
{
    bool ok = false;
    Type* t = deref(e ? e->type() : 0);
    return t && t->isInteger() && constVal(e, &ok) == 0 && ok;
}

bool MicronGen::nonNegative(Expression* e)
{
    if( e == 0 )
        return false;
    bool ok = false;
    const qint64 v = constVal(e, &ok);
    if( ok )
        return v >= 0;
    qint64 lo = 0, hi = 0;
    if( interval(e, lo, hi) && lo >= 0 )
        return true;
    Type* t = deref(e->type());
    if( t && ( t->kind == Type::BYTE || t->kind == Type::CHAR || t->kind == Type::SET ) )
        return true;
    switch( e->kind )
    {
    case Expression::Add:
    case Expression::Mul:
    case Expression::Div:
    case Expression::Mod:
        return nonNegative(e->lhs) && nonNegative(e->rhs);
    case Expression::Plus:
        return nonNegative(e->lhs);
    case Expression::Call:
        return isBuiltinCall(e, Builtin::ORD) || isBuiltinCall(e, Builtin::LEN) ||
                isBuiltinCall(e, Builtin::SIZE) || isBuiltinCall(e, Builtin::ABS) ||
                isBuiltinCall(e, Builtin::SYSTEM_TYPECODE);
    }
    return false;
}

bool MicronGen::needsObDiv(Expression* e)
{
    return e && !( nonNegative(e->lhs) && nonNegative(e->rhs) );
}

bool MicronGen::isRelation(int op)
{
    switch( op )
    {
    case Expression::Eq:
    case Expression::Neq:
    case Expression::Lt:
    case Expression::Leq:
    case Expression::Gt:
    case Expression::Geq:
        return true;
    }
    return false;
}

bool MicronGen::isChrCall(Expression* e)
{
    if( isBuiltinCall(e, Builtin::SYSTEM_VAL) && e->rhs && e->rhs->next )
    {
        Type* to = deref(e->rhs->type());
        Type* from = deref(e->rhs->next->type());
        return to && to->kind == Type::CHAR && from && from->isInteger();
    }
    return isBuiltinCall(e, Builtin::CHR) || isBuiltinCall(e, Builtin::CAP);
}

bool MicronGen::isCharExpr(Expression* e)
{
    Type* t = deref(e ? e->type() : 0);
    if( t == 0 )
        return false;
    if( t->kind == Type::CHAR )
        return true;
    return t->kind == Type::StrLit && e->val.toByteArray().size() <= 1;
}

void MicronGen::charAsNum(Expression* e)
{
    Type* t = deref(e ? e->type() : 0);
    if( t && t->kind == Type::StrLit )
    {
        const QByteArray str = e->val.toByteArray();
        out << hexNum(str.isEmpty() ? 0 : quint8(str[0]));
    }else if( t && t->kind == Type::CHAR && e->kind == Expression::Literal )
        out << hexNum(e->val.toULongLong());
    else if( isChrCall(e) && e->rhs )
    {
        Expression* arg = isBuiltinCall(e, Builtin::SYSTEM_VAL) ? e->rhs->next : e->rhs;
        bool ok = false;
        const qint64 v = constVal(arg, &ok);
        if( isBuiltinCall(e, Builtin::CAP) )
        {
            out << "val(int32, band(ord(";
            Expr(e->rhs);
            out << "), 0DFH))";
        }else if( ok )
            out << hexNum(v);
        else
        {
            out << "val(int32, ";
            Expr(arg);
            out << ")";
        }
    }else if( t && t->kind == Type::CHAR )
    {
        out << "val(int32, ord(";
        Expr(e);
        out << "))";
    }else
        Expr(e);
}

void MicronGen::ordExpr(Expression* e, bool signedRes)
{
    const bool old = wantAddr;
    wantAddr = false;
    Expression* base = 0;
    Expression* off = 0;
    if( !signedRes && ptrOffsetForm(e, &base, &off) )
    {
        out << "ord(ptroff(";
        wantAddr = true;
        Expr(base);
        wantAddr = false;
        bool ok = false;
        const qint64 v = constVal(off, &ok);
        out << ", ";
        if( e->kind == Expression::Sub && ok )
            out << -v;
        else if( e->kind == Expression::Sub )
        {
            out << "-(";
            Expr(off);
            out << ")";
        }else
            Expr(off, AddOp + 1);
        out << "))";
        wantAddr = old;
        return;
    }
    if( isPtrForm(e) )
    {
        out << ( signedRes ? "val(int32, ord(" : "ord(" );
        wantAddr = true;
        Expr(e);
        wantAddr = false;
        out << ( signedRes ? "))" : ")" );
    }else
        Expr(e);
    wantAddr = old;
}

bool MicronGen::elemAddr(Expression* e)
{
    if( e == 0 || e->kind != Expression::Add || e->lhs == 0 || e->rhs == 0 )
        return false;
    Expression* base = e->lhs;
    if( !isBuiltinCall(base, Builtin::SYSTEM_ADR) || base->rhs == 0 )
        return false;
    Expression* arr = base->rhs;
    Type* t = deref(arr->type());
    if( t == 0 || t->kind != Type::Array || byteWidth(deref(t->type())) != 1 )
        return false;
    const bool old = wantAddr;
    wantAddr = false;
    out << "@";
    designator(arr, false);
    out << "[";
    Expr(e->rhs);
    out << "]";
    wantAddr = old;
    return true;
}

QByteArray MicronGen::numStr(Expression* e)
{
    const QByteArray res = toStr(e);
    return isPtrOrAddr(e) ? "val(int32, ord(" + res + "))" : res;
}

QByteArray MicronGen::byteOffset(const QList<Expression*>& ops, const QList<Expression*>& offs)
{
    const bool old = wantAddr;
    wantAddr = false; // an index is a number, so an address operand is rendered with ORD
    QByteArray res;
    for( int i = 0; i < ops.size(); i++ )
    {
        if( !res.isEmpty() )
            res += ops[i]->kind == Expression::Add ? " + " : " - ";
        else if( ops[i]->kind == Expression::Sub )
            res += "-";
        res += numStr(offs[i]);
    }
    wantAddr = old;
    return res.isEmpty() ? QByteArray("0") : res;
}

QByteArray MicronGen::memIndex(const QList<Expression*>& ops, const QList<Expression*>& offs, int size)
{
    const bool old = wantAddr;
    wantAddr = false; // an index is a number, so an address operand is rendered with ORD
    QByteArray res;
    bool allConst = true;
    qint64 total = 0;
    for( int i = 0; i < ops.size(); i++ )
    {
        QByteArray term;
        bool ok = false;
        const qint64 c = constVal(offs[i], &ok);
        if( ok )
            total += ops[i]->kind == Expression::Add ? c : -c;
        else
            allConst = false;
        if( size == 1 )
            term = numStr(offs[i]);
        else if( ok && c % size == 0 )
            term = QByteArray::number(c / size);
        else if( offs[i]->kind == Expression::Mul )
        {
            bool l = false, r = false;
            const qint64 lc = constVal(offs[i]->lhs, &l);
            const qint64 rc = constVal(offs[i]->rhs, &r);
            if( l && lc == size )
                term = numStr(offs[i]->rhs);
            else if( r && rc == size )
                term = numStr(offs[i]->lhs);
        }
        if( term.isEmpty() )
        {
            wantAddr = old;
            return QByteArray();
        }
        if( !res.isEmpty() )
            res += ops[i]->kind == Expression::Add ? " + " : " - ";
        else if( ops[i]->kind == Expression::Sub )
            res += "-";
        res += term;
    }
    wantAddr = old;
    if( allConst && total < 0 )
        return QByteArray(); // micc rejects a negative index, even of an open array
    return res.isEmpty() ? QByteArray("0") : res;
}

bool MicronGen::memAccess(Expression* adr, Expression* val, bool get)
{
    Type* vt = deref(val->type());
    const int kind = isPtrOrAddr(val) ? (int)Type::PTR : ( vt ? (int)vt->kind : 0 );
    const QByteArray pt = castPtr.value(kind);
    if( pt.isEmpty() )
        return false;
    const int size = kind == Type::PTR ? 4 : byteWidth(vt);
    if( size == 0 )
        return false;
    QList<Expression*> ops;
    QList<Expression*> offs;
    Expression* base = mdl->moveChain(adr, &ops, &offs);
    bool isConst = false;
    qint64 c = constVal(base, &isConst);
    if( isConst )
    {
        for( int i = 0; i < ops.size(); i++ )
        {
            bool ok = false;
            const qint64 o = constVal(offs[i], &ok);
            if( !ok )
                return false;
            c += ops[i]->kind == Expression::Add ? o : -o;
        }
    }else if( !isPtrOrAddr(base) )
        return false;
    const bool byteMove = !isConst && !ops.isEmpty();
    const QByteArray vp = kind == Type::PTR && vt && vt->kind == Type::Pointer ?
                typeRef(vt) : QByteArray();
    if( get )
    {
        Expression* reint = isBuiltinCall(val, Builtin::SYSTEM_VAL) && val->rhs ?
                    val->rhs->next : 0;
        if( reint )
        {
            out << "cast(" << pt << ", ";
            addressOf(reint);
            out << ")^";
        }else
            designator(val);
        out << " := ";
        if( !vp.isEmpty() )
            out << "cast(" << vp << ", ";
    }
    if( isConst )
        out << pt << "{" << constAddr(base, c) << "}^";
    else
    {
        QByteArray b = toStr(base, false, true);
        if( b.startsWith("cast(") && b.endsWith(")") )
        {
            const int comma = b.indexOf(", ");
            if( comma > 0 && !b.mid(5, comma-5).contains('(') )
                b = b.mid(comma+2, b.size()-comma-3);
        }
        out << "cast(" << pt << ", ";
        if( byteMove )
            out << "ptroff(";
        out << b;
        if( byteMove )
            out << ", " << byteOffset(ops, offs) << ")";
        out << ")^";
    }
    if( get && !vp.isEmpty() )
        out << ")";
    if( !get )
    {
        out << " := ";
        if( kind != Type::PTR )
            coerce(val, vt);
        else if( vp.isEmpty() )
            addrExpr(val);
        else {
            out << "cast(" << anyPtr << ", ";
            addrExpr(val);
            out << ")";
        }
    }
    return true;
}

void MicronGen::moveAdr(Expression* e)
{
    Type* t = deref(e->type());
    if( t && t->kind == Type::Pointer && !isBuiltinCall(e, Builtin::SYSTEM_ADR) )
    {
        out << "cast(" << anyPtr << ", ";
        Expr(e);
        out << ")";
    }else
        addrExpr(e);
}

bool MicronGen::memMove(Expression* src, Expression* dst, Expression* n)
{
    if( sysModule.isEmpty() || anyPtr.isEmpty() )
        return false;
    out << sysModule << ".MOVE(";
    moveAdr(src);
    out << ", ";
    moveAdr(dst);
    out << ", ";
    Type* nt = deref(n->type());
    bool isConst = false;
    const qint64 c = constVal(n, &isConst);
    if( isConst && c >= 0 && n->kind == Expression::Literal )
        out << c;
    else if( nt && nt->kind == Type::LONGINT && !isPtrForm(n) )
        Expr(n);
    else
    {
        out << "val(int32, ";
        if( isPtrForm(n) )
            ordExpr(n);
        else
            Expr(n);
        out << ")";
    }
    out << ")";
    return true;
}

QByteArray MicronGen::constAddr(Expression* e, qint64 c)
{
    Declaration* d = e ? MicronModel::designated(e) : 0;
    if( d && d->kind == Declaration::ConstDecl && mdl->isAddress(d) )
        return toStr(e);
    return "0" + QByteArray::number((quint32)c, 16).toUpper() + "H";
}

void MicronGen::addrExpr(Expression* e, Type* to)
{
    const bool old = wantAddr;
    Type* oldType = wantType;
    wantAddr = true;
    wantType = isPointerKind(to) ? to : 0;
    bool isConst = false;
    const qint64 c = constVal(e, &isConst);
    if( isZeroLit(e) )
        out << "nil";
    else if( isConst )
        out << addrTypeName() << "{" << constAddr(e, c) << "}";
    else if( !elemAddr(e) && !numToPtr(e, addrTypeName()) )
        Expr(e);
    wantAddr = old;
    wantType = oldType;
}

bool MicronGen::numToPtr(Expression* e, const QByteArray& toName)
{
    if( anyPtr.isEmpty() || toName.isEmpty() || e == 0 )
        return false;
    Type* t = deref(e->type());
    if( t == 0 || !( t->isInteger() || t->kind == Type::SET ) )
        return false;
    if( isPtrOrAddr(e) || isPtrForm(e) || ptrOffsetForm(e) )
        return false;
    const bool wrap = toName != anyPtr;
    if( wrap )
        out << "cast(" << toName << ", ";
    out << "ptroff(" << anyPtr << "{0}, ";
    const bool old = wantAddr;
    wantAddr = false;
    if( t->kind == Type::SET )
    {
        out << "val(int32, ord(";
        Expr(e);
        out << "))";
    }else
        Expr(e);
    wantAddr = old;
    out << ")";
    if( wrap )
        out << ")";
    return true;
}

QByteArray MicronGen::addrTypeName()
{
    return wantType ? typeRef(wantType) : anyPtr;
}

static bool isSignedInt(Type* t)
{
    if( t == 0 )
        return false;
    switch( t->kind )
    {
    case Type::SHORTINT:
    case Type::INTEGER:
    case Type::LONGINT:
    case Type::HUGEINT:
        return true;
    }
    return false;
}

static bool isIntLit(Expression* e)
{
    while( e && ( e->kind == Expression::Plus || e->kind == Expression::Minus ) )
        e = e->lhs;
    if( e == 0 || ( e->kind != Expression::Literal && e->kind != Expression::ConstVal ) )
        return false;
    Type* t = MicronGen::deref(e->type());
    return t && t->isInteger() && t->kind != Type::BYTE;
}

static bool hasBuiltinCall(Expression* e)
{
    if( e == 0 )
        return false;
    if( e->kind == Expression::Call )
    {
        Declaration* d = e->lhs ? e->lhs->val.value<Declaration*>() : 0;
        if( d && d->kind == Declaration::Builtin )
            return true;
    }
    return hasBuiltinCall(e->lhs) || hasBuiltinCall(e->rhs);
}

static bool fitsInt(Type* t, qint64 v);

static bool litFits(Expression* e, Type* t)
{
    bool ok = false;
    const qint64 v = MicronGen::constVal(e, &ok);
    return ok && fitsInt(t, v);
}

static bool fitsInt(Type* t, qint64 v)
{
    switch( t->kind )
    {
    case Type::BYTE:
    case Type::SHORTINT:
        return v >= -128 && v <= 127;
    case Type::INTEGER:
        return v >= -32768 && v <= 32767;
    case Type::LONGINT:
        return v >= Q_INT64_C(-2147483648) && v <= Q_INT64_C(2147483647);
    }
    return true;
}

static bool simpleByte(Expression* e)
{
    if( e == 0 )
        return false;
    if( isBuiltinCall(e, Builtin::ORD) )
        e = Expression::getList(e->rhs).first();
    else if( e->kind != Expression::DeclRef && e->kind != Expression::Select &&
             e->kind != Expression::Index && e->kind != Expression::Deref )
        return false;
    Type* t = MicronGen::deref(e ? e->type() : 0);
    return t && ( t->kind == Type::BYTE || t->kind == Type::CHAR );
}

bool MicronGen::plainConst(Expression* e, Type* to)
{
    bool ok = false;
    const qint64 v = constVal(e, &ok);
    if( !ok || v < 0 || v > 255 )
        return false;
    Declaration* d = e->val.value<Declaration*>();
    if( d && d->kind == Declaration::ConstDecl )
    {
        if( constFeeds.contains(d) || d->expr == 0 || mdl->isAddress(d) )
            return false;
        to = deref(d->expr->type());
    }else if( !isIntLit(e) )
        return false;
    const int oldCtx = constCtx;
    constCtx = 0;
    const bool plain = suffix(to, v).isEmpty();
    constCtx = oldCtx;
    return plain;
}

void MicronGen::coerce(Expression* e, Type* to, int prec)
{
    to = deref(to);
    Type* from = deref(e->type());
    if( noPromote > 0 && isSignedInt(to) && !isPointerKind(to) && from &&
            ( from->kind == Type::BYTE || from->kind == Type::CHAR ) && !isIntLit(e) )
    {
        if( from->kind == Type::CHAR )
        {
            out << "ord(";
            Expr(e);
            out << ")";
        }else
            Expr(e, prec);
        return;
    }
    if( to && to->isInteger() && !isPointerKind(to) )
    {
        bool ok = false;
        const qint64 v = constVal(e, &ok);
        if( ok && !fitsInt(to, v) )
        {
            out << "val(" << basicType(to) << ", ";
            Expr(e);
            out << ")";
            return;
        }
    }
    if( to && to->isInteger() && !isPointerKind(to) && isPtrForm(e) )
    {
        if( isNil(e) )
            out << "0";
        else
            ordExpr(e);
        return;
    }
    if( to && to->kind == Type::BYTE && isIntLit(e) )
    {
        const qint64 v = constVal(e);
        if( v >= 0 && v <= 255 )
        {
            out << v << "u1";
            return;
        }
    }
    if( isSignedInt(to) && !isPointerKind(to) )
    {
        bool ok = false;
        const qint64 v = constVal(e, &ok);
        if( ok && isIntLit(e) )
        {
            const bool paren = v < 0 && prec > AddOp;
            out << ( paren ? "(" : "" ) << v << suffix(to, v) << ( paren ? ")" : "" );
            return;
        }
        if( isSignedInt(from) && !isPointerKind(from) && rank(to) >= rank(from) &&
                !hasBuiltinCall(e) )
        {
            Expr(e, prec);
            return;
        }
    }
    if( to == 0 || from == 0 || rank(to) == 0 || rank(from) == 0 || to->kind == from->kind )
    {
        Expr(e, prec);
        return;
    }
    if( isSignedInt(to) && isBuiltinCall(e, Builtin::ORD) )
    {
        const ExpList a = Expression::getList(e->rhs);
        Type* t = deref(a.first()->type());
        const bool num = t && t->isNumberOrByte();
        out << "val(" << basicType(to) << ", ";
        if( !num )
            out << "ord(";
        Expr(a.first());
        if( !num )
            out << ")";
        out << ")";
    }else if( from->kind == Type::CHAR )
    {
        Expression* inner = isBuiltinCall(e, Builtin::CHR) ?
                    Expression::getList(e->rhs).first() : 0;
        qint64 lo = 0, hi = 0;
        if( inner && interval(inner, lo, hi) && lo >= 0 && hi <= 255 )
        {
            out << "val(" << basicType(to) << ", ";
            Expr(inner);
            out << ")";
        }else
        {
            out << "val(" << basicType(to) << ", ord(";
            Expr(e);
            out << "))";
        }
    }else if( to->kind == Type::CHAR )
    {
        out << "chr(";
        Expr(e);
        out << ")";
    }else
    {
        out << "val(" << basicType(to) << ", ";
        Expr(e);
        out << ")";
    }
}

void MicronGen::constructor(Expression* e)
{
    out << "{";
    Expression* elem = e->rhs;
    while( elem )
    {
        if( elem != e->rhs )
            out << ",";
        Expr(elem);
        elem = elem->next;
    }
    out << "}";
}

bool MicronGen::isStructuredVal(Expression* e, Type*& to, Expression*& arg)
{
    if( e == 0 || e->kind != Expression::Call || e->lhs == 0 )
        return false;
    Declaration* d = e->lhs->val.value<Declaration*>();
    if( d == 0 || d->kind != Declaration::Builtin || d->id != Builtin::SYSTEM_VAL )
        return false;
    Expression* a = e->rhs;
    if( a == 0 || a->next == 0 )
        return false;
    to = deref(a->type());
    arg = a->next;
    return to && to->isStructured();
}

void MicronGen::addressOf(Expression* e)
{
    if( e == 0 )
        return;
    if( e->kind == Expression::DeclRef )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( lifted(d) )
        {
            out << escape(lifted(d)->name);
            return;
        }
        if( isPointerForm(d) )
        {
            if( withCast.contains(d) && castOnly.contains(d) )
                out << "cast(" << withCast.value(d) << ", " << qualident(d) << ")";
            else if( withCast.contains(d) && !narrowVars.contains(d) )
                out << qualident(d) << "(" << withCast.value(d) << ")";
            else
                out << qualident(d);
            return;
        }
    }else if( e->kind == Expression::Deref )
    {
        designator(e->lhs, true);
        return;
    }else if( e->kind == Expression::Cast )
    {
        Declaration* d = MicronModel::designated(e);
        if( d && ( lifted(d) || isPointerForm(d) ) )
        {
            designator(e, false);
            return;
        }
    }else
    {
        Expression* arg = 0;
        Type* to = 0;
        if( isStructuredVal(e, to, arg) )
        {
            out << "cast(" << pointerTo(to) << ", ";
            addressOf(arg);
            out << ")";
            return;
        }
    }
    Expression* desc = arr2dOf(e);
    if( desc )
    {
        const QByteArray d = toMicron(desc);
        out << "@" << d << ".data[(";
        Expr(e->rhs);
        out << ") * " << d << ".len1]";
        return;
    }
    out << "@";
    designator(e, false);
}

void MicronGen::lenOf(Expression* e)
{
    if( isArr2d(e->type()) )
    {
        out << toMicron(e) << ".len0";
        return;
    }
    Expression* desc = arr2dOf(e);
    if( desc )
    {
        out << toMicron(desc) << ".len1";
        return;
    }
    Type* t = deref(e->type());
    if( t && t->kind == Type::Array && t->expr )
    {
        bool ok = false;
        const qint64 len = constVal(t->expr, &ok);
        if( ok && len >= 0 )
        {
            out << len;
            if( t->expr->kind != Expression::Literal )
                out << " (* " << toStr(t->expr) << " *)";
        }else
            Expr(t->expr);
        return;
    }
    if( e->kind == Expression::DeclRef )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( isOpenArrayParam(d) && mdl->needsLen(d) )
        {
            out << MicronModel::lenName(d);
            return;
        }
    }
    out << "0";
    todo("the length of this open array is not available", e->pos);
}

bool MicronGen::interval(Expression* e, qint64& lo, qint64& hi)
{
    if( e == 0 )
        return false;
    bool ok = false;
    const qint64 v = constVal(e, &ok);
    if( ok )
    {
        lo = hi = v;
        return true;
    }
    Type* t = deref(e->type());
    if( t && ( t->kind == Type::BYTE || t->kind == Type::CHAR ) )
    {
        lo = 0;
        hi = 255;
        return true;
    }
    qint64 llo = 0, lhi = 0, rlo = 0, rhi = 0;
    switch( e->kind )
    {
    case Expression::Plus:
        return interval(e->lhs, lo, hi);
    case Expression::Minus:
        if( !interval(e->lhs, llo, lhi) )
            return false;
        lo = -lhi;
        hi = -llo;
        return true;
    case Expression::Mod:
        if( !interval(e->rhs, rlo, rhi) || rlo != rhi || rlo <= 0 )
            return false;
        lo = 0;
        hi = rlo - 1;
        return true;
    case Expression::Div:
        if( !interval(e->rhs, rlo, rhi) || rlo != rhi || rlo <= 0 ||
                !interval(e->lhs, llo, lhi) )
            return false;
        lo = llo >= 0 ? llo / rlo : -( ( -llo + rlo - 1 ) / rlo );
        hi = lhi >= 0 ? lhi / rlo : -( ( -lhi + rlo - 1 ) / rlo );
        return true;
    case Expression::Add:
    case Expression::Sub:
        if( !interval(e->lhs, llo, lhi) || !interval(e->rhs, rlo, rhi) )
            return false;
        if( e->kind == Expression::Add )
        {
            lo = llo + rlo;
            hi = lhi + rhi;
        }else
        {
            lo = llo - rhi;
            hi = lhi - rlo;
        }
        return true;
    default:
        return false;
    }
}

QByteArray MicronGen::negStr(Expression* e)
{
    if( e->kind == Expression::Sub )
    {
        const QByteArray lhs = toStr(e->lhs);
        return toStr(e->rhs) + " - " +
                ( precOf(e->lhs) > AddOp ? lhs : "(" + lhs + ")" );
    }
    const QByteArray str = toStr(e);
    return precOf(e) > AddOp ? "-" + str : "-(" + str + ")";
}

bool MicronGen::isShift(Expression* e)
{
    if( e == 0 || e->kind != Expression::Call )
        return false;
    Declaration* callee = e->lhs ? e->lhs->val.value<Declaration*>() : 0;
    if( callee == 0 || callee->kind != Declaration::Builtin )
        return false;
    return callee->id == Builtin::ASH || callee->id == Builtin::SYSTEM_LSH ||
            callee->id == Builtin::SYSTEM_ROT;
}

void MicronGen::shift(Expression* x, Expression* n, bool arithmetic, Expression* e)
{
    Type* t = deref(x->type());
    const bool set = t && t->kind == Type::SET;
    const bool ch = t && t->kind == Type::CHAR;
    QByteArray val = set || ch ? "ord(" + toStr(x) + ")" : toStr(x);
    if( ( ch || (t && (t->kind == Type::BYTE || t->kind == Type::SHORTINT || t->kind == Type::INTEGER)) )
            && !isShift(x) )
        // Oberon shifts in the machine word, whereas Micron would shift in the
        // width of the operand and thus lose the bits of a narrow one
        val = "val(int32, " + val + ")";

    bool ok = false;
    const qint64 count = constVal(n, &ok);
    qint64 lo = 0, hi = 0;
    const bool ranged = !ok && interval(n, lo, hi) && ( lo >= 0 || hi <= 0 );
    const bool right = ( ok && count < 0 ) || ( ranged && hi <= 0 );
    const bool signedResult = ( right || !( ok || ranged ) ) && arithmetic && !set;
    QByteArray res;
    if( ok && !right )
        res = "shl(" + val + ", " + QByteArray::number(count) + ")";
    else if( ok )
        res = (signedResult ? "asr(" : "shr(") + val + ", " + QByteArray::number(-count) + ")";
    else if( ranged && !right )
        res = "shl(" + val + ", usig(" + toStr(n) + "))";
    else if( ranged )
        res = (signedResult ? "asr(" : "shr(") + val + ", usig(" + negStr(n) + "))";
    else
    {
        // the direction of the shift is not known statically, so both are applied,
        // the count split by a sign mask which is as wide as the shifted value
        Type* nt = deref(n->type());
        const QByteArray cnt = nt && nt->kind == Type::LONGINT && !isPtrForm(n) ?
                    toStr(n) : "val(int32, " + toStr(n) + ")";
        const QByteArray neg = "band(" + cnt + ", asr(" + cnt + ", 31))";
        const QByteArray left = "usig((" + cnt + " - " + neg + "))";
        const QByteArray right = "usig(-" + neg + ")";
        if( arithmetic && !set )
            res = "asr(sigc(shl(" + val + ", " + left + ")), " + right + ")";
        else
            res = "shr(shl(" + val + ", " + left + "), " + right + ")";
    }

    if( set )
        out << "bset(" << res << ")";
    else if( ch )
        out << "chr(" << res << ")";
    else if( t && t->isInteger() && !signedResult )
        out << "sigc(" << res << ")";
    else
        out << res;
}

void MicronGen::rotate(Expression* x, Expression* n, Expression* e)
{
    if( sysModule.isEmpty() )
    {
        shift(x, n, false, e);
        todo("SYSTEM.ROT is rendered as a shift, since SYS is not imported", e->pos);
        return;
    }
    Type* t = deref(x->type());
    const bool set = t && t->kind == Type::SET;
    const bool ch = t && t->kind == Type::CHAR;
    const QByteArray val = set || ch ? "sigc(ord(" + toStr(x) + "))" : toStr(x);
    bool ok = false;
    const qint64 count = constVal(n, &ok);
    Type* nt = deref(n->type());
    const QByteArray cnt = ok ? QByteArray::number(count) :
            ( nt && nt->kind == Type::LONGINT && !isPtrForm(n) ?
                  toStr(n) : "val(int32, " + toStr(n) + ")" );
    const QByteArray res = sysModule + ".ROT(" + val + ", " + cnt + ")";
    if( set )
        out << "bset(usig(" << res << "))";
    else if( ch )
        out << "chr(usig(" << res << "))";
    else
        out << res;
}

void MicronGen::call(Expression* e)
{
    Expression* lhs = e->lhs;
    if( lhs && lhs->kind == Expression::Super )
        lhs = lhs->lhs;

    Declaration* proc = lhs ? lhs->val.value<Declaration*>() : 0;
    if( proc && proc->kind != Declaration::Procedure && proc->kind != Declaration::Builtin )
        proc = 0;

    if( proc && proc->kind == Declaration::Builtin )
    {
        if( builtin(proc->id, e->rhs, e) )
            return;
        error(QString("built-in procedure %1 not supported").arg(Builtin::name[proc->id]), e->pos);
        return;
    }

    Type* procType = deref(lhs ? lhs->type() : 0);
    if( procType && procType->kind != Type::Procedure )
        procType = 0;
    const DeclList formals = proc ? proc->getParams(true) : procType ? procType->subs : DeclList();

    designator(lhs, true);
    out << "(";
    arguments(e, formals, proc);
    out << ")";
}

void MicronGen::arguments(Expression* e, const DeclList& formals, Declaration* proc)
{
    Expression* arg = e->rhs;
    int i = 0;
    while( arg )
    {
        if( i != 0 )
            out << ", ";
        Declaration* formal = i < formals.size() ? formals[i] : 0;
        actual(arg, formal);
        arg = arg->next;
        i++;
    }

    const ClosureLifter::ProcPlan* plan = proc ? cl.plan(proc) : 0;
    if( plan )
    {
        foreach( const ClosureLifter::LiftParam& lp, plan->addedParams )
        {
            if( i != 0 )
                out << ", ";
            i++;
            const ClosureLifter::LiftParam* mine = lifted(lp.sourceDecl);
            if( mine )
                out << escape(mine->name);
            else if( isPointerForm(lp.sourceDecl) )
                out << escape(lp.sourceDecl->name);
            else
                out << "@" << escape(lp.sourceDecl->name);
        }
    }
}

template<class T>
struct ScopedValue
{
    T& ref;
    T old;
    ScopedValue(T& r, T val):ref(r),old(r) { ref = val; }
    ~ScopedValue() { ref = old; }
};

QByteArray MicronGen::varRefCast(Expression* arg, Declaration* formal, Type* ft)
{
    Declaration* ad = MicronModel::designated(arg);
    if( ad == 0 || ft == 0 )
        return QByteArray();
    const bool toAddr = mdl->isAddress(formal);
    if( toAddr == mdl->isAddress(ad) )
        return QByteArray();
    if( !ft->isInteger() && ft->kind != Type::Pointer )
        return QByteArray();
    return castPtr.value(toAddr ? (int)Type::PTR : (int)ft->kind);
}

void MicronGen::actual(Expression* arg, Declaration* formal)
{
    if( formal == 0 )
    {
        Expr(arg);
        return;
    }
    const ScopedValue<bool> want(wantAddr, mdl->isAddress(formal));
    Type* ft = deref(formal->type());
    Type* at = deref(arg->type());
    if( isArr2d(formal->type()) )
    {
        if( formal->type()->kind == Type::Reference )
            addressOf(arg);
        else
            designator(arg, false);
        return;
    }
    if( ft && ft->kind == Type::Array )
    {
        if( at && at->kind == Type::StrLit )
            Expr(arg);
        else if( arg->varArrOfByte || mdl->isBytesRef(formal, arg) )
        {
            out << "cast(" << ( bytesPtr.isEmpty() ? formalType(formal) : bytesPtr ) << ", ";
            addressOf(arg);
            out << ")";
        }else
            addressOf(arg);
        if( mdl->needsLen(formal) )
        {
            out << ", ";
            if( at && at->kind == Type::StrLit )
                out << arg->val.toByteArray().size() + 1;
            else
                lenOf(arg);
        }
    }else if( formal->type() && formal->type()->kind == Type::Reference )
    {
        Expression* inner = MicronModel::varValRef(formal, arg);
        if( inner && castPtr.contains(ft->kind) )
        {
            out << "cast(" << castPtr.value(ft->kind) << ", ";
            addressOf(inner);
            out << ")";
        }else if( MicronModel::isByteRef(formal, arg) && !bytePtr.isEmpty() )
        {
            out << "cast(" << bytePtr << ", ";
            addressOf(inner ? inner : arg);
            out << ")";
        }else if( Type* guard = narrowed(arg, ft) )
        {
            out << "cast(" << pointerTo(guard) << ", ";
            addressOf(inner ? inner : arg);
            out << ")";
        }else if( !varRefCast(arg, formal, ft).isEmpty() )
        {
            const QByteArray pt = varRefCast(arg, formal, ft);
            out << "cast(" << pt << ", ";
            addressOf(inner ? inner : arg);
            out << ")";
        }else
            addressOf(inner ? inner : arg);
    }else if( MicronModel::isAnyRef(formal, arg) && !anyPtr.isEmpty() )
    {
        out << "cast(" << anyPtr << ", ";
        Expr(arg);
        out << ")";
    }
    else if( mdl->isAddress(formal) )
        addrExpr(arg, formal->type());
    else if( ft && ft->isSO() && formal->type()->kind != Type::Reference )
        recordDesig(arg);
    else if( Type* guard = narrowed(arg, ft) )
    {
        designator(arg, false);
        out << "(" << pointerTo(guard) << ")";
    }
    else
        coerce(arg, ft);
}

bool MicronGen::builtin(int id, Expression* args, Expression* e)
{
    const ExpList a = Expression::getList(args);
    switch( id )
    {
    case Builtin::ABS:
        out << "abs(";
        Expr(a[0]);
        out << ")";
        return true;
    case Builtin::ODD:
        out << "((";
        if( isPtrForm(a[0]) )
            ordExpr(a[0]);
        else
            Expr(a[0]);
        out << " mod 2) # 0)";
        return true;
    case Builtin::CAP:
        out << "chr(band(ord(";
        Expr(a[0]);
        out << "), 0DFH))";
        return true;
    case Builtin::ASH:
        shift(a[0], a[1], true, e);
        return true;
    case Builtin::LEN:
        out << "val(" << basicType(deref(e->type())) << ", ";
        lenOf(a[0]);
        out << ")";
        return true;
    case Builtin::MAX:
    case Builtin::MIN: {
            Declaration* d = a[0]->val.value<Declaration*>();
            if( a.size() == 1 && a[0]->kind == Expression::DeclRef && d &&
                    (d->kind == Declaration::TypeDecl || d->kind == Declaration::Builtin) )
            {
                out << limit(deref(a[0]->type()), id == Builtin::MAX);
                return true;
            }
            todo("MIN and MAX with two arguments are not yet implemented by the Micron compiler",
                 e->pos);
            out << (id == Builtin::MAX ? "max(" : "min(");
            for( int i = 0; i < a.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                Expr(a[i]);
            }
            out << ")";
        } return true;
    case Builtin::SIZE:
        out << "val(" << basicType(deref(e->type())) << ", size(" << typeRef(a[0]->type()) << "))";
        return true;
    case Builtin::ORD: {
            Expression* arg = a[0];
            if( isBuiltinCall(arg, Builtin::CHR) )
            {
                Expression* inner = Expression::getList(arg->rhs).first();
                qint64 lo = 0, hi = 0;
                if( interval(inner, lo, hi) && lo >= 0 && hi <= 255 )
                    arg = inner;
            }
            Type* t = deref(arg->type());
            const bool num = t && t->isNumberOrByte();
            const bool plain = noPromote > 0 && t &&
                    ( t->kind == Type::BYTE || t->kind == Type::CHAR );
            if( !plain )
                out << "val(" << basicType(deref(e->type())) << ", ";
            if( !num )
                out << "ord(";
            Expr(arg);
            if( !num )
                out << ")";
            if( !plain )
                out << ")";
        } return true;
    case Builtin::CHR: {
            bool ok = false;
            const qint64 ch = constVal(a[0], &ok);
            if( ok && ch >= 0 && ch < 256 )
                out << charLit(ch);
            else
            {
                out << "chr(";
                Expr(a[0]);
                out << ")";
            }
        } return true;
    case Builtin::SHORT:
    case Builtin::LONG: {
            Type* t = deref(a[0]->type());
            Type* res = deref(e->type());
            QByteArray to = res && rank(res) != 0 ? basicType(res) : QByteArray("int32");
            if( t && ( res == 0 || rank(res) == 0 ) )
            {
                switch( t->kind )
                {
                case Type::HUGEINT:
                    to = id == Builtin::SHORT ? "int32" : "int64";
                    break;
                case Type::LONGINT:
                    to = id == Builtin::SHORT ? "int16" : "int64";
                    break;
                case Type::INTEGER:
                    to = id == Builtin::SHORT ? "int8" : "int32";
                    break;
                case Type::SHORTINT:
                    to = id == Builtin::SHORT ? "int8" : "int16";
                    break;
                case Type::REAL:
                    to = id == Builtin::SHORT ? "real" : "longreal";
                    break;
                case Type::LONGREAL:
                    to = id == Builtin::SHORT ? "real" : "longreal";
                    break;
                }
            }
            if( t && ( t->kind == Type::CHAR || t->kind == Type::BYTE ) )
            {
                out << "val(" << to << ", ord(";
                Expr(a[0]);
                out << "))";
            }else
            {
                out << "val(" << to << ", ";
                Expr(a[0]);
                out << ")";
            }
        } return true;
    case Builtin::ENTIER:
        out << "floor(";
        Expr(a[0]);
        out << ")";
        return true;
    case Builtin::INC:
    case Builtin::DEC: {
        Expression* to = 0;
        Expression* ptr = 0;
        Expression* target = a[0];
        if( valArgs(a[0], to, ptr) && isPtrOrAddr(ptr) )
            target = ptr;
        out << (id == Builtin::INC ? "inc(" : "dec(");
        designator(target);
        if( a.size() > 1 )
        {
            out << ", ";
            if( isPtrOrAddr(target) && isPtrOrAddr(a[1]) )
                ordExpr(a[1]);
            else
                Expr(a[1]);
        }
        out << ")";
        } return true;
    case Builtin::INCL:
    case Builtin::EXCL: {
            Expression* to = 0;
            Expression* ptr = 0;
            if( valArgs(a[0], to, ptr) && isSetOf(to) && isPtrOrAddr(ptr) )
            {
                const QByteArray p = toStr(ptr, true);
                const QByteArray bits = "bset(val(uint32, ord(" + p + ")))";
                out << ( id == Builtin::INCL ? "inc(" : "dec(" ) << p << ", ord({"
                    << toStr(a[1]) << "}";
                if( id == Builtin::INCL )
                    out << " - " << bits; // only the bits not yet set
                else
                    out << " * " << bits; // only the bits actually set
                out << "))";
                return true;
            }
            Type* it = deref(ptr ? ptr->type() : 0);
            if( to != 0 && isSetOf(to) && it && it->isInteger() )
            {
                const QByteArray i = toStr(ptr, true);
                out << i << " := val(" << basicType(it) << ", ord(bset(usig(" << i << "))"
                    << (id == Builtin::INCL ? " + {" : " - {") << toStr(a[1]) << "}))";
                return true;
            }
            const QByteArray set = toStr(a[0], true);
            out << set << " := " << set << (id == Builtin::INCL ? " + {" : " - {");
            Expr(a[1]);
            out << "}";
        } return true;
    case Builtin::COPY:
        out << "copy(";
        addressOf(a[1]);
        out << ", ";
        if( deref(a[0]->type()) && deref(a[0]->type())->kind == Type::StrLit )
            Expr(a[0]);
        else
            addressOf(a[0]);
        out << ", ";
        lenOf(a[1]);
        out << ")";
        return true;
    case Builtin::NEW: {
            if( isArr2d(a[0]->type()) )
            {
                const QByteArray d = toMicron(a[0]);
                if( a.size() < 3 )
                {
                    todo("NEW of a two dimensional open array without both lengths", e->pos);
                    out << "// ";
                }
                out << d << ".len0 := ";
                if( a.size() > 1 )
                    Expr(a[1]);
                else
                    out << "0";
                out << ";" << endl << ws() << d << ".len1 := ";
                if( a.size() > 2 )
                    Expr(a[2]);
                else
                    out << "0";
                out << ";" << endl << ws()
                    << (level >= 4 ? "newgc(" : "newinit(") << d << ".data, "
                    << d << ".len0 * " << d << ".len1)";
                return true;
            }
            if( openDims(a[0]->type()) > 2 )
                todo("open arrays with more than two dimensions are not supported by Micron",
                     e->pos);
            out << (level >= 4 ? "newgc(" : "newinit(");
            designator(a[0]);
            Type* t = deref(a[0]->type());
            Type* base = t && t->kind == Type::Pointer ? deref(t->type()) : 0;
            if( base && base->kind == Type::Array && base->expr == 0 )
            {
                out << ", ";
                if( a.size() > 1 )
                    Expr(a[1]);
                else
                {
                    out << "0";
                    todo("NEW of an open array without length", e->pos);
                }
            }
            out << ")";
        } return true;
    case Builtin::HALT:
        out << "halt(";
        Expr(a[0]);
        out << ")";
        return true;
    case Builtin::ASSERT:
        out << "assert(";
        Expr(a[0]);
        out << ")";
        return true;
    case Builtin::AWAIT:
        out << "// AWAIT(" << toStr(a[0]) << ")";
        todo("AWAIT is not supported by Micron", e->pos);
        return true;

    // SYSTEM
    case Builtin::SYSTEM_ADR:
        if( wantAddr )
        {
            out << "cast(" << addrTypeName() << ", ";
            addressOf(a[0]);
            out << ")";
        }else
        {
            out << "val(int32, ord(";
            addressOf(a[0]);
            out << "))";
        }
        return true;
    case Builtin::SYSTEM_VAL: {
            Type* to = deref(a[0]->type());
            Type* from = deref(a[1]->type());
            QByteArray toName = typeRef(a[0]->type());
            const bool fromAddr = from && ( from->isInteger() || from->kind == Type::PTR );
            const bool srcPtr = isPointerKind(from) || mdl->isAddrExpr(a[1]);
            const bool dstPtr = isPointerKind(to) || ( to && to->isInteger() && wantAddr );
            if( dstPtr && to && to->isInteger() )
                toName = addrTypeName();
            if( dstPtr && srcPtr )
            {
                out << "cast(" << toName << ", ";
                const bool oldWant = wantAddr;
                wantAddr = true;
                Expr(a[1]);
                wantAddr = oldWant;
                out << ")";
            }else if( srcPtr && to && to->kind == Type::SET )
            {
                out << "bset(val(uint32, ord(";
                addrExpr(a[1]);
                out << ")))";
            }else if( srcPtr && to && to->isInteger() )
            {
                out << "val(" << toName << ", ord(";
                addrExpr(a[1]);
                out << "))";
            }else if( to && from && ( to == from || ( to->kind == from->kind && to->kind < Type::MaxBasicType ) ) )
            {
                bool ok = false;
                const qint64 v = constVal(a[1], &ok);
                if( to->isInteger() && ok && !fitsInt(to, v) )
                {
                    out << "val(" << toName << ", ";
                    Expr(a[1]);
                    out << ")";
                }else
                    Expr(a[1]);
            }
            else if( to && from && to->isInteger() && from->kind == Type::CHAR )
            {
                out << "val(" << toName << ", ord(";
                Expr(a[1]);
                out << "))";
            }else if( to && to->kind == Type::CHAR && from && from->isInteger() )
            {
                out << "chr(";
                Expr(a[1]);
                out << ")";
            }else if( to && ( to->kind == Type::CHAR || to->kind == Type::BYTE ) && from && from->kind == Type::SET )
            {
                out << ( to->kind == Type::CHAR ? "chr(" : "val(byte, " );
                out << "val(uint8, ord(";
                Expr(a[1]);
                out << ")))";
            }else if( to && to->kind == Type::SET && from && ( from->kind == Type::CHAR || from->kind == Type::BYTE ) )
            {
                out << "bset(val(uint32, ord(";
                Expr(a[1]);
                out << ")))";
            }else if( to && to->kind == Type::SET && from && from->kind == Type::REAL )
            {
                out << "bset(cast(uint32, ";
                Expr(a[1]);
                out << "))";
            }else if( to && to->kind == Type::REAL && from && from->kind == Type::SET )
            {
                out << "cast(real, ord(";
                Expr(a[1]);
                out << "))";
            }else if( to && to->isInteger() && isPointerKind(from) )
            {
                out << "val(" << toName << ", ord(";
                Expr(a[1]);
                out << "))";
            }else if( isSignedInt(to) && isIntLit(a[1]) && litFits(a[1], to) )
                out << constVal(a[1]) << suffix(to, constVal(a[1]));
            else if( to && from && to->isNumberOrByte() && from->isNumberOrByte() )
            {
                const bool same = byteWidth(to) == byteWidth(from);
                out << ( same ? "cast(" : "val(" ) << toName << ", ";
                Expr(a[1]);
                out << ")";
            }else if( isPointerKind(to) && isPointerKind(from) )
            {
                out << "cast(" << toName << ", ";
                Expr(a[1]);
                out << ")";
            }else if( to && to->kind == Type::SET && fromAddr )
            {
                out << "bset(usig(";
                Expr(a[1]);
                out << "))";
            }else if( to && to->isInteger() && from && from->kind == Type::SET )
            {
                out << "val(" << toName << ", ord(";
                Expr(a[1]);
                out << "))";
            }else if( to && to->kind == Type::SET && ( isPointerKind(from) || ( from && from->kind == Type::Procedure ) ) )
            {
                out << "bset(val(uint32, ord(";
                Expr(a[1]);
                out << ")))";
            }else if( isPointerKind(to) && ( fromAddr || ( from && from->kind == Type::SET ) ) )
            {
                bool ok = false;
                const qint64 addr = constVal(a[1], &ok);
                if( ok )
                    out << toName << "{" << addr << "}";
                else if( !numToPtr(a[1], toName) )
                {
                    out << "cast(" << toName << ", ";
                    Expr(a[1]);
                    out << ")";
                    todo("Micron has no conversion from a dynamic address to a pointer", e->pos);
                }
            }else if( to && to->isInteger() && from && ( from->kind == Type::Procedure || from->kind == Type::SET ) )
            {
                out << "val(" << toName << ", ord(";
                Expr(a[1]);
                out << "))";
            }else if( to && to->kind == Type::CHAR && from && from->isNumberOrByte() )
            {
                out << "chr(";
                if( byteWidth(from) != 1 )
                {
                    out << "val(uint8, ";
                    Expr(a[1]);
                    out << ")";
                }else
                    Expr(a[1]);
                out << ")";
            }else if( to && to->isStructured() )
            {
                out << "cast(" << pointerTo(to) << ", ";
                addressOf(a[1]);
                out << ")^";
            }else if( to && to->kind == Type::Procedure )
            {
                out << "nil";
                todo("Micron cannot convert an address to a procedure, so this is nil",
                     e->pos);
            }else
            {
                out << "cast(" << toName << ", ";
                Expr(a[1]);
                out << ")";
                todo(QString("SYSTEM.VAL from %1 needs review")
                     .arg(from ? Type::name[from->kind] : "?"), e->pos);
            }
        } return true;
    case Builtin::SYSTEM_LSH:
        shift(a[0], a[1], false, e);
        return true;
    case Builtin::SYSTEM_ROT:
        rotate(a[0], a[1], e);
        return true;
    case Builtin::SYSTEM_BIT:
        out << "(" << toStr(a[1]) << " in bset(usig(";
        Expr(a[0]);
        out << ")))";
        todo("SYSTEM.BIT accesses an absolute address", e->pos);
        return true;
    case Builtin::SYSTEM_MOVE:
        if( a.size() == 3 && memMove(a[0], a[1], a[2]) )
            return true;
        // else fall through to the comment
    case Builtin::SYSTEM_GET:
    case Builtin::SYSTEM_PUT:
        if( a.size() == 2 && memAccess(a[0], a[1], id == Builtin::SYSTEM_GET) )
            return true;
        // else fall through to the comment
    case Builtin::SYSTEM_GET8:
    case Builtin::SYSTEM_GET16:
    case Builtin::SYSTEM_GET32:
    case Builtin::SYSTEM_GET64:
    case Builtin::SYSTEM_PUT8:
    case Builtin::SYSTEM_PUT16:
    case Builtin::SYSTEM_PUT32:
    case Builtin::SYSTEM_PUT64:
    case Builtin::SYSTEM_NEW:
    case Builtin::SYSTEM_CC:
    case Builtin::SYSTEM_TYPECODE:
    case Builtin::SYSTEM_GETREG:
    case Builtin::SYSTEM_PUTREG:
    case Builtin::SYSTEM_PORTIN:
    case Builtin::SYSTEM_PORTOUT:
    case Builtin::SYSTEM_CLI:
    case Builtin::SYSTEM_STI:
    case Builtin::SYSTEM_ENABLEINTERRUPTS:
    case Builtin::SYSTEM_DISABLEINTERRUPTS:
    case Builtin::SYSTEM_RESTOREINTERRUPTS: {
            QByteArray str = QByteArray("SYSTEM.") + Builtin::name[id] + "(";
            for( int i = 0; i < a.size(); i++ )
            {
                if( i != 0 )
                    str += ", ";
                str += toStr(a[i]);
            }
            str += ")";
            out << "// " << str;
            const QString line = sourceLine(e->pos);
            manual << QString("%1:%2:%3: %4 needs manual translation%5")
                      .arg(QString::fromUtf8(curMod->name)).arg(e->pos.d_row)
                      .arg(e->pos.d_col).arg(str.constData())
                      .arg(line.isEmpty() ? QString() : QString("\n\tOberon: %1").arg(line));
        } return true;
    }
    return false;
}
