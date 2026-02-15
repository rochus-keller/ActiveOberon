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
#include "AoBuilins.h"
#include <QtDebug>
using namespace Ao;
using namespace Ast;

Builins::Builins(Ast::AstModel *mdl):mdl(mdl)
{

}

static inline bool expectingNArgs(const ExpList& args,int n)
{
    if( args.size() != n )
        throw QString("expecting %1 arguments").arg(n);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type() == 0 )
            return false;
    return true;
}

static inline bool expectingNMArgs(const ExpList& args,int n, int m)
{
    if( args.size() < n || args.size() > m)
        throw QString("expecting %1 to %2 arguments").arg(n).arg(m);
    for( int i = 0; i < args.size(); i++ )
        if( args[i]->type() == 0 )
            return false;
    return true;
}

static inline bool expectingPtr(Expression* e, quint8 which)
{
    static const char* names[] = { "", "first", "second", "third", "fourth", "fifth" };
    if( e == 0 )
        return true;
    bool res = true;
    if( e->type() )
    {
        Type* t = e->type();
        if( t )
            t = t->deref();
        res = t && (t->kind == Type::PTR
                    || t->isInteger()  // should be replaced by PTR
                    || t->kind == Type::Pointer // also happens one time in the Oberon System 2001-10-11
                    );
        if( !res )
            throw QString("expecting SYSTEM.PTR type %1 argument").arg(names[which]);
    }
    return res;
}

bool Builins::checkArgs(quint8 builtin, const ExpList& args, Type** ret, const RowCol& pos)
{
    // NOTE: args are already visited at this point

    Q_ASSERT(ret);

    *ret = mdl->getType(Type::NoType);

    // TODO: check arg types
    try
    {
    switch(builtin)
    {
    // functions:
    case Builtin::ABS:
        if( !expectingNArgs(args,1) )
            break;
        *ret = args.first()->type();
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
        else
        {
            // second argument only appears in Gad/AosProfiler.Mod, i.e. 5 lines of ~400k
            if( args.size() == 2 )
                qWarning() << "LEN() used with two arguments";
            *ret = mdl->getType(Type::INTEGER);
            Type* t = deref(args.first()->type());
            // arg can be VAR p: POINTER TO ARRAY
            if( t->kind == Type::Reference )
                t = deref(t->type());
            if( t->kind == Type::Pointer )
                t = deref(t->type());
            if( t->kind != Type::Array )
                return report("expecting an array or pointer to array argument", pos);
        }
        break;
    case Builtin::MAX:
    case Builtin::MIN:
        if(!expectingNMArgs(args,1,2))
            break;
        if( deref(args[0]->type())->kind == Type::SET )
            *ret = mdl->getType(Type::SHORTINT);
        else
            *ret = args[0]->type();
        break;
    case Builtin::ODD:
        if( !expectingNArgs(args,1) )
            return false;
        *ret = mdl->getType(Type::BOOLEAN);
       break;
    case Builtin::ORD:
        if( !expectingNArgs(args,1) )
            return false;
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
        else
        {
            Type* lhs = deref(args.first()->type());
            // arg can be VAR p: POINTER TO ARRAY
            if( lhs->kind == Type::Reference )
                lhs = deref(lhs->type());
            if( lhs->kind == Type::Pointer )
                lhs = deref(lhs->type());
            if( lhs->kind != Type::Array && lhs->kind != Type::StrLit )
                return report("expecting an array, pointer to array or string literal first argument", pos);
            Type* rhs = deref(args.last()->type());
            // arg can be VAR p: POINTER TO ARRAY
            if( rhs->kind == Type::Reference )
                rhs = deref(rhs->type());
            if( rhs->kind == Type::Pointer )
                rhs = deref(rhs->type());
            if( rhs->kind != Type::Array)
                return report("expecting an array or pointer to array second argument", pos);
        }
        break;
        break;

    case Builtin::HALT:
        expectingNArgs(args,1);
        break;
    case Builtin::CAP:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::CHAR);
        break;
    case Builtin::ASH:
        expectingNArgs(args,2);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SHORT:
        if( !expectingNArgs(args,1) )
            break;
        switch(deref(args[0]->type())->kind)
        {
        case Type::HUGEINT:
            *ret = mdl->getType(Type::LONGINT);
            break;
        case Type::LONGINT:
            *ret = mdl->getType(Type::INTEGER);
            break;
        case Type::INTEGER:
            *ret = mdl->getType(Type::SHORTINT);
            break;
        case Type::LONGREAL:
            *ret = mdl->getType(Type::REAL);
            break;
        default:
            *ret = args[0]->type();
            break;
        }
        break;
    case Builtin::LONG:
        if( !expectingNArgs(args,1) )
            break;
        switch(deref(args[0]->type())->kind)
        {
        case Type::LONGINT:
            *ret = mdl->getType(Type::HUGEINT);
            break;
        case Type::INTEGER:
            *ret = mdl->getType(Type::LONGINT);
            break;
        case Type::SHORTINT:
        case Type::CHAR: // CHAR is equivalent to BYTE
            *ret = mdl->getType(Type::INTEGER);
            break;
        case Type::REAL:
            *ret = mdl->getType(Type::LONGREAL);
            break;
        default:
            *ret = args[0]->type();
            break;
        }
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
        *ret = mdl->getType(Type::PTR);
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
        *ret = args[0]->type();
        break;
    case Builtin::SYSTEM_TYPECODE:
        expectingNArgs(args,1);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SYSTEM_GET8:
        if( !expectingNArgs(args,1) )
            break;
        expectingPtr(args[0], 1);
        *ret = mdl->getType(Type::SHORTINT);
        break;
    case Builtin::SYSTEM_GET16:
        if( !expectingNArgs(args,1) )
            break;
        expectingPtr(args[0], 1);
        *ret = mdl->getType(Type::INTEGER);
        break;
    case Builtin::SYSTEM_GET32:
        if( !expectingNArgs(args,1) )
            break;
        expectingPtr(args[0], 1);
        *ret = mdl->getType(Type::LONGINT);
        break;
    case Builtin::SYSTEM_GET64:
        if( !expectingNArgs(args,1) )
            break;
        expectingPtr(args[0], 1);
        *ret = mdl->getType(Type::HUGEINT);
        break;

    case Builtin::SYSTEM_GET:
        if( !expectingNArgs(args,2) )
            break;
        expectingPtr(args[0], 1);
        break;

    case Builtin::SYSTEM_PUT:
    case Builtin::SYSTEM_PUT8:
    case Builtin::SYSTEM_PUT16:
    case Builtin::SYSTEM_PUT32:
    case Builtin::SYSTEM_PUT64:
        if( !expectingNArgs(args,2) )
            break;
        expectingPtr(args[0], 1);
        break;

    case Builtin::SYSTEM_MOVE:
        if( !expectingNArgs(args,3) )
            break;
        expectingPtr(args[0], 1);
        expectingPtr(args[1], 2);
        break;

    case Builtin::SYSTEM_NEW:
        if( args.isEmpty() )
            throw "expecting at least one argument";
        if( deref(args[0]->type())->kind != Type::Pointer
                && deref(args[0]->type())->kind != Type::PTR // this one only happens once in Bluebootle, not in Oberon System
                )
            throw "first argument must be a pointer";
        break;

        // system procs
    case Builtin::SYSTEM_PORTOUT:
    case Builtin::SYSTEM_PORTIN:
    case Builtin::SYSTEM_CLI:
    case Builtin::SYSTEM_STI:
    case Builtin::SYSTEM_GETREG:
    case Builtin::SYSTEM_PUTREG:
    case Builtin::SYSTEM_ENABLEINTERRUPTS:
    case Builtin::SYSTEM_DISABLEINTERRUPTS:
    case Builtin::SYSTEM_RESTOREINTERRUPTS:
        break;
    default:
        Q_ASSERT(false);
    }
    }catch( const QString& err )
    {
        return report(err,pos);
    }catch( const char* str)
    {
        return report(QString::fromUtf8(str),pos);
    }

    return true;
}

Type *Builins::deref(Ast::Type *t)
{
    // never returns zero
    if( t == 0 )
        return mdl->getType(Type::NoType);
    if( t->kind == Type::NameRef )
    {
        if( !t->validated )
            qWarning() << "Builins::deref type not validated";
        return deref(t->type());
    }else
        return t;
}

bool Builins::report(const QString &msg, const RowCol &pos)
{
    error = msg;
    errPos = pos;
    return false;
}
