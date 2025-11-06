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

bool Builins::checkArgs(quint8 builtin, const ExpList& args, Type** ret, const RowCol& pos)
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
        // second argument only appears in Gad/AosProfiler.Mod, i.e. 5 lines of ~400k
        *ret = mdl->getType(Type::INTEGER);
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
    case Builtin::SYSTEM_ENABLEINTERRUPTS:
    case Builtin::SYSTEM_DISABLEINTERRUPTS:
    case Builtin::SYSTEM_RESTOREINTERRUPTS:
        break;
    default:
        Q_ASSERT(false);
    }
    }catch( const QString& err )
    {
        errPos = pos;
        error = err;
        return false;
    }catch( const char* str)
    {
        errPos = pos;
        error = QString::fromUtf8(str);
        return false;
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
        Q_ASSERT( t->validated );
        return deref(t->type());
    }else
        return t;
}
