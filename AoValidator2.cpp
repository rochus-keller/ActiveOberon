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
#include "AoAsmToIntelXpiler.h"
#include <QtDebug>
using namespace Ao;
using namespace Ast;

// TODO
// check visibility/accessibility

#define _ALLOW_POINTER_BASE_TYPE
#define _ALLOW_RETURN_STRUCTURED_TYPES
// #define _SUPPORT_STRANGE_RELATIONS

static QByteArray SELF;

static inline void dummy() {}

static inline bool isConstChar( Expression* e )
{
    if( e == 0 )
        return false;
    else
        return e->isCharLiteral();
}

static inline bool isTextual( Expression* e )
{
    if( e == 0 )
        return false;
    Type* t = e->type() ? e->type()->deref() : 0;
    if( t && t->kind == Type::StrLit )
        return true;
    if( t && t->kind == Type::Array )
    {
        t = t->type() ? t->type()->deref() : 0;
        return t && t->kind == Type::CHAR;
    }
    if( e->kind == Expression::DeclRef && e->type() && e->type()->kind == Type::StrLit )
    {
        Declaration* d = e->val.value<Declaration*>();
        if( d && d->kind == Declaration::ConstDecl )
            return true;
    }
    return false;
}

Validator2::Validator2(Ast::AstModel *mdl, Ast::Importer *imp, bool haveXref):module(0),mdl(mdl),imp(imp),
    first(0),last(0),curObj(0)
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
        mod->imported = true;
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
        decl(d);
        d = d->next;
	}
    return d;
}

void Validator2::ConstDecl(Ast::Declaration* d) {
    ConstExpr(d->expr);
    d->setType(d->expr->type());
    // no effect, value is in expr d->data = d->expr->val;
}

bool Validator2::checkIfObjectInit(Type* t)
{
    switch(t->kind)
    {
    case Type::Array:
        return checkIfObjectInit(deref(t->type()));
    case Type::Record:
    case Type::Object:
        return true;
    case Type::NameRef: {
        Type* t2 = deref(t);
        if( t == t2 )
            return false;
        return checkIfObjectInit(t2);
    }
    default:
        return false;
    }
}

bool Validator2::checkIfPointerInit(Type* t)
{
    switch(t->kind)
    {
    case Type::Array:
        return checkIfPointerInit(deref(t->type()));
    case Type::Record:
    case Type::Object:
        foreach( Declaration* d, t->subs )
        {
            if( d->kind == Declaration::Field && deref(d->type())->kind == Type::Pointer )
                return true;
        }
        return false;
    case Type::NameRef: {
        Type* t2 = deref(t);
        if( t == t2 )
            return false;
        return checkIfPointerInit(t2);
    }
    default:
        return false;
    }
}

void Validator2::TypeDecl(Ast::Declaration* d) {
    Type* t = d->type();
    Type_(t);
    t->objectInit = checkIfObjectInit(t);
    t->pointerInit = checkIfPointerInit(t);
}

void Validator2::VarDecl(Ast::Declaration* d) {
    Type* t = d->type();
    Type_(t);
    char what = ' ';
    switch(d->kind)
    {
    case Declaration::VarDecl:
        what = 'm';
        break;
    case Declaration::LocalDecl:
        what = 'l';
        break;
    case Declaration::Field:
        what = 'f';
        break;
    default:
        Q_ASSERT(false);
    }
    t->objectInit = checkIfObjectInit(t);
    t->pointerInit = checkIfPointerInit(t);
    //arrayStats(d->type(), d->pos, what);
}

void Validator2::Assembler(Ast::Declaration* proc) {
    //expect(Tok_CODE, false, "Assembler");
    // TODO
    // qDebug() << "###" << module->name.constData() << proc->pos.d_row << proc->name.constData() << "\n" << proc->data.toByteArray().mid(5).constData();
#if 0
    // TEST
    QString err;
    TranspileOptions opt;
    qDebug() << module->name << proc->pos.d_row << proc->name << AsmToIntelXpiler::transform(proc->data.toString().mid(5), opt, &err);
    if( !err.isEmpty() )
        qCritical() << "### error" << err;
#endif
}

void Validator2::ProcDecl(Ast::Declaration * proc) {
    // SysFlag();
    scopeStack.push_back(proc);

    Ast::Declaration* d = proc->link;
    while( d && d->kind == Ast::Declaration::ParamDecl )
    {
        markDecl(d);
        d->validated = true;
        //if( !d->varParam && d->type()->kind == Type::Array && d->type()->expr == 0 )
        //    qDebug() << "non-var open array param at" << module->name << d->pos.d_row;
        if( !d->receiver )
            Type_(d->type());
        //arrayStats(d->type(), d->pos, d->varParam ? 'v' : 'p');
        d = d->next;
    }
    if( proc->type() )
    {
        Type_(proc->type());
        Type* t = deref(proc->type());
        if( t->kind == Type::Array || t->kind == Type::Record )
            error(proc->pos, "return type cannot be an array nor a record");
        //arrayStats(proc->type(), proc->pos, 'r');
    }
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
    // SysFlag();
    if( t->expr )
    {
        ConstExpr(t->expr);
        if( !deref(t->expr->type())->isInteger() )
            error(t->expr->pos,"expecting an integer array length");
    }
    return Type_(t->type());
}

bool Validator2::RecordType(Ast::Type* t) {
    // SysFlag();
    if( t->type() && Type_(t->type()) ) // optional base class
    {
        Type* base = deref(t->type());
        if( base->kind == Type::Object )
        {
            // ok?
            // happens indeed four time in bluebottle:
            // Apps/PCBT.Mod:114:23
            // Apps/PCT.Mod:412:22
            // Apps/PCT.Mod:417:25
            // Apps/PCT.Mod:441:21
        }else
        {
            if( base->kind == Type::Pointer )
                base = deref(base->type());
            if( base->kind != Type::Record )
                error(t->pos, "invalid base record type");
        }
        QList<Type*> seen;
        if( !checkCircularBaseTypes(base, seen) )
            error(t->pos, "circular base type detected");
    }
    return FieldList(t);
}

bool Validator2::PointerType(Ast::Type* t) {
    // SysFlag();
    if( Type_(t->type()) )
    {
        Type* to = deref(t->type());
        if( to->kind != Type::Record && to->kind != Type::Array )
            return error(t->pos, "pointer base type must be ARRAY or RECORD");
    }else
        return false;
}

bool Validator2::ObjectType(Ast::Type* t) {

    // qDebug() << module->name.constData(); // << t->pos.d_row;

    // SysFlag();
    if( t->type() && !t->type()->validated ) // optional base class
    {
        // resolve base objects if present
        resolveIfNamedType(t->type(), t->pos);
    }
    if( t->type() && Type_(t->type()) ) // optional base class
    {
        Type* base = deref(t->type());
        if( base->kind == Type::Pointer )
        {
            base = deref(base->type());
            if( base->kind != Type::Record )
                error(t->pos, "invalid base object type");
                /*
                indeed happens in bluebottle:
                Sys/AosActive.Mod:186:2
                Apps/WMGraphics.Mod:27:2
                Apps/PCBT.Mod:62:2
                Apps/PCBT.Mod:86:2
                Apps/PCBT.Mod:118:2
                Apps/PCLIR.Mod:246:2
                Apps/PCB.Mod:78:2
                Apps/PCOM.Mod:165:2
                Apps/PCOM.Mod:182:2
                Apps/PCGARM.Mod:55:2
                */
        }else if( base->kind != Type::Object )
            error(t->pos, "invalid base object type");
        QList<Type*> seen;
        if( !checkCircularBaseTypes(base, seen) )
            error(t->pos, "circular base type detected");
    }

    QList<Ast::Declaration*> boundProcs;
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
            bindProc(t, member);
            boundProcs << member;
            break;
        default:
            invalid("ObjectDeclSeq", t->pos);
            break;
        }
    }

    if( !boundProcs.isEmpty() )
    {
        if(curObj == 0)
        {
            curObj = t;
            foreach( Ast::Declaration* p, boundProcs )
                ProcDecl(p);
            curObj = 0;
        }else
            error(t->pos, "nested object type and method declarations not supported");
    }

    return true;
}

bool Validator2::ProcedureType(Ast::Type* t) {
    // SysFlag();
    // Attributes();
    int ok = 1;
    foreach( Ast::Declaration* param, t->subs )
    {
        if( param->kind == Ast::Declaration::ParamDecl )
        {
            ok &= Type_(param->type());
            //arrayStats(param->type(), param->pos, param->varParam ? 'v' : 'p');
        }
    }
    if( t->type() && t->type()->kind != Ast::Type::NoType )
    {
        ok &= Type_(t->type());
        //arrayStats(t->type(), t->decl ? t->decl->pos : RowCol(), 'r');
    }
    return ok;
}

bool Validator2::AliasType(Ast::Type* t) {
    t->validated = false; // it was false when entering this proc, and resolveIfNamedType does nothing otherwise
    resolveIfNamedType(t, t->pos);
    return true;
}

bool Validator2::Type_(Ast::Type* t) {
    if( t == 0 || t->validated )
        return true;
    t->validated = true;

    bool res = false;
    switch( t->kind )
    {
    case Ast::Type::NameRef:
        res = AliasType(t);
        break;
    case Ast::Type::Pointer:
        res = PointerType(t);
        break;
    case Ast::Type::Reference:
        res = Type_(t->type());
        break;
    case Ast::Type::Procedure:
        res = ProcedureType(t);
        break;
    case Ast::Type::Array:
        res = ArrayType(t);
        break;
    case Ast::Type::Record:
        res = RecordType(t);
        break;
    case Ast::Type::Object:
        res = ObjectType(t);
        break;
    case Ast::Type::NoType:
    case Ast::Type::ANYOBJ:
    case Ast::Type::ANY:
        return true;
    default:
        invalid("Type", t->pos);
    }

    if( t->kind == Type::Object || t->kind == Type::Record)
    {
        if( t->type() )
        {
            if( t->decl == 0 )
                return error(t->pos, "cannot use anonymous base object/record type TODO");
            Ast::Type* baseType = t->type();
            Q_ASSERT( baseType->kind == Type::NameRef );
            if( !baseType->validated || baseType->type() == 0 )
                return error(t->pos, "base type not yet validated TODO");
            Declaration* super = baseType->type()->deref()->decl;
            if( super == 0 )
                return error(t->pos, "referencing anonymous base object/record type TODO");
            super->hasSubs = true;
            t->decl->super = super;
            if( first )
                subs[super].append(t->decl);
        }
    }

    return res;
}

bool Validator2::FieldList(Ast::Type* t) {
    foreach( Ast::Declaration* f, t->subs )
    {
        if( f->validated )
            continue;
        f->validated = true;
        markDecl(f);
        Type_(f->type());
        arrayStats(f->type(), f->pos, 'f');
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

    Expr(s->lhs);
    Expr(s->rhs);

    if( s->lhs == 0 || s->rhs == 0 )
        return; // already reported

    if( !s->lhs->isLvalue() )
        error(s->pos, "cannot assign to lhs (not an lvalue)");

    if( !assigCompat(s->lhs->type(), s->rhs) )
    {
        //assigCompat(s->lhs->type(), s->rhs); // TEST
        error(s->pos, "rhs is not assignment compatible with lhs");
    }
}

Ast::Statement *Validator2::IfStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::If);
    Expr(s->rhs); // if
    if( s->rhs && !deref(s->rhs->type())->kind == Type ::BOOLEAN )
        error(s->rhs->pos, "expecting a boolean expression");
    StatSeq(s->body);
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::Elsif ) {
        s = s->getNext();
        Expr(s->rhs);
        if( s->rhs && !deref(s->rhs->type())->kind == Type ::BOOLEAN )
            error(s->rhs->pos, "expecting a boolean expression");
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

    Expr(s->rhs); // case
    Type* te = s->rhs ? deref(s->rhs->type()) : deref(0);
    if( s->rhs && te->kind != Type::CHAR && !te->isInteger() )
        error(s->rhs->pos, "expecing an integer or CHAR case expression");

    // TODO: check labels unique
    while( s && s->getNext() && s->getNext()->kind == Ast::Statement::CaseLabel )
    {
        s = s->getNext();
        Ast::Expression* label = s->rhs;
        while( label )
        {
            ConstExpr(label);

            if( !assigCompat(te, label) )
            {
                //assigCompat(te, label); // TEST
                error(label->pos, "label not compatible with case expression type");
            }
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
    Expr(s->rhs);
    if( s->rhs && deref(s->rhs->type())->kind != Type::BOOLEAN )
        error(s->rhs->pos, "expecting boolean expression");
    StatSeq(s->body);
}

void Validator2::RepeatStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Repeat);
    StatSeq(s->body);
    Expr(s->rhs);
    if( s->rhs && deref(s->rhs->type())->kind != Type::BOOLEAN )
        error(s->rhs->pos, "expecting boolean expression");
}

Ast::Statement *Validator2::ForStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::ForAssig);

    Expression* cv = s->lhs;
    Expr(cv); // i := val
    if( cv && !deref(cv->type())->isInteger() )
        error(cv->pos, "expecting integer type control variable");
    Expr(s->rhs); // val
    if( cv && s->rhs && !assigCompat(cv->type(), s->rhs->type()) )
        error(s->rhs->pos, "expression not compatible with control variable");
    Ast::Statement* body = s->body;

    if( s && s->getNext() && s->getNext()->kind == Ast::Statement::ForToBy )
    {
        s = s->getNext();
        Expr(s->lhs); // to
        if( cv && s->lhs && !assigCompat(cv->type(), s->lhs) )
            error(s->lhs->pos, "expression not compatible with control variable");
        if( s->rhs )
        {
            ConstExpr(s->rhs); // by
            if( cv && !assigCompat(cv->type(), s->rhs) )
                error(s->rhs->pos, "expression not compatible with control variable");
        }
    }else
        invalid("For Statement", s->pos);

    StatSeq(body);
    return s;
}

void Validator2::LoopStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Loop);
    loopStack.push_back(s);
    StatSeq(s->body);
    loopStack.pop_back();
}

bool Validator2::isPtrOrVarWithRecordObject(Expression* e)
{
    if( e->kind != Expression::DeclRef )
        return false;
    Declaration* d = e->val.value<Declaration*>();
    if( d == 0 || !d->isLvalue() )
        return false;
    Type* t = deref(d->type());
    if( t->kind == Type::Object )
        return true;
    if( t->kind == Type::PTR )
        return true; // special case which happens 20 times in Bluebottle
    else if( t->kind == Type::Pointer )
    {
        t = deref(t->type());
        return t->kind == Type::Record;
    }else if( t->kind == Type::Record )
        return d->isVarParam();
    return false;
}

bool Validator2::checkCircularBaseTypes(Ast::Type * t, QList<Ast::Type *> &seen)
{
#if 1
    while( t && t->kind != Type::NoType )
    {
        seen << t;
        t = deref(t->type());
        if( seen.contains(t) )
            return false;
    }
#endif
    return true;
}

Ast::Statement *Validator2::WithStat(Ast::Statement *s) {
    Q_ASSERT(s && s->kind == Ast::Statement::With);
    Expr(s->lhs);
    Expr(s->rhs);

    if( s->lhs == 0 || s->rhs == 0 )
        return s; // already reported

    if( !isPtrOrVarWithRecordObject(s->lhs) )
    {
        error(s->lhs->pos, "expecting pointer variable or a variable parameter of record/object type");
        return s;
    }

    Q_ASSERT(s->rhs->kind == Expression::Literal); // just a dummy expression to hold the type

    if( !assigCompat(s->lhs->type(), s->rhs->type()) )
    {
        error(s->rhs->pos, "guard is not an extension of the variable type");
        return s;
    }

    Declaration* d = s->lhs->val.value<Declaration*>();
    Ast::Type* t = d->overrideType(s->rhs->type());
    StatSeq(s->body);
    d->overrideType(t);

    return s;
}

void Validator2::ReturnStat(Ast::Statement* s) {
    Q_ASSERT(s && s->kind == Ast::Statement::Return);
    Declaration* proc = 0;
    if( !scopeStack.isEmpty() && scopeStack.back()->kind == Declaration::Procedure )
        proc = scopeStack.back();
    if( proc == 0 )
    {
        error(s->pos, "RETURN statement is not in a PROCEDURE");
        return;
    }
    bool isFunction = false;
    if( proc->type() && proc->type()->kind != Type::NoType )
        isFunction = true;
    if( s->rhs ) {
        if( !isFunction )
            error(s->pos, "RETURN expression in a proper procedure");
        else
        {
            if( Expr(s->rhs) && !assigCompat(proc->type(), s->rhs) )
                error( s->rhs->pos, "incompatible return expression");
        }
    }else if( isFunction )
        error(s->pos, "RETURN requires an expression");
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
        if( loopStack.isEmpty() )
            error(s->pos, "EXIT statement is not in a LOOP statement");
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
    if( !Expr(e) )
        return false;
    if( !e->isConst() )
        return error(e->pos, "expression is not constant");
    return true;
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

bool Validator2::relation(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);

    if( e->lhs == 0 || e->rhs == 0 )
        return true; // already reported

    Ast::Type* lhsT = deref(e->lhs->type());
    Ast::Type* rhsT = deref(e->rhs->type());

    if( e->kind == Expression::Is )
    {
        if( !e->lhs->isLvalue() )
            error(e->pos, "lhs of IS relation must be an lvalue");

        if(lhsT->kind == Type::Object)
        {
            if( rhsT->kind == Type::Pointer )
            {
                // in bluebottle there are some cases where OBJECT IS POINTER TO RECORD!
                rhsT = deref(rhsT->type());
                if( rhsT->kind != Type::Record )
                    error(e->pos, "IS cannot be applied to these operand types");
            }else if( rhsT->kind != Type::Object )
                error(e->pos, "IS cannot be applied to these operand types");
            else if( !assigCompat(lhsT,rhsT) )
                error(e->pos, "OBJECT operands not related, so IS cannot be applied");
        }else if( lhsT->kind == Type::PTR )
        {
            if( !assigCompat(lhsT, rhsT) )
                error(e->pos, "IS cannot be applied to these operand types");
        }else if( lhsT->kind == Type::Pointer )
        {
            lhsT = deref(lhsT->type());
            if( rhsT->kind == Type::Pointer )
                rhsT = deref(rhsT->type());
            if( lhsT->kind != Type::Record )
                error(e->pos, "IS can only be applied to RECORD or OBJECT types");
            else if( !assigCompat(lhsT, rhsT) )
            {
                assigCompat(lhsT, rhsT); // TEST
                error(e->pos, "RECORD operands not related, so IS cannot be applied");
            }
        }else if( lhsT->kind == Type::Record )
        {
            if( !assigCompat(lhsT, rhsT) )
                error(e->pos, "RECORD operands not related, so IS cannot be applied");
        }
#ifdef _SUPPORT_STRANGE_RELATIONS
        // TODO: ANY IS OBJECT kommt vor!
        else
            error(e->pos, "IS cannot be applied to given operand types");
#endif
    }else if( isConstChar(e->lhs) && isConstChar(e->rhs) )
    {
        mdl->assureCharLit(e->lhs);
        mdl->assureCharLit(e->rhs);
        if( e->kind == Expression::In || e->kind == Expression::Is )
            error(e->pos, "CHAR operands not compatible with IN or IS operator");
    }else if( isTextual(e->lhs) && isTextual(e->rhs) )
    {
        if( e->kind == Expression::In || e->kind == Expression::Is )
            error(e->pos, "string operands not compatible with IN or IS operator");
    }else if( lhsT->isNumber() && rhsT->isNumber() )
    {
        if( e->kind == Expression::In || e->kind == Expression::Is )
            error(e->pos, "numeric operands not compatible with IN or IS operator");
    }else if( lhsT->isSet() && rhsT->isSet() )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "SET operands not compatible with given operator");
    }else if( lhsT->isInteger() && rhsT->isSet() )
    {
        if( e->kind != Expression::In )
            error(e->pos, "operands not compatible with IN operator");
    }else if( lhsT->kind == Type::BOOLEAN && rhsT->kind == Type::BOOLEAN )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "BOOLEAN operands not compatible with given operator");
    }else if( (lhsT->kind == Type::Pointer && rhsT->kind == Type::Pointer) ||
              (lhsT->kind == Type::NIL && rhsT->kind == Type::Pointer) ||
              (lhsT->kind == Type::Pointer && rhsT->kind == Type::NIL) )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "POINTER operands not compatible with given operator");
    }else if( (lhsT->kind == Type::Procedure && rhsT->kind == Type::Procedure) ||
              (lhsT->kind == Type::NIL && rhsT->kind == Type::Procedure) ||
              (lhsT->kind == Type::Procedure && rhsT->kind == Type::NIL) )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "procedure type operands not compatible with given operator");
    }else if( (lhsT->kind == Type::Object && rhsT->kind == Type::Object) ||
              (lhsT->kind == Type::NIL && rhsT->kind == Type::Object) ||
              (lhsT->kind == Type::Object && rhsT->kind == Type::NIL) )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "object operands not compatible with given operator");
    }else if( (lhsT->kind == Type::PTR && rhsT->kind == Type::PTR) ||
              (lhsT->kind == Type::NIL && rhsT->kind == Type::PTR) ||
              (lhsT->kind == Type::PTR && rhsT->kind == Type::NIL) )
    {
        if( e->kind != Expression::Eq && e->kind != Expression::Neq )
            error(e->pos, "SYSTEM.PTR operands not compatible with given operator");
    }
#ifdef _SUPPORT_STRANGE_RELATIONS
    else
        error(e->pos, "operands not compatible with given operator");
        // TODO: folgendes kommt vor: POINTER TO ARRAY-StrLit, POINTER-PTR, ANY-NIL, ANY-ANY, ANY-PTR, PTR-OBJECT
#endif

    e->setType(mdl->getType(Type::BOOLEAN));
    return false;
}

bool Validator2::unaryOp(Ast::Expression *e)
{
    if( e == 0 )
        return false;

    Expr(e->lhs);

    if( e->lhs == 0 )
        return true; // already reported

    if( e->lhs->type() == 0 )
        return true; // already reported

    Ast::Type* lhsT = deref(e->lhs->type());
    if( e->kind == Expression::Plus )
    {
        if( !lhsT->isNumber() )
            return error(e->pos, "unary operator not applicable to this type");
        e->setType(lhsT);
    }else if( e->kind == Expression::Minus )
    {
        if( lhsT->isNumber() || lhsT->isSet() )
            e->setType(lhsT);
        else if( lhsT->kind == Type::PTR )
            e->setType(mdl->getType(Type::LONGINT)); // happens once in AosActive.Mod:1052
        else
            return error(e->pos, "unary operator not applicable to this type");
    }else if( e->kind == Expression::Not )
    {
        if( !lhsT->isBoolean()  )
            return error(e->pos, "unary '~' or 'NOT' not applicable to this type");
        e->setType(lhsT);
    }else
        Q_ASSERT(false);
    return true;
}

bool Validator2::arithOp(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);

    Ast::Type* lhsT = e->lhs ? deref(e->lhs->type()) : deref(0);
    Ast::Type* rhsT = e->rhs ? deref(e->rhs->type()) : deref(0);

    if( lhsT->isIntegerOrByte() && rhsT->isIntegerOrByte() )
    {
        if( e->kind == Expression::Fdiv )
            e->setType(mdl->getType(Ast::Type::REAL));
        else
            e->setType(includingType(lhsT,rhsT));
    }else if( lhsT->isNumberOrByte() && rhsT->isNumberOrByte() )
    {
        if( e->kind == Expression::Mod || e->kind == Expression::Div )
            error(e->pos, "incompatible operands for DIV or MOD operator");
        e->setType(includingType(lhsT,rhsT));
    }else if( (lhsT->isIntegerOrByte() && rhsT->kind == Type::PTR) ||
              (lhsT->kind == Type::PTR && rhsT->isIntegerOrByte()) )
    {
        // explicit pointer arithmetics
        if( e->kind == Expression::Add || e->kind == Expression::Sub )
            e->setType(mdl->getType(Type::PTR));
        else if( lhsT->kind == Type::PTR && (e->kind == Expression::Div || e->kind == Expression::Mod) )
            e->setType(mdl->getType(Type::LONGINT));
        else
            error(e->pos, "operation not supported in pointer arithmetics");
    }else if( lhsT->kind == Type::PTR && rhsT->kind == Type::PTR )
    {
        // explicit pointer difference
        if( e->kind != Expression::Sub )
            error(e->pos, "only pointer difference is supported");
        e->setType(mdl->getType(Type::LONGINT));
    }else if( lhsT->isSet() && rhsT->isSet() )
    {
        switch(e->kind)
        {
        case Expression::Add:
        case Expression::Sub:
        case Expression::Mul:
        case Expression::Fdiv:
            break;
        default:
            error(e->pos, "incompatible operands for SET operators");
            break;
        }

        e->setType(lhsT);
    }else
    {
        error(e->pos, QString("incompatible operands (%1, %2) for arithmetic operators")
              .arg(Type::name[lhsT->kind]).arg(Type::name[rhsT->kind]));
        e->setType(mdl->getType(Type::NoType));
    }
    return true;
}

bool Validator2::logicOp(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    Ast::Type* lhsT = e->lhs ? deref(e->lhs->type()) : deref(0);
    Ast::Type* rhsT = e->rhs ? deref(e->rhs->type()) : deref(0);
    if( (lhsT->kind != Ast::Type::BOOLEAN) || (rhsT->kind != Ast::Type::BOOLEAN) )
        return error(e->pos, "expecing boolean operators");

    e->setType(mdl->getType(Type::BOOLEAN));
    return true;
}

bool Validator2::declRef(Ast::Expression *e)
{
    if( e == 0 )
        return false;
    Declaration* d = e->val.value<Declaration*>();
    if( d )
    {
        if( !d->validated )
            decl(d);
        e->setType(d->type());
        return true;
    }
    e->setType(mdl->getType(Type::NoType));
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
    // NOTE: this is not yet generated by parser
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
    ExpList args = Expression::getList(e->rhs);
    if( proc && proc->kind != Declaration::Builtin && args.size() != formals.size() )
        error(e->pos, "number of actual arguments does not correspond to number of formal paramteters");
    else
        for(int i = 0; i < args.size(); i++ )
        {
            Expr(args[i]);
            if( proc && proc->kind != Declaration::Builtin && !paramCompat(formals[i]->type(), args[i]) )
                error(args[i]->pos, "actual argument is not compatible with formal parameter");
        }

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
#if 0
            if( proc->id == Builtin::LEN && actuals.size() > 1 )
                qDebug() << "LEN(" << actuals.size()-1 << ")" << module->name.constData() << e->pos.d_row; // never happens in OS v2.3.7
#endif
#if 0
            if( proc->id == Builtin::LEN && actuals.size() == 1 && actuals[0]->kind == Expression::Index )
                qDebug() << "LEN(subarray)" << module->name.constData() << e->pos.d_row;
#endif

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
    return true;
}

bool Validator2::constructor(Ast::Expression *e)
{
    if( e == 0 )
        return false;

    Expression* comp = e->rhs;
    while( comp )
    {
        if( comp->kind == Expression::Constructor )
            return error(comp->pos,"component type not supported for SET constructors");
        Expr(comp);
        if( comp->type() && !deref(comp->type())->isIntegerOrByte() )
            return error(comp->pos,"expecting integer compontents for SET constructors");
        comp = comp->next;
    }
    e->setType(mdl->getType(Type::SET));
    return true;
}

bool Validator2::range(Ast::Expression *e)
{
    if( e->lhs == 0 || e->rhs == 0 )
        return false;
    Expr(e->lhs);
    Expr(e->rhs);
    if( isConstChar(e->lhs) && isConstChar(e->rhs) )
    {
        mdl->assureCharLit(e->lhs);
        mdl->assureCharLit(e->rhs);
        e->setType(mdl->getType(Type::CHAR));
    }else if( deref(e->lhs->type())->isInteger() && deref(e->rhs->type())->isInteger() )
        e->setType(includingType(deref(e->lhs->type()), deref(e->rhs->type())));
    else
        return error(e->pos, "invalid range");
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
    }else
        nameRef->forward = true;

    Symbol* s = markRef(r.second, pos);
    if( nameRef->needsLval )
        s->kind = Symbol::Lval;
    resolveIfNamedType(r.second->type(), nameRef->pos);

    nameRef->kind = Expression::DeclRef;
    nameRef->val = QVariant::fromValue(r.second);

    if( !r.second->validated && r.second->kind == Declaration::ConstDecl )
        decl(r.second); // AosTV does use ConstDecls in expressions before they were visited
    // TODO: maybe we should conduct a separate initial run to just import and resolve NameRefs.

    nameRef->setType(r.second->type());

    if( r.second->kind == Declaration::LocalDecl || r.second->kind == Declaration::ParamDecl ||
            (r.second->kind == Declaration::Procedure && r.second->outer->kind == Declaration::Procedure) )
    {
        if( !scopeStack.isEmpty() && r.second->outer && r.second->outer != scopeStack.back() )
        {
            nameRef->nonlocal = true;
            Q_ASSERT(r.second->outer->kind == Declaration::Procedure);
            r.second->outer->nonlocal = true;
            // accessing parameter or local variable of outer procedures
            // qDebug() << "non-local access" << module->name << nameRef->pos.d_col << nameRef->pos.d_row;
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

void Validator2::bindProc(Ast::Type* object, Ast::Declaration * proc)
{
    // object->decl can be a type decl or even a var decl, see AosFS:604
    Q_ASSERT( object && object->kind == Ast::Type::Object );

    // do this only for module-level procedures; these can have nested procedures
    // which are not bound to the object, e.g. Adaptec7 line 335 proc Data nested in proc Synchronize
    proc->receiver = true;
    proc->outer = object->decl;
    Declaration* self = new Declaration();
    self->kind = Declaration::ParamDecl;
    self->name = SELF;
    self->setType(object);
    self->pos = object->pos;
    self->receiver = true;
    self->next = proc->link;
    self->outer = proc;
    proc->link = self;

    if( object->type() )
    {
        // there is a super class, so look whether this is an inherited method and connect
        Declaration* super = findInType(object->type()->deref(),proc->name);
        if( super )
        {
            super->hasSubs = true;
            proc->super = super;
            if( first )
                subs[super].append(proc);
        }
    }
}

bool Validator2::decl(Ast::Declaration * d)
{
    if( d->validated )
        return true;
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
    return true;
}

bool Validator2::assigCompat(Ast::Type *lhs, Ast::Type *rhs)
{
    if( lhs == 0 || rhs == 0 )
        return true; // already reported
    lhs = deref(lhs);
    rhs = deref(rhs);

    if( lhs == rhs )
        return true;
    if(lhs->isNumber() && rhs->isNumber() )
        return lhsIncludeRhs(lhs, rhs);
    if( lhs->kind == Type::BYTE && (rhs->kind == Type::CHAR || rhs->kind == Type::SHORTINT) )
        return true;
    if(lhs->kind == Type::Pointer && rhs->kind == Type::Pointer )
        return assigCompat(lhs->type(), rhs->type());
    if( (lhs->kind == Type::Object && rhs->kind == Type::Object) ||
        (lhs->kind == Type::Record && rhs->kind == Type::Record) )
        return lhsIsBaseOfRhs(lhs, rhs);
    if( lhs->kind == Type::ANYOBJ && rhs->kind == Type::Object )
        return true;
    if( (lhs->kind == Type::Pointer || lhs->kind == Type::Object || lhs->kind == Type::ANYOBJ ||
         lhs->kind == Type::PTR || lhs->kind == Type::Procedure)
            && rhs->kind == Type::NIL )
        return true;
    if( lhs->kind == Type::PTR && rhs->kind == Type::Pointer )
        return true; // happens in Oberon Systen 2.3.7
    return false;
}

bool Validator2::assigCompat(Ast::Type *lhsT, Ast::Expression *rhs)
{
    if( lhsT == 0 || rhs == 0 )
        return true; // already reported

    lhsT = deref(lhsT);
    Type* rhsT = deref(rhs->type());

    if( /*rhs->isConst() && */ lhsT->isNumber() && rhsT->isNumber() )
        return true; // NOTE: joker added because we don't do const eval here so that e.g. OPM.Mod:169 is INTEGER instead of SHORTINT,
                     // or, there are LONGREAL consts assigned to REAL, e.g. JPEG.Mod:1559
                     // there are LONGREAL consts used in arith exprs and assigned to REAL, e.g. JPEG.Mod:2027, same for integers

    if( lhsT->kind == Type::Array && lhsT->expr != 0 && deref(lhsT->type())->kind == Type::CHAR && rhsT->kind == Type::StrLit )
        return true; // TODO: check m < n for ARRAY n OF CHAR and string constant with m characters

    if( (lhsT->kind == Type::CHAR || lhsT->kind == Type::BYTE) && rhsT->kind == Type::StrLit )
    {
        if( !isConstChar(rhs) )
            return false;
        mdl->assureCharLit(rhs);
        return true;
    }

    if( lhsT->kind == Type::Procedure && rhs->kind == Expression::DeclRef )
    {
        Declaration* d = rhs->val.value<Declaration*>();
        if( d && d->kind == Declaration::Procedure )
        {
            if( d->outer && d->outer->kind == Declaration::Procedure )
            {
                error(rhs->pos, "cannot take address of nested procedures");
                return false;
            }
            return paramListsMatch(lhsT->subs, lhsT->type(), d->getParams(true), d->type());
        } // else: lhs could be a variable of proc type
    }

    return assigCompat(lhsT, rhsT);
}

bool Validator2::paramCompat(Ast::Type *lhs, Ast::Expression *rhs)
{
    if( arrayCompat(lhs, rhs->type()))
        return true;
    const bool res = assigCompat(lhs, rhs);
    if( !res )
    {
        Type* tf = deref(lhs);
        Type* ta = deref(rhs->type());
        if( tf->kind == Type::Array && tf->expr == 0 && deref(tf->type())->kind == Type::BYTE && lhs->kind == Type::Reference )
        {
            // this is the Oberon VAR ARRAY OF SYSTEM.BYTE trick.
            rhs->varArrOfByte = true;
            return true;
        }
        if( tf->kind == Type::CHAR && rhs->kind == Expression::Literal )
        {
            if( ta->isInteger() && rhs->val.toLongLong() >= 0 && rhs->val.toLongLong() <= 255 )
            {
                rhs->setType(tf);
                return true; // illegal shortcut used in OPL.Mod
            }
        }
        if( tf->kind == Type::CHAR && rhs->kind == Expression::DeclRef )
        {
            Declaration* d = rhs->val.value<Declaration*>();
            if( ta->isInteger() && d->kind == Declaration::ConstDecl && d->expr && d->expr->kind == Expression::Literal &&
                    d->expr->val.toLongLong() >= 0 && d->expr->val.toLongLong() <= 255  )
            {
                rhs->setType(tf);
                return true; // illegal shortcut used in OPL.Mod
            }
        }
        if( lhs->kind == Type::Reference  && tf->kind == Type::Array && deref(tf->type())->kind == Type::CHAR && tf->expr == 0 )
        {
            Type* taa = deref(ta->type());
            Type* taaa = deref(taa->type());
            if( ta->kind == Type::Pointer && taa->kind == Type::Array && taaa->kind == Type::CHAR || ta->kind == Type::NIL )
                return true; // happens in OFSFATVolumes v2.3.7
        }
    }
    return res;
}

bool Validator2::arrayCompat(Ast::Type *lhs, Ast::Type *rhs)
{
    Type* tf = deref(lhs);
    Type* ta = deref(rhs);

    // Tf and Ta are the same type
    if( tf == ta )
        return true;

    // Tf is an open array, Ta is any array, and their element types are array compatible
    if( tf->kind == Type::Array && tf->expr == 0 && ta->kind == Type::Array )
        return arrayCompat(tf->type(), ta->type());

    // Tf is ARRAY OF CHAR and a is a string
    if( tf->kind == Type::Array && deref(tf->type())->kind == Type::CHAR && ta->kind == Type::StrLit)
        return true;

    return false;
}

Type *Validator2::includingType(Ast::Type * lhs, Ast::Type * rhs)
{
    Q_ASSERT( lhs && rhs );
    if( !lhs->isNumber() || !rhs->isNumber() )
        return mdl->getType(Ast::Type::NoType);
    if( lhs->kind == rhs->kind == Type::BYTE )
        return mdl->getType(Type::SHORTINT);
    if( lhs->kind >= rhs->kind )
        return lhs;
    else
        return rhs;
}

bool Validator2::lhsIncludeRhs(Ast::Type *lhs, Ast::Type *rhs)
{
    Q_ASSERT( lhs && rhs );
    if( !lhs->isNumber() || !rhs->isNumber() )
        return false;
    // LONGREAL ⊇ REAL ⊇ HUGEINT ⊇ LONGINT ⊇ INTEGER ⊇ SHORTINT
    return lhs->kind >= rhs->kind;
}

bool Validator2::lhsIsBaseOfRhs(Ast::Type *lhs, Ast::Type *rhs)
{
    if( lhs == 0 || rhs == 0 )
        return false;
    lhs = deref(lhs);
    if( lhs->kind == Type::Pointer )
        lhs = deref(lhs->type());
    if( lhs->kind != Type::Object && lhs->kind != Type::Record )
        return false;
    rhs = deref(rhs);
    QList<Type*> seen;
    while( rhs->kind != Type::NoType )
    {
        seen << rhs;
        if( rhs->kind == Type::Pointer )
            rhs = deref(rhs->type());
        if( rhs == lhs )
            return true;
        rhs = deref(rhs->type());
        if( seen.contains(rhs) )
            return error(rhs->pos, "circular depenceny in base type hierarchy");
    }
    return false;
}

bool Validator2::paramListsMatch(const Ast::DeclList & lhs, Ast::Type * lt, const Ast::DeclList & rhs, Ast::Type * rt)
{
    if( lhs.size() != rhs.size() )
        return false;
    for(int i = 0; i < lhs.size(); i++ )
    {
        Declaration* l = lhs[i];
        Declaration* r = rhs[i];
        const bool varL = deref(l->type(),false)->kind == Type::Reference;
        const bool varR = deref(r->type(),false)->kind == Type::Reference;
        if( varL != varR )
            return false;
        if( !equals(l->type(), r->type()) )
            return false;
    }
    return equals(lt, rt);
}

bool Validator2::equals(Ast::Type * lhs, Ast::Type * rhs)
{
    lhs = deref(lhs);
    rhs = deref(rhs);
    // Type::Reference is transparent for type comparison; it cannot be assessed by its address
    // because it is created ad hoc, implicitly when a VAR parameter is declared
    if( lhs == rhs )
        return true;
    if( lhs->kind == Type::Array && lhs->expr == 0 && rhs->kind == Type::Array && rhs->expr == 0 )
        return equals( lhs->type(), rhs->type() );
    if( lhs->kind == Type::Procedure && rhs->kind == Type::Procedure )
        return paramListsMatch(lhs->subs, lhs->type(), rhs->subs, rhs->type() );
    return false;
}

void Validator2::arrayStats(Ast::Type * t, const RowCol & rc, char what)
{
#if 0
    if( t == 0 )
        return;
    t = deref(t);
    bool ptr = false;
    if( t->kind == Type::Pointer )
    {
        t = deref(t->type());
        ptr = true;
    }
    if( t->kind == Type::Array )
    {
        int dim = 0;
        int open = 0;
        while( t && t->kind == Type::Array )
        {
            dim++;
            if( t->expr == 0 )
                open++;
            t = deref(t->type());
        }
#if 0
        qDebug() << dim << module->name.constData() << rc.d_row << what;
#else
        //if( dim == 2 && (what == 'p' || what == 'v'))
        if( dim >= 2 )
            qDebug() << dim << ptr << module->name.constData() << rc.d_row << what << "open" << open;
#endif
    }
#endif
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

Type *Validator2::deref(Ast::Type *t, bool transparentReference )
{
    // never returns zero
    if( t == 0 )
        return mdl->getType(Type::NoType);
    if( t->kind == Type::NameRef || (transparentReference && t->kind == Type::Reference) )
    {
        if( !t->validated )
        {
            // This is necessary because declarations can follow their use, but all so far unvalidated NameRefs
            // must be local, assuming that all NameRefs in imported modules must already have been validated at
            // this point.
#if 0
            Quali q = *t->quali;
            if( !q.first.isEmpty() && q.first.constData() != module->name.constData() )
                qWarning() << "Validator::deref: unvalidated quali" << q.first << "." << q.second << "in"
                           << module->name; // happens 21 times in bluebottle
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
    ResolvedQ r = find(q, nameRef->pos);
    if(r.second == 0)
        return;
    RowCol pos = nameRef->pos;
    if( r.first != 0 )
    {
        markRef(r.first, pos);
        pos.d_col += q.first.size() + 1;
    }else
        nameRef->forward = true;
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

        if( d == 0 && curObj )
            d = findInType(curObj,q.second);
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
}

