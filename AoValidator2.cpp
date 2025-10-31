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

void Validator2::validate(Ast::Declaration* module) {
	errors.clear();
    sourcePath = module->data.toString();
    Module(module);
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
    if( d->body ) {
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
    if( ConstExpr(d->expr) )
    {

    }
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
        Type(d->type());
        d = d->next;
    }
    d = DeclSeq(d);
    if( proc->body ) {
        if( proc->body->kind == Ast::Statement::Assembler )
            Assembler(proc);
        else
            Body(proc->body);
    } else
        invalid("ProcDecl", proc->pos);
}

void Validator2::SysFlag() {
    // TODO
}

bool Validator2::ArrayType(Ast::Type* t) {
    // TODO
    // SysFlag();
    if( !ConstExpr(t->expr) )
        return false;
    return Type(t->type());
}

bool Validator2::RecordType(Ast::Type* t) {
    // TODO
    // SysFlag();
    if( !Type(t->type()) )  // base class
        return false;
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
    if( !Type(t->type()) )  // base class
        return false;

    foreach( Ast::Declaration* member, t->subs )
    {
        switch( member->kind )
        {
        case Ast::Declaration::VarDecl:
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
    if( s ) {
        StatSeq(s);
	}
}

void Validator2::StatSeq(Ast::Statement*s) {
    s = Statement(s);
    while( s ) {
        s = Statement(s);
	}
}

void Validator2::assig(Ast::Statement* s) {
    // TODO
    // Designator();
    Expr(s->rhs);
}

void Validator2::IfStat() {
    // TODO
    // case Ast::Statement::Elsif:
    // case Ast::Statement::Else:
    Expr(0);
    StatSeq(0);
    //while( la.d_type == Tok_ELSIF ) {
        Expr(0);
        // expect(Tok_THEN, false, "IfStat");
        StatSeq(0);
    //}
    //if( la.d_type == Tok_ELSE ) {
        //expect(Tok_ELSE, false, "IfStat");
        StatSeq(0);
    //}
}

void Validator2::CaseStat() {
    // TODO
    //case Ast::Statement::CaseLabel:

    Expr(0);
    /*
	if( la.d_type == Tok_DO ) {
		expect(Tok_DO, false, "CaseStat");
	} else if( la.d_type == Tok_OF ) {
		expect(Tok_OF, false, "CaseStat");
	} else
		invalid("CaseStat");
        */
	Case();
    //while( la.d_type == Tok_Bar ) {
		Case();
    //}
    //if( la.d_type == Tok_ELSE ) {
        StatSeq(0);
    //}
}

void Validator2::WhileStat() {
    // TODO
    Expr(0);
    //expect(Tok_DO, false, "WhileStat");
    StatSeq(0);
}

void Validator2::RepeatStat() {
    // TODO
    StatSeq(0);
    //expect(Tok_UNTIL, false, "RepeatStat");
    Expr(0);
}

void Validator2::ForStat() {
    // TODO
    Expr(0);
    //expect(Tok_TO, false, "ForStat");
    Expr(0);
    //if( la.d_type == Tok_BY ) {
        ConstExpr(0);
    //}
    //expect(Tok_DO, false, "ForStat");
    StatSeq(0);
}

void Validator2::LoopStat() {
    StatSeq(0);
}

void Validator2::WithStat() {
	Qualident();
    //expect(Tok_Colon, false, "WithStat");
	Qualident();
    //expect(Tok_DO, false, "WithStat");
    StatSeq(0);
}

void Validator2::ReturnStat(Ast::Statement* s) {
    // TODO
    if( s->rhs ) {
        Expr(s->rhs);
	}
}

Ast::Statement* Validator2::Statement(Ast::Statement* s) {
    switch( s->kind )
    {
    case Ast::Statement::Assig:
        assig(s);
        break;
    case Ast::Statement::Call:
        call(s);
        break;
    case Ast::Statement::If:
        IfStat();
        break;
    case Ast::Statement::Case:
        CaseStat();
        break;
    case Ast::Statement::With:
        WithStat();
        break;
    case Ast::Statement::Loop:
        LoopStat();
        break;
    case Ast::Statement::While:
        WhileStat();
        break;
    case Ast::Statement::Repeat:
        RepeatStat();
        break;
    case Ast::Statement::Exit:
        break;
    case Ast::Statement::Return:
        ReturnStat(s);
        break;
    case Ast::Statement::ForAssig:
        ForStat();
        break;
    case Ast::Statement::ForToBy:
        break;
    case Ast::Statement::End:
        break;
    default:
        invalid("Statement", s->pos);
    }
    return s->getNext();
}

void Validator2::Case() {
    // TODO
		CaseLabels();
        //while( la.d_type == Tok_Comma ) {
			CaseLabels();
        //}
        //expect(Tok_Colon, false, "Case");
        StatSeq(0);
}

void Validator2::CaseLabels() {
    // TODO
    ConstExpr(0);
    //if( la.d_type == Tok_2Dot ) {
        ConstExpr(0);
    //}
}

bool Validator2::ConstExpr(Ast::Expression* e) {
    // TODO
    return Expr(e);
}

bool Validator2::Expr(Ast::Expression* e) {
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

void Validator2::Qualident() {
    /*
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {
		expect(Tok_ident, false, "Qualident");
		expect(Tok_Dot, false, "Qualident");
		expect(Tok_ident, false, "Qualident");
	} else if( la.d_type == Tok_ident ) {
		expect(Tok_ident, false, "Qualident");
	} else
		invalid("Qualident");
        */
}

bool Validator2::relation(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::unaryOp(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::arithOp(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::logicOp(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::declRef(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::select(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::index(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::deref(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::cast(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::call(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::literal(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::constructor(Ast::Expression *e)
{
    // TODO
    return false;
}

bool Validator2::nameRef(Ast::Expression *e)
{
    // TODO
    return false;
}

void Validator2::call(Ast::Statement *s)
{
    // TODO
}


