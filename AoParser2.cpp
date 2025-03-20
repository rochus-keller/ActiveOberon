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

#include "AoParser2.h"
#include "AoAst.h"
#include <QtDebug>
#include <limits>
using namespace Ao;
using namespace Ast;


static inline bool FIRST_ImportList(int tt) {
	return tt == Tok_IMPORT;
}

static inline bool FIRST_DeclSeq(int tt) {
	return tt == Tok_VAR || tt == Tok_CONST || tt == Tok_PROCEDURE || tt == Tok_TYPE;
}

static inline bool FIRST_ConstDecl(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_TypeDecl(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_VarDecl(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_Assembler(int tt) {
	return tt == Tok_CODE;
}

static inline bool FIRST_ProcDecl(int tt) {
	return tt == Tok_PROCEDURE;
}

static inline bool FIRST_ProcHead(int tt) {
	return tt == Tok_Amp || tt == Tok_Minus || tt == Tok_Star || tt == Tok_Lbrack || tt == Tok_ident;
}

static inline bool FIRST_SysFlag(int tt) {
	return tt == Tok_Lbrack;
}

static inline bool FIRST_FormalPars(int tt) {
	return tt == Tok_Lpar;
}

static inline bool FIRST_FPSection(int tt) {
	return tt == Tok_VAR || tt == Tok_ident;
}

static inline bool FIRST_ArrayType(int tt) {
	return tt == Tok_ARRAY;
}

static inline bool FIRST_RecordType(int tt) {
	return tt == Tok_RECORD;
}

static inline bool FIRST_PointerType(int tt) {
	return tt == Tok_POINTER;
}

static inline bool FIRST_ObjectType(int tt) {
	return tt == Tok_OBJECT;
}

static inline bool FIRST_ProcedureType(int tt) {
	return tt == Tok_PROCEDURE;
}

static inline bool FIRST_AliasType(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_Type(int tt) {
	switch(tt){
	case Tok_RECORD:
	case Tok_OBJECT:
	case Tok_ARRAY:
	case Tok_PROCEDURE:
	case Tok_POINTER:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_FieldDecl(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_FieldList(int tt) {
	return tt == Tok_Semi || tt == Tok_ident;
}

static inline bool FIRST_Body(int tt) {
	return tt == Tok_BEGIN;
}

static inline bool FIRST_Attributes(int tt) {
	return tt == Tok_Lbrace;
}

static inline bool FIRST_StatBlock(int tt) {
	return tt == Tok_BEGIN;
}

static inline bool FIRST_StatSeq(int tt) {
	switch(tt){
	case Tok_CASE:
	case Tok_FOR:
	case Tok_EXIT:
	case Tok_IF:
	case Tok_LOOP:
	case Tok_REPEAT:
	case Tok_WITH:
	case Tok_WHILE:
	case Tok_BEGIN:
	case Tok_RETURN:
	case Tok_ident:
	case Tok_Semi:
		return true;
	default: return false;
	}
}

static inline bool FIRST_AssigOrCall(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_IfStat(int tt) {
	return tt == Tok_IF;
}

static inline bool FIRST_CaseStat(int tt) {
	return tt == Tok_CASE;
}

static inline bool FIRST_WhileStat(int tt) {
	return tt == Tok_WHILE;
}

static inline bool FIRST_RepeatStat(int tt) {
	return tt == Tok_REPEAT;
}

static inline bool FIRST_ForStat(int tt) {
	return tt == Tok_FOR;
}

static inline bool FIRST_LoopStat(int tt) {
	return tt == Tok_LOOP;
}

static inline bool FIRST_WithStat(int tt) {
	return tt == Tok_WITH;
}

static inline bool FIRST_ReturnStat(int tt) {
	return tt == Tok_RETURN;
}

static inline bool FIRST_Statement(int tt) {
	switch(tt){
	case Tok_CASE:
	case Tok_FOR:
	case Tok_EXIT:
	case Tok_IF:
	case Tok_LOOP:
	case Tok_REPEAT:
	case Tok_WITH:
	case Tok_WHILE:
	case Tok_RETURN:
	case Tok_BEGIN:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Case(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_CaseLabels(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_ConstExpr(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Expr(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_SimpleExpr(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Term(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Factor(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Tilde:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Set(int tt) {
	return tt == Tok_Lbrace;
}

static inline bool FIRST_Element(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_Relation(int tt) {
	switch(tt){
	case Tok_Eq:
	case Tok_Lt:
	case Tok_IN:
	case Tok_Leq:
	case Tok_IS:
	case Tok_Geq:
	case Tok_Hash:
	case Tok_Gt:
		return true;
	default: return false;
	}
}

static inline bool FIRST_MulOp(int tt) {
	return tt == Tok_DIV || tt == Tok_Amp || tt == Tok_Star || tt == Tok_MOD || tt == Tok_Slash;
}

static inline bool FIRST_AddOp(int tt) {
	return tt == Tok_Plus || tt == Tok_Minus || tt == Tok_OR;
}

static inline bool FIRST_Designator(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_Selector(int tt) {
	return tt == Tok_Dot || tt == Tok_Lpar || tt == Tok_Lbrack || tt == Tok_Hat;
}

static inline bool FIRST_ExprList(int tt) {
	switch(tt){
	case Tok_NIL:
	case Tok_Plus:
	case Tok_Tilde:
	case Tok_Minus:
	case Tok_string:
	case Tok_Lpar:
	case Tok_hexchar:
	case Tok_integer:
	case Tok_real:
	case Tok_Lbrace:
	case Tok_ident:
		return true;
	default: return false;
	}
}

static inline bool FIRST_IdentList(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_Qualident(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_IdentDef(int tt) {
	return tt == Tok_ident;
}

static inline bool FIRST_number(int tt) {
	return tt == Tok_integer || tt == Tok_real;
}

Parser2::Parser2(AstModel* m, Scanner2* s):scanner(s),mdl(m),thisMod(0)
{
    predefSymbols.resize(MaxAttr);
    predefSymbols[UNTRACED] = Token::getSymbol("UNTRACED");
    predefSymbols[ACTIVE] = Token::getSymbol("ACTIVE");
    predefSymbols[DELEGATE] = Token::getSymbol("DELEGATE");
    predefSymbols[EXCLUSIVE] = Token::getSymbol("EXCLUSIVE");
    predefSymbols[PRIORITY] = Token::getSymbol("PRIORITY");
    predefSymbols[SAFE] = Token::getSymbol("SAFE");
    BEGIN = Token::getSymbol("BEGIN");
}

Parser2::~Parser2()
{
    if( thisMod )
        Declaration::deleteAll(thisMod);
}

void Parser2::RunParser() {
	errors.clear();
	next();
	Module();
}

Declaration* Parser2::takeResult()
{
    Declaration* res = thisMod;
    thisMod = 0;
    return res;
}

void Parser2::next() {
	cur = la;
	la = scanner->next();
	while( la.d_type == Tok_Invalid ) {
        errors << Error(la.d_val, la.toRowCol(), la.d_sourcePath);
		la = scanner->next();
	}
}

Token Parser2::peek(int off) {
	if( off == 1 )
		return la;
	else if( off == 0 )
		return cur;
	else
		return scanner->peek(off-1);
}

void Parser2::invalid(const char* what) {
    errors << Error(QString("invalid %1").arg(what),la.toRowCol(), la.d_sourcePath);
}

bool Parser2::expect(int tt, bool pkw, const char* where) {
    if( la.d_type == tt) {
        next();
        return true;
    }
    else {
        errors << Error(QString("'%1' expected in %2")
                        .arg(tokenTypeString(tt))
                        .arg(where),la.toRowCol(), la.d_sourcePath);
        return false;
    }
}

static inline void dummy() {}

void Parser2::Module() {
    if( la.d_type != Tok_MODULE )
    {
        la.d_sourcePath = scanner->source();
        la.d_lineNr = 1;
        error(la,"not a Luon module");
        return;
    }
    Declaration* m = new Declaration();
    m->kind = Declaration::Module;
    if( thisMod )
        Declaration::deleteAll(thisMod);
    thisMod = m;

    mdl->openScope(m);

    expect(Tok_MODULE, false, "Module");
	expect(Tok_ident, false, "Module");
    m->name = cur.d_val;
    m->pos = cur.toRowCol();
    ModuleData md;
    md.sourcePath = scanner->source();
    md.fullName = cur.d_val;
    expect(Tok_Semi, false, "Module");
	if( FIRST_ImportList(la.d_type) ) {
		ImportList();
	}
	DeclSeq();
	if( FIRST_Body(la.d_type) ) {
        la.d_val = BEGIN;
        Declaration* procDecl = addDecl(la, Declaration::Private, Declaration::Procedure);
        if( procDecl == 0 )
            return;
        procDecl->begin = 1;
        mdl->openScope(procDecl);
        procDecl->body = Body();
        mdl->closeScope();
    }
	expect(Tok_END, false, "Module");
	expect(Tok_ident, false, "Module");
    md.end = cur.toRowCol();
    m->data = QVariant::fromValue(md);
    mdl->closeScope();
    expect(Tok_Dot, false, "Module");
}

void Parser2::ImportDecl() {
    Token localName;
    if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_ColonEq )  ) {
        expect(Tok_ident, false, "ImportDecl");
        localName = cur;
        expect(Tok_ColonEq, false, "ImportDecl");
    }
    expect(Tok_ident, false, "ImportDecl");
    Token module = cur;
    if( localName.d_type == 0 )
        localName = module;
    Declaration* importDecl = addDecl(localName, 0, Declaration::Import);
    if( importDecl == 0 )
        return;

    Import import;
    import.moduleName = module.d_val;
    import.importedAt = module.toRowCol();
    import.importer = thisMod;
    importDecl->data = QVariant::fromValue(import);
}

void Parser2::ImportList() {
	expect(Tok_IMPORT, false, "ImportList");
	ImportDecl();
	while( la.d_type == Tok_Comma ) {
		expect(Tok_Comma, false, "ImportList");
		ImportDecl();
	}
	expect(Tok_Semi, false, "ImportList");
    // done
}

void Parser2::DeclSeq(bool inObjectType) {
	while( la.d_type == Tok_CONST || la.d_type == Tok_TYPE || la.d_type == Tok_VAR || FIRST_ProcDecl(la.d_type) ) {
        if( !inObjectType && la.d_type == Tok_CONST ) {
			expect(Tok_CONST, false, "DeclSeq");
			while( FIRST_ConstDecl(la.d_type) ) {
				ConstDecl();
				expect(Tok_Semi, false, "DeclSeq");
			}
        } else if( !inObjectType && la.d_type == Tok_TYPE ) {
			expect(Tok_TYPE, false, "DeclSeq");
			while( FIRST_TypeDecl(la.d_type) ) {
				TypeDecl();
				expect(Tok_Semi, false, "DeclSeq");
			}
		} else if( la.d_type == Tok_VAR ) {
			expect(Tok_VAR, false, "DeclSeq");
			while( FIRST_VarDecl(la.d_type) ) {
                VarDecl(inObjectType);
				expect(Tok_Semi, false, "DeclSeq");
			}
		} else if( FIRST_ProcDecl(la.d_type) || la.d_type == Tok_END || la.d_type == Tok_CONST || la.d_type == Tok_END || la.d_type == Tok_VAR || la.d_type == Tok_TYPE || la.d_type == Tok_CODE || la.d_type == Tok_BEGIN || la.d_type == Tok_PROCEDURE || la.d_type == Tok_END ) {
			while( FIRST_ProcDecl(la.d_type) ) {
				ProcDecl();
				expect(Tok_Semi, false, "DeclSeq");
			}
		} else
			invalid("DeclSeq");
	}
    // done
}

void Parser2::ConstDecl() {
    ID id = IdentDef();
	expect(Tok_Eq, false, "ConstDecl");
    Declaration* d = addDecl(id.name, id.visi, Declaration::ConstDecl);
    if( d == 0 )
        return;
    d->expr = ConstExpr();
}

void Parser2::TypeDecl() {
    const ID id = IdentDef();
	expect(Tok_Eq, false, "TypeDecl");

    // declare the type immediately so it is known in the forthcoming declaration
    Declaration* d = addDecl(id.name, id.visi, Declaration::TypeDecl);
    if( d == 0 )
        return;

    d->type = Type_(false);
    if( d->type && d->type->decl == 0 )
    {
        d->type->decl = d;
        d->ownstype = true;
    }
}

void Parser2::VarDecl(bool inObjectType) {
    const QList<ID> ids = IdentList();
	expect(Tok_Colon, false, "VarDecl");
    Ast::Type* t = Type_();
    if( t == 0 )
        return;
    Declaration* outer = mdl->getTopScope();
    foreach( const ID& id, ids )
    {
        Declaration* d = addDecl(id.name, id.visi, outer->kind == Declaration::Module ?
                                     Declaration::VarDecl : Declaration::LocalDecl);
        if( d == 0 )
            continue;
        if( inObjectType )
            d->kind = Declaration::Field;
        else
            d->outer = outer;
        d->type = t;
    }
}

QByteArray Parser2::Assembler() {
	expect(Tok_CODE, false, "Assembler");
    return cur.d_val;
}

void Parser2::ProcDecl() {
	expect(Tok_PROCEDURE, false, "ProcDecl");
	if( FIRST_ProcHead(la.d_type) ) {
        Declaration* procDecl = ProcHead(false);
		expect(Tok_Semi, false, "ProcDecl");
        mdl->openScope(procDecl);
        DeclSeq();
		if( FIRST_Body(la.d_type) || FIRST_Assembler(la.d_type) ) {
			if( FIRST_Body(la.d_type) ) {
                procDecl->body = Body();
			} else if( FIRST_Assembler(la.d_type) ) {
                procDecl->data = Assembler();
                procDecl->body = new Ast::Statement(Ast::Statement::Assembler);
			} else
				invalid("ProcDecl");
		}
		expect(Tok_END, false, "ProcDecl");
		expect(Tok_ident, false, "ProcDecl");
        mdl->closeScope();
    } else if( la.d_type == Tok_Hat ) {
		expect(Tok_Hat, false, "ProcDecl");
        ProcHead(true);
    } else
		invalid("ProcDecl");
}

Declaration* Parser2::ProcHead(bool forwardDecl) {
	if( FIRST_SysFlag(la.d_type) ) {
        SysFlag(); // ignore
	}
    bool isConstr = false;
	if( la.d_type == Tok_Star || la.d_type == Tok_Amp || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Star ) {
			expect(Tok_Star, false, "ProcHead");
		} else if( la.d_type == Tok_Amp ) {
			expect(Tok_Amp, false, "ProcHead");
            isConstr = true;
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "ProcHead");
		} else
			invalid("ProcHead");
	}
    ID id = IdentDef();
    if( forwardDecl )
    {
        // we just ignore forward declarations
        if( FIRST_FormalPars(la.d_type) ) {
            mdl->openScope(0);
            FormalPars();
            Declaration* d = mdl->closeScope(true);
            Declaration::deleteAll(d);
        }
        return 0;
    }
    Declaration* procDecl = addDecl(id.name, id.visi, Declaration::Procedure);
    if( procDecl == 0 )
        return 0;
    procDecl->constructor = isConstr;

    procDecl->outer = mdl->getTopScope();
    mdl->openScope(procDecl);

	if( FIRST_FormalPars(la.d_type) ) {
        procDecl->type = FormalPars();
	}
    mdl->closeScope();
    return procDecl;
}

bool Parser2::SysFlag() {
	expect(Tok_Lbrack, false, "SysFlag");
	expect(Tok_ident, false, "SysFlag");
    const bool res = cur.d_val.constData() == predefSymbols[UNTRACED].constData();
    if( !res )
        error(cur, QString("unknown system flag '%1'").arg(cur.d_val.constData()));
	expect(Tok_Rbrack, false, "SysFlag");
    return res;
}

Ast::Type* Parser2::FormalPars() {
	expect(Tok_Lpar, false, "FormalPars");
	if( FIRST_FPSection(la.d_type) ) {
		FPSection();
		while( la.d_type == Tok_Semi ) {
			expect(Tok_Semi, false, "FormalPars");
			FPSection();
		}
	}
	expect(Tok_Rpar, false, "FormalPars");
    Type* res = 0;
    if( la.d_type == Tok_Colon ) {
		expect(Tok_Colon, false, "FormalPars");
        res = NamedType();
	}
    if(res == 0)
        res = mdl->getType(Type::NoType);
    return res;
}

void Parser2::FPSection() {
    bool varParam = false;
    if( la.d_type == Tok_VAR ) {
		expect(Tok_VAR, false, "FPSection");
        varParam = true;
	}
	expect(Tok_ident, false, "FPSection");
    TokenList l;
    l << cur;
    while( la.d_type == Tok_Comma ) {
		expect(Tok_Comma, false, "FPSection");
		expect(Tok_ident, false, "FPSection");
        l << cur;
    }
	expect(Tok_Colon, false, "FPSection");
    Type* t = Type_();
    for(int i = 0; i < l.size(); i++ )
    {
        Declaration* d = addDecl(l[i], 0, Declaration::ParamDecl);
        if( d == 0 )
            continue;
        d->varParam = varParam;
        d->type = t;
    }
}

Type* Parser2::ArrayType() {
    QList<Expression*> len;
    expect(Tok_ARRAY, false, "ArrayType");
	if( FIRST_SysFlag(la.d_type) ) {
        SysFlag(); // ignore
	}
	if( FIRST_ConstExpr(la.d_type) ) {
        Expression* e = ConstExpr();
        if( e == 0 )
            return 0;
        len << e;
		while( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "ArrayType");
            e = ConstExpr();
            if( e == 0 )
                return 0;
            len << e;
		}
	}
	expect(Tok_OF, false, "ArrayType");
    Type* et = Type_();
    Type* arr = new Type();
    arr->kind = Type::Array;
    arr->base = et;
    if( !len.isEmpty() )
    {
        Type* a = arr;
        a->expr = len.first();
        for( int i = 1; i < len.size(); i++ )
        {
            a->base = new Type();
            a->base->kind = Type::Array;
            a->base->base = et;
            a->base->expr = len[i];
            a = a->base;
        }
    }
    return arr;
}

Type* Parser2::RecordType() {
	expect(Tok_RECORD, false, "RecordType");
    Type* rec = new Type();
    rec->kind = Type::Record;
    mdl->openScope(0);
    if( FIRST_SysFlag(la.d_type) ) {
        SysFlag(); // ignored
	}
	if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "RecordType");
        rec->base = NamedType();
		expect(Tok_Rpar, false, "RecordType");
	}
	if( FIRST_FieldList(la.d_type) ) {
		FieldList();
	}
    rec->subs = AstModel::toList(mdl->closeScope(true));
    Declaration* outer = mdl->getTopScope();
    for( int i = 0; i < rec->subs.size(); i++ )
        rec->subs[i]->outer = outer;
    expect(Tok_END, false, "RecordType");
    return rec;
}

Type* Parser2::PointerType() {
	expect(Tok_POINTER, false, "PointerType");
	if( FIRST_SysFlag(la.d_type) ) {
        SysFlag(); // ignore
	}
    Type* ptr = new Type();
    ptr->kind = Type::Pointer;
    expect(Tok_TO, false, "PointerType");
    ptr->base = Type_();
    return ptr;
}

Type* Parser2::ObjectType() {
	expect(Tok_OBJECT, false, "ObjectType");
    Type* obj = new Type();
    obj->kind = Type::Object;
    if( FIRST_SysFlag(la.d_type) || la.d_type == Tok_Lpar || FIRST_DeclSeq(la.d_type) || FIRST_Body(la.d_type) || la.d_type == Tok_END ) {
		if( FIRST_SysFlag(la.d_type) ) {
            SysFlag(); // ignored
		}
        mdl->openScope(0);
        if( la.d_type == Tok_Lpar ) {
			expect(Tok_Lpar, false, "ObjectType");
            obj->base = NamedType();
            expect(Tok_Rpar, false, "ObjectType");
		}
        DeclSeq(true);

        if( FIRST_Body(la.d_type) ) {
            la.d_val = BEGIN;
            Declaration* procDecl = addDecl(la, Declaration::Private, Declaration::Procedure);
            Q_ASSERT(procDecl);
            procDecl->begin = 1;
            mdl->openScope(procDecl);
            procDecl->body = Body();
            mdl->closeScope();
        }

        obj->subs = AstModel::toList(mdl->closeScope(true));
        expect(Tok_END, false, "ObjectType");
		if( la.d_type == Tok_ident ) {
			expect(Tok_ident, false, "ObjectType");
		}
	}
    return obj;
}

Type* Parser2::ProcedureType() {
	expect(Tok_PROCEDURE, false, "ProcedureType");
    mdl->openScope(0);
    Type* p = new Type();
    p->kind = Type::Procedure;
    if( FIRST_SysFlag(la.d_type) ) {
		SysFlag();
	}
	if( FIRST_Attributes(la.d_type) ) {
        std::bitset<MaxAttr> a = Attributes();
        if( a.test(DELEGATE) )
            p->delegate = true;
	}
    Type* ret = mdl->getType(Type::NoType);
    if( FIRST_FormalPars(la.d_type) ) {
        ret = FormalPars();
	}
    p->subs = AstModel::toList(mdl->closeScope(true));
    p->base = ret;
    return p;
}

Type* Parser2::AliasType() {
    return NamedType();
}

Type* Parser2::Type_(bool needsHelperDecl) {
    Type* res = 0;
    if( FIRST_AliasType(la.d_type) ) {
        res = AliasType();
	} else if( FIRST_ArrayType(la.d_type) ) {
        res = ArrayType();
	} else if( FIRST_RecordType(la.d_type) ) {
        res = RecordType();
	} else if( FIRST_PointerType(la.d_type) ) {
        res = PointerType();
	} else if( FIRST_ObjectType(la.d_type) ) {
        res = ObjectType();
	} else if( FIRST_ProcedureType(la.d_type) ) {
        res = ProcedureType();
	} else
		invalid("Type");
    if( res && res->kind != Type::Undefined // only valid types are considered
            && res->kind != Type::NameRef // NameRef already has a helper decl
            && needsHelperDecl )
        addHelper(res);
        // if the type is not directly owned by a declaration (because it is declared in place and anonymously)
        // we need a helper declaration with a artificial ident so we can refer to it by name later.
    return res;
}

void Parser2::FieldDecl() {
	if( FIRST_IdentList(la.d_type) ) {
        QList<ID> ids = IdentList();
		expect(Tok_Colon, false, "FieldDecl");
        Type* t = Type_();
        for( int i = 0; i < ids.size(); i++ )
        {
            Declaration* d = addDecl(ids[i].name,ids[i].visi,Declaration::Field);
            if( d == 0 )
                continue;
            d->type = t;
        }
    }
}

void Parser2::FieldList() {
	FieldDecl();
	while( la.d_type == Tok_Semi ) {
		expect(Tok_Semi, false, "FieldList");
		FieldDecl();
	}
    // done
}

Statement* Parser2::Body() {
    return StatBlock();
}

std::bitset<Parser2::MaxAttr> Parser2::Attributes() {
    std::bitset<MaxAttr> res;
	expect(Tok_Lbrace, false, "Attributes");
    if( la.d_type == Tok_ident ) {
        int a = Attribute();
        if( a > 0 )
            res.set(a);
        while( la.d_type == Tok_Comma ) {
            expect(Tok_Comma, false, "Attributes");
            a = Attribute();
            if( a > 0 )
                res.set(a);
        }
    }
    expect(Tok_Rbrace, false, "Attributes");
    return res;
}

int Parser2::Attribute() {
    expect(Tok_ident, false, "Attribute");
    int res = -1;
    for( int i = 1; i < MaxAttr; i++ )
    {
        if( predefSymbols[i].constData() == cur.d_val.constData() )
        {
            res = i;
            break;
        }
    }
    if( la.d_type == Tok_Lpar ) {
        expect(Tok_Lpar, false, "Attribute");
        ConstExpr(); // ignore
        expect(Tok_Rpar, false, "Attribute");
    }
    return res;
}

Statement* Parser2::StatBlock() {
	expect(Tok_BEGIN, false, "StatBlock");
    Statement* s = new Statement(Statement::StatBlock, cur.toRowCol());
    if( FIRST_Attributes(la.d_type) ) {
        std::bitset<MaxAttr> a = Attributes();
        s->active = a.test(ACTIVE);
        s->exclusive = a.test(EXCLUSIVE);
	}
	if( FIRST_StatSeq(la.d_type) ) {
        s->body = StatSeq();
	}
    return s;
}

Statement* Parser2::StatSeq() {
    Statement* res = Statement_();
    while( la.d_type == Tok_Semi ) {
        expect(Tok_Semi, false, "StatSeq");
        Statement* s = Statement_();
        if( s == 0 )
            continue;
        if( res )
            res->append(s);
        else
            res = s;
    }
    if( res && la.d_type == Tok_END )
    {
        Statement* end = new Statement(Statement::End, la.toRowCol());
        res->append(end);
    }
    return res;
}

Statement* Parser2::AssigOrCall() {
    Token t = la;
    Expression* lhs = Designator(true);
    if( lhs == 0 )
        return 0;
    if( la.d_type == Tok_ColonEq ) {
		expect(Tok_ColonEq, false, "AssigOrCall");
        t = cur;
        Expression* rhs = Expr();
        if( rhs == 0 )
        {
            delete lhs;
            return 0;
        }
        Statement* stat = new Statement(Statement::Assig, t.toRowCol());
        stat->lhs = lhs;
        stat->rhs = rhs;
        return stat;
    }else
    {
        Statement* stat = new Statement(Statement::Call, t.toRowCol());
        stat->lhs = lhs;
        return stat;
    }
}

Statement* Parser2::IfStat() {
	expect(Tok_IF, false, "IfStat");
    Statement* first = new Statement(Statement::If,cur.toRowCol());
    first->rhs = Expr();
    if( first->rhs == 0 )
    {
        Statement::deleteAll(first);
        return 0;
    }
	expect(Tok_THEN, false, "IfStat");
    first->body = StatSeq();
    Statement* last = first;
    while( la.d_type == Tok_ELSIF ) {
		expect(Tok_ELSIF, false, "IfStat");
        Statement* stat = new Statement(Statement::Elsif,cur.toRowCol());
        stat->rhs = Expr();
        if( stat->rhs == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        expect(Tok_THEN, false, "IfStat");
        stat->body = StatSeq();
        last->append(stat);
        last = stat;
    }
	if( la.d_type == Tok_ELSE ) {
		expect(Tok_ELSE, false, "IfStat");
        Statement* stat = StatSeq();
        if( stat )
            last->append(stat);
    }
	expect(Tok_END, false, "IfStat");
    return first;
}

Statement* Parser2::CaseStat() {
	expect(Tok_CASE, false, "CaseStat");
    Statement* first = new Statement(Statement::Case, cur.toRowCol());
    first->rhs = Expr();
    if( first->rhs == 0 )
    {
        Statement::deleteAll(first);
        return 0;
    }
    if( la.d_type == Tok_DO ) {
		expect(Tok_DO, false, "CaseStat");
	} else if( la.d_type == Tok_OF ) {
		expect(Tok_OF, false, "CaseStat");
	} else
		invalid("CaseStat");
    Statement* last = first;
    if( FIRST_CaseLabels(la.d_type) ) {
        Statement* stat = Case();
        if( stat == 0 )
        {
            Statement::deleteAll(first);
            return 0;
        }
        last->append(stat);
        last = stat;
    }
    while( la.d_type == Tok_Bar ) {
        expect(Tok_Bar, false, "CaseStat");
        if( FIRST_CaseLabels(la.d_type) ) {
            Statement* stat = Case();
            if( stat == 0 )
            {
                Statement::deleteAll(first);
                return 0;
            }
            last->append(stat);
            last = stat;
        }
    }
	if( la.d_type == Tok_ELSE ) {
		expect(Tok_ELSE, false, "CaseStat");
        Statement* stat = new Statement(Statement::Else, cur.toRowCol());
        last->append(stat);
        last = stat;
        stat->body = StatSeq();
    }
	expect(Tok_END, false, "CaseStat");
    return first;
}

Statement* Parser2::WhileStat() {
	expect(Tok_WHILE, false, "WhileStat");
    Statement* res = new Statement(Statement::While, cur.toRowCol());
    res->rhs = Expr();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    expect(Tok_DO, false, "WhileStat");
    res->body = StatSeq();
	expect(Tok_END, false, "WhileStat");
    return res;
}

Statement* Parser2::RepeatStat() {
	expect(Tok_REPEAT, false, "RepeatStat");
    Statement* res = new Statement(Statement::Repeat, cur.toRowCol());
    res->body = StatSeq();
    expect(Tok_UNTIL, false, "RepeatStat");
    res->rhs = Expr();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    return res;
}

Statement* Parser2::ForStat() {
	expect(Tok_FOR, false, "ForStat");
    Statement* res = new Statement(Statement::ForAssig, cur.toRowCol());
    expect(Tok_ident, false, "ForStat");
    res->lhs = new Expression(Expression::NameRef, cur.toRowCol() );
    res->lhs->val = QVariant::fromValue(Qualident(QByteArray(),cur.d_val));
    expect(Tok_ColonEq, false, "ForStat");
    res->rhs = Expr();
    if(res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    expect(Tok_TO, false, "ForStat");
    Statement* forby = new Statement(Statement::ForToBy, cur.toRowCol());
    res->append(forby);
    forby->lhs = Expr(); // to
    if(forby->lhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    if( la.d_type == Tok_BY ) {
		expect(Tok_BY, false, "ForStat");
        forby->rhs = ConstExpr(); // by
    }
	expect(Tok_DO, false, "ForStat");
    res->body = StatSeq();
    expect(Tok_END, false, "ForStat");
    return res;
}

Statement* Parser2::LoopStat() {
	expect(Tok_LOOP, false, "LoopStat");
    Statement* res = new Statement(Statement::Loop, cur.toRowCol());
    res->body = StatSeq();
    expect(Tok_END, false, "LoopStat");
    return res;
}

Statement* Parser2::WithStat() {
	expect(Tok_WITH, false, "WithStat");
    Statement* res = new Statement(Statement::With, cur.toRowCol());

    Token t = la;
    Qualident q = Qualident_();
    res->lhs = new Expression(Expression::NameRef, t.toRowCol());
    res->lhs->val = QVariant::fromValue(q);

    expect(Tok_Colon, false, "WithStat");
    res->rhs = new Expression(); // dummy to hold type
    res->rhs->kind = Expression::Literal;
    res->rhs->type = NamedType();

	expect(Tok_DO, false, "WithStat");
    res->body = StatSeq();
	expect(Tok_END, false, "WithStat");
    return res;
}

Statement* Parser2::ReturnStat() {
	expect(Tok_RETURN, false, "ReturnStat");
    Statement* res = new Statement(Statement::Return, cur.toRowCol());
    if( FIRST_Expr(la.d_type) ) {
        res->rhs = Expr();
    }
    return res;
}

Statement* Parser2::Statement_() {
    Statement* s = 0;
	if( FIRST_AssigOrCall(la.d_type) || FIRST_IfStat(la.d_type) || FIRST_CaseStat(la.d_type) || FIRST_WhileStat(la.d_type) || FIRST_RepeatStat(la.d_type) || FIRST_ForStat(la.d_type) || FIRST_LoopStat(la.d_type) || FIRST_WithStat(la.d_type) || la.d_type == Tok_EXIT || FIRST_ReturnStat(la.d_type) || FIRST_StatBlock(la.d_type) ) {
		if( FIRST_AssigOrCall(la.d_type) ) {
            s = AssigOrCall();
		} else if( FIRST_IfStat(la.d_type) ) {
            s = IfStat();
		} else if( FIRST_CaseStat(la.d_type) ) {
            s = CaseStat();
		} else if( FIRST_WhileStat(la.d_type) ) {
            s = WhileStat();
		} else if( FIRST_RepeatStat(la.d_type) ) {
            s = RepeatStat();
		} else if( FIRST_ForStat(la.d_type) ) {
            s = ForStat();
		} else if( FIRST_LoopStat(la.d_type) ) {
            s = LoopStat();
		} else if( FIRST_WithStat(la.d_type) ) {
            s = WithStat();
		} else if( la.d_type == Tok_EXIT ) {
			expect(Tok_EXIT, false, "Statement");
            s = new Statement(Statement::Exit, cur.toRowCol());
		} else if( FIRST_ReturnStat(la.d_type) ) {
            s = ReturnStat();
		} else if( FIRST_StatBlock(la.d_type) ) {
            s = StatBlock();
			expect(Tok_END, false, "Statement");
		} else
			invalid("Statement");
	}
    return s;
}

Statement* Parser2::Case() {
    Statement* res = new Statement(Statement::CaseLabel, la.toRowCol());
    res->rhs = CaseLabels();
    if( res->rhs == 0 )
    {
        Statement::deleteAll(res);
        return 0;
    }
    while( la.d_type == Tok_Comma ) {
        expect(Tok_Comma, false, "Case");
        Expression* e = CaseLabels();
        if( e == 0 )
        {
            Statement::deleteAll(res);
            return 0;
        }
        Expression::append(res->rhs, e);
    }
    expect(Tok_Colon, false, "Case");
    res->body = StatSeq();
    return res;
}

Expression* Parser2::CaseLabels() {
    Expression* res = ConstExpr();
    if( res == 0 )
        return 0;
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "CaseLabels");
        const Token t = cur;
        Expression* rhs = ConstExpr();
        if( rhs == 0 )
        {
            delete res;
            return 0;
        }
        Expression* range = new Expression(Expression::Range, t.toRowCol());
        range->lhs = res;
        range->rhs = rhs;
        res = range;
    }
    return res;
}

Expression* Parser2::ConstExpr() {
    return Expr(); // done
}

Ast::Expression* Parser2::Expr(bool lvalue) {
    Expression* res = SimpleExpr(lvalue);
    if( res == 0 )
        return 0;
    if( FIRST_Relation(la.d_type) ) {
        const Token tok = la;
        Expression* tmp = Expression::createFromToken(Relation(), tok.toRowCol());
        tmp->lhs = res;
        tmp->type = mdl->getType(Type::BOOLEAN);
        res = tmp;
        res->rhs = SimpleExpr(false);
        if( res->rhs == 0 )
        {
            delete tmp;
            return 0;
        }
    }
    return res;
}

Expression* Parser2::SimpleExpr(bool lvalue) {
    quint8 op = 0;
    Token tok = la;
    if( la.d_type == Tok_Plus || la.d_type == Tok_Minus ) {
        if( la.d_type == Tok_Plus ) {
            expect(Tok_Plus, false, "Term");
            op = Tok_Plus;
        } else if( la.d_type == Tok_Minus ) {
            expect(Tok_Minus, false, "Term");
            op = Tok_Minus;
        } else
            invalid("Term");
    }
    Expression* res = Term(lvalue);
    if( res == 0 )
        return 0;
    if( op != 0 ) {
        Expression* tmp = new Expression(op == Tok_Plus ? Expression::Plus : Expression::Minus, tok.toRowCol());
        tmp->lhs = res;
        tmp->type = res->type;
        res = tmp;
    }
    while( FIRST_AddOp(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(AddOp(), tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = Term(false);
        if( res->rhs == 0 )
        {
            delete tmp;
            return 0;
        }
    }
    return res;
}

Expression* Parser2::Term(bool lvalue) {
    Expression* res = Factor(lvalue);
    if( res == 0 )
        return 0;
    while( FIRST_MulOp(la.d_type) ) {
        Token tok = la;
        Expression* tmp = Expression::createFromToken(MulOp(),tok.toRowCol());
        tmp->lhs = res;
        res = tmp;
        res->rhs = Factor(false);
        if( res->rhs == 0 )
            return 0;
    }
    return res;
}

static QByteArray dequote(const QByteArray& str)
{
    QByteArray res;
    if( (str.startsWith('\'') && str.endsWith('\'')) ||
            (str.startsWith('"') && str.endsWith('"')) )
        res = str.mid(1,str.size()-2);
    else
        res = str;
    // no longer append an explicit zero; just confuses LuaJIT
    return res;
}

Expression* Parser2::Factor(bool lvalue) {
    Expression* res = 0;
    if( FIRST_Designator(la.d_type) ) {
        res = Designator(lvalue);
	} else if( FIRST_number(la.d_type) ) {
        res = number();
	} else if( la.d_type == Tok_hexchar ) {
		expect(Tok_hexchar, false, "Factor");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(Type::CHAR);
        QByteArray tmp = cur.d_val;
        tmp.chop(1); // remove X postfix
        res->val = tmp.toUInt(0,16);
    } else if( la.d_type == Tok_string ) {
		expect(Tok_string, false, "Factor");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(Type::StrLit);
        res->val = dequote(cur.d_val);
    } else if( la.d_type == Tok_NIL ) {
		expect(Tok_NIL, false, "Factor");
        res = new Expression(Expression::Literal,cur.toRowCol());
        res->type = mdl->getType(Type::NIL);
        res->val = QVariant();
    } else if( FIRST_Set(la.d_type) ) {
        res = Set();
	} else if( la.d_type == Tok_Lpar ) {
		expect(Tok_Lpar, false, "Factor");
        res = Expr();
		expect(Tok_Rpar, false, "Factor");
	} else if( la.d_type == Tok_Tilde ) {
		expect(Tok_Tilde, false, "Factor");
        Expression* tmp = Factor(false);
        if( tmp == 0 )
            return 0;
        res = new Expression(Expression::Not, cur.toRowCol());
        res->lhs = tmp;
    } else
		invalid("Factor");
    return res;
}

Expression* Parser2::Set() {
    Expression* res = new Expression(Expression::Constructor, la.toRowCol());
    expect(Tok_Lbrace, false, "Set");
	if( FIRST_Element(la.d_type) ) {
        Expression* e = Element();
        if( e == 0 )
        {
            delete res;
            return 0;
        }
        res->appendRhs(e);
        while( la.d_type == Tok_Comma ) {
			expect(Tok_Comma, false, "Set");
            e = Element();
            if( e == 0 )
            {
                delete res;
                return 0;
            }
            res->appendRhs(e);
        }
	}
	expect(Tok_Rbrace, false, "Set");
    return res;
}

Expression* Parser2::Element() {
    Expression* res = Expr();
    if( res == 0 )
        return 0;
	if( la.d_type == Tok_2Dot ) {
		expect(Tok_2Dot, false, "Element");
        const Token t = cur;
        Expression* rhs = Expr();
        if( rhs == 0 )
        {
            delete res;
            return 0;
        }
        Expression* range = new Expression(Expression::Range, t.toRowCol());
        range->lhs = res;
        range->rhs = rhs;
        res = range;
    }
    return res;
}

quint8 Parser2::Relation() {
	if( la.d_type == Tok_Eq ) {
		expect(Tok_Eq, false, "Relation");
	} else if( la.d_type == Tok_Hash ) {
		expect(Tok_Hash, false, "Relation");
	} else if( la.d_type == Tok_Lt ) {
		expect(Tok_Lt, false, "Relation");
	} else if( la.d_type == Tok_Leq ) {
		expect(Tok_Leq, false, "Relation");
	} else if( la.d_type == Tok_Gt ) {
		expect(Tok_Gt, false, "Relation");
	} else if( la.d_type == Tok_Geq ) {
		expect(Tok_Geq, false, "Relation");
	} else if( la.d_type == Tok_IN ) {
		expect(Tok_IN, false, "Relation");
	} else if( la.d_type == Tok_IS ) {
		expect(Tok_IS, false, "Relation");
	} else
		invalid("Relation");
    return cur.d_type;
}

quint8 Parser2::MulOp() {
	if( la.d_type == Tok_Star ) {
		expect(Tok_Star, false, "MulOp");
	} else if( la.d_type == Tok_DIV ) {
		expect(Tok_DIV, false, "MulOp");
	} else if( la.d_type == Tok_MOD ) {
		expect(Tok_MOD, false, "MulOp");
	} else if( la.d_type == Tok_Slash ) {
		expect(Tok_Slash, false, "MulOp");
	} else if( la.d_type == Tok_Amp ) {
		expect(Tok_Amp, false, "MulOp");
	} else
		invalid("MulOp");
    return cur.d_type;
}

quint8 Parser2::AddOp() {
	if( la.d_type == Tok_Plus ) {
		expect(Tok_Plus, false, "AddOp");
	} else if( la.d_type == Tok_Minus ) {
		expect(Tok_Minus, false, "AddOp");
	} else if( la.d_type == Tok_OR ) {
		expect(Tok_OR, false, "AddOp");
	} else
		invalid("AddOp");
    return cur.d_type;
}

Expression* Parser2::maybeQualident()
{
    expect(Tok_ident, false, "designator");
    Token tok = cur;

    Declaration* d = mdl->findDecl(cur.d_val);
    if( d && d->kind == Declaration::Import )
    {
        // a full qualident; we require a dot
        expect(Tok_Dot, false, "selector");
        expect(Tok_ident, false, "selector");
        Qualident q;
        q.first = tok.d_val;
        q.second = cur.d_val;
        Expression* res = new Expression(Expression::NameRef, tok.toRowCol());
        res->val = QVariant::fromValue(q);
        return res;
    }else
    {
        // A qualident without module reference; we save the dot (if presend) for the downstream
        Expression* res = new Expression(Expression::NameRef, tok.toRowCol());
        Qualident q;
        q.second = tok.d_val;
        res->val = QVariant::fromValue(q);
        return res;
    }
}

// designator results in an lvalue if possible, unless needsLvalue is false
Expression* Parser2::Designator(bool needsLvalue) {

    Expression* res = maybeQualident();
    if( !res )
        return 0;

    while( FIRST_Selector(la.d_type) ) {
        // integrated Selector
        if( ( peek(1).d_type == Tok_Dot && peek(2).d_type == Tok_ident )  ) {
            expect(Tok_Dot, false, "Selector");
            expect(Tok_ident, false, "Selector");
            Expression* tmp = new Expression(Expression::Select, cur.toRowCol() );
            tmp->val = QVariant::fromValue(cur.d_val);
            tmp->lhs = res;
            res = tmp;
        } else if( la.d_type == Tok_Lbrack ) {
            expect(Tok_Lbrack, false, "Selector");
            ExpList l = ExprList();
            if( l.isEmpty() )
            {
                delete res;
                return 0;
            }
            for( int i = 0; i < l.size(); i++ )
            {
                Expression* tmp = new Expression(Expression::Index, cur.toRowCol() );
                tmp->lhs = res;
                res = tmp;
                res->rhs = l[i];
            }
            expect(Tok_Rbrack, false, "Selector");
        } else if( la.d_type == Tok_Hat ) {
            expect(Tok_Hat, false, "Selector");
            Expression* tmp = new Expression(Expression::Deref, cur.toRowCol() );
            tmp->lhs = res;
            res = tmp;
       } else if( la.d_type == Tok_Lpar ) {
            expect(Tok_Lpar, false, "Selector");
            const Token lpar = cur;

            Expression* args = 0;
            if( FIRST_ExprList(la.d_type) ) {
                ExpList l = ExprList();
                if( l.isEmpty() )
                {
                    delete res;
                    return 0;
                }
                for( int i = 1; i < l.size(); i++ )
                    Expression::append(l[0],l[i]);
                args = l[0];
            }
            expect(Tok_Rpar, false, "Selector");

            Expression* tmp = new Expression(Expression::Call, lpar.toRowCol() ); // could be call or typecast at this point
            tmp->lhs = res; // proc
            tmp->rhs = args;
            res = tmp;
        } else
            invalid("Selector");
	}
    res->needsLval = needsLvalue;
    return res;
}

ExpList Parser2::ExprList() {
    Expression* e = Expr();
    if( e == 0 )
        return ExpList();
    ExpList res;
    res << e;
	while( la.d_type == Tok_Comma ) {
		expect(Tok_Comma, false, "ExprList");
        Expression* e = Expr();
        if( e == 0 )
        {
            foreach( e, res )
                delete e;
            return ExpList();
        }
        res << e;
	}
    return res;
}

QList<Parser2::ID> Parser2::IdentList() {
    QList<ID> res;
    res << IdentDef();
	while( la.d_type == Tok_Comma ) {
		expect(Tok_Comma, false, "IdentList");
        res << IdentDef();
	}
    return res;
}

Qualident Parser2::Qualident_() {
    Qualident q;
	if( ( peek(1).d_type == Tok_ident && peek(2).d_type == Tok_Dot )  ) {
		expect(Tok_ident, false, "Qualident");
        q.first = cur.d_val;
		expect(Tok_Dot, false, "Qualident");
		expect(Tok_ident, false, "Qualident");
        q.second = cur.d_val;
	} else if( la.d_type == Tok_ident ) {
		expect(Tok_ident, false, "Qualident");
        q.second = cur.d_val;
	} else
		invalid("Qualident");
    return q;
}

Ast::Type* Parser2::NamedType() {
    const Token t = la;
    Qualident q = Qualident_();
    Ast::Type* res = new Ast::Type();
    res->kind = Ast::Type::NameRef;
    Declaration* helper = addHelper(res);
    helper->pos = t.toRowCol();
    helper->data = QVariant::fromValue(q);
    return res;
}

Parser2::ID Parser2::IdentDef() {
	expect(Tok_ident, false, "IdentDef");
    ID res;
    res.name = cur;
	if( la.d_type == Tok_Star || la.d_type == Tok_Minus ) {
		if( la.d_type == Tok_Star ) {
			expect(Tok_Star, false, "IdentDef");
            res.visi = ID::Public;
		} else if( la.d_type == Tok_Minus ) {
			expect(Tok_Minus, false, "IdentDef");
            res.visi = ID::ReadOnly;
		} else
			invalid("IdentDef");
	}
	if( FIRST_SysFlag(la.d_type) ) {
        res.untraced = SysFlag();
	}
    return res;
}

Type* Parser2::smallestIntType(quint64 i)
{
    if( i <= std::numeric_limits<qint8>::max() )
        return mdl->getType(Type::SHORTINT);
    else if( i <= std::numeric_limits<qint16>::max() )
        return mdl->getType(Type::INTEGER);
    else if( i <= std::numeric_limits<qint32>::max() )
        return mdl->getType(Type::LONGINT);
    else
        return mdl->getType(Type::HUGEINT);
}

Expression* Parser2::number() {
    Expression* res = Expression::createFromToken(la.d_type,la.toRowCol());
    if( la.d_type == Tok_integer ) {
		expect(Tok_integer, false, "number");
        quint64 i = 0;
        if( cur.d_val.endsWith('h') || cur.d_val.endsWith('H') )
            i = cur.d_val.left(cur.d_val.size()-2).toULongLong();
        else
            i = cur.d_val.toULongLong();
        res->type = smallestIntType(i);
        res->val = i;
	} else if( la.d_type == Tok_real ) {
		expect(Tok_real, false, "number");
        res->type = 0;
        if( cur.d_val.contains('d') || cur.d_val.contains('D') )
        {
            cur.d_val.replace('d','e');
            cur.d_val.replace('D','e');
            res->type = mdl->getType(Type::LONGREAL);
        }else if( cur.d_val.contains('s') || cur.d_val.contains('S') )
        {
            res->type = mdl->getType(Type::REAL);
            cur.d_val.replace('s','e');
            cur.d_val.replace('S','e');
        }
        const double d = cur.d_val.toDouble();
        if( res->type == 0 )
        {
            const int dot = cur.d_val.indexOf('.');
            int len = 0;
            if( dot != -1 )
            {
                while( dot+1+len < cur.d_val.size() && isdigit(cur.d_val[dot+1+len]) )
                    len++;
            }
            if( len > 5 )
                res->type = mdl->getType(Type::LONGREAL);
            else
                res->type = mdl->getType(Type::REAL);
        }
        res->val = d;
    } else
		invalid("number");
    return res;
}

Declaration*Parser2::addDecl(const Token& id, quint8 visi, quint8 mode)
{
    // NOTE: we don't check here whether names are unique; this is to be done in the validator
    Declaration* d = mdl->addDecl(id.d_val);
    if( d == 0 )
    {
        error( id, "a declaration with this name already exists");
        return 0;
    }
    d->kind = mode;
    d->visi = visi;
    d->pos.d_row = id.d_lineNr;
    d->pos.d_col = id.d_colNr;
    return d;
}

void Parser2::error(const Token& t, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg,RowCol(t.d_lineNr, t.d_colNr), t.d_sourcePath);
}

void Parser2::error(const RowCol& pos, const QString& msg)
{
    Q_ASSERT(!msg.isEmpty());
    errors << Error(msg, pos, scanner->source());
}

Declaration*Parser2::addHelper(Ast::Type* t)
{
    Declaration* decl = mdl->addHelper();
    // we need these syntetic declarations because emitter doesn't support anonymous types
    decl->type = t;
    decl->ownstype = true;
    decl->outer = thisMod;
    Q_ASSERT(t->decl == 0);
    t->decl = decl;
    t->anonymous = true;
    return decl;
}


