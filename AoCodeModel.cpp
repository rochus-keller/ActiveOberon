/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the ActiveOberon parser/navigator project.
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

// Adopted from the Lisa Pascal Navigator

#include "AoCodeModel.h"
#include "AoLexer.h"
#include "AoParser.h"
#include <QFile>
#include <QPixmap>
#include <QtDebug>
#include <QCoreApplication>
using namespace Ao;

#define LISA_WITH_MISSING

class AoModelVisitor
{
    CodeModel* d_mdl;
    UnitFile* d_cf;
    QList<Scope*> d_scopes;

    struct Id {
        Token t;
        quint8 visi;
        Id(const Token& t = Token()):t(t),visi(0){}
    };

    typedef QList<Id> IdList;

public:
    AoModelVisitor(CodeModel* m):d_mdl(m) {}

    void visit(UnitFile* cf, SynTree* top)
    {
        d_cf = cf;
        Q_ASSERT(cf->d_body == 0);
        Scope* s = new Scope();
        s->d_owner = d_cf;
        s->d_kind = Thing::Body;
        d_cf->d_body = s;

        if( top->d_children.isEmpty() )
            return;
        if( top->d_tok.d_type == Tok_Invalid )
            top = top->d_children.first();
        switch(top->d_tok.d_type)
        {
        case SynTree::R_Module:
            d_scopes.push_back(s);
            Module(top);
            d_scopes.pop_back();
            break;
        }
    }

    void Module(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Module);
        Declaration* module = 0;
        bool done = false;
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_MODULE:
                break;
            case Tok_ident: // there are two module name idents, one at the end
                if( !done )
                {
                    module = addDecl(sub->d_tok,Thing::Module);
                    module->d_body = new Scope();
                    module->d_body->d_owner = module;
                    module->d_body->d_kind = Thing::Body;
                    module->d_body->d_outer = d_mdl->getPredecls();
                    d_cf->d_module = module;
                }
                done = true;
                break;
            case Tok_Semi:
                break;
            case SynTree::R_ImportList:
                d_scopes.push_back(module->d_body);
                ImportList(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_DeclSeq:
                d_scopes.push_back(module->d_body);
                DeclSeq(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_Body:
                d_scopes.push_back(module->d_body);
                attrs = Body(sub);
                d_scopes.pop_back();
                break;
            case Tok_Dot:
                break;
            }
        }
        if( attrs.contains("ACTIVE") )
            module->d_body->d_active = true;
#if 0
        // There are 635 modules; no ACTIVE attribute is associated with a module BEGIN block.
        qDebug() << "module\t" << (module->d_body->d_active?"ACTIVE":"") << "\t" << d_cf->d_file->d_name << "\t" << st->d_tok.d_lineNr;
#endif
    }

    void ImportDecl(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ImportDecl);
        TokenList pair;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                pair << sub->d_tok;
                break;
            case Tok_ColonEq:
                break;
            }
        }
        Declaration* d = addDecl( pair.first(), Thing::Import );

        if( pair.last().d_val.constData() == d_mdl->getSystem()->d_name.constData() )
            d->d_body = d_mdl->getSystem()->d_body;
        else
            foreach( UnitFile* uf, d_cf->d_import )
            {
                if( uf->d_module && uf->d_module->d_name.constData() == pair.last().d_val.constData() )
                {
                    d->d_body = uf->d_module->d_body;
                    break;
                }
            }

        if( d->d_body == 0 )
            return; // error already reported

        Q_ASSERT( d->d_me && d->d_body->d_owner->d_kind == Thing::Module );
        Declaration* m = static_cast<Declaration*>(d->d_body->d_owner);
        //d->d_me->d_decl = m; // No, since we want the list of uses of the import in the present module
        // make that all imports of m appear in the uses list of the module name of m
        m->d_refs[d->d_loc.d_filePath].append(d->d_me);
        // make that the module name of m appears in the uses list of the declaration importing m
        d->d_refs[m->d_loc.d_filePath].append(m->d_me);

    }

    void ImportList(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ImportList);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_IMPORT:
                break;
            case SynTree::R_ImportDecl:
                ImportDecl(sub);
                break;
            case Tok_Comma:
                break;
            case Tok_Semi:
                break;
            }
        }
    }

    typedef QList<Declaration*> Decls;
    struct Deferred
    {
        SynTree* st;
        Decls decls;
        void (AoModelVisitor::*handler)(SynTree* st, const Decls&);
    };

    static void defer( QList<Deferred>& dd, SynTree* st, const Decls& decls, void (AoModelVisitor::*handler)(SynTree* st, const Decls&) )
    {
        Deferred d;
        d.st = st;
        d.decls = decls;
        d.handler = handler;
        dd << d;
    }

    void DeclSeq(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_DeclSeq);
        QList<Deferred> dd;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_CONST:
                break;
            case SynTree::R_ConstDecl:
                defer(dd, sub, ConstDecl1(sub), &AoModelVisitor::ConstDecl2);
                break;
            case Tok_Semi:
                break;
            case Tok_TYPE:
                break;
            case SynTree::R_TypeDecl:
                defer(dd, sub, TypeDecl1(sub), &AoModelVisitor::TypeDecl2);
                break;
            case Tok_VAR:
                break;
            case SynTree::R_VarDecl:
                defer(dd, sub, VarDecl1(sub), &AoModelVisitor::VarDecl2);
                break;
            case SynTree::R_ProcDecl:
                defer(dd, sub, ProcDecl1(sub), &AoModelVisitor::ProcDecl2);
                break;
            }
        }

        // since the declarations can appear in any order, first declare all names,
        // and only then resolve symbols.
        foreach( const Deferred& d, dd )
            (this->*d.handler)(d.st,d.decls);
    }

    Decls ConstDecl1(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ConstDecl);
        Decls res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_IdentDef:
                res << addDecl( IdentDef(sub), Thing::Const);
                break;
            }
        }
        return res;
    }

    void ConstDecl2(SynTree* st, const Decls& decls) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ConstDecl);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_ConstExpr:
                ConstExpr(sub);
                break;
            }
        }
    }

    Decls TypeDecl1(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_TypeDecl);
        Decls res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_IdentDef:
                res << addDecl(IdentDef(sub), Thing::TypeDecl);
                break;
            }
        }
        return res;
    }

    void TypeDecl2(SynTree* st, const Decls& decls) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_TypeDecl);
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Type:
                t = Type_(sub);
                break;
            }
        }
        foreach( Declaration* d, decls )
            if( d )
            {
                 d->d_type = t;
                 if( t && t->d_members && t->d_members->d_active )
                    d->d_active = true;
            }
    }

    Decls VarDecl1(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_VarDecl);
        Decls res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_IdentList:
                foreach( const Id& id, IdentList(sub) )
                    res << addDecl(id, Thing::Var);
                break;
            }
        }
        return res;
    }

    void VarDecl2(SynTree* st, const Decls& decls) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_VarDecl);
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Type:
                t = Type_(sub);
                break;
            }
        }
        foreach( Declaration* d, decls )
            d->d_type = t;
    }

    void Assembler(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Assembler);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_CODE:
                break;
            case Tok_END:
                break;
            }
        }
    }

    Decls ProcDecl1(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ProcDecl);
        Decls res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_ProcHead:
                res << ProcHead(sub);
                break;
            case Tok_Hat:
                return res; // this is a forward declaration, just ignore it
            }
        }
        return res;
    }

    //QList<Declaration*> procDeclLevel;
    void ProcDecl2(SynTree* st, const Decls& decls) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ProcDecl);

        if( decls.isEmpty() )
            return;

        Declaration* d = decls.first();
        //procDeclLevel.push_back(d);
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_DeclSeq:
                d_scopes.push_back(d->d_body);
                DeclSeq(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_Body:
                d_scopes.push_back(d->d_body);
                attrs = Body(sub);
                d_scopes.pop_back();
                break;
            case SynTree::R_Assembler:
                Assembler(sub);
                break;
            }
        }
        d->d_exclusive = attrs.count("EXCLUSIVE");
        //procDeclLevel.pop_back();
    }

    Declaration* createProcDecl(const Id& id )
    {
        Scope* scope = d_scopes.back();

        Declaration* d = addDecl(scope, id, Thing::Proc);
        d->d_body = new Scope();
        d->d_body->d_kind = Thing::Body;
        d->d_body->d_owner = d;
        d->d_body->d_outer = scope;

        if( scope->d_kind == Thing::Members )
        {
            Token name;
            name.d_type = Tok_ident;
            name.d_val = Lexer::getSymbol("SELF");
            Declaration* self = addDecl(d->d_body, name, Thing::Self);
            self->d_type = scope->d_type;
            d->d_body->d_altOuter = scope->d_altOuter;

        }

        return d;
    }

    Declaration* ProcHead(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ProcHead);
        Declaration* d = 0;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case Tok_Star:
                break;
            case Tok_Amp:
                break;
            case Tok_Minus:
                break;
            case SynTree::R_IdentDef:
                d = createProcDecl(IdentDef(sub));
                break;
            case SynTree::R_FormalPars:
                d_scopes.push_back(d->d_body);
                FormalPars(sub);
                d_scopes.pop_back();
                break;
            }
        }
        return d;
    }

    void SysFlag(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_SysFlag);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Lbrack:
                break;
            case Tok_ident:
                break;
            case Tok_Rbrack:
                break;
            }
        }
    }

    Type::Ref FormalPars(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_FormalPars);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Lpar:
                break;
            case SynTree::R_FPSection:
                FPSection(sub);
                break;
            case Tok_Semi:
                break;
            case Tok_Rpar:
                break;
            case Tok_Colon:
                break;
            case SynTree::R_Qualident:
                Qualident(sub);
                break;
            }
        }
        return res;
    }

    void FPSection(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_FPSection);
        Type::Ref t;
        IdList ids;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_VAR:
                break;
            case Tok_ident:
                ids << sub->d_tok;
                break;
            case Tok_Comma:
                break;
            case Tok_Colon:
                break;
            case SynTree::R_Type:
                t = Type_(sub);
                break;
            }
        }
        foreach( const Id& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Param);
            d->d_type = t;
        }
    }

    Type::Ref ArrayType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ArrayType);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ARRAY:
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case SynTree::R_ConstExpr:
                ConstExpr(sub);
                break;
            case Tok_Comma:
                break;
            case Tok_OF:
                break;
            case SynTree::R_Type:
                res = Type_(sub);
                break;
            }
        }
        Type::Ref arr( new Type() );
        arr->d_kind = Type::Array;
        arr->d_type = res;
        return arr;
    }

    Type::Ref RecordType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_RecordType);
        Type::Ref res(new Type());
        res->d_kind = Type::Record;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        d_scopes.push_back(res->d_members);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_RECORD:
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_Qualident:
                Qualident(sub);
                break;
            case Tok_Rpar:
                break;
            case SynTree::R_FieldList:
                FieldList(sub);
                break;
            case Tok_END:
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    Type::Ref PointerType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_PointerType);
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_POINTER:
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case Tok_TO:
                break;
            case SynTree::R_Type:
                t = Type_(sub);
                break;
            }
        }
        Type::Ref ptr(new Type());
        ptr->d_kind = Type::Pointer;
        ptr->d_type = t;
        return ptr;
    }

    Type::Ref ObjectType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ObjectType);
        Type::Ref res(new Type());
        res->d_kind = Type::Class;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_altOuter = d_scopes.back();
        res->d_members->d_type = res.data();
        d_scopes.push_back(res->d_members);
        Declaration* super;
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_OBJECT:
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_Qualident:
                super = Qualident(sub);
                if( super && super->d_kind == Thing::TypeDecl )
                {
                    res->d_type = super->d_type;
                    res->d_members->d_outer = super->d_type->d_members;
                }
                break;
            case Tok_Rpar:
                break;
            case SynTree::R_DeclSeq:
                DeclSeq(sub);
                break;
            case SynTree::R_Body:
                attrs = Body(sub);
                break;
            case Tok_ident:
                break;
            }
        }
        d_scopes.pop_back();
        if( attrs.contains("ACTIVE") )
            res->d_members->d_active = true;
#if 0
        // There are 735 OBJECT declarations in 222 modules
        // 660 OBJECT declarations are passive and 75 are ACTIVE
        qDebug() << "class\t" << (res->d_members->d_active?"ACTIVE":"") << "\t" << d_cf->d_file->d_name << "\t" << st->d_tok.d_lineNr;
#endif
#if 0
        // 619 of the 735 OBJECT declarations have one or more procedures
        // 441 of the OBJECTs with procedures have no EXCLUSIVE block, so only 178 have an EXCLUSIVE block
        // Only 91 of the said 178 OBJECTs have public EXCLUSIVE procedures
        // Of these 91 OBJECTs, 54 have more non EXCLUSIVE than EXCLUSIVE public procedures!
        // Only one procedure has more than one EXCLUSIVE block: WMWindowManager.DoubleBufferWindow.Draw
        // There are 419 passive objects with no exclusive block, 135 passive objects with one or more exclusive blocks,
        // 21 active objects with no exclusive block, and 41 active objects with one or more exclusive blocks
        int numProc = 0, numPublicProc = 0, numExclusiveProc = 0, numExclusivePublicProc = 0, numExclusiveBlocks = 0;
        for( int i = 0; i < res->d_members->d_order.size(); i++ )
        {
            Declaration* d = res->d_members->d_order[i];
            if( d->d_kind == Thing::Proc )
            {
                res->d_members->d_exclusive += d->d_exclusive;
                numProc++;
                if( d->d_visi > 0 )
                    numPublicProc++;
                if( d->d_exclusive > 0 )
                    numExclusiveProc++;
                numExclusiveBlocks += d->d_exclusive;
                if( d->d_visi > 0 && d->d_exclusive > 0 )
                    numExclusivePublicProc++;
            }
        }
        if( numProc )
            qDebug() << res->d_members->d_active << "\t" << numProc << "\t" << numPublicProc << "\t"
                     << numExclusiveProc<< "\t"<< numExclusivePublicProc<< "\t" << numExclusiveBlocks
                     << "\t" << d_cf->d_file->d_name << "\t" << st->d_tok.d_lineNr;
#else
        for( int i = 0; i < res->d_members->d_order.size(); i++ )
        {
            Declaration* d = res->d_members->d_order[i];
            if( d->d_kind == Thing::Proc )
                res->d_members->d_exclusive += d->d_exclusive;
        }
#endif
        return res;
    }

    Type::Ref ProcedureType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ProcedureType);
        Type::Ref res(new Type());
        res->d_kind = Type::Procedural;
        res->d_members = new Scope();
        res->d_members->d_kind = Thing::Members;
        res->d_members->d_outer = d_scopes.back();
        QByteArrayList attrs;
        d_scopes.push_back(res->d_members);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_PROCEDURE:
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            case SynTree::R_Attributes:
                attrs = Attributes(sub);
                // qDebug() << "ProcType Attrs" << attrs << d_cf->d_file->d_name << st->d_tok.d_lineNr;
                // only DELEGATE seen here, 74 in total
                break;
            case SynTree::R_FormalPars:
                FormalPars(sub);
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    Type::Ref AliasType(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_AliasType);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Qualident:
                res = resolveType(Qualident(sub));
                break;
            }
        }
        return res;
    }

    Type::Ref Type_(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Type);
        Type::Ref res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_AliasType:
                res = AliasType(sub);
                break;
            case SynTree::R_ArrayType:
                res = ArrayType(sub);
                break;
            case SynTree::R_RecordType:
                res = RecordType(sub);
                break;
            case SynTree::R_PointerType:
                res = PointerType(sub);
                break;
            case SynTree::R_ObjectType:
                res = ObjectType(sub);
                break;
            case SynTree::R_ProcedureType:
                res = ProcedureType(sub);
                break;
            }
        }
        return res;
    }

    void FieldDecl(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_FieldDecl);
        IdList ids;
        Type::Ref tp;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_IdentList:
                ids = IdentList(sub);
                break;
            case Tok_Colon:
                break;
            case SynTree::R_Type:
                tp = Type_(sub);
                break;
            }
        }
        foreach( const Id& id, ids )
        {
            Declaration* d = addDecl(id, Thing::Field);
            d->d_type = tp;
        }
    }

    void FieldList(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_FieldList);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_FieldDecl:
                FieldDecl(sub);
                break;
            case Tok_Semi:
                break;
            }
        }
    }

    QByteArrayList Body(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Body);
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_StatBlock:
                attrs = StatBlock(sub);
                break;
            case Tok_END:
                break;
            }
        }
        return attrs;
    }

    QByteArrayList fetchIdents(SynTree* st)
    {
        QByteArrayList res;
        switch(st->d_tok.d_type)
        {
        case SynTree::R_ExprList:
            for( int i = 0; i < st->d_children.size(); i++ )
                res << fetchIdents(st->d_children[i]);
            break;
        case SynTree::R_Expr:
        case SynTree::R_SimpleExpr:
        case SynTree::R_Term:
        case SynTree::R_Factor:
        case SynTree::R_Designator:
            if( !st->d_children.isEmpty() )
                res << fetchIdents(st->d_children.first());
            break;
        case Tok_ident:
            res << st->d_tok.d_val;
            break;
        }
        return res;
    }

    QByteArrayList Attributes(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Attributes);
        d_scopes.push_back(d_mdl->getAttrs());
        QByteArrayList res;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Lbrace:
                break;
            case SynTree::R_ExprList:
                ExprList(sub);
                res = fetchIdents(sub);
                break;
            case Tok_Rbrace:
                break;
            }
        }
        d_scopes.pop_back();
        return res;
    }

    //QList<QByteArrayList> statBlocks;
    QByteArrayList StatBlock(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_StatBlock);
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_BEGIN:
                break;
            case SynTree::R_Attributes:
                attrs = Attributes(sub);
#if 0
                // found 80 nested statblocks, all level 1, 32 in an ACTIVE block,
                // the others in a normal block, no nested blocks where both levels are EXCLUSIVES!
                // all ACTIVE are proclevel 0, all others are proclevel 1
                // there is no nested block which is not EXCLUSIVE!
                if( !statBlocks.isEmpty() )
                    qDebug() << "StatBlock Attrs"
                             << "level 0" << statBlocks.first()
                             << "level 1" << attrs << statBlocks.size()
                             << "proclevel" << procDeclLevel.size()
                             << d_cf->d_file->d_name << st->d_tok.d_lineNr;
#endif
                break;
            case SynTree::R_StatSeq:
                //statBlocks.push_back(attrs);
                attrs += StatSeq(sub);
                //statBlocks.pop_back();
                break;
            case Tok_END:
                break;
            }
        }
        return attrs;
    }

    QByteArrayList StatSeq(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_StatSeq);
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Statement:
                attrs += Statement(sub);
                break;
            case Tok_Semi:
                break;
            }
        }
        return attrs;
    }

    void AssigOrCall(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_AssigOrCall);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Designator:
                Designator(sub);
                break;
            case Tok_ColonEq:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_ExprList:
                ExprList(sub);
                break;
            case Tok_Rpar:
                break;
            }
        }
    }

    void IfStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_IfStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_IF:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_THEN:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_ELSIF:
                break;
            case Tok_ELSE:
                break;
            case Tok_END:
                break;
            }
        }
    }

    void CaseStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_CaseStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_CASE:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_DO:
                break;
            case Tok_OF:
                break;
            case SynTree::R_Case:
                Case(sub);
                break;
            case Tok_Bar:
                break;
            case Tok_ELSE:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_END:
                break;
            }
        }
    }

    void WhileStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_WhileStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_WHILE:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_DO:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_END:
                break;
            }
        }
    }

    void RepeatStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_RepeatStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_REPEAT:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_UNTIL:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            }
        }
    }

    void ForStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ForStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_FOR:
                break;
            case Tok_ident:
                break;
            case Tok_ColonEq:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_TO:
                break;
            case Tok_BY:
                break;
            case SynTree::R_ConstExpr:
                ConstExpr(sub);
                break;
            case Tok_DO:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_END:
                break;
            }
        }
    }

    void LoopStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_LoopStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_LOOP:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_END:
                break;
            }
        }
    }

    void WithStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_WithStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_WITH:
                break;
            case SynTree::R_Qualident:
                Qualident(sub);
                break;
            case Tok_Colon:
                break;
            case Tok_DO:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            case Tok_END:
                break;
            }
        }
    }

    void ReturnStat(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ReturnStat);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_RETURN:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            }
        }
    }

    QByteArrayList Statement(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Statement);
        QByteArrayList attrs;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_AssigOrCall:
                AssigOrCall(sub);
                break;
            case SynTree::R_IfStat:
                IfStat(sub);
                break;
            case SynTree::R_CaseStat:
                CaseStat(sub);
                break;
            case SynTree::R_WhileStat:
                WhileStat(sub);
                break;
            case SynTree::R_RepeatStat:
                RepeatStat(sub);
                break;
            case SynTree::R_ForStat:
                ForStat(sub);
                break;
            case SynTree::R_LoopStat:
                LoopStat(sub);
                break;
            case SynTree::R_WithStat:
                WithStat(sub);
                break;
            case Tok_EXIT:
                break;
            case SynTree::R_ReturnStat:
                ReturnStat(sub);
                break;
            case SynTree::R_StatBlock:
                attrs += StatBlock(sub);
                break;
            }
        }
        return attrs;
    }

    void Case(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Case);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_CaseLabels:
                CaseLabels(sub);
                break;
            case Tok_Comma:
                break;
            case Tok_Colon:
                break;
            case SynTree::R_StatSeq:
                StatSeq(sub);
                break;
            }
        }
    }

    void CaseLabels(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_CaseLabels);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_ConstExpr:
                ConstExpr(sub);
                break;
            case Tok_2Dot:
                break;
            }
        }
    }

    void ConstExpr(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ConstExpr);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Expr:
                Expr(sub);
                break;
            }
        }
    }

    void Expr(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Expr);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_SimpleExpr:
                SimpleExpr(sub);
                break;
            case SynTree::R_Relation:
                Relation(sub);
                break;
            }
        }
    }

    void SimpleExpr(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_SimpleExpr);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Term:
                Term(sub);
                break;
            case SynTree::R_MulOp:
                MulOp(sub);
                break;
            }
        }
    }

    void Term(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Term);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Plus:
                break;
            case Tok_Minus:
                break;
            case SynTree::R_Factor:
                Factor(sub);
                break;
            case SynTree::R_AddOp:
                AddOp(sub);
                break;
            }
        }
    }

    void Factor(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Factor);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Designator:
                Designator(sub);
                break;
            case SynTree::R_number:
                number(sub);
                break;
            case Tok_hexchar:
                break;
            case Tok_string:
                break;
            case Tok_NIL:
                break;
            case SynTree::R_Set:
                Set(sub);
                break;
            case Tok_Lpar:
                break;
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_Rpar:
                break;
            case Tok_Tilde:
                break;
            case SynTree::R_Factor:
                Factor(sub);
                break;
            }
        }
    }

    void Set(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Set);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Lbrace:
                break;
            case SynTree::R_Element:
                Element(sub);
                break;
            case Tok_Comma:
                break;
            case Tok_Rbrace:
                break;
            }
        }
    }

    void Element(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Element);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_2Dot:
                break;
            }
        }
    }

    void Relation(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Relation);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Eq:
                break;
            case Tok_Hash:
                break;
            case Tok_Lt:
                break;
            case Tok_Leq:
                break;
            case Tok_Gt:
                break;
            case Tok_Geq:
                break;
            case Tok_IN:
                break;
            case Tok_IS:
                break;
            }
        }
    }

    void MulOp(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_MulOp);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Star:
                break;
            case Tok_DIV:
                break;
            case Tok_MOD:
                break;
            case Tok_Slash:
                break;
            case Tok_Amp:
                break;
            }
        }
    }

    void AddOp(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_AddOp);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Plus:
                break;
            case Tok_Minus:
                break;
            case Tok_OR:
                break;
            }
        }
    }

    void Designator(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Designator);
        Declaration* res = 0;
        Scope* scope = d_scopes.back();
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    Symbol* sym = addSym(scope,sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                    {
                        res = (sym->d_decl);
                        scope = res->d_body;
                    }
                }
                break;
            case SynTree::R_Selector:
                Selector(sub, res, scope);
                break;
            }
        }
    }

    void Selector(SynTree* st, Declaration* d, Scope* s) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Selector);
        bool firstRun = true;
        Type::Ref t;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_Dot:
                break;
            case Tok_ident:
                if( firstRun && s )
                {
                    Symbol* sym = addSym(s,sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                    {
                        d = (sym->d_decl);
                        t = d->d_type;
                        break;
                    }
                }else if( firstRun && d->d_type.data() )
                {
                    t = d->d_type.data();
                    if( t && t->d_kind == Type::Pointer )
                        t = t->d_type.data();
                }
                if( t && t->d_members )
                {
                    Symbol* sym = addSym(t->d_members,sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                        t = (sym->d_decl)->d_type.data();
                }
                break;
            case Tok_Lbrack:
                break;
            case SynTree::R_ExprList:
                ExprList(sub);
                break;
            case Tok_Rbrack:
                if( t && t->d_kind == Type::Array )
                    t = t->d_type.data();
                break;
            case Tok_Hat:
                if( t && t->d_kind == Type::Pointer )
                    t = t->d_type.data();
                break;
            case Tok_Lpar:
                break;
            case Tok_Rpar:
                // TODO: t := return type of procedure t or result of typecast
                break;
            }
            firstRun = false;
        }
    }

    void ExprList(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_ExprList);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_Expr:
                Expr(sub);
                break;
            case Tok_Comma:
                break;
            }
        }
    }

    IdList IdentList(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_IdentList);
        IdList l;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case SynTree::R_IdentDef:
                l << IdentDef(sub);
                break;
            case Tok_Comma:
                break;
            }
        }
        return l;
    }

    void error( const Token& e, const QString& message)
    {
        const FileSystem::File* f = d_cf->d_file;
        const QString line = QString("%1:%2:%3: %4").arg( f ? f->getVirtualPath() : e.d_sourcePath )
                .arg(e.d_lineNr).arg(e.d_colNr).arg(message);
        qCritical() << line.toUtf8().constData();
    }

    Type::Ref resolveType(Declaration* d)
    {
        if( d == 0 || d->d_kind != Thing::TypeDecl )
            return Type::Ref();
        return d->d_type; // TODO: we need to preserve quali data if not yet resolvable
    }

    Declaration* Qualident(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_Qualident);
        Declaration* res = 0;
        Scope* scope = d_scopes.back();
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                {
                    Symbol* sym = addSym(scope,sub->d_tok);
                    if( sym && sym->d_decl && sym->d_decl->isDeclaration() )
                    {
                        res = (sym->d_decl);
                        if( res && res->d_body )
                            scope = res->d_body;
                    }
                    // TODO: defer if not resolved
                    //else
                    //    error(sub->d_tok,QString("cannot resolve ident %1").arg(sub->d_tok.d_val.constData()));
                }
                break;
            case Tok_Dot:
                break;
            }
        }
        return res;
    }

    Id IdentDef(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_IdentDef);
        Id id;
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_ident:
                id.t = sub->d_tok;
                break;
            case Tok_Star:
                id.visi = Thing::Public;
                break;
            case Tok_Minus:
                id.visi = Thing::ReadOnly;
                break;
            case SynTree::R_SysFlag:
                SysFlag(sub);
                break;
            }
        }
        return id;
    }

    void number(SynTree* st) {
        Q_ASSERT(st && st->d_tok.d_type == SynTree::R_number);
        for(int i = 0; i < st->d_children.size(); i++ ) {
            SynTree* sub = st->d_children[i];
            switch(sub->d_tok.d_type) {
            case Tok_integer:
                break;
            case Tok_real:
                break;
            }
        }
    }

    Declaration* addDecl(const Id& id, int type )
    {
        return addDecl( d_scopes.back(), id, type );
    }

    Declaration* addDecl(Scope* scope, const Id& id, int type )
    {
        Declaration* d = new Declaration();
        d->d_kind = type;
        d->d_name = id.t.d_val;
        d->d_visi = id.visi;
        if( !id.t.d_sourcePath.isEmpty() )
            d->d_loc.d_pos = id.t.toLoc();
        d->d_loc.d_filePath = id.t.d_sourcePath;
        d->d_owner = scope;
        scope->d_order.append(d);

        // each decl is also a symbol

        Symbol* sy = new Symbol();
        sy->d_loc = d->d_loc.d_pos;
        d_cf->d_syms[id.t.d_sourcePath].append(sy);
        d->d_me = sy;

        if( !id.t.d_sourcePath.isEmpty() )
        {
            sy->d_decl = d;
            d->d_refs[id.t.d_sourcePath].append(sy);
        }

        return d;
    }

    Symbol* addSym(Scope* scope, const Token& t)
    {
        Declaration* d = scope->findDecl(t.d_val.constData(), true);
#ifndef LISA_WITH_MISSING
        if( d )
#else
        if( true )
#endif
        {
            Symbol* sy = new Symbol();
            sy->d_loc = t.toLoc();
            d_cf->d_syms[t.d_sourcePath].append(sy);
            sy->d_decl = d;
            if( d )
                d->d_refs[t.d_sourcePath].append(sy);
            return sy;
        }else
            return 0;
    }
};

static void addDecl( Scope* scope, const char* name, int kind)
{
    Declaration* d = new Declaration();
    d->d_kind = kind;
    d->d_name = Lexer::getSymbol(name);
    d->d_visi = Thing::Public;
    d->d_owner = scope;
    scope->d_order.append(d);
}

CodeModel::CodeModel(QObject *parent) : ItemModel(parent),d_sloc(0),d_errCount(0)
{
    d_fs = new FileSystem(this);
    d_system.d_kind = Thing::Module;
    d_system.d_name = Lexer::getSymbol("SYSTEM");
    Scope* s = new Scope();
    s->d_kind = Thing::Body;
    s->d_owner = &d_system;
    d_system.d_body = s;

    addDecl(s, "MOVE", Thing::Proc);
    addDecl(s, "ADR", Thing::Proc);
    addDecl(s, "VAL", Thing::Proc);
    addDecl(s, "PORTOUT", Thing::Proc);
    addDecl(s, "PUT64", Thing::Proc);
    addDecl(s, "PUT32", Thing::Proc);
    addDecl(s, "PUT16", Thing::Proc);
    addDecl(s, "PUT8", Thing::Proc);
    addDecl(s, "PUT", Thing::Proc);
    addDecl(s, "GET", Thing::Proc);
    addDecl(s, "GET8", Thing::Proc);
    addDecl(s, "GET16", Thing::Proc);
    addDecl(s, "GET32", Thing::Proc);
    addDecl(s, "GET64", Thing::Proc);
    addDecl(s, "PORTIN", Thing::Proc);
    addDecl(s, "HALT", Thing::Proc);
    addDecl(s, "CLI", Thing::Proc);
    addDecl(s, "STI", Thing::Proc);
    addDecl(s, "BYTE", Thing::TypeDecl);
    addDecl(s, "LSH", Thing::Proc);

    addDecl( &d_attrs, "DELEGATE", Thing::Attribute);
    addDecl( &d_attrs, "ACTIVE", Thing::Attribute);
    addDecl( &d_attrs, "EXCLUSIVE", Thing::Attribute);
    addDecl( &d_attrs, "UNTRACED", Thing::Attribute);
    addDecl( &d_attrs, "SAFE", Thing::Attribute);
    addDecl( &d_attrs, "PRIORITY", Thing::Proc);

    addDecl( &d_predecls, "AWAIT", Thing::Proc);
    addDecl( &d_predecls, "HALT", Thing::Proc);
}

bool CodeModel::load(const QString& rootDir)
{
    beginResetModel();
    d_root = ModelItem();
    d_top.clear();
    d_map1.clear();
    d_map2.clear();
    d_sloc = 0;
    d_errCount = 0;
    d_mutes.clear();
    d_fs->load(rootDir);
    QList<ModelItem*> fileSlots;
    fillFolders(&d_root,&d_fs->getRoot(), &d_top, fileSlots);
    foreach( ModelItem* s, fileSlots )
    {
        Q_ASSERT( s->d_thing );
        if( s->d_thing->d_kind != Thing::Unit )
            continue;
        UnitFile* f = static_cast<UnitFile*>(s->d_thing);
        Q_ASSERT( f->d_file );
        parseAndResolve(f);
    }
    endResetModel();
    return true;
}

ItemModel::ItemModel(QObject* parent):QAbstractItemModel(parent)
{

}

const Thing* ItemModel::getThing(const QModelIndex& index) const
{
    if( !index.isValid() )
        return 0;
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    return s->d_thing;
}

QModelIndex ItemModel::findThing(const Thing* nt) const
{
    return findThing( &d_root, nt );
}

QVariant ItemModel::data(const QModelIndex& index, int role) const
{
    return QVariant();
}

Symbol*CodeModel::findSymbolBySourcePos(const QString& path, int line, int col) const
{
    UnitFile::SymList syms;

    UnitFile* uf = getUnitFile(path);
    if( uf != 0 )
        syms = uf->d_syms.value(path);


    // TODO maybe make that faster by ordering by rowcol and e.g. binary search
    foreach( Symbol* s, syms )
    {
        if( s->d_decl && s->d_loc.d_row == line &&
                s->d_loc.d_col <= col && col < s->d_loc.d_col + s->d_decl->getLen() )
            return s;
    }
    return 0;
}

CodeFile*CodeModel::getCodeFile(const QString& path) const
{
    return d_map2.value(path);
}

UnitFile*CodeModel::getUnitFile(const QString& path) const
{
    CodeFile* cf = d_map2.value(path);;
    if( cf )
    {
        UnitFile* uf = cf->toUnit();
        return uf;
    }
    return 0;
}

Ranges CodeModel::getMutes(const QString& path)
{
    return d_mutes.value(path);
}

QVariant CodeModel::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return static_cast<UnitFile*>(s->d_thing)->d_file->d_name;
        case Thing::Folder:
            {
                CodeFolder* cf = static_cast<CodeFolder*>(s->d_thing);
                if( cf->d_dir->d_name.isEmpty() && ( s->d_parent == 0 || s->d_parent->d_parent == 0 ) )
                    return "<root>";
                return cf->d_dir->d_name;
            }
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            return QPixmap(":/images/module.png");
        case Thing::Folder:
            return QPixmap(":/images/folder.png");
        }
        break;
    case Qt::ToolTipRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::Unit:
            {
                UnitFile* cf = static_cast<UnitFile*>(s->d_thing);
                return QString("<html><b>%1 %2</b><br>"
                               "<p>Logical path: %3</p>"
                               "<p>Real path: %4</p></html>")
                        .arg("Module")
                        .arg(cf->d_file->d_moduleName.constData())
                        .arg(cf->d_file->getVirtualPath())
                        .arg(cf->d_file->d_realPath);
            }
        }
        break;
    case Qt::FontRole:
        break;
    case Qt::ForegroundRole:
        break;
    }
    return QVariant();
}

QModelIndex ItemModel::index(int row, int column, const QModelIndex& parent) const
{
    const ModelItem* s = &d_root;
    if( parent.isValid() )
    {
        s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
    }
    if( row < s->d_children.size() && column < columnCount( parent ) )
        return createIndex( row, column, s->d_children[row] );
    else
        return QModelIndex();
}

QModelIndex ItemModel::parent(const QModelIndex& index) const
{
    if( index.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
        Q_ASSERT( s != 0 );
        if( s->d_parent == &d_root )
            return QModelIndex();
        // else
        Q_ASSERT( s->d_parent != 0 );
        Q_ASSERT( s->d_parent->d_parent != 0 );
        return createIndex( s->d_parent->d_parent->d_children.indexOf( s->d_parent ), 0, s->d_parent );
    }else
        return QModelIndex();
}

int ItemModel::rowCount(const QModelIndex& parent) const
{
    if( parent.isValid() )
    {
        ModelItem* s = static_cast<ModelItem*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
        return s->d_children.size();
    }else
        return d_root.d_children.size();
}


class Lex : public Scanner
{
public:
    Lexer lex;
    Token next()
    {
        return lex.nextToken();
    }

    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
    Lex( FileSystem* fs ):lex(fs) {}
};

void CodeModel::parseAndResolve(UnitFile* unit)
{
    if( unit->d_file->d_parsed )
        return; // already done

    QByteArrayList usedNames = unit->findUses();
    for( int i = 0; i < usedNames.size(); i++ )
    {
        if( usedNames[i] == "SYSTEM" )
            continue;
        const FileSystem::File* u = d_fs->findModule(unit->d_file->d_dir,usedNames[i]);
        if( u == 0 )
        {
            const QString line = tr("%1: cannot resolve referenced unit '%2'")
                    .arg( unit->d_file->getVirtualPath(false) ).arg(usedNames[i].constData());
            qCritical() << line.toUtf8().constData();
            d_errCount++;
        }else
        {
            UnitFile* uf = d_map1.value(u);
            if( uf == unit )
                return;
            Q_ASSERT( uf );
            unit->d_import.append( uf );
            parseAndResolve(uf);
        }
    }

    const_cast<FileSystem::File*>(unit->d_file)->d_parsed = true;
    Lex lex(d_fs);
    lex.lex.setStream(unit->d_file->d_realPath);

    Parser p(&lex);
    p.RunParser();
    const int off = d_fs->getRootPath().size();
    if( !p.errors.isEmpty() )
    {
        foreach( const Parser::Error& e, p.errors )
        {
            const FileSystem::File* f = d_fs->findFile(e.path);
            const QString line = tr("%1:%2:%3: %4").arg( f ? f->getVirtualPath() : e.path.mid(off) ).arg(e.row)
                    .arg(e.col).arg(e.msg);
            qCritical() << line.toUtf8().constData();
            d_errCount++;
        }

    }

    d_sloc += lex.lex.getSloc();
#if 0
    for( QHash<QString,Ranges>::const_iterator i = lex.lex.getMutes().begin(); i != lex.lex.getMutes().end(); ++i )
        d_mutes.insert(i.key(),i.value());
#endif

    AoModelVisitor v(this);
    v.visit(unit,&p.root);

    QCoreApplication::processEvents();
}

QModelIndex ItemModel::findThing(const ModelItem* slot, const Thing* nt) const
{
    for( int i = 0; i < slot->d_children.size(); i++ )
    {
        ModelItem* s = slot->d_children[i];
        if( s->d_thing == nt )
            return createIndex( i, 0, s );
        QModelIndex index = findThing( s, nt );
        if( index.isValid() )
            return index;
    }
    return QModelIndex();
}

bool ModelItem::lessThan(const ModelItem* lhs, const ModelItem* rhs)
{
    if( lhs->d_thing == 0 || rhs->d_thing == 0 )
        return false;
    return lhs->d_thing->getName().compare(rhs->d_thing->getName(),Qt::CaseInsensitive) < 0;
}

void CodeModel::fillFolders(ModelItem* root, const FileSystem::Dir* super, CodeFolder* top, QList<ModelItem*>& fileSlots)
{
    for( int i = 0; i < super->d_subdirs.size(); i++ )
    {
        CodeFolder* f = new CodeFolder();
        f->d_dir = super->d_subdirs[i];
        top->d_subs.append(f);
        ModelItem* s = new ModelItem(root,f);
        fillFolders(s,super->d_subdirs[i],f,fileSlots);
    }
    for( int i = 0; i < super->d_files.size(); i++ )
    {
        const int t = super->d_files[i]->d_type;
        if( t == FileSystem::OberonModule )
        {
            UnitFile* f = new UnitFile();
            f->d_file = super->d_files[i];
            f->d_folder = top;
            d_map1[f->d_file] = f;
            d_map2[f->d_file->d_realPath] = f;
            top->d_files.append(f);
            ModelItem* s = new ModelItem(root,f);
            fileSlots.append(s);
        }
    }
    std::sort( root->d_children.begin(), root->d_children.end(), ModelItem::lessThan );
}

QString CodeFile::getName() const
{
    return d_file->d_name;
}

QByteArrayList UnitFile::findUses() const
{
    QByteArrayList res;
    if( d_file == 0 || d_file->d_type != FileSystem::OberonModule )
        return res;
    QFile f(d_file->d_realPath);
    f.open(QIODevice::ReadOnly);
    Lexer lex;
    lex.setStream(&f,f.fileName());
    Token t = lex.nextToken();
    while( t.isValid() )
    {
        switch( t.d_type )
        {
        case Tok_IMPORT:
            t = lex.nextToken();
            // TODO
            while( t.isValid() && t.d_type != Tok_Semi )
            {
                if( t.d_type == Tok_Comma )
                {
                    t = lex.nextToken();
                    continue;
                }
                if( t.d_type == Tok_ident )
                {
                    const QByteArray id = t.d_val;
                    t = lex.nextToken();
                    if( t.d_type == Tok_ColonEq )
                    {
                        t = lex.nextToken();
                        if( t.d_type == Tok_ident ) // just to make sure
                        {
                            res.append( t.d_val );
                            t = lex.nextToken();
                        }
                    }else
                        res.append(id);
                }else
                    t = lex.nextToken();
            }
            return res;
        }
        t = lex.nextToken();
    }
    return res;
}

UnitFile::~UnitFile()
{
    if( d_body )
        delete d_body;
    QHash<QString,SymList>::const_iterator j;
    for( j = d_syms.begin(); j != d_syms.end(); ++j )
        for( int i = 0; i < j.value().size(); i++ )
            delete j.value()[i];
}

UnitFile*Scope::getUnitFile() const
{
    if( d_owner == 0 )
        return 0; // happens in global scope
    if( d_owner->d_kind == Thing::Unit )
        return static_cast<UnitFile*>(d_owner);
    else if( d_owner->isDeclaration() )
    {
        Declaration* d = static_cast<Declaration*>(d_owner);
        Q_ASSERT( d->d_owner != 0 );
        return d->d_owner->getUnitFile();
    }else
        return 0; // happens in assembler
}

Declaration*Scope::findDecl(const char* id, bool useAlt) const
{
    foreach( Declaration* d, d_order )
    {
        if( d->d_name.constData() == id )
            return d;
    }
    if( d_outer )
    {
        Declaration* d = d_outer->findDecl(id, false);
        if( d )
            return d;
    }
    if( d_altOuter && useAlt )
        return d_altOuter->findDecl(id, false);

    return 0;
}

void Scope::clear()
{
    for( int i = 0; i < d_order.size(); i++ )
        delete d_order[i];
    d_order.clear();
}

Scope::~Scope()
{
    clear();
}

QString Declaration::getName() const
{
    return d_name;
}

UnitFile*Declaration::getUnitFile() const
{
    Q_ASSERT( d_owner != 0 );
    return d_owner->getUnitFile();
}

Declaration::~Declaration()
{
    if( d_body && d_kind != Import )
        delete d_body;
}

QString CodeFolder::getName() const
{
    return d_dir->d_name;
}

void CodeFolder::clear()
{
    for( int i = 0; i < d_subs.size(); i++ )
    {
        d_subs[i]->clear();
        delete d_subs[i];
    }
    d_subs.clear();
    for( int i = 0; i < d_files.size(); i++ )
        delete d_files[i];
    d_files.clear();
}

QString Thing::getName() const
{
    return QString();
}

const char*Thing::typeName() const
{
    switch( d_kind )
    {
    case Const:
        return "Const";
    case TypeDecl:
        return "Type";
    case Var:
        return "Var";
    case Proc:
        return "Procedure";
    case Module:
        return "Module";
    case Param:
        return "Parameter";
    case Field:
        return "Field";
    case Attribute:
        return "Attribute";
    case Import:
        return "Import";
    case Self:
        return "SELF";
    default:
        return "";
    }
}

Thing::~Thing()
{
}

UnitFile*CodeFile::toUnit()
{
    if( d_kind == Unit )
        return static_cast<UnitFile*>(this);
    else
        return 0;
}

CodeFile::~CodeFile()
{
}

Type::~Type()
{
    if( d_members )
        delete d_members;
}


ModuleDetailMdl::ModuleDetailMdl(QObject* parent)
{

}

void ModuleDetailMdl::load(Scope* impl)
{
    if( d_impl == impl )
        return;

    beginResetModel();
    d_root = ModelItem();
    d_impl = impl;

    if( impl )
        fillItems(&d_root, impl);

    endResetModel();
}

QVariant ModuleDetailMdl::data(const QModelIndex& index, int role) const
{
    ModelItem* s = static_cast<ModelItem*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        {
            QString str;
            if( s->d_thing->d_active || s->d_thing->d_exclusive )
                str += "! ";
            str += s->d_thing->getName();
            return str;
        }
        break;
    case Qt::DecorationRole:
        switch( s->d_thing->d_kind )
        {
        case Thing::TypeDecl:
            {
                Declaration* d = static_cast<Declaration*>(s->d_thing);
                if( d->d_type.data() )
                {
                    switch( d->d_type->d_kind )
                    {
                    case Type::Pointer:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/pointer_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/pointer.png");
                        }
                        break;
                    case Type::Array:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/array_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/array.png");
                        }
                        break;
                    case Type::Record:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/struct_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/struct.png");
                        }
                        break;
                    case Type::Class:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/class_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/class.png");
                        }
                        break;
                    case Type::Procedural:
                        switch( s->d_thing->d_visi )
                        {
                        case Thing::NA:
                            return QPixmap(":/images/alias_priv.png");
                        case Thing::Public:
                            return QPixmap(":/images/alias.png");
                        }
                        break;
                    }
                }
            }
            break;
        case Thing::Var:
        case Thing::Field:
            switch( s->d_thing->d_visi )
            {
            case Thing::NA:
                return QPixmap(":/images/var_priv.png");
            case Thing::ReadOnly:
                return QPixmap(":/images/var_prot.png");
            case Thing::Public:
                return QPixmap(":/images/var.png");
            }
            break;
        case Thing::Proc:
            switch( s->d_thing->d_visi )
            {
            case Thing::NA:
                return QPixmap(":/images/func_priv.png");
            case Thing::Public:
                return QPixmap(":/images/func.png");
            }
            break;
        case Thing::Import:
            return QPixmap(":/images/module.png");
        }
        break;
    }
    return QVariant();
}

static bool DeclLessThan(Declaration* lhs, Declaration* rhs)
{
    return lhs->getName().compare(rhs->getName(),Qt::CaseInsensitive) < 0;
}

void ModuleDetailMdl::fillItems(ModelItem* parentItem, Scope* scope)
{
    QList<Declaration*> decls;
    foreach( Declaration* d, scope->d_order )
    {
        if( d->d_kind > Thing::Const && d->d_kind < Thing::Attribute )
            decls.append(d);
    }
    std::sort( decls.begin(), decls.end(), DeclLessThan );
    for( int j = 0; j < decls.size(); j++ )
    {
        Declaration* dd = decls[j];
        ModelItem* item = new ModelItem(parentItem,dd);
        if( dd->d_kind == Thing::TypeDecl && dd->d_type && dd->d_type->d_kind == Type::Class )
            fillSubs(item,dd->d_type->d_members);
    }
}

void ModuleDetailMdl::fillSubs(ModelItem* parentItem, Scope* scope)
{
    QList<Declaration*> funcs;
    foreach( Declaration* d, scope->d_order )
    {
        if( d->d_kind == Thing::Proc )
            funcs.append(d);
    }
    std::sort( funcs.begin(), funcs.end(), DeclLessThan );
    for( int j = 0; j < funcs.size(); j++ )
        new ModelItem(parentItem,funcs[j]);
}

