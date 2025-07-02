#ifndef AOVALIDATOR_H
#define AOVALIDATOR_H

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

#include "AoAst.h"

namespace Ao
{
    class Importer {
    public:
        virtual Ast::Declaration* loadModule( const Ast::Import& imp ) = 0;
    };

    class Validator
    {
    public:
        Validator(Ast::AstModel* mdl, Importer* imp = 0, bool haveXref = false);
        ~Validator();

        bool validate(Ast::Declaration* module, const Ast::Import& import = Ast::Import());

        Ast::Xref takeXref();

        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };
        mutable QList<Error> errors;
    protected:
        void error( const RowCol&, const QString& msg ) const;
        void markDecl(Ast::Declaration* d);
        Ast::Symbol* markRef(Ast::Declaration* d, const RowCol& pos);
        Ast::Symbol* markUnref(int len, const RowCol& pos);
        Ast::Type* deref(Ast::Type* t);
        void resolveIfNamedType(Ast::Type* nameRef);
        void resolveDesig(Ast::Expression* nameRef);
        typedef QPair<Ast::Declaration*,Ast::Declaration*> ResolvedQ;
        ResolvedQ find(const Ast::Qualident& q, RowCol pos);
        Ast::Declaration* findInType(Ast::Type*, const QByteArray& field);
        bool checkBuiltinArgs(quint8 builtin, const Ast::ExpList& args, Ast::Type** ret, const RowCol& pos);

        void visitScope( Ast::Declaration* scope );
        void visitDecl( Ast::Declaration* );
        void visitImport(Ast::Declaration* import);
        void visitBody(Ast::Statement* body);
        void visitExpr(Ast::Expression*, bool followNext = true);
        void unaryOp(Ast::Expression*);
        void binaryOp(Ast::Expression*);
        void selectOp(Ast::Expression*);
        void derefOp(Ast::Expression*);
        void indexOp(Ast::Expression*);
        void callOp(Ast::Expression*);
        void constructor(Ast::Expression*);
        void visitType(Ast::Type*);

    private:
        Ast::AstModel* mdl;
        Importer* imp;
        Ast::Declaration* module;
        QList<Ast::Declaration*> scopeStack, boundProcs;
        Ast::Declaration* curObjectTypeDecl;
        Ast::Symbol* first;
        Ast::Symbol* last;
        QHash<Ast::Declaration*,Ast::SymList> xref;
        QHash<Ast::Declaration*,Ast::DeclList> subs;
    };
}

#endif // AOVALIDATOR_H
