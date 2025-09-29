// ClosureLifter.h
#ifndef CLOSURE_LIFTER_H
#define CLOSURE_LIFTER_H

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

// Produces lambda-lifting parameter plans for nested procedures, including intermediates.

#include "AoAst.h"
#include <QList>
#include <QVector>
#include <QSet>
#include <QHash>
#include <QByteArray>
#include <QTextStream>

namespace Ao { namespace Ast {

// Lambda-lifting analyzer (per module).
class ClosureLifter {
public:
    struct LiftParam {
        QByteArray   name;          // chosen name (may be renamed)
        QByteArray   originalName;  // source identifier
        Declaration* sourceProc;    // declaring procedure (owner of the var/param/local)
        Declaration* sourceDecl;    // the captured declaration
        bool         renamed;       // true if name had to be changed
    };

    struct ProcPlan {
        Declaration*          proc;           // this nested procedure
        QList<QByteArray>     path;           // module → … → proc
        QList<LiftParam>      addedParams;    // VAR-form parameters to add (ordered)
        QSet<Declaration*>    required;       // set of required declarations (fixpoint)
        QSet<Declaration*>    directFree;     // direct free variables used in body
        QList<Declaration*>   outerProcs;     // lexical ancestors (outermost first)
        QList<Declaration*>   callees;        // direct nested-proc callees (by DeclRef)
    };


    ClosureLifter() { }

    // Analyze a module and return true on success
    bool analyze(Declaration* module);

    // Pretty print resulting plans.
    void printPlans(QTextStream& out) const;

    // return plans for all nested procedures that need to accept
    // and/or forward extra VAR parameters (including intermediates).
    const QVector<ProcPlan>& plans() const { return plans_; }

private:
    // Internal per-procedure state during analysis.
    struct ProcState {
        QList<QByteArray>   path;
        QList<Declaration*> outers;
        QSet<Declaration*>  directFree;
        QSet<Declaration*>  required;
        QList<Declaration*> callees;
    };

    // Step 1: index a procedure and recurse into nested ones.
    void indexProcedure(Declaration* proc);

    // Scan body for direct free variables (outer locals/params) used by proc.
    QSet<Declaration*> findDirectFree(Declaration* proc) const;

    void scanStmt(Statement* s, Declaration* currentProc, QSet<Declaration*>& acc) const;
    void scanExpr(Expression* e, Declaration* currentProc, QSet<Declaration*>& acc) const;

    // A declaration use in currentProc is a free variable if it is a var/local/param
    // whose owner is an ancestor procedure of currentProc (and not currentProc itself).
    bool isFreeVarUse(Declaration* d, Declaration* currentProc) const;

    // Find owner procedure that declares d.
    Declaration* ownerProc(Declaration* d) const;

    bool isAncestor(Declaration* anc, Declaration* desc) const;

    // Build list of direct nested-proc callees by scanning calls whose lhs resolves to a proc decl.
    QList<Declaration*> findDirectCallees(Declaration* proc) const;

    void collectCallees(Statement* s, QList<Declaration*>& out, QSet<Declaration*>& seen) const;

    void collectCalleesExpr(Expression* e, QList<Declaration*>& out, QSet<Declaration*>& seen) const;

    // Remove declarations owned by p (p provides actuals, no param needed for own locals/params).
    void stripOwned(Declaration* p, QSet<Declaration*>& s) const;

    QSet<QByteArray> existingParamNames(Declaration* proc) const;
    QSet<QByteArray> existingLocalNames(Declaration* proc) const;

    QList<QByteArray> buildPathFor(Declaration* proc) const;

    static QByteArray join(const QList<QByteArray>& path);

    struct DeclLess;
    int depthOfOwner(Declaration* d) const;
private:
    // State
    QVector<ProcPlan>                plans_;
    QHash<Declaration*, ProcState>   procIndex_;
    QList<Declaration*>              procList_;
    QList<Declaration*>              procStack_;
    QList<QByteArray>                pathStack_;
    QByteArray                       moduleName;
};

}} // namespace Ao::Ast

#endif // CLOSURE_LIFTER_H
