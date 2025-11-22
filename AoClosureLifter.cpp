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

#include "AoClosureLifter.h".h"
#include <QtAlgorithms>
using namespace Ao;
using namespace Ast;

struct ClosureLifter::DeclLess {
    explicit DeclLess(const ClosureLifter* self) : self_(self) { }
    bool operator()(Declaration* a, Declaration* b) const {
        if (a == b) return false;
        const int da = self_->depthOfOwner(a);
        const int db = self_->depthOfOwner(b);
        if (da != db) return da < db;
        const QByteArray an = a ? a->name : QByteArray();
        const QByteArray bn = b ? b->name : QByteArray();
        return an < bn;
    }
    const ClosureLifter* self_;
};

bool ClosureLifter::analyze(Declaration* module)
{
    plans_.clear();
    procIndex_.clear();
    procList_.clear();
    pathStack_.clear();
    if (!module || module->kind != Declaration::Module || !module->validated)
        return false;

    moduleName = module->name;

    // Seed path with module name.
    pathStack_.append(module->name);

    // 1) Index all procedures and gather per-proc basics.
    for (Declaration* d = module->link; d; d = d->next) {
        if (d->kind == Declaration::Procedure)
            indexProcedure(d);
    }

    // 2) Compute direct free-variable captures (i.e. non-local accesses) per proc.
    for (int i = 0; i < procList_.size(); ++i) {
        Declaration* p = procList_[i];
        ProcState& st = procIndex_[p];
        st.directFree = findDirectFree(p);
        st.required = st.directFree;
    }

    // 3) Build direct call graph among nested procedures (by DeclRef).
    for (int i = 0; i < procList_.size(); ++i) {
        Declaration* p = procList_[i];
        ProcState& st = procIndex_[p];
        st.callees = findDirectCallees(p);
    }

    // 4) Fixpoint propagate required declarations along calls, stopping at owners.
    bool changed = true;
    while (changed) {
        changed = false;
        for (int i = 0; i < procList_.size(); ++i) {
            Declaration* p = procList_[i];
            ProcState& sp = procIndex_[p];
            QSet<Declaration*> acc = sp.required;
            for (int j = 0; j < sp.callees.size(); ++j) {
                Declaration* q = sp.callees[j];
                ProcState& sq = procIndex_[q];
                // Add requirements of q that p does not own.
                QSet<Declaration*> toAdd = sq.required;
                stripOwned(p, toAdd);
                acc.unite(toAdd);
            }
            if (acc != sp.required) {
                sp.required = acc;
                changed = true;
            }
        }
    }

    // 5) Emit plans for procs that need to accept/forward anything or capture directly.
    for (int i = 0; i < procList_.size(); ++i) {
        Declaration* p = procList_[i];
        ProcState& st = procIndex_[p];

        // Do not add parameters at the owner scope itself for its own locals/params.
        QSet<Declaration*> reqForParams = st.required;
        stripOwned(p, reqForParams);

        if (reqForParams.isEmpty()) {
            // If truly nothing to accept or forward, skip (still nested but no need to change).
            continue;
        }

        ProcPlan plan;
        plan.proc = p;
        plan.path = buildPathFor(p);
        plan.outerProcs = st.outers;
        plan.directFree = st.directFree;
        plan.required = st.required;
        plan.callees = st.callees;

        // Name collision avoidance: parameters and locals of p.
        QSet<QByteArray> taken = existingParamNames(p);
        QSet<QByteArray> locals = existingLocalNames(p);
        taken.unite(locals);

        // Stable order: by declaring-proc depth, then by name.
        QList<Declaration*> ordered = reqForParams.toList();
        qSort(ordered.begin(), ordered.end(), DeclLess(this));

        for (int k = 0; k < ordered.size(); ++k) {
            Declaration* d = ordered[k];
            LiftParam lp;
            lp.sourceDecl = d;
            lp.sourceProc = ownerProc(d);
            lp.originalName = d ? d->name : QByteArray();
            lp.name = lp.originalName;
            lp.renamed = false;
            int ctr = 1;
            while (taken.contains(lp.name)) {
                lp.name = lp.originalName + '_' + QByteArray::number(ctr++);
                lp.renamed = true;
            }
            taken.insert(lp.name);
            plan.addedParams.append(lp);
        }

        plans_.append(plan);
    }

    // Pop module name.
    pathStack_.removeLast();
    return true;
}

void ClosureLifter::printPlans(QTextStream& out) const
{
    if (plans_.isEmpty()) {
        // out << "No nested procedures requiring closure parameters found.\n";
        return;
    }

    out << "Module " << moduleName << ": found " << plans_.size() << " nested procedures requiring closure parameters:\n\n";
    for (int i = 0; i < plans_.size(); ++i) {
        const ProcPlan& pp = plans_[i];
        out << (i+1) << ". Proc: " << pp.proc->name << "\n";
        out << "   Path: " << join(pp.path) << "\n";
        out << "   Nesting depth: " << pp.outerProcs.size() << "\n";
        out << "   Additional VAR parameters:\n";
        for (int j = 0; j < pp.addedParams.size(); ++j) {
            const LiftParam& p = pp.addedParams[j];
            out << "     " << (j+1) << ") " << p.name;
            if (p.renamed) out << " (renamed from " << p.originalName << ")";
            out << " <- " << p.originalName << " from "
                << (p.sourceProc ? p.sourceProc->name : QByteArray("<module>")) << "\n";
        }
        // Optional: show callees that caused forwarding
        if (!pp.callees.isEmpty()) {
            out << "   For callees:";
            for (int c = 0; c < pp.callees.size(); ++c) {
                out << (c ? "," : "") << ' ' << pp.callees[c]->name;
            }
            out << "\n";
        }
        out << "\n";
    }
}

const ClosureLifter::ProcPlan* ClosureLifter::plan(Declaration * module) const
{
    for(int i = 0; i < plans_.size(); i++ )
    {
        if( plans_[i].proc == module )
            return &plans_[i];
    }
    return 0;
}

void ClosureLifter::indexProcedure(Declaration* proc)
{
    if (!proc || proc->kind != Declaration::Procedure) return;

    // Enter stack.
    procStack_.append(proc);
    pathStack_.append(proc->name);

    // Register state if first time.
    if (!procIndex_.contains(proc)) {
        ProcState st;
        st.path = pathStack_;
        st.outers = procStack_.mid(0, procStack_.size()-1); // exclude self
        procIndex_.insert(proc, st);
        procList_.append(proc);
    }

    // Recurse into nested procedures declared within this procedure.
    for (Declaration* d = proc->link; d; d = d->next) {
        if (d->kind == Declaration::Procedure)
            indexProcedure(d);
    }

    // Leave stack.
    pathStack_.removeLast();
    procStack_.removeLast();
}

QSet<Declaration*> ClosureLifter::findDirectFree(Declaration* proc) const
{
    QSet<Declaration*> acc;
    if (!proc || !proc->body)
        return acc;
    scanStmt(proc->body, proc, acc);
    return acc;
}

void ClosureLifter::scanStmt(Statement* s, Declaration* currentProc, QSet<Declaration*>& acc) const
{
    while (s) {
        if (s->lhs) scanExpr(s->lhs, currentProc, acc);
        if (s->rhs) scanExpr(s->rhs, currentProc, acc);
        if (s->body) scanStmt(s->body, currentProc, acc);
        s = s->getNext();
    }
}

void ClosureLifter::scanExpr(Expression* e, Declaration* currentProc, QSet<Declaration*>& acc) const
{
    if (!e) return;

    if (e->kind == Expression::DeclRef) {
        Declaration* d = qvariant_cast<Declaration*>(e->val);
        if (d && isFreeVarUse(d, currentProc)) {
            acc.insert(d);
        }
    }

    // Calls: nothing special here; callees are collected separately.
    if (e->lhs)  scanExpr(e->lhs,  currentProc, acc);
    if (e->rhs)  scanExpr(e->rhs,  currentProc, acc);
    if (e->next) scanExpr(e->next, currentProc, acc);
}

bool ClosureLifter::isFreeVarUse(Declaration* d, Declaration* currentProc) const
{
    if (!d || !currentProc) return false;
    if (d->kind != Declaration::VarDecl &&
            d->kind != Declaration::LocalDecl &&
            d->kind != Declaration::ParamDecl) return false;

    Declaration* owner = ownerProc(d);
    if (!owner) return false;               // module-level → ignore
    if (owner == currentProc) return false; // not free for current proc
    return isAncestor(owner, currentProc);
}

Declaration* ClosureLifter::ownerProc(Declaration* d) const
{
    for (Declaration* o = d ? d->outer : 0; o; o = o->outer) {
        if (o->kind == Declaration::Procedure) return o;
    }
    return 0;
}

bool ClosureLifter::isAncestor(Declaration* anc, Declaration* desc) const
{
    for (Declaration* o = desc ? desc->outer : 0; o; o = o->outer) {
        if (o == anc) return true;
    }
    return false;
}

QList<Declaration*> ClosureLifter::findDirectCallees(Declaration* proc) const
{
    QList<Declaration*> out;
    if (!proc || !proc->body) return out;
    QSet<Declaration*> seen;
    collectCallees(proc->body, out, seen);
    return out;
}

void ClosureLifter::collectCallees(Statement* s, QList<Declaration*>& out, QSet<Declaration*>& seen) const
{
    while (s) {
        collectCalleesExpr(s->lhs, out, seen);
        collectCalleesExpr(s->rhs, out, seen);
        if (s->body) collectCallees(s->body, out, seen);
        s = s->getNext();
    }
}

void ClosureLifter::collectCalleesExpr(Expression* e, QList<Declaration*>& out, QSet<Declaration*>& seen) const
{
    if (!e) return;
    if (e->kind == Expression::Call) {
        // Super call: target in e->lhs->lhs
        Expression* tgt = e->lhs;
        if (tgt && tgt->kind == Expression::Super) tgt = tgt->lhs;

        Declaration* callee = 0;
        if (tgt && (tgt->kind == Expression::DeclRef || tgt->kind == Expression::Select)) {
            callee = qvariant_cast<Declaration*>(tgt->val);
            if (callee && callee->kind == Declaration::Procedure) {
                if (!seen.contains(callee)) {
                    seen.insert(callee);
                    out.append(callee);
                }
            }
        }
    }
    if (e->lhs)  collectCalleesExpr(e->lhs,  out, seen);
    if (e->rhs)  collectCalleesExpr(e->rhs,  out, seen);
    if (e->next) collectCalleesExpr(e->next, out, seen);
}

void ClosureLifter::stripOwned(Declaration* p, QSet<Declaration*>& s) const
{
    QSet<Declaration*> toRemove;
    for (QSet<Declaration*>::const_iterator it = s.begin(); it != s.end(); ++it) {
        Declaration* d = *it;
        if (ownerProc(d) == p) toRemove.insert(d);
    }
    for (QSet<Declaration*>::const_iterator it = toRemove.begin(); it != toRemove.end(); ++it) {
        s.remove(*it);
    }
}

QSet<QByteArray> ClosureLifter::existingParamNames(Declaration* proc) const
{
    QSet<QByteArray> names;
    if (!proc || proc->kind != Declaration::Procedure) return names;
    QList<Declaration*> params = proc->getParams(/*skipReceiver*/false);
    for (int i = 0; i < params.size(); ++i) names.insert(params[i]->name);
    return names;
}

QSet<QByteArray> ClosureLifter::existingLocalNames(Declaration* proc) const
{
    QSet<QByteArray> names;
    for (Declaration* d = proc ? proc->link : 0; d; d = d->next) {
        if (d->kind == Declaration::LocalDecl) names.insert(d->name);
    }
    return names;
}

QList<QByteArray> ClosureLifter::buildPathFor(Declaration* proc) const
{
    QList<QByteArray> path;
    // Recompute by walking up outers to module for resilience.
    QList<QByteArray> rev;
    for (Declaration* o = proc; o; o = o->outer) {
        if (o->kind == Declaration::Module || o->kind == Declaration::Procedure) {
            rev.append(o->name);
        }
    }
    for (int i = rev.size() - 1; i >= 0; --i) path.append(rev[i]);
    return path;
}

QByteArray ClosureLifter::join(const QList<QByteArray>& path)
{
    if (path.isEmpty()) return QByteArray();
    QByteArray s = path[0];
    for (int i = 1; i < path.size(); ++i) { s += '.'; s += path[i]; }
    return s;
}

int ClosureLifter::depthOfOwner(Declaration* d) const
{
    Declaration* o = ownerProc(d);
    int depth = 0;
    for (Declaration* x = o ? o->outer : 0; x; x = x->outer) {
        if (x->kind == Declaration::Procedure) ++depth;
    }
    return depth;
}


const ClosureLifter::LiftParam *ClosureLifter::ProcPlan::findFromSourceDecl(Declaration *sourceDecl) const
{
    foreach( const LiftParam& p, addedParams )
        if( p.sourceDecl == sourceDecl )
            return &p;
    return 0;
}
