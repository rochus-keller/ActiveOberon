#ifndef AOCOMMENTS_H
#define AOCOMMENTS_H

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

// Comments are not part of the AST; they are collected per module while lexing and
// associated with AST nodes by source position when the code is generated.

#include <ActiveOberon/AoParser2.h>
#include <ActiveOberon/AoLexer.h>

namespace Ao
{
namespace Ast
{
    struct Comment
    {
        RowCol pos; // opening '(*'
        quint32 endRow; // row of the closing '*)'
        QByteArray text; // verbatim source text including delimiters
        bool ownLine; // there is no code before the comment on its first row
        bool used;

        Comment():endRow(0),ownLine(false),used(false){}
        bool isMultiLine() const { return endRow > pos.d_row; }
    };

    class CommentTable
    {
    public:
        // All comments of one module, ordered by position. The generator walks the AST in
        CommentTable():first(0){}

        void add(const Token&); // expects a Tok_Comment, in increasing position order
        void add(const Comment&);

        bool isEmpty() const { return all.isEmpty(); }
        int count() const { return all.size(); }
        const QList<Comment>& comments() const { return all; }


        QList<Comment*> takeBefore(const RowCol& pos);  
        QList<Comment*> takeUntil(quint32 row);
        Comment* takeTrailing(quint32 row);
        QList<Comment*> takeAll();

        // look ahead without consuming
        const Comment* peekFirstUnused() const;
        bool hasBefore(const RowCol& pos) const;
        bool hasTrailing(quint32 row) const;
    private:
        void skipUsed();
        QList<Comment> all;
        int first; // index of the first potentially unused comment
    };
}

    class CommentScanner : public Scanner2
    {
    public:
        // collects the comments on the way; the parser never sees a comment token
        CommentScanner(Ast::CommentTable* = 0);

        void setTable(Ast::CommentTable* t) { table = t; }
        void setStream(const QByteArray& code, const QString& sourcePath);
        bool setStream(const QString& sourcePath);

        Token next();
        Token peek(int offset);
        QString source() const { return path; }
    private:
        Token fetch();
        Lexer lex;
        QList<Token> buffer; // lookahead without comments
        QString path;
        Ast::CommentTable* table;
        quint32 lastRow;
    };
}

#endif // AOCOMMENTS_H
