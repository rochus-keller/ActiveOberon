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

#include "AoComments.h"
using namespace Ao;
using namespace Ao::Ast;

static inline bool isBefore(const RowCol& lhs, const RowCol& rhs)
{
    if( lhs.d_row != rhs.d_row )
        return lhs.d_row < rhs.d_row;
    else
        return lhs.d_col < rhs.d_col;
}

void CommentTable::add(const Token& t)
{
    Comment c;
    c.pos = t.toRowCol();
    c.text = t.d_val;
    c.endRow = c.pos.d_row + c.text.count('\n');
    add(c);
}

void CommentTable::add(const Comment& c)
{
    all.append(c);
}

void CommentTable::skipUsed()
{
    while( first < all.size() && all[first].used )
        first++;
}

QList<Comment*> CommentTable::takeBefore(const RowCol& pos)
{
    // all unused comments starting before pos
    QList<Comment*> res;
    skipUsed();
    for( int i = first; i < all.size(); i++ )
    {
        if( !isBefore(all[i].pos, pos) )
            break;
        if( all[i].used )
            continue;
        all[i].used = true;
        res.append(&all[i]);
    }
    skipUsed();
    return res;
}

QList<Comment*> CommentTable::takeUntil(quint32 row)
{
    // all unused comments starting on or before row
    QList<Comment*> res;
    skipUsed();
    for( int i = first; i < all.size(); i++ )
    {
        if( all[i].pos.d_row > row )
            break;
        if( all[i].used )
            continue;
        all[i].used = true;
        res.append(&all[i]);
    }
    skipUsed();
    return res;
}

Comment* CommentTable::takeTrailing(quint32 row)
{
    // the unused comment on row which follows code on the same row if any
    skipUsed();
    for( int i = first; i < all.size(); i++ )
    {
        if( all[i].pos.d_row > row )
            break;
        if( all[i].used || all[i].pos.d_row != row || all[i].ownLine )
            continue;
        all[i].used = true;
        Comment* res = &all[i];
        skipUsed();
        return res;
    }
    return 0;
}

QList<Comment*> CommentTable::takeAll()
{
    // everything not yet used
    QList<Comment*> res;
    skipUsed();
    for( int i = first; i < all.size(); i++ )
    {
        if( all[i].used )
            continue;
        all[i].used = true;
        res.append(&all[i]);
    }
    first = all.size();
    return res;
}

bool CommentTable::hasBefore(const RowCol& pos) const
{
    const Comment* c = peekFirstUnused();
    return c && isBefore(c->pos, pos);
}

bool CommentTable::hasTrailing(quint32 row) const
{
    for( int i = first; i < all.size(); i++ )
    {
        if( all[i].pos.d_row > row )
            break;
        if( !all[i].used && all[i].pos.d_row == row && !all[i].ownLine )
            return true;
    }
    return false;
}

const Comment* CommentTable::peekFirstUnused() const
{
    for( int i = first; i < all.size(); i++ )
    {
        if( !all[i].used )
            return &all[i];
    }
    return 0;
}

CommentScanner::CommentScanner(Ast::CommentTable* t):table(t),lastRow(0)
{
    lex.setIgnoreComments(false);
    lex.setPackComments(true);
}

void CommentScanner::setStream(const QByteArray& code, const QString& sourcePath)
{
    path = sourcePath;
    lex.setStream(code, sourcePath);
}

bool CommentScanner::setStream(const QString& sourcePath)
{
    path = sourcePath;
    return lex.setStream(sourcePath);
}

Token CommentScanner::fetch()
{
    Token t = lex.nextToken();
    while( t.d_type == Tok_Comment )
    {
        if( table )
        {
            Comment c;
            c.pos = t.toRowCol();
            c.text = t.d_val;
            c.endRow = c.pos.d_row + c.text.count('\n');
            c.ownLine = t.d_lineNr != lastRow;
            table->add(c);
        }
        t = lex.nextToken();
    }
    if( !t.isEof() )
        lastRow = t.d_lineNr;
    return t;
}

Token CommentScanner::next()
{
    if( !buffer.isEmpty() )
        return buffer.takeFirst();
    return fetch();
}

Token CommentScanner::peek(int offset)
{
    if( offset < 1 )
        offset = 1;
    while( buffer.size() < offset )
        buffer.append(fetch());
    return buffer[offset-1];
}
