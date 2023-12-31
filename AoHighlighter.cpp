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


// Adopted from https://github.com/rochus-keller/LisaPascal/

#include "AoHighlighter.h"
#include "AoLexer.h"
#include <QBuffer>
using namespace Ao;

OberonPainter::OberonPainter(QObject* parent) :
    QSyntaxHighlighter(parent)
{
    for( int i = 0; i < C_Max; i++ )
    {
        d_format[i].setFontWeight(QFont::Normal);
        d_format[i].setForeground(Qt::black);
        d_format[i].setBackground(Qt::transparent);
    }
    d_format[C_Num].setForeground(QColor(0, 153, 153));
    d_format[C_Str].setForeground(QColor(208, 16, 64));
    d_format[C_Cmt].setForeground(QColor(153, 153, 136));
    d_format[C_Kw].setForeground(QColor(68, 85, 136));
    d_format[C_Kw].setFontWeight(QFont::Bold);
    d_format[C_Op].setForeground(QColor(153, 0, 0));
    d_format[C_Op].setFontWeight(QFont::Bold);
    d_format[C_Type].setForeground(QColor(153, 0, 115));
    d_format[C_Type].setFontWeight(QFont::Bold);
    d_format[C_Asm].setForeground(QColor(0, 128, 0));
}

void OberonPainter::addBuiltIn(const QByteArray& bi)
{
    d_builtins << bi;
}

void OberonPainter::addKeyword(const QByteArray& kw)
{
    d_keywords << kw;
}

void OberonPainter::addBuiltIns()
{
    const char** str = s_builtInTypes;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }
    str = s_builtInConsts;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }
    str = s_builtInProcs;
    while(*str)
    {
        d_builtins << *str;
        str++;
    }

}

QTextCharFormat OberonPainter::formatForCategory(int c) const
{
    return d_format[c];
}

void OberonPainter::highlightBlock(const QString& text)
{
    const int previousBlockState_ = previousBlockState();
    int lexerState = 0, initialBraceDepth = 0;
    if (previousBlockState_ != -1) {
        lexerState = previousBlockState_ & 0xff;
        initialBraceDepth = previousBlockState_ >> 8;
    }

    int braceDepth = initialBraceDepth;

    // Protocol:
    // lexerState == 1: multi-line (* *) comment
    // lexerState == 2: multi-line CODE END region

    int start = 0;
    if( lexerState == 1 )
    {
        QTextCharFormat f = formatForCategory( C_Cmt );
        int pos = text.indexOf("*)");
        if( pos == -1 )
        {
            // the whole block ist part of comment
            setFormat( start, text.size(), f );
            setCurrentBlockState( (braceDepth << 8) | lexerState);
            return;
        }else
        {
            // End of comment found
            pos += 2;
            setFormat( start, pos , f );
            lexerState = 0;
            braceDepth--;
            start = pos;
        }
    }else if( lexerState == 2 )
    {
        QTextCharFormat f = formatForCategory(C_Asm);
        int pos = text.indexOf("END");
        if( pos == -1 )
        {
            // the whole block ist part of comment
            setFormat( start, text.size(), f );
            setCurrentBlockState( (braceDepth << 8) | lexerState);
            return;
        }else
        {
            // End of section found
            pos += 0; // END is not part of it
            setFormat( start, pos , f );
            lexerState = 0;
            braceDepth--;
            start = pos;
        }
    }

    Lexer lex;
    lex.setIgnoreComments(false);
    lex.setPackComments(false);

    QList<Token> tokens = lex.tokens(text.mid(start));
    for( int i = 0; i < tokens.size(); ++i )
    {
        Token &t = tokens[i];
        t.d_colNr += start;

        QTextCharFormat f;
        if( t.d_type == Tok_Latt )
        {
            braceDepth++;
            f = formatForCategory(C_Cmt);
            lexerState = 1;
        }else if( t.d_type == Tok_Ratt )
        {
            braceDepth--;
            f = formatForCategory(C_Cmt);
            lexerState = 0;
        }else if( t.d_type == Tok_string || t.d_type == Tok_hexchar )
            f = formatForCategory(C_Str);
        else if( t.d_type == Tok_integer || t.d_type == Tok_real )
            f = formatForCategory(C_Num);
        else if( tokenTypeIsLiteral(t.d_type) )
        {
            f = formatForCategory(C_Op);
        }else if( tokenTypeIsKeyword(t.d_type) )
        {
            if( t.d_type == Tok_CODE )
            {
                const int pos = text.indexOf('END', t.d_colNr);
                if( pos == -1 )
                {
                    braceDepth++;
                    lexerState = 2;
                }
                setFormat( t.d_colNr + 4, t.d_val.size()-4, formatForCategory(C_Asm) );
                t.d_val = QByteArray();
                t.d_len = 4;
            }
            f = formatForCategory(C_Kw);
        }else if( t.d_type == Tok_ident )
        {
            if( d_builtins.contains(t.d_val) )
                f = formatForCategory(C_Type);
            else if( d_keywords.contains(t.d_val) )
                f = formatForCategory(C_Kw);
            else
                f = formatForCategory(C_Ident);
        }

        if( f.isValid() )
        {
            const int len = t.d_val.isEmpty() ? t.d_len : t.d_val.size();
            setFormat( t.d_colNr-1, len, f );
        }
    }

    setCurrentBlockState((braceDepth << 8) | lexerState );
}

LogPainter::LogPainter(QTextDocument* parent):QSyntaxHighlighter(parent)
{

}

void LogPainter::highlightBlock(const QString& text)
{
    QColor c = Qt::black;
    if( text.startsWith("WRN:") )
        c = Qt::blue;
    else if( text.startsWith("ERR:") )
        c = Qt::red;

    setFormat( 0, text.size(), c );
}

const char* OberonPainter::s_builtInTypes[] = {
    "BOOLEAN",
    "CHAR",
    "INTEGER",
    "LONGREAL",
    "REAL",
    "SHORTINT",
    "SET",
    "LONGINT",
    "HUGEINT",
    "PTR",
    0
};

const char* OberonPainter::s_builtInConsts[] = {
    "FALSE",
    "TRUE",
    "NIL",
    0
};

const char* OberonPainter::s_builtInProcs[] = {
    "ABS",
    "LEN",
    "ASH",
    "LONG",
    "CAP",
    "MAX",
    "CHR",
    "MIN",
    "COPY",
    "NEW",
    "DEC",
    "ODD",
    "ENTIER",
    "ORD",
    "EXCL",
    "SHORT",
    "INC",
    "INCL",
    "SIZE",
    "ASSERT",
    0
};
