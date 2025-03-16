#ifndef LISAHIGHLIGHTER_H
#define LISAHIGHLIGHTER_H

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

#include <QSyntaxHighlighter>
#include <QSet>

namespace Ao
{
    class OberonPainter2 : public QSyntaxHighlighter
    {
    public:
        enum { TokenProp = QTextFormat::UserProperty };
        explicit OberonPainter2(QObject *parent = 0);
        void addBuiltIn(const QByteArray& bi);
        void addKeyword(const QByteArray& kw);
        void addBuiltIns();
    protected:
        QTextCharFormat formatForCategory(int) const;

        // overrides
        void highlightBlock(const QString &text);

    private:
        enum Category { C_Num, C_Str, C_Kw, C_Type, C_Ident, C_Op, C_Asm, C_Cmt, C_Max };
        QTextCharFormat d_format[C_Max];
        QSet<QByteArray> d_builtins, d_keywords;
    };

    class LogPainter : public QSyntaxHighlighter
    {
    public:
        explicit LogPainter(QTextDocument *parent = 0);
    protected:
        // overrides
        void highlightBlock(const QString &text);
    };
}

#endif // LISAHIGHLIGHTER_H
