#ifndef AOLEXER_H
#define AOLEXER_H

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

#include <QObject>
#include <ActiveOberon/AoToken.h>

class QIODevice;

namespace Ao
{
    class Lexer : public QObject
    {
    public:
        explicit Lexer(QObject *parent = 0);

        void setStream( QIODevice*, const QString& sourcePath );
        bool setStream(const QString& sourcePath);
        void setIgnoreComments( bool b ) { d_ignoreComments = b; }
        void setPackComments( bool b ) { d_packComments = b; }

        Token nextToken();
        Token peekToken(quint8 lookAhead = 1);
        QList<Token> tokens( const QString& code );
        QList<Token> tokens( const QByteArray& code, const QString& path = QString() );
        quint32 getSloc() const { return d_sloc; }
        static void parseComment( const QByteArray& str, int& pos, int& level );

        static bool isOberonFormat(QIODevice*);
        static QByteArray extractText(QIODevice*); // recognizes Oberon file format and ASCII, returns Latin-1 UTF-8
        static QPair<quint32,quint32> inferTextRange(QIODevice*); // offset, len (or 0 for all)
        static bool isV4File( QIODevice* );
        static QByteArray readV4Text(QIODevice*);
        static bool skipBom( QIODevice* );
    protected:
        Token nextTokenImp();
        int skipWhiteSpace();
        void nextLine();
        int lookAhead(int off = 1) const;
        Token token(TokenType tt, int len = 1, const QByteArray &val = QByteArray());
        Token ident();
        Token number();
        Token string();
        Token comment();
        Token assembler();
        void countLine();
    private:
        QIODevice* d_in;
        quint32 d_sloc;
        quint32 d_lineNr;
        quint16 d_colNr;
        QString d_sourcePath;
        QByteArray d_line;
        QList<Token> d_buffer;
        Token d_lastToken;
        bool d_ignoreComments;  // don't deliver comment tokens
        bool d_packComments;    // Only deliver one Tok_Comment for /**/ instead of Tok_Lcmt and Tok_Rcmt
        bool d_lineCounted;
    };
}

#endif // AOLEXER_H
