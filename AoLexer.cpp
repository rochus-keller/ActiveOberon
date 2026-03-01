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

#include "AoLexer.h"
#include <QBuffer>
#include <QFile>
#include <QIODevice>
#include <QtDebug>
using namespace Ao;

Lexer::Lexer(QObject *parent) : QObject(parent),
    d_lastToken(Tok_Invalid),d_lineNr(0),d_colNr(0),d_in(0),
    d_ignoreComments(true), d_packComments(true),d_sloc(0),d_lineCounted(false)
{

}

void Lexer::setStream(QIODevice* in, const QString& sourcePath)
{
    if( in == 0 )
        setStream( sourcePath );
    else
    {
        d_in = in;
        d_lineNr = 0;
        d_colNr = 0;
        d_sourcePath = sourcePath;
        d_lastToken = Tok_Invalid;
        d_sloc = 0;
        d_lineCounted = false;

        if( isOberonFormat(in) )
        {
            const QByteArray text = Lexer::extractText(d_in);

            QBuffer* b = new QBuffer( this );
            b->buffer() = text;
            b->open(QIODevice::ReadOnly);

            if( d_in != 0 && d_in->parent() == this )
                d_in->deleteLater();

            d_in = b;

        }
    }
}

bool Lexer::setStream(const QString& sourcePath)
{
    QIODevice* in = 0;

    QFile* file = new QFile(sourcePath, this);
    if( !file->open(QIODevice::ReadOnly) )
    {
        delete file;
        return false;
    }
    in = file;

     // else
    setStream( in, sourcePath );
    return true;
}

Token Lexer::nextToken()
{
    Token t;
    if( !d_buffer.isEmpty() )
    {
        t = d_buffer.first();
        d_buffer.pop_front();
    }else
        t = nextTokenImp();
    if( t.d_type == Tok_Comment && d_ignoreComments )
        t = nextToken();
    return t;
}

Token Lexer::peekToken(quint8 lookAhead)
{
    Q_ASSERT( lookAhead > 0 );
    while( d_buffer.size() < lookAhead )
        d_buffer.push_back( nextTokenImp() );
    return d_buffer[ lookAhead - 1 ];
}

QList<Token> Lexer::tokens(const QString& code)
{
    return tokens( code.toLatin1() );
}

QList<Token> Lexer::tokens(const QByteArray& code, const QString& path)
{
    QBuffer in;
    in.setData( code );
    in.open(QIODevice::ReadOnly);
    setStream( &in, path );

    QList<Token> res;
    Token t = nextToken();
    while( t.isValid() )
    {
        res << t;
        t = nextToken();
    }
    return res;
}

Token Lexer::nextTokenImp()
{
    if( d_in == 0 )
        return token(Tok_Eof);
    skipWhiteSpace();

    while( d_colNr >= d_line.size() )
    {
        if( d_in->atEnd() )
        {
            Token t = token( Tok_Eof, 0 );
            if( d_in->parent() == this )
            {
                d_in->deleteLater();
                d_in = 0;
            }
            return t;
        }
        nextLine();
        skipWhiteSpace();
    }
    Q_ASSERT( d_colNr < d_line.size() );
    while( d_colNr < d_line.size() )
    {
        const char ch = quint8(d_line[d_colNr]);

        if( ch == '"' || ch == '\'' )
            return string();
        else if( ::isalpha(ch) )
            return ident();
        else if( ::isdigit(ch) )
            return number();
        // else
        int pos = d_colNr;
        TokenType tt = tokenTypeFromString(d_line,&pos);

        if( tt == Tok_Latt )
            return comment();
        else if( tt == Tok_Invalid || pos == d_colNr )
            return token( Tok_Invalid, 1, QString("unexpected character '%1' %2").arg(char(ch)).arg(int(ch)).toUtf8() );
        else {
            const int len = pos - d_colNr;
            return token( tt, len, d_line.mid(d_colNr,len) );
        }
    }
    Q_ASSERT(false);
    return token(Tok_Invalid);
}

int Lexer::skipWhiteSpace()
{
    const int colNr = d_colNr;
    while( d_colNr < d_line.size() && ::isspace( d_line[d_colNr] ) )
        d_colNr++;
    return d_colNr - colNr;
}

void Lexer::nextLine()
{
    d_colNr = 0;
    d_lineNr++;
    d_line = d_in->readLine();
    d_lineCounted = false;

    if( d_line.endsWith("\r\n") )
        d_line.chop(2);
    else if( d_line.endsWith('\n') || d_line.endsWith('\r') || d_line.endsWith('\025') )
        d_line.chop(1);
}

int Lexer::lookAhead(int off) const
{
    if( int( d_colNr + off ) < d_line.size() )
    {
        return d_line[ d_colNr + off ];
    }else
        return 0;
}

Token Lexer::token(TokenType tt, int len, const QByteArray& val)
{
    if( tt != Tok_Invalid && tt != Tok_Comment && tt != Tok_Eof )
        countLine();
    QByteArray v = val;
    if( tt != Tok_Comment && tt != Tok_Invalid )
        v = Token::getSymbol(v);
    Token t( tt, d_lineNr, d_colNr + 1, len, v );
    d_lastToken = t;
    d_colNr += len;
    t.d_sourcePath = d_sourcePath;
    return t;
}

Token Lexer::ident()
{
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !QChar(c).isLetterOrNumber() ) // QChar wegen möglichen Umlauten
            break;
        else
            off++;
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    Q_ASSERT( !str.isEmpty() );
    int pos = 0;
    TokenType t = tokenTypeFromString( str, &pos );
    if( t != Tok_Invalid && pos != str.size() )
        t = Tok_Invalid;
    if( t == Tok_CODE )
        return assembler();
    if( t != Tok_Invalid )
        return token( t, off );
    else
        return token( Tok_ident, off, str );
}

static inline bool isHexDigit( char c )
{
    return ::isdigit(c) || c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F';
}

static inline bool checkHexNumber( QByteArray str )
{
    const int pos = str.indexOf('\'');
    if( pos != -1 )
        str = str.left(pos);
    if( str.size() < 2 || ( !str.endsWith('H') && !str.endsWith('h')
                            && !str.endsWith('X') && !str.endsWith('x') ) )
        return false;
    else
        return true;
}

static inline bool checkDecNumber( QByteArray str, bool oneOff = false )
{
    for( int i = 0; i < (str.size() - (oneOff ? 1 : 0)); i++ )
    {
        if( !::isdigit(str[i]) )
            return false;
    }
    return true;
}

Token Lexer::number()
{
    // integer      ::=  digit {digit} | digit {hexDigit} 'H'
    // real         ::=  digit {digit} '.' {digit} [ScaleFactor]
    // ScaleFactor  ::=  ('E'|'D'|'S') ['+' | '-'] digit {digit}
    int lhsPlaces = 0, rhsPlaces = 0, expPlaces = 0;
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !isHexDigit(c) ) // also accepts d and e!
            break;
        else
            off++;
    }
    lhsPlaces = off;
    bool isHex = false;
    bool is64bit = false;
    bool is32bit = false;
    bool isChar = false;
    bool isReal = false;
    int commaPos = -1, ePos = -1;
    const char o1 = lookAhead(off);
    if( o1 == 'H' || o1 == 'h' )
    {
        isHex = true;
        off++;
    }else if( o1 == 'X' || o1 == 'x' )
    {
        isChar = true;
        off++;
    }else if( o1 == '.' && lookAhead(off+1) == '.' )
    {
        ; // look for decimal point but not for range
    }else if( o1 == '.'  )
    {
        if( !checkDecNumber(d_line.mid(d_colNr, off) ) )
                return token( Tok_Invalid, off, "invalid mantissa" );
        commaPos = off;
        off++;
        isReal = true;
        while( true )
        {
            const char c = lookAhead(off);
            if( !::isdigit(c) )
                break;
            else
                off++;
            rhsPlaces++;
        }
        const char de = lookAhead(off);
        if( de == 'E' || de == 'D' || de == 'S' || de == 'e' || de == 'd' || de == 's' )
        {
            is64bit = ( de == 'D' || de == 'd' );
            is32bit = ( de == 'S' || de == 's' );

            ePos = off;
            off++;
            char o = lookAhead(off);
            if( o == '+' || o == '-' )
            {
                off++;
                o = lookAhead(off);
            }
            if( !::isdigit(o) )
                return token( Tok_Invalid, off, "invalid real" );
            while( true )
            {
                const char c = lookAhead(off);
                if( !::isdigit(c) )
                    break;
                else
                    off++;
                expPlaces++;
            }
        }
    }
    QByteArray str = d_line.mid(d_colNr, off );
    Q_ASSERT( !str.isEmpty() );
    if( isHex && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal integer" );
    else if( isChar && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal string" );

    if( isChar )
    {
        return token( Tok_hexchar, off, str );
    }
    else if( isReal)
    {
        Token tok = token( Tok_real, off, str );
        QByteArray mantissa = ePos != -1 ? str.left(ePos) : str;
        QByteArray lhs = mantissa;
        if( commaPos != -1 )
        {
            lhs = lhs.left(commaPos);
            mantissa.remove(commaPos,1);
        }
        bool mOk, lOk;
        const quint64 l = lhs.toULongLong(&lOk);
        const quint64 m = mantissa.toULongLong(&mOk); // !ok if mantissa is too large
        const int e = ePos != -1 ? str.mid(ePos+1).toInt() : 0;
        tok.d_double = !is32bit && ( !mOk || is64bit || e > 127 || e < -126 || m > 8388607 );
        if( is32bit && ( !lOk || e > 127 || e < -126 || l > 8388607 ) )
            return token( Tok_Invalid, off, "literal too large for REAL" );
        if( tok.d_double && ( e > 1023 || e < -1022 || l > 9007199254740991L ) )
            return token( Tok_Invalid, off, "literal too large for LONGREAL" );
        return tok;
    }else if( !isHex && !checkDecNumber(str, is32bit || is64bit) )
        return token( Tok_Invalid, off, "invalid decimal integer" );
    else
        // NOTE: we dont have to deal with is32bit and is64bit here because the string includes the suffices
        return token( Tok_integer, off, str );
}

Token Lexer::string()
{
    const char quote = lookAhead(0);
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        off++;
        if( c == quote )
            break;
        if( c == 0 )
            return token( Tok_Invalid, off, "non-terminated string" );
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    return token( Tok_string, off, str );
}

Token Lexer::comment()
{
    const int startLine = d_lineNr;
    const int startCol = d_colNr;

    int level = 0;
    int pos = d_colNr;
    parseComment( d_line, pos, level );
    QByteArray str = d_line.mid(d_colNr,pos-d_colNr);
    while( level > 0 && !d_in->atEnd() )
    {
        nextLine();
        pos = 0;
        parseComment( d_line, pos, level );
        if( !str.isEmpty() )
            str += '\n';
        str += d_line.mid(d_colNr,pos-d_colNr);
    }
    if( d_packComments && level > 0 && d_in->atEnd() )
    {
        d_colNr = d_line.size();
        Token t( Tok_Invalid, startLine, startCol + 1, str.size(), tr("non-terminated comment").toLatin1() );
        return t;
    }
    // Col + 1 weil wir immer bei Spalte 1 beginnen, nicht bei Spalte 0
    Token t( ( d_packComments ? Tok_Comment : Tok_Latt ), startLine, startCol + 1, str.size(), str );
    t.d_sourcePath = d_sourcePath;
    d_lastToken = t;
    d_colNr = pos;
    if( !d_packComments && level == 0 )
    {
        Token t(Tok_Ratt,d_lineNr, pos - 2 + 1, 2 );
        t.d_sourcePath = d_sourcePath;
        d_lastToken = t;
        d_buffer.append( t );
    }
    return t;
}

Token Lexer::assembler()
{
    const int startLine = d_lineNr;
    const int startCol = d_colNr;

    // qDebug() << "CODE found in" << d_sourcePath << d_lineNr;

    int pos = d_line.indexOf("END", d_colNr);
    QByteArray str = pos==-1 ? d_line.mid(d_colNr) : d_line.mid(d_colNr,pos-d_colNr);
    while( pos == -1 && !d_in->atEnd() )
    {
        nextLine();
        pos = d_line.indexOf("END", d_colNr);
        if( pos != -1 )
        {
            const int semi = d_line.indexOf(';');
            if( semi != -1 && semi < pos )
                pos = -1;
        }
        if( !str.isEmpty() )
            str += '\n';
        str += pos==-1 ? d_line.mid(d_colNr) : d_line.mid(d_colNr,pos-d_colNr);
    }
    if( d_packComments && pos == -1 )
    {
        Token t( Tok_Invalid, startLine, startCol, 4, tr("non-terminated CODE section").toLatin1() );
        return t;
    }
    // Col + 1 weil wir immer bei Spalte 1 beginnen, nicht bei Spalte 0
    Token t( Tok_CODE, startLine, startCol + 1, str.size(), str );
    t.d_sourcePath = d_sourcePath;
    d_lastToken = t;
    if( pos != -1 )
        d_colNr = pos;
    else
        d_colNr = d_line.size();
    return t;
}

void Lexer::countLine()
{
    if( !d_lineCounted )
        d_sloc++;
    d_lineCounted = true;
}

void Lexer::parseComment(const QByteArray& str, int& pos, int& level)
{
    enum State { Idle, Lb, Star } state = Idle;
    while( pos < str.size() )
    {
        const char* tmp = str.constData() + pos;
        const char c = str[pos++];
        switch( state )
        {
        case Idle:
            if( c == '(')
                state = Lb;
            else if( c == '*' )
                state = Star;
            break;
        case Lb:
            if( c == '*' )
            {
                level++;
                state = Idle;
            }else if( c != '(')
                state = Idle;
            break;
        case Star:
            if( c == ')')
            {
                level--;
                state = Idle;
            }else if( c != '*' )
                state = Idle;
            if( level <= 0 )
                return;
            break;
        }
    }
}

bool Lexer::isOberonFormat(QIODevice * in)
{
    if( in == 0 )
        return false;
    in->reset();
    const bool v4 = isV4File(in);
    in->reset();
    if( v4 )
        return true;
    QPair<quint32,quint32> v3 = inferTextRange(in);
    in->reset();
    if( v3.first || v3.second )
        return true;
    return false;
}


// The following code was copy/pasted from ObLexer
static inline char convert(char ch)
{
    // https://en.wikibooks.org/wiki/Oberon/ETH_Oberon/keyboard
    static const char* utf8 = "ÄÖÜäöüâêîôûàèìòùéëïçáñß£¶Ç"; // 0x80 .. 0x99
    static const QByteArray code = QString::fromUtf8(utf8).toLatin1();
    const quint8 c = (quint8)ch;
    if( c >= 0x80 && c <= 0x99 )
        return code[c-0x80];
    else
        return ch;
}

static inline bool convertCharSet( QByteArray& str )
{
    bool latin1 = false;
    for( int i = 0; i < str.size(); i++ )
    {
        if( quint8(str[i]) > 127 )
            latin1 = true;
        char ch = str[i];
        if( iscntrl(ch) )
        {
            if( !(ch == 0x0a || ch == 0x0d) )
                ch = ' ';
        }
        str[i] = convert(ch);
    }
    return latin1;
}

QByteArray Lexer::extractText(QIODevice* in)
{
    if( in == 0 )
        return QByteArray();
    QByteArray text;
    if( isV4File(in) )
    {
        text = readV4Text(in);
        if( convertCharSet(text) )
            text = QString::fromLatin1(text).toUtf8();
        return text;
    }
    QPair<quint32,quint32> r = inferTextRange(in);
    in->reset();
    skipBom(in);
    in->read(r.first);
    if( r.second )
        text = in->read(r.second);
    else
        text = in->readAll();

    if( r.first )
    {
        text.replace( '\r', '\n' );
        text.replace( 0x00, 0x20 );
        if( convertCharSet(text) )
            text = QString::fromLatin1(text).toUtf8();
    }

    return text;
}

bool Lexer::isV4File(QIODevice* in)
{
    const QByteArray buf = in->peek(2);
    const quint8* raw = (const quint8*)buf.constData();
    return buf.size() == 2 && raw[0] == 0xf0 && raw[1] == 0x01;
}

static quint8 readUInt8(QIODevice* in)
{
    char ch = 0;
    in->getChar(&ch);
    return ch;
}

static quint32 readUInt32(QIODevice* in)
{
    const QByteArray buf = in->read(4);
    if( buf.size() != 4 )
    {
        qWarning() << "cannot read 4 bytes from stream";
        return 0;
    }
    const quint8* raw = (const quint8*)buf.constData();
    const quint32 len = raw[0] + ( raw[1] << 8 ) + ( raw[2] << 16 ) + ( raw[3] << 24 );
    return len;
}

static QByteArray readString(QIODevice* in)
{
    QByteArray res;
    while( !in->atEnd() )
    {
        char ch;
        in->getChar(&ch);
        if( ch == 0 )
            break;
        res += ch;
    }
    return res;
}

struct V4TextRun
{
    quint32 col;
    qint32 font;
    quint32 voff;
    quint32 pos;
    qint32 len;
    qint32 element;
    quint32 width;
    quint32 height;
    QByteArray data;
    bool operator<( const V4TextRun& rhs) const { return pos < rhs.pos; }
    V4TextRun():font(-1),len(1),element(-1),width(0),height(0){}
};

static QByteArray extractV4Text(QIODevice* in, bool forElement = false )
{
    const quint32 headerLen = readUInt32(in); // includes tag and version
    QMap<qint32,QByteArray> eltypes, fonts;
    QList<V4TextRun> runs;

    int fontCount = 0;
    int elementCount = 0;
    int fontNumber = readUInt8(in);
    int pos = headerLen;
    if( forElement )
        pos--; // expects tag and version, but only a tag is present
    while( !in->atEnd() && fontNumber != 0 )
    {
        V4TextRun piece;
        piece.font = fontNumber;
        piece.pos = pos;
        if( fontNumber > fontCount )
        {
            fontCount = fontNumber;
            const QByteArray fontName = readString( in );
            fonts.insert(fontNumber, fontName);
        }
        piece.col = readUInt8(in);
        piece.voff = readUInt8(in);
        piece.len = readUInt32(in);

        if( piece.len <= 0 )
        {
            // this is an element
            const int elementDataLength = -piece.len;
            piece.len = 1;
            piece.width = readUInt32(in);
            piece.height = readUInt32(in);
            piece.element = readUInt8(in);
            if( piece.element > elementCount )
            {
                elementCount = piece.element;
                const QByteArray module = readString( in );
                const QByteArray procedure = readString( in );
                eltypes.insert(piece.element, module + "." + procedure);
            }
            piece.data = in->read(elementDataLength);
        }

        pos += piece.len;
        runs.append(piece);
        fontNumber = readUInt8(in);
    }

    qSort(runs);
    QByteArray res;
    bool blockText = false;
    foreach( const V4TextRun& piece, runs )
    {
        in->seek(piece.pos);
        if( !piece.data.isEmpty() )
        {
            if( eltypes[piece.element] == "FoldElems.New" )
            {
                if( piece.data[0] == 0 )
                {
                    QBuffer buf;
                    buf.setData(piece.data);
                    buf.open(QIODevice::ReadOnly);
                    buf.read(1);
                    QByteArray text = extractV4Text(&buf,true);
                    text.replace('\r', '\n');
                    res += text;
                    blockText = true;
                }else if( piece.data[0] == 1 )
                    blockText = false;
            }
        }else if( !blockText )
        {
            QByteArray text = in->read(piece.len);
            text.replace('\r', '\n');
            res += text;
        }
    }

    return res;
}

QByteArray Lexer::readV4Text(QIODevice* in)
{
    quint8 tag = readUInt8(in);
    quint8 ver = readUInt8(in);
    if( tag == 0xf0 && ver == 0x01 )
        return extractV4Text(in);
    return QByteArray();
}

bool Lexer::skipBom(QIODevice* in)
{
    const QByteArray buf = in->peek(3);
    if( buf.size() == 3 && buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf )
    {
        in->read(3);
        return true;
    }else
        return false;
}

bool Lexer::findModuleName(QIODevice * in, QByteArray &name)
{
    Lexer lex;
    lex.setIgnoreComments(true);
    lex.setStream(in, "");
    Token t = lex.nextToken();
    while(t.isValid())
    {
        if( t.d_type == Tok_MODULE )
        {
            t = lex.nextToken();
            if( t.d_type == Tok_ident )
            {
                name = t.d_val;
                return true;
            }else
                return false;
        }
        t = lex.nextToken();
    }
    return false;
}

bool Lexer::findModuleName(const QString &path, QByteArray &name)
{
    QFile f(path);
    if( !f.open(QFile::ReadOnly) )
        return false;
    else
        return findModuleName( &f, name);
}

// Note: -4095 in S4.Texts corresponds to 0xf0 0x01
//          S3.Texts: OldTextBlockId = 1X; OldTextSpex = 0F0X;
// Oberon System 3 uses 0xf7 0x07
//          S3.Texts: TextBlockId := 0F0X, DocBlockId := 0F7X, TextSpex := 1X
// Oberon System 2 uses 0x01 0xf0 (which is the inverse of the later System 4)

QPair<quint32, quint32> Lexer::inferTextRange(QIODevice* in)
{
    QPair<quint32, quint32> res;
    if( in == 0 )
        return res;

    skipBom(in);

    quint8 ch = readUInt8(in);

    static const quint8 DocBlockId = 0xF7;
    static const quint8 TextBlockId = 0xF0;
    static const quint8 OldTextBlockId = 0x01;

    if( ch == DocBlockId )
    {
        // this is a V3 DocBlock; skip the DocHeader and redetermine type
        ch = readUInt8(in); // ignore
        readString(in);
        in->read(8); // now 4 times int16, ignore
        ch = readUInt8(in);
        if( ch == 0xF7 )
        {
            // ignore meta info
            ch = readUInt8(in);
            if( ch == 0x08 )
            {
                const quint32 len = readUInt32(in);
                in->read(len); // ignore
                ch = readUInt8(in);
            }
        }
    }
    if( ch == TextBlockId || ch == OldTextBlockId )
    {
        res.first = in->pos();
        readUInt8(in); // type, not required
        const quint32 hlen = readUInt32(in);
        in->seek( res.first - 1 + hlen - 4 );
        const quint32 tlen = readUInt32(in);
        res.first = in->pos();
        res.second = tlen;
    }
    return res;
}
