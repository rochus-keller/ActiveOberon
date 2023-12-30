#ifndef __AO_TOKENTYPE__
#define __AO_TOKENTYPE__
// This file was automatically generated by EbnfStudio; don't modify it!


#include <QByteArray>

namespace Ao {
	enum TokenType {
		Tok_Invalid = 0,

		TT_Literals,
		Tok_Hash,
		Tok_Amp,
		Tok_Lpar,
		Tok_Latt,
		Tok_Rpar,
		Tok_Star,
		Tok_Ratt,
		Tok_Plus,
		Tok_Comma,
		Tok_Minus,
		Tok_Dot,
		Tok_2Dot,
		Tok_Slash,
		Tok_Colon,
		Tok_ColonEq,
		Tok_Semi,
		Tok_Lt,
		Tok_Leq,
		Tok_Eq,
		Tok_Gt,
		Tok_Geq,
		Tok_Lbrack,
		Tok_Rbrack,
		Tok_Hat,
		Tok_Lbrace,
		Tok_Bar,
		Tok_Rbrace,
		Tok_Tilde,

		TT_Keywords,
		Tok_ARRAY,
		Tok_BEGIN,
		Tok_BY,
		Tok_CASE,
		Tok_CODE,
		Tok_CONST,
		Tok_DIV,
		Tok_DO,
		Tok_ELSE,
		Tok_ELSIF,
		Tok_END,
		Tok_EXIT,
		Tok_FOR,
		Tok_IF,
		Tok_IMPORT,
		Tok_IN,
		Tok_IS,
		Tok_LOOP,
		Tok_MOD,
		Tok_MODULE,
		Tok_NIL,
		Tok_OBJECT,
		Tok_OF,
		Tok_OR,
		Tok_POINTER,
		Tok_PROCEDURE,
		Tok_RECORD,
		Tok_REPEAT,
		Tok_RETURN,
		Tok_THEN,
		Tok_TO,
		Tok_TYPE,
		Tok_UNTIL,
		Tok_VAR,
		Tok_WHILE,
		Tok_WITH,

		TT_Specials,
		Tok_ident,
		Tok_integer,
		Tok_real,
		Tok_string,
		Tok_hexchar,
		Tok_Comment,
		Tok_Eof,

		TT_MaxToken,

		TT_Max
	};

	const char* tokenTypeString( int ); // Pretty with punctuation chars
	const char* tokenTypeName( int ); // Just the names without punctuation chars
	bool tokenTypeIsLiteral( int );
	bool tokenTypeIsKeyword( int );
	bool tokenTypeIsSpecial( int );
	TokenType tokenTypeFromString( const QByteArray& str, int* pos = 0 );
}
#endif // __AO_TOKENTYPE__
