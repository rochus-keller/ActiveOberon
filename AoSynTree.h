#ifndef __AO_SYNTREE__
#define __AO_SYNTREE__
// This file was automatically generated by EbnfStudio; don't modify it!

#include <AoTokenType.h>
#include <AoToken.h>
#include <QList>

namespace Ao {

	struct SynTree {
		enum ParserRule {
			R_First = TT_Max + 1,
			R_AddOp,
			R_AliasType,
			R_ArrayType,
			R_Assembler,
			R_AssigOrCall,
			R_Attribute,
			R_Attributes,
			R_Body,
			R_Case,
			R_CaseLabels,
			R_CaseStat,
			R_ConstDecl,
			R_ConstExpr,
			R_DeclSeq,
			R_Designator,
			R_Element,
			R_Expr,
			R_ExprList,
			R_FPSection,
			R_Factor,
			R_FieldDecl,
			R_FieldList,
			R_ForStat,
			R_FormalPars,
			R_IdentDef,
			R_IdentList,
			R_IfStat,
			R_ImportDecl,
			R_ImportList,
			R_LoopStat,
			R_Module,
			R_MulOp,
			R_ObjectDeclSeq,
			R_ObjectType,
			R_PointerType,
			R_ProcDecl,
			R_ProcHead,
			R_ProcedureType,
			R_Qualident,
			R_RecordType,
			R_Relation,
			R_RepeatStat,
			R_ReturnStat,
			R_Selector,
			R_Set,
			R_SimpleExpr,
			R_StatBlock,
			R_StatSeq,
			R_Statement,
			R_SysFlag,
			R_Term,
			R_Type,
			R_TypeDecl,
			R_VarDecl,
			R_WhileStat,
			R_WithStat,
			R_comment_,
			R_number,
			R_Last
		};
		SynTree(quint16 r = Tok_Invalid, const Token& = Token() );
		SynTree(const Token& t ):d_tok(t){}
		~SynTree() { foreach(SynTree* n, d_children) delete n; }

		static const char* rToStr( quint16 r );

		Ao::Token d_tok;
		QList<SynTree*> d_children;
	};

}
#endif // __AO_SYNTREE__
