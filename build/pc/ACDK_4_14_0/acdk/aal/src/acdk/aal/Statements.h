// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_Statements_h
#define acdk_aal_Statements_h


#include "Identifier.h"
#include "TypeName.h"
#include "VarName.h"

namespace acdk {
namespace aal {


USING_CLASS(acdk::aci::, Code);
USING_CLASS(acdk::aci::, Executable);
USING_CLASS(acdk::aci::, ParseNode);
USING_CLASS(acdk::aci::, EvalEnv);
USING_CLASS(acdk::aci::, Compiler);
USING_CLASS(acdk::aci::, SemanticElem);
USING_CLASS(acdk::aci::, DClazzMethodInfo);
USING_CLASS(acdk::aci::, CodeWithSymbolTable);
USING_CLASS(acdk::aci::, SymbolTable);

ACDK_DECL_INTERFACE(Breakable);
class ACDK_AAL_PUBLIC Breakable
      ACDK_INTERFACEBASE
{
public:
  /**
    return start of loop label
  */
  virtual RString getNextLabel() = 0;
  /**
    return end of loop label
  */
  virtual RString getLastLabel() = 0;
  
};

ACDK_DECL_CLASS(BreakableStatement);
class ACDK_AAL_PUBLIC BreakableStatement
: extends Code
, implements Breakable
{
public:
  RString _breakLabel;
  RString _continueLabel;
  BreakableStatement(IN(RParseNode) templ) 
  : Code(templ)
  {
  }
  RString getNextLabel() { return _continueLabel; }
  RString getLastLabel() { return _breakLabel; }
};

ACDK_DECL_CLASS(GotoLabelStatement);
class ACDK_AAL_PUBLIC GotoLabelStatement
: extends Code
{
public:
  RString _targetLabel;
  GotoLabelStatement(IN(RParseNode) templ) 
  : Code(templ)
  {
  }
};

ACDK_DECL_CLASS(BreakableBlockStatement);
class ACDK_AAL_PUBLIC BreakableBlockStatement
: extends CodeWithSymbolTable
, implements Breakable
{
public:
  RString _breakLabel;
  RString _continueLabel;
  /// used for for loop
  RString _firstLabel;
  BreakableBlockStatement(IN(RParseNode) templ) 
  : CodeWithSymbolTable(templ)
  {
  }
  RString getNextLabel() { return _continueLabel; }
  RString getLastLabel() { return _breakLabel; }
};

ACDK_STD_NODE_CODE_EMIT(FunctionBlock, ParseNode, Code);
ACDK_STD_NODE_CODE_EMIT(ReturnStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(BreakStatement, ParseNode, GotoLabelStatement);

ACDK_STD_NODE_CODE(CodeText, ParseNode, CodeWithSymbolTable);
ACDK_STD_NODE_CODE_EMIT(ExprStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(WhileStatement, ParseNode, BreakableStatement);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(DoStatement, ParseNode, BreakableStatement);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ForStatement, ParseNode, BreakableBlockStatement);

ACDK_STD_NODE_CODE_EMIT(GotoStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_EMIT(LabeledStatement, ParseNode, Code);

ACDK_STD_NODE_CODE_EMIT(IfStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE(Block, ParseNode, CodeWithSymbolTable);


ACDK_DECL_CLASS(CatchableBlockStatement);
class ACDK_AAL_PUBLIC CatchableBlockStatement
: extends CodeWithSymbolTable
{
public:
  RString _nextBreakLabel;
  RString _finallyLabel;
  
  CatchableBlockStatement(IN(RParseNode) templ) 
  : CodeWithSymbolTable(templ)
  {
  }
};

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ThrowStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(TryCatchStatement, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(CatchBlock, ParseNode, CatchableBlockStatement);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(FinallyBlock, ParseNode, Code);



ACDK_DECL_CLASS(SwitchBaseCode);
class ACDK_AAL_PUBLIC SwitchBaseCode
: extends BreakableStatement
{
public:
  RString _tempTestVar;
  SwitchBaseCode(IN(RParseNode) templ) 
  : BreakableStatement(templ)
  {
  }
};


ACDK_DECL_CLASS(ThreeLabelBreakable);
class ACDK_AAL_PUBLIC ThreeLabelBreakable
: extends BreakableStatement
{
public:
  RString _thirdLabel;
  ThreeLabelBreakable(IN(RParseNode) templ) 
  : BreakableStatement(templ)
  {
  }
};



ACDK_STD_NODE_CODE_POSTPARSE_EMIT(SwitchStatement, ParseNode, SwitchBaseCode);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(CaseClause, ParseNode, ThreeLabelBreakable);


} // aal
} // acdk


#endif //acdk_aal_Statements_h
