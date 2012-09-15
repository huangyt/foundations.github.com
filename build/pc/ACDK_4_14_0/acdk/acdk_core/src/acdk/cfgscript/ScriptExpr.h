// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 

#ifndef acdk_cfgscript_ScriptExr_h
#define acdk_cfgscript_ScriptExr_h
#if !defined(DOXYGENONLY)

#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StringReader.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/MetaInfoChildsArray.h>
#include <acdk/lang/reflect/Method.h>

#include "ScriptObject.h"
#include "Script.h"
#include "ScriptException.h"
#include "ScriptEval.h"


namespace acdk {
namespace cfgscript {

foreign
enum OpFlags
{
  OpPrefix = 0x0001,
  OpSuffix = 0x0002,
  OpInfix =  0x0004,
  OpOutfix = 0x0008,
  OpEmpty      = 0x0010,
  OpVarOrIdent = 0x0020,
  OpRightAssoc = 0x0040
};

struct OpDescription
{
  const char* op;
  int prio;
  int flags;
};

inline
StringBuffer& operator<<(StringBuffer& sb, const OpDescription& opd)
{
  return sb << opd.op << "," << opd.flags;
}

ACDK_DECL_CLASS(ScriptExpr);
foreign
class ScriptExpr
: extends acdk::lang::Object
{
public:
  OpDescription opDescription;
  int beginTokenIdx;
  RScriptExpr leftExpr;
  RScriptExpr rightExpr;
  ScriptExpr()
  : beginTokenIdx(-1)
  {
    opDescription.op = "";
    opDescription.prio = 0;
    opDescription.flags = 0;
  }
  ScriptExpr(INP(OpDescription) desc, int beginIdx, INP(RScriptExpr) leftExp = Nil,
                                                        INP(RScriptExpr) rightExp = Nil)
  : opDescription(desc)
  , beginTokenIdx(beginIdx)
  , leftExpr(leftExp)
  , rightExpr(rightExp)
  {
  }
  
};


class EmptyExpr
: extends ScriptExpr
{
public:
  EmptyExpr()
  {
    opDescription.flags = OpEmpty;
    opDescription.prio = 0;
    opDescription.op = "";
  }
};

class VarOrValExpr
: extends ScriptExpr
{
public:
  VarOrValExpr(int beginIdx)
  : ScriptExpr(OpDescription(), beginIdx)
  {
    opDescription.flags = OpVarOrIdent;
    opDescription.prio = 0;
    opDescription.op = "";
  }
};

extern OpDescription OpPriorities[];

int getOperatorPredence(INP(RString) op);
int getOperatorFlags(INP(RString) op);
int compareOperators(INP(RString) left, INP(RString) right);
bool isEndExpr(const SourceToken& stk);
bool valOrIdent(const SourceToken& stk);
bool isOperator(const SourceToken& stk);



STD_PARSENODE_DECL(ExpressionParseNode);
//STD_PARSENODE_DECL(ExpressionParseNodeOne);
STD_PARSENODE_DECL(ExpressionParseNode1);
STD_PARSENODE_DECL(ExpressionParseNode2);
STD_PARSENODE_DECL(PreProcParseNode);
STD_PARSENODE_DECL(PragmaParseNode);
STD_PARSENODE_DECL(ParametersParseNode);
STD_PARSENODE_DECL(NewExprParseNode);
STD_PARSENODE_DECL(ExecPoke);
STD_PARSENODE_DECL(ExecPeek);
STD_PARSENODE_DECL(ExecStaticPeek);
STD_PARSENODE_DECL(StatementParseNode);
STD_PARSENODE_DECL(StatementParseNode2);
STD_PARSENODE_DECL(StatementOrPreProc);
STD_PARSENODE_DECL(FuncOrMemberParseNode);
STD_PARSENODE_DECL(StaticFuncOrMemberParseNode);
STD_PARSENODE_DECL(OperatorCall);
STD_PARSENODE_DECL(ExecBinaryOp);
STD_PARSENODE_DECL(ExecUnaryPrefixOp);
STD_PARSENODE_DECL(ExecUnarySuffixOp);
STD_PARSENODE_DECL(AtomPreOrSuffixOp);
STD_PARSENODE_DECL(OptionalSuffixOp);
STD_PARSENODE_DECL(SuffixOp);
STD_PARSENODE_DECL(ExecuteMethodParseNode);
STD_PARSENODE_DECL(IfStatementParseNode);
STD_PARSENODE_DECL(WithParseNode);
STD_PARSENODE_DECL(WhileParseNode);
STD_PARSENODE_DECL(DoParseNode);
STD_PARSENODE_DECL(ForParseNode);
STD_PARSENODE_DECL(ForEachParseNode);
STD_PARSENODE_DECL(SwitchParseNode);
STD_PARSENODE_DECL(BreakParseNode);
STD_PARSENODE_DECL(ContinueParseNode);
STD_PARSENODE_DECL(UsingParseNode);
STD_PARSENODE_DECL(SynchronizeParseNode);
STD_PARSENODE_DECL(ClassParseNode);
STD_PARSENODE_DECL(InterfaceParseNode);
STD_PARSENODE_DECL(TryCatchParseNode);
STD_PARSENODE_DECL(ThrowParseNode);
STD_PARSENODE_DECL(ClassBodyParseNode);
STD_PARSENODE_DECL(ClassMemberOrFieldParseNode);
STD_PARSENODE_DECL(ConstructorMethodParseNode);
STD_PARSENODE_DECL(EnumParseNode);
STD_PARSENODE_DECL(ArrayLiteralParseNode);
STD_PARSENODE_DECL(BacktickParseNode);
STD_PARSENODE_DECL(DictLiteralParseNode);
STD_PARSENODE_DECL(TypeAliasParseNode);
STD_PARSENODE_DECL(DelegateParseNode);
STD_PARSENODE_DECL(LambdaParseNode);
STD_PARSENODE_DECL(LogParseNode);
STD_PARSENODE_DECL(ReturnParseNode);


} // namespace cfgscript
} // namespace acdk 
  
#endif //!defined(DOXYGENONLY)
#endif //acdk_cfgscript_ScriptExr_h
