// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_Expressions_h
#define acdk_aal_Expressions_h


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

//********************************** Arguments **********************************



//********************************** Expression **********************************
ACDK_DECL_CLASS(Expression); 
ACDK_DECL_CLASS(ExpressionParseNode); 

class ACDK_AAL_PUBLIC Expression 
: extends Code 
{ 
public: 
  Expression(IN(RParseNode) pn);
  void createObjectCall(IN(RCompiler) comp, IN(RString) opcallname, IN(RCode) lhex,  IN(RCode) rhex);
};

class ACDK_AAL_PUBLIC ExpressionParseNode 
: extends ParseNode 
{ 
public: 
  ExpressionParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new Expression(this); } 
}; 

inline 
Expression::Expression(IN(RParseNode) pn) 
: Code(pn) 
{
} 


//********************************** ArithmeticExpression **********************************
ACDK_DECL_CLASS(ArithmeticExpression); 
ACDK_DECL_CLASS(ArithmeticExpressionParseNode); 

class ACDK_AAL_PUBLIC ArithmeticExpression 
: extends Expression 
{ 
public: 
  ArithmeticExpression(IN(RParseNode) pn);
  
};

class ACDK_AAL_PUBLIC ArithmeticExpressionParseNode 
: extends ExpressionParseNode 
{ 
public: 
  ArithmeticExpressionParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ExpressionParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new ArithmeticExpression(this); } 
}; 

inline 
ArithmeticExpression::ArithmeticExpression(IN(RParseNode) pn) 
: Expression(pn) 
{
} 

//********************************** LogicalExpr **********************************
ACDK_DECL_CLASS(LogicalExpr); 
ACDK_DECL_CLASS(LogicalExprParseNode); 

class ACDK_AAL_PUBLIC LogicalExpr 
: extends Expression 
{ 
public: 
  LogicalExpr(IN(RParseNode) pn);
  virtual void postParse(IN(RCompiler) comp);
  virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
  virtual RSemanticElem getExpressionSemanticElement();
};

class ACDK_AAL_PUBLIC LogicalExprParseNode 
: extends ExpressionParseNode 
{ 
public: 
  LogicalExprParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ExpressionParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new LogicalExpr(this); } 
  
}; 

inline 
LogicalExpr::LogicalExpr(IN(RParseNode) pn) 
: Expression(pn) 
{
} 


//ACDK_STD_NODE_CODE_POSTPARSE_EMIT(CastExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(AssignmentExpr, ParseNode, Code);
//ACDK_STD_NODE_CODE_POSTPARSE_EMIT(NewExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(AdditiveExpr, ParseNode, ArithmeticExpression);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(MultiplicativeExpr, ParseNode, ArithmeticExpression);

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(RelationalExpr, LogicalExprParseNode, LogicalExpr);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(EqualityExpr, LogicalExprParseNode, LogicalExpr);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(EqualsExpr, LogicalExprParseNode, EqualityExpr);

//ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ArraySubscribeExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(PostfixExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(PrefixExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(BitwiseInfixExpr, ParseNode, Code);

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ConditionalExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_EMIT(EmptyExpression, ParseNode, Code);


//ACDK_STD_NODE_CODE_POSTPARSE(Argument, ParseNode, Code);

} // aal
} // acdk


#endif //acdk_aal_Expressions_h
