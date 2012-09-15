// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_SubscribeExpressions_h
#define acdk_aal_SubscribeExpressions_h


#include "Identifier.h"
#include "TypeName.h"
#include "VarName.h"
#include "Expressions.h"

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




ACDK_DECL_CLASS(Arguments); 
ACDK_DECL_CLASS(ArgumentsParseNode); 
class ACDK_AAL_PUBLIC Arguments 
: extends Code 
{ 
  /**
    Currently only counts parameters
    later should also hold the type of an expression
    for static binding
  */
  RCodeArray _args; 
  // Arguments[0] -> ArgumentList[0-n] -> Argument## maybe this is too compilcated and errorphrone
public: 
  Arguments(IN(RParseNode) pn); 
  void addArgument(IN(RCode) cd)
  {
    _args->append(cd);
  }
  RCodeArray getArgumentCodes() { return _args; }
  void setArgumentCodes(IN(RCodeArray) args);
  void getArgumentTypes(acdk::lang::dmi::ArgumentExprTypes& args);
  void reorgArgs(const acdk::lang::dmi::ArgumentExprTypes& argtypes);

};

class ACDK_AAL_PUBLIC ArgumentsParseNode 
: extends ParseNode 
{ 
public: 
  ArgumentsParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new Arguments(this); } 
}; 

inline 
Arguments::Arguments(IN(RParseNode) pn) 
: Code(pn) 
, _args(new CodeArray(0))
{
} 

//********************************** SubscribeExpr **********************************
ACDK_DECL_CLASS(SubscribeExpr); 
ACDK_DECL_CLASS(SubscribeExprParseNode); 
class ACDK_AAL_PUBLIC SubscribeExpr 
: extends Code 
{ 
  /**
    Can be an RVarDefinition or RTypeDefinition or RString for namespace
  */
  RSemanticElem _sem;
public: 
  SubscribeExpr(IN(RParseNode) pn); 
  virtual void postParse(IN(RCompiler) comp);
  virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
};

class ACDK_AAL_PUBLIC SubscribeExprParseNode 
: extends ParseNode 
{ 
public: 
  SubscribeExprParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new SubscribeExpr(this); } 
}; 

inline 
SubscribeExpr::SubscribeExpr(IN(RParseNode) pn) 
: Code(pn) 
{
} 


//********************************** FuncSubscribeExpr **********************************
ACDK_DECL_CLASS(FuncSubscribeExpr); 
ACDK_DECL_CLASS(FuncSubscribeExprParseNode); 

class ACDK_AAL_PUBLIC FuncSubscribeExpr 
: extends Code 
{ 
public: 
  /// hold possible MiIvWeakBind for dynamic binding
  int dflags;
  FuncSubscribeExpr(IN(RParseNode) pn);
  RString getName();
  virtual void postParse(IN(RCompiler) comp);
  virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
};

class ACDK_AAL_PUBLIC FuncSubscribeExprParseNode 
: extends ParseNode 
{ 
public: 
  FuncSubscribeExprParseNode(IN(RString) name, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(name, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new FuncSubscribeExpr(this); } 
}; 

inline 
FuncSubscribeExpr::FuncSubscribeExpr(IN(RParseNode) pn) 
: Code(pn) 
, dflags(0)
{
} 

//********************************** MemberSubscribeExpr **********************************
ACDK_DECL_CLASS(MemberSubscribeExpr); 
ACDK_DECL_CLASS(MemberSubscribeExprParseNode); 

class ACDK_AAL_PUBLIC MemberSubscribeExpr 
: extends Code 
{ 
public: 
  /// posibly MiIvWeakBind for dynamic binding
  int dflags;
  MemberSubscribeExpr(IN(RParseNode) pn);
  virtual RString getName();
  virtual void postParse(IN(RCompiler) comp);
  virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
  virtual RSemanticElem getExpressionSemanticElement() { return _subNodes[1]->getExpressionSemanticElement(); }
  virtual RString getSubscribeOperator() { return _subNodes[0]->toString(); }

};

class ACDK_AAL_PUBLIC MemberSubscribeExprParseNode 
: extends ParseNode 
{ 
public: 
  MemberSubscribeExprParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new MemberSubscribeExpr(this); } 
}; 

inline 
MemberSubscribeExpr::MemberSubscribeExpr(IN(RParseNode) pn) 
: Code(pn) 
, dflags(0)
{
} 



ACDK_STD_NODE_CODE_POSTPARSE_EMIT(CastExpr, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(NewExpr, ParseNode, Code);

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ArraySubscribeExpr, ParseNode, Code);

ACDK_STD_NODE_CODE_POSTPARSE(Argument, ParseNode, Code);

} // aal
} // acdk


#endif //acdk_aal_SubscribeExpressions_h
