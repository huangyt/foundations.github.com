// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_Identifier_h
#define acdk_aal_Identifier_h

#include "VarName.h"

#include "../aci/ClazzSymbolTable.h"

#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>

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


//********************************** Identifier **********************************

ACDK_DECL_CLASS(Identifier);

class ACDK_AAL_PUBLIC Identifier
: extends IdentifierCode
{
public:
  RString _varName;
  Identifier(IN(RParseNode) templ, IN(RString) tpname = "") 
  : IdentifierCode(templ, tpname)
  {
  }
  RString getVarName() { return _identifier; }
  virtual void postParse(IN(RCompiler) compiler);
  virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
  void execute_inner(IN(REvalEnv) env);
};

ACDK_DECL_CLASS(IdentifierParseNode);
class ACDK_AAL_PUBLIC IdentifierParseNode
: extends ParseNode
{
public:
  IdentifierParseNode(IN(RString) name, IN(RString) rulesyntax, IN(RString) help = "") : ParseNode(name, rulesyntax, help) {}
  RCode createStandarCode() { return new Identifier(this); }
  RCode parse(IN(RCompiler) comp);
  
};


USING_CLASS(acdk::aci::, Terminal);

//********************************** DecimalTerminalParseNode **********************************
ACDK_DECL_CLASS(DecimalTerminalParseNode);

class ACDK_AAL_PUBLIC DecimalTerminalParseNode
: extends acdk::aci::TerminalParseNode
{
public:
  DecimalTerminalParseNode(IN(RString) name, IN(RString) rulesyntax, IN(RString) help = "") 
    : ACDK_FQ_SUPER_QUALIFIER(acdk::aci::, TerminalParseNode)(name, rulesyntax, help) {}

  RCode parse(IN(RCompiler) comp);
};

//********************************** FloatTerminalParseNode **********************************
ACDK_DECL_CLASS(FloatTerminalParseNode);

class ACDK_AAL_PUBLIC FloatTerminalParseNode
: extends acdk::aci::TerminalParseNode
{
public:
  FloatTerminalParseNode(IN(RString) name, IN(RString) rulesyntax, IN(RString) help = "") 
    : ACDK_FQ_SUPER_QUALIFIER(acdk::aci::, TerminalParseNode)(name, rulesyntax, help) {}
  
  RCode parse(IN(RCompiler) comp);
};

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(BooleanLiteral, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE(Literal, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(LVarDecl, ParseNode, Code);

ACDK_STD_NODE_CODE_EMIT(Variable, ParseNode, Code);
ACDK_STD_NODE_CODE_EMIT(LeftHandVariable, ParseNode, Code);


} // aal
} // acdk


#endif //acdk_aal_Identifier_h
