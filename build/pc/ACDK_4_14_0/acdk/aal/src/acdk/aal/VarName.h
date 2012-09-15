// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_VarName_h
#define acdk_aal_VarName_h


#include "../aci/Code.h"
#include "../aci/Compiler.h"

namespace acdk {
namespace aal {

USING_CLASS(acdk::aci::, Code);
USING_CLASS(acdk::aci::, ParseNode);
USING_CLASS(acdk::aci::, Compiler);
USING_CLASS(acdk::aci::, Executable);
USING_CLASS(acdk::aci::, EvalEnv);

ACDK_DECL_CLASS(IdentifierCode);
class ACDK_AAL_PUBLIC IdentifierCode
: extends Code
{
  ACDK_WITH_METAINFO(IdentifierCode)
protected:
  RString _identifier;
public:
  IdentifierCode(IN(RParseNode) templ, IN(RString) identifier = "") 
  : Code(templ)
  , _identifier(identifier)
  {
    _safeCode = true;
  }
  virtual RString getName() { return _identifier; }
  virtual void printCodeTree(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
  {
    Code::printThisNode(out, indent);
    out->println(indent + "  " + _identifier);
  }
  RString getIdentifier() { return _identifier; }
  void setIdentifier(IN(RString) identifier) { _identifier = identifier; }
  RString getCodeString() { return _identifier; }
};

ACDK_DECL_CLASS(VarName);

class ACDK_AAL_PUBLIC VarName
: extends IdentifierCode
{
  ACDK_WITH_METAINFO(VarName)
public:
  VarName(IN(RParseNode) templ, IN(RString) tpname = "") 
  : IdentifierCode(templ, tpname)
  {
  }
  void postParse(IN(RCompiler) comp);
  void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
  void execute_inner(IN(REvalEnv) env);
  //virtual void postParse(IN(RCompiler) compiler);
  RString getVarName() { return _identifier; }
  void setVarName(IN(RString) name) { _identifier = name; }

  static RCode createCode(IN(RCompiler) comp, IN(RString) value, IN(RString) nodename = "VarName");
};

ACDK_DECL_CLASS(VarNameParseNode);

class ACDK_AAL_PUBLIC VarNameParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(VarNameParseNode)
public:
  VarNameParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
    : ParseNode(nodename, rulesyntax, help) {}
  RCode parse(IN(RCompiler) comp);
  virtual RCode createStandarCode() { return new VarName(this); }
};

ACDK_DECL_CLASS(Operator);

class ACDK_AAL_PUBLIC Operator
: extends VarName
{
  ACDK_WITH_METAINFO(Operator)
public:
  Operator(IN(RParseNode) templ, IN(RString) op = "") 
  : VarName(templ, op)
  {
  }
  void postParse(IN(RCompiler) comp);
  static RString operatorToFuncName(IN(RString) opstr);
  RString operatorToFuncName() { return operatorToFuncName(_identifier); }
  static RCode createCode(IN(RCompiler) comp, IN(RString) op, IN(RString) codename = "Operator")
  {
    ROperator operat = (ROperator)comp->createCode(codename);
    operat->setIdentifier(op);
    return &operat;
  }
};

ACDK_DECL_CLASS(OperatorParseNode);

class ACDK_AAL_PUBLIC OperatorParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(OperatorParseNode)
public:
  OperatorParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
    : ParseNode(nodename, rulesyntax, help) {}
  RCode parse(IN(RCompiler) comp);
  virtual RCode createStandarCode() { return new Operator(this); }
};


ACDK_DECL_CLASS(Label);

class ACDK_AAL_PUBLIC Label
: extends IdentifierCode
{
  ACDK_WITH_METAINFO(Label)
public:
  Label(IN(RParseNode) templ, IN(RString) op = "") 
  : IdentifierCode(templ, op)
  {
  }
  void postParse(IN(RCompiler) comp) { } 
  void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca) { }
  static RCode createCode(IN(RCompiler) comp, IN(RString) label, IN(RString) codename = "Label")
  {
    RLabel operat = (RLabel)comp->createCode(codename);
    operat->setIdentifier(label);
    return &operat;
  }
};

ACDK_DECL_CLASS(LabelParseNode);

class ACDK_AAL_PUBLIC LabelParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(LabelParseNode)
public:
  LabelParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
    : ParseNode(nodename, rulesyntax, help) {}
  RCode parse(IN(RCompiler) comp);
  virtual RCode createStandarCode() { return new Label(this); }
};

} // aal
} // acdk


#endif //acdk_aal_VarName_h
