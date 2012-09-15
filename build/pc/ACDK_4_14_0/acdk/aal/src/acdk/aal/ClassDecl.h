// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_ClassDecl_h
#define acdk_aal_ClassDecl_h


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


ACDK_DECL_CLASS(ClassCode);

class ACDK_AAL_PUBLIC ClassCode
: extends CodeWithSymbolTable
{
public:
  ClassCode(IN(RParseNode) templ, IN(RString) tpname = "") 
  : CodeWithSymbolTable(templ)
  {
  }
  virtual RSymbolTable getSymbolTable()
  {
    if (_symbolTable == Nil)
      _symbolTable = new acdk::aci::ClazzSymbolTable();
    return _symbolTable;
  }
};


ACDK_DECL_CLASS(ClassMemberAttr); 
ACDK_DECL_CLASS(ClassMemberAttrParseNode); 

class ACDK_AAL_PUBLIC ClassMemberAttr 
: extends Code 
{ 
  ACDK_WITH_METAINFO(ClassMemberAttr)
  int _memberFlags;
public: 
  ClassMemberAttr(IN(RParseNode) pn);
  virtual void postParse(IN(RCompiler) comp);
  static int parseFlags(IN(RString) attrname);
  int getMemberFlags() { return _memberFlags; }
  //virtual void emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca);
};

class ACDK_AAL_PUBLIC ClassMemberAttrParseNode 
: extends ParseNode 
{ 
public: 
  ClassMemberAttrParseNode(IN(RString) name, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(name, rulesyntax, help)
  {} 
  virtual RCode createStandarCode() { return new ClassMemberAttr(this); } 
}; 

inline 
ClassMemberAttr::ClassMemberAttr(IN(RParseNode) pn) 
: Code(pn) 
, _memberFlags(0)
{
} 


ACDK_STD_NODE_CODE_POSTPARSE(NamespaceDecl, ParseNode, CodeWithSymbolTable);
ACDK_STD_NODE_CODE_POSTPARSE(UsingDecl, ParseNode, Code);


ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ClassDeclDef, ParseNode, ClassCode);
ACDK_STD_NODE_CODE_POSTPARSE(DerivedSuperDef, ParseNode, ClassCode);
ACDK_STD_NODE_CODE_POSTPARSE(DerivedInterfaceDef, ParseNode, ClassCode);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ClassDeclMember, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ClassDeclMethod, ParseNode, Code);
ACDK_STD_NODE_CODE(ClassDeclConstructorInitializerList, ParseNode, CodeWithSymbolTable);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ClassDeclConstructorInitializer, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE_EMIT(Parameter, ParseNode, Code);
ACDK_STD_NODE_CODE_POSTPARSE(FunctionParamsDecl, ParseNode, CodeWithSymbolTable);

ACDK_STD_NODE_CODE_POSTPARSE(ClosureExpr, ParseNode, ClassCode);
ACDK_STD_NODE_CODE_POSTPARSE(DefunDecl, ParseNode, ClassCode);

ACDK_STD_NODE_CODE_POSTPARSE_EMIT(ExtendStatement, ParseNode, Code);

} // aal
} // acdk

#endif //acdk_aal_ClassDecl_h
