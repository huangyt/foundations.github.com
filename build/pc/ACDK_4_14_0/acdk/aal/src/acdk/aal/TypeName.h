// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aal_TypeName_h
#define acdk_aal_TypeName_h


#include "VarName.h"
#include "../aci/Compiler.h"

namespace acdk {
namespace aal {

USING_CLASS(acdk::aci::, Code);
USING_CLASS(acdk::aci::, ParseNode);

ACDK_DECL_CLASS(TypeName);

class ACDK_AAL_PUBLIC TypeName
: extends IdentifierCode
{
  ACDK_WITH_METAINFO(TypeName)
public:
  TypeName(IN(RParseNode) templ, IN(RString) tpname = "") 
  : IdentifierCode(templ, tpname)
  {
  }
  
  void execute_inner(IN(REvalEnv) env);
  virtual RString getTypeName() { return _identifier; }
  void setTypeName(IN(RString) name) { _identifier = name; }
  static RCode createCode(IN(RCompiler) comp, IN(RString) name, IN(RString) codename = "TypeName")
  {
    RTypeName tp = (RTypeName)comp->createCode(codename);
    tp->setIdentifier(name);
    return &tp;
  }
};

ACDK_DECL_CLASS(TypeNameParseNode);

class ACDK_AAL_PUBLIC TypeNameParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(TypeNameParseNode)
public:
  TypeNameParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help) {}
  
  RCode parse(IN(RCompiler) comp);
  virtual RCode createStandarCode() { return new TypeName(this); }
};

ACDK_DECL_CLASS(FqTypeName);

class ACDK_AAL_PUBLIC FqTypeName
: extends TypeName
{
  ACDK_WITH_METAINFO(FqTypeName)
public:
  FqTypeName(IN(RParseNode) templ) 
  : TypeName(templ)
  {
  }
  /**
    return the fully qualified name like acdk.lang.Integer
  */
  virtual RString getName() { return getTypeName(); }
  /**
    @see getName
  */
  RString getTypeName();
  /**
    return the last element in a fully qualified
    name. 
    If fq name is acdk.lang.Integer it returns Integer
  */
  RString getLastElemName();
  /**
    return all but the last element in a fully
    qualified name.
    if fq name is acdk.lang.Integer it returns acdk.lang
  */
  RString getNotLastElemName();
  static RCode createCode(IN(RCompiler) comp, IN(RString) fqname, IN(RString) codename = "FqTypeName");
};

ACDK_DECL_CLASS(FqTypeNameParseNode);

class ACDK_AAL_PUBLIC FqTypeNameParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(FqTypeNameParseNode)
public:
  FqTypeNameParseNode(IN(RString) nodename, IN(RString) rulesyntax, IN(RString) help = "") 
  : ParseNode(nodename, rulesyntax, help) {}
  
  virtual RCode createStandarCode() { return new FqTypeName(this); }
};


} // aal
} // acdk


#endif //acdk_aal_TypeName_h
