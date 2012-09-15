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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/Keyword.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_ast_Keyword_h
#define acdk_aci_ast_Keyword_h

#include "Terminal.h"


namespace acdk {
namespace aci {
namespace ast {

ACDK_DECL_CLASS(Keyword);

/**
  A keyword is a terminal which used as keyword in the language syntax
*/
class ACDK_ACI_PUBLIC Keyword
: extends Terminal
{
  ACDK_WITH_METAINFO(Keyword)
public:
  RString _keyword;
  Keyword(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(acdk::aci::util::RCodeLocation) cl, IN(RString) nodeName, IN(RString) keyword)
  : Terminal(templ, cl, nodeName)
  , _keyword(keyword)
  {
    setSaveNodeAfterBuild(true);
  }
  /*
  Keyword(IN(acdk::aci::parser::RTerminalParseNode) templ, IN(RString) keyword)
  : Terminal(Nil, templ, "Keyword")
  , _keyword(keyword)
  {
    setSaveNodeAfterBuild(true);
  }
  */
  virtual RString getCodeString() { return _keyword; }
  RString getKeyword() { return _keyword; }
  virtual void printCodeTree(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
  {
    out->println(indent + "'" + _keyword + "'" );
  }
  virtual RString toString() { return "Keyword: " + _keyword; }
  //static RCode createCode(IN(RCompiler) comp, IN(RString) value, IN(RString) codename = "Keyword");
};


} // ast
} // aci
} // acdk



#endif //acdk_aci_ast_Keyword_h
