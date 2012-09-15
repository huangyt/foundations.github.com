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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/KeywordParseNode.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $


#include "KeywordParseNode.h"
#include "../ast/Keyword.h"
#include "../Compiler.h"

namespace acdk {
namespace aci {
namespace parser {

  /*
RAstNode
KeywordParseNode::parse(IN(RCompiler) comp)
{
  ScannerTokenStack ss(comp->scanner);
  RTerminal n = ss.getNext();
  if (instanceof(n, Keyword) == false)
    return Nil;
  if (RKeyword(n)->getKeyword()->equals(_keyword) == false)
    return Nil;
  ss.commit();
  return n;
}*/

KeywordParseNode::KeywordParseNode(IN(RString) nodename, IN(RString) keyword, IN(RString) help)
: RegScanParseNode(nodename, help)
, _keyword(keyword)
{
  setScannerPrio(7);
  RString regExp = acdk::text::RegExp::escape(keyword);
  if (Character::isLetter(keyword->charAt(0)) == true || keyword->charAt(0) == '_')
  {
    regExp = "^(" + regExp  + ")([^0-9A-Za-z_]|$)";
    _bindTo = 1;
  }
  else
  {
    regExp = "^" + regExp;
    _bindTo = 0;
  }
  setScannerSyntax(regExp);
}


acdk::aci::ast::RTerminal 
KeywordParseNode::createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
{
  return new acdk::aci::ast::Keyword(this, cl, getNodeName(), input);
}

} // parser
} // aci
} // acdk



