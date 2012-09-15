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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/LineCommentParseNode.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_LineCommentParseNode_h
#define acdk_aci_parser_LineCommentParseNode_h

#include "RegScanParseNode.h"
#include "../ast/Whitespace.h"
#include "../util/CodeLocation.h"
#include "../Compiler.h"

namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(LineCommentParseNodeParseNode);

/**
  Implements the standard white space for C/C++/Java-text
*/
class ACDK_ACI_PUBLIC LineCommentParseNodeParseNode
: extends RegScanParseNode
{
  ACDK_WITH_METAINFO(LineCommentParseNodeParseNode)
public:
  LineCommentParseNodeParseNode(IN(RString) nodename = "LINECOMMENT", IN(RString) start = "//")
    : RegScanParseNode(nodename, "^" + acdk::text::RegExp::escape(start) + "(.*?)\\n", 1)
  {
    setScannerPrio(8);
  }
  //virtual bool isLineCommentParseNode() { return true; }
  virtual acdk::aci::ast::RTerminal createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp) 
  {
    return new acdk::aci::ast::Whitespace(this, cp, getNodeName(), input);
  }
  virtual bool isWhiteSpace() { return true; }
  virtual bool isComment() { return true; }
  static void registerParseNode(IN(RCompiler) comp, IN(RString) start = "//")
  {
    if (comp->getParseNode("LINECOMMENT") == Nil)
      comp->registerTerminal(new LineCommentParseNodeParseNode("LINECOMMENT", start));
  }
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_LineCommentParseNode_h

