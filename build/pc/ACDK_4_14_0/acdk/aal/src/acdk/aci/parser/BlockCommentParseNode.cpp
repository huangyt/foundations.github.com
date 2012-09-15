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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/BlockCommentParseNode.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $

#include "BlockCommentParseNode.h"

namespace acdk {
namespace aci {
namespace parser {

acdk::aci::ast::RTerminal 
BlockCommentParseNode::scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
{
  if (input->startsWith(_begin) == false)
    return Nil;
  RString sub = input->substr(_begin->length());
  int idx = sub->indexOf(_end);
  if (idx == -1)
  {
    // ### @todo error
    return Nil;
  }
  cl->setEndCharPos(cl->getCharPos() + idx + _begin->length() + _end->length());
  return createTerminal(sub->substr(idx), cl);
}
  
  //static 
void 
BlockCommentParseNode::registerParseNode(IN(RCompiler) comp, IN(RString) start, IN(RString) end)
{
  if (comp->getParseNode("BLOCKCOMMENT") == Nil)
    comp->registerTerminal(new BlockCommentParseNode("BLOCKCOMMENT", start));
}


} // parser
} // aci
} // acdk


