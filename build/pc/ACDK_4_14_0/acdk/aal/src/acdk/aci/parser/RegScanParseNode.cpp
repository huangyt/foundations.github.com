// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "RegScanParseNode.h"

namespace acdk {
namespace aci {
namespace parser {

acdk::aci::ast::RTerminal 
RegScanParseNode::scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
{
  int slots[32];
  int size = _regExp->match(input, slots, 32, 0);
  if (size == -1)
    return Nil;
  if (size < _bindTo)
    return Nil;
  int offset = slots[_bindTo * 2 + 1];
  if (offset == 0)
    THROW1(Exception, SBSTR("Terminal reads 0 character: re: " << _syntax << "; bindTo: " << _bindTo));
  RString matched = input->substr(slots[_bindTo * 2], offset);
 
  cl->setEndCharPos(cl->getCharPos() + offset);
  return createTerminal(matched, cl);
}


} // parser
} // aci
} // acdk




