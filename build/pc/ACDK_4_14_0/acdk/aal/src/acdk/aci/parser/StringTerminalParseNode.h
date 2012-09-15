// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_StringTerminalParseNode_h
#define acdk_aci_StringTerminalParseNode_h

#include "TerminalParseNode.h"
#include <acdk/text/RegExp.h>

namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(StringTerminalParseNode);

/**
  StringTerminalParseNode read standard String
  @seealso acdk::aci::ast::Terminal
*/
class ACDK_ACI_PUBLIC StringTerminalParseNode
: extends TerminalParseNode
{
  ACDK_WITH_METAINFO(StringTerminalParseNode)
public:
  StringTerminalParseNode(IN(RString) nodename, IN(RString) help = "")
  : TerminalParseNode(nodename, help)
  {
  }
  
  virtual RTerminal scanNextFromSource(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl);
  /**
    scan a standard string terminal
    it should point to the leading " or String prefix
    after parsing it points behind the finishing ".
  */
  foreign static RString scanStringText(String::iterator& it, String::iterator end);

};

} // parser
} // aci
} // acdk


#endif //acdk_aci_StringTerminalParseNode_h

