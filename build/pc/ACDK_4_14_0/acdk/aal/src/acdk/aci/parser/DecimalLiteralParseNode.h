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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/DecimalLiteralParseNode.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_DecimalLiteralParseNode_h
#define acdk_aci_parser_DecimalLiteralParseNode_h

#include "LiteralParseNode.h"


namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(DecimalLiteralParseNode);

/**
  implements the standard decimal parser
  by default it register as "DEC_LITERAL"
  with the syntax "([0-9])+[LISB]?"
*/
class ACDK_ACI_PUBLIC DecimalLiteralParseNode
: extends LiteralParseNode
{
  ACDK_WITH_METAINFO(DecimalLiteralParseNode)
public:
  DecimalLiteralParseNode(IN(RString) nodename = "DEC_LITERAL", IN(RString) syntax = "^([0-9])+[LISB]?", int bindTo = 0, IN(RString) help = "")
  : LiteralParseNode(nodename, syntax, bindTo, help)
  {
  }

  virtual acdk::aci::ast::RTerminal createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cp);
  
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_DecimalLiteralParseNode_h

