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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/LiteralParseNode.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_parser_LiteralParseNode_h
#define acdk_aci_parser_LiteralParseNode_h

#include "RegScanParseNode.h"


namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(LiteralParseNode);

class ACDK_ACI_PUBLIC LiteralParseNode
: extends RegScanParseNode
{
public:
  LiteralParseNode(IN(RString) nodename, IN(RString) syntax, int bindTo = 1, IN(RString) help = "")
  : RegScanParseNode(nodename, syntax, bindTo, help)
  {
  }
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_LiteralParseNode_h

