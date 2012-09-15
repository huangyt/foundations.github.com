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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/parser/FloatLiteralParseNode.cpp,v 1.4 2005/02/05 10:44:51 kommer Exp $


#include "FloatLiteralParseNode.h"
#include "../ast/Literal.h"

namespace acdk {
namespace aci {
namespace parser {

acdk::aci::ast::RTerminal 
FloatLiteralParseNode::createTerminal(IN(RString) input, IN(acdk::aci::util::RCodeLocation) cl)
{
  double d = Double::parseDouble(input);
  return new acdk::aci::ast::Literal(this, cl, inOf(d));
}
  

} // parser
} // aci
} // acdk



