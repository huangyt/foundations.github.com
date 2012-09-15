// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright(C) 2000-2003 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLXPathExpression.cpp,v 1.3 2005/03/03 13:49:49 kommer Exp $


#include "LibXMLXPathExpression.h"
#include "LibXMLXPathResult.h"
#include "LibXMLDOMInternals.h"

namespace acdk {
namespace xml {
namespace libxmldom {


LibXMLXPathExpression::LibXMLXPathExpression(IN(RLibXMLDocument) doc, IN(RString) expression, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver)
{
  _expr = xmlXPathCompile(STR2XML(expression));
}

LibXMLXPathExpression::~LibXMLXPathExpression()
{
  xmlXPathFreeCompExpr(_expr);
}

acdk::lang::RObject 
LibXMLXPathExpression::evaluate(IN(org::w3c::dom::RNode) contextNode, short type, IN(RObject) result) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException)
{
  if (instanceOf(contextNode, LibXMLNode) == false)
    return Nil;
  xmlNodePtr nodePtr = RLibXMLNode(contextNode)->getNodePtr();

  xmlXPathContextPtr ctx = xmlXPathNewContext(nodePtr->doc);
  if (ctx == 0)
    return Nil;
  
  ctx->node = nodePtr;
  xmlXPathObjectPtr eval = xmlXPathCompiledEval(_expr, ctx);
  xmlXPathFreeContext(ctx);
  return new LibXMLXPathResult(eval, nodePtr);
}

} // namespace libxmldom
} // namespace xml
} // namespace acdk

