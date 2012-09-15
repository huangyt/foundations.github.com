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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLXPathExpression.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLXPathExpression_h
#define acdk_xml_libxmldom_LibXMLXPathExpression_h

#include "libxmldom.h"

#include <org/w3c/dom/xpath/XPathExpression.h>
#include <org/w3c/dom/xpath/XPathNSResolver.h>

#include "LibXMLDocument.h"

struct _xmlXPathCompExpr;
typedef struct _xmlXPathCompExpr xmlXPathCompExpr;
typedef xmlXPathCompExpr *xmlXPathCompExprPtr;

namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLXPathExpression);

class ACDK_XML_PUBLIC LibXMLXPathExpression
: extends acdk::lang::Object
, implements org::w3c::dom::xpath::XPathExpression
{
  ACDK_WITH_METAINFO(LibXMLXPathExpression)
protected:
  foreign xmlXPathCompExprPtr _expr;
public:
  LibXMLXPathExpression(IN(RLibXMLDocument) doc, IN(RString) expression, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver);
  foreign ~LibXMLXPathExpression();
  acdk::lang::RObject evaluate(IN(org::w3c::dom::RNode) contextNode, short type, IN(RObject) result) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException);
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLXPathExpression_h
