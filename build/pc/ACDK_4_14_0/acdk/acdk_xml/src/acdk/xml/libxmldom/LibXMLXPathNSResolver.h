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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLXPathNSResolver.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLXPathNSResolver_h
#define acdk_xml_libxmldom_LibXMLXPathNSResolver_h

#include "libxmldom.h"
#include <org/w3c/dom/xpath/XPathNSResolver.h>

#include "LibXMLDocument.h"

namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLXPathNSResolver);

class ACDK_XML_PUBLIC LibXMLXPathNSResolver
: extends acdk::lang::Object
, implements org::w3c::dom::xpath::XPathNSResolver
{
  ACDK_WITH_METAINFO(LibXMLXPathNSResolver)
protected:
  RLibXMLDocument _doc;
public:
  LibXMLXPathNSResolver(IN(RLibXMLDocument) doc)
  : _doc(doc)
  {
  }
  RString lookupNamespaceURI(IN(RString) prefix);
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLXPathNSResolver_h
