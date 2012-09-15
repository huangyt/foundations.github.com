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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLNamedNodeMap.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLNamedNodeMap_h
#define acdk_xml_libxmldom_LibXMLNamedNodeMap_h

#include "LibXMLNode.h"
#include <org/w3c/dom/NamedNodeMap.h>

namespace acdk {
namespace xml {
namespace libxmldom {

enum NamedNodeMapType
{
  NNMTAttributes = 0,
  NNMTEntities  = 1,
  NNMTNotations = 2
};

ACDK_DECL_CLASS(LibXMLNamedNodeMap);

class ACDK_XML_PUBLIC LibXMLNamedNodeMap
: extends acdk::lang::Object
, implements org::w3c::dom::NamedNodeMap
{
  ACDK_WITH_METAINFO(LibXMLNamedNodeMap)
protected:
  xmlNodePtr _nodePtr;
  int _type;
public:
  foreign LibXMLNamedNodeMap(xmlNodePtr np, int type)
  : _nodePtr(np)
  , _type(type)
  {
  }
  org::w3c::dom::RNode getNamedItem(IN(RString) name);
  org::w3c::dom::RNode setNamedItem(IN(org::w3c::dom::RNode) arg) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNode removeNamedItem(IN(RString) name) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNode item(int index);
  int getLength();
  org::w3c::dom::RNode getNamedItemNS(IN(RString) namespaceURI, IN(RString) localName);
  org::w3c::dom::RNode setNamedItemNS(IN(org::w3c::dom::RNode) arg) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNode removeNamedItemNS(IN(RString) namespaceURI, IN(RString) localName);
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLNamedNodeMap_h
