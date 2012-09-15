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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLElement.h,v 1.5 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLElement_h
#define acdk_xml_libxmldom_LibXMLElement_h

#include "LibXMLNode.h"

namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLElement);

class ACDK_XML_PUBLIC LibXMLElement
: extends LibXMLNode
, implements org::w3c::dom::Element
{
  ACDK_WITH_METAINFO(LibXMLElement)
public:
  foreign LibXMLElement(xmlNodePtr np, bool ownsPtr = false)
  : LibXMLNode(np, ownsPtr == true ? XmlNodePtrHolder::xmlFreeNode : 0)
  {
  }
  RString getTagName()
  {
    return getNodeName();
  }

  RString getAttribute(IN(RString) name);

  void setAttribute(IN(RString) name, IN(RString) value) THROWS1(org::w3c::dom::RDOMException);
  
  void removeAttribute(IN(RString) name) THROWS1(org::w3c::dom::RDOMException)
  {
    org::w3c::dom::RAttr attr = getAttributeNode(name);
    if (attr != Nil)
      removeAttributeNode(attr);
  }

  org::w3c::dom::RAttr getAttributeNode(IN(RString) name);
  
  org::w3c::dom::RAttr setAttributeNode(IN(org::w3c::dom::RAttr) newAttr) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RAttr removeAttributeNode(IN(org::w3c::dom::RAttr) oldAttr) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNodeList getElementsByTagName(IN(RString) name);
  
  RString getAttributeNS(IN(RString) namespaceURI, IN(RString) localName);
  
  void setAttributeNS(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(RString) value) THROWS1(org::w3c::dom::RDOMException);

  void removeAttributeNS(IN(RString) namespaceURI, IN(RString) localName) THROWS1(org::w3c::dom::RDOMException)
  {
    org::w3c::dom::RAttr attr = getAttributeNodeNS(namespaceURI, localName);
    if (attr != Nil)
      removeAttributeNode(attr);
  }
  
  org::w3c::dom::RAttr getAttributeNodeNS(IN(RString) namespaceURI, IN(RString) localName);

  org::w3c::dom::RAttr setAttributeNodeNS(IN(org::w3c::dom::RAttr) newAttr) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNodeList getElementsByTagNameNS(IN(RString) namespaceURI, IN(RString) localName);
  
  bool hasAttribute(IN(RString) name);

  bool hasAttributeNS(IN(RString) namespaceURI, IN(RString) localName);

  // ### @todo implement me RTypeInfo getSchemaTypeInfo();
  
  void setIdAttribute(IN(RString) name, bool isId)
  {
    setIdAttributeNS(Nil, name, isId);
  }

  void setIdAttributeNode(IN(org::w3c::dom::RAttr) isAddr, bool isId);

  void setIdAttributeNS(IN(RString) namespaceURI, IN(RString) localName, bool isId);

  RString toString()
  {
    return SBSTR(getClass()->getName() << "[tagName=" << getTagName() << "]");
  }
  void normalize() { LibXMLNode::normalize(); }
  int attributeCount();
  org::w3c::dom::RAttr attribute(int idx);
  foreign RString toXML() { return LibXMLNode::toXML(); }
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk



#endif //acdk_xml_libxmldom_LibXMLElement_h
