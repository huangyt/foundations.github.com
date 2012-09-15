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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLElement.h,v 1.10 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLElement_h
#define acdk_xml_dom_XMLElement_h
#include "dom.h"

#include "XMLNode.h"
#include "XMLAttr.h"
#include <org/w3c/dom/Element.h>

namespace acdk {
namespace xml {
namespace dom {



//using namespace org::w3c::dom;
using org::w3c::dom::RDOMException;
USING_CLASS(org::w3c::dom::, Attr);

ACDK_DECL_CLASS(XMLElement);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLElement
: extends XMLNode
, implements org::w3c::dom::Element
{
  ACDK_WITH_METAINFO(XMLElement)
protected:
  RXMLAttrArray _attributes;
  RDocument _document;
public:
  XMLElement(IN(RString) name)
  : XMLNode(name, ELEMENT_NODE)
  , _attributes(new XMLAttrArray(0))
  {
  }
  RString toString()
  {
    StringBuffer sb;
    sb << "XMLElement(" << getNodeName() << "): ";
    for (int i = 0; i < _attributes->length(); ++i)
    {
      sb << " " << _attributes[i]->toString(); 
    }
    sb << _childsToString();
    return sb.toString();
  }

  RDocument getOwnerDocument()
  {
    if (_document != Nil)
      return _document;
    return XMLNode::getOwnerDocument();
  }
  OUT(RDocument) getOwnerDocumentRef() { return _document; }
  void setOwnerDocument(IN(RDocument) doc) { _document = doc; }
  virtual RString getTagName() { return XMLNode::getNodeName(); }
  RNamedNodeMap getAttributes();
  virtual RString getAttribute(IN(RString) name);
  virtual void setAttribute(IN(RString) name, IN(RString) value) THROWS1(RDOMException);
  virtual void removeAttribute(IN(RString) name) THROWS1(RDOMException);
  virtual RAttr getAttributeNode(IN(RString) name);
  virtual RAttr setAttributeNode(IN(RAttr) newAttr) THROWS1(RDOMException);
  virtual RAttr removeAttributeNode(IN(RAttr) oldAttr) THROWS1(RDOMException);
  virtual RNodeList getElementsByTagName(IN(RString) name);
  virtual void normalize();
  RXMLAttrArray getAttribues() { return _attributes; }
  /** dom4j like extension */
  virtual int attributeCount();
  /** dom4j like extension */
  virtual RAttr attribute(int idx);

  foreign RString toXML() { return XMLNode::toXML(); }
protected:
  /**
    returns Nil if not found
  */
  RXMLAttr _getAttr(IN(RString) name);
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLElement_h
