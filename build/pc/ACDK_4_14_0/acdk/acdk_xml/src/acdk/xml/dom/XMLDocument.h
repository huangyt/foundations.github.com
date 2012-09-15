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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLDocument.h,v 1.14 2005/04/08 10:53:21 kommer Exp $

#ifndef acdk_xml_dom_Document_h
#define acdk_xml_dom_Document_h


#include "dom.h"
#include "XMLElement.h"


#include <org/w3c/dom/Document.h>
#include <org/w3c/dom/Element.h>

namespace acdk {
namespace xml {
namespace dom {

USING_CLASS(::org::w3c::dom::, DOMImplementation);
USING_CLASS(::org::w3c::dom::, Element);
USING_CLASS(::org::w3c::dom::, EntityReference);
USING_CLASS(::org::w3c::dom::, Text);
USING_CLASS(::org::w3c::dom::, Comment);
USING_CLASS(::org::w3c::dom::, CDATASection);
USING_CLASS(::org::w3c::dom::, ProcessingInstruction);
USING_CLASS(::org::w3c::dom::, Attr);
USING_CLASS(::org::w3c::dom::, NodeList);


ACDK_DECL_CLASS(XMLDocument);

/** 
  API: acdk.xml.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/04/08 10:53:21 $
*/
class ACDK_XML_PUBLIC XMLDocument
: extends XMLNode
, implements org::w3c::dom::Document
, implements org::w3c::dom::DOMImplementation
{
  ACDK_WITH_METAINFO(XMLDocument)
public: 
  XMLDocument()
  : XMLNode("", org::w3c::dom::DOCUMENT_NODE)
  {
  }
  virtual RString toString()
  {
    return "XMLDocument: " + _childsToString();
  }
  virtual RDocument getOwnerDocument() { return this; }
  
  virtual ::org::w3c::dom::RDocumentType getDoctype();
  /** @bug not supported */
  virtual RDOMImplementation getImplementation();
  
  virtual ::org::w3c::dom::RElement getDocumentElement() 
  { 
    if (_childs->length() > 0)
      return (::org::w3c::dom::RElement)_childs[0];
    return Nil; 
  }
  void setDocumentRoot(IN(RXMLElement) el);
  
  /** @bug not supported */
  virtual org::w3c::dom::RElement createElement(IN(RString) s) THROWS1(org::w3c::dom::RDOMException);
  /** @bug not supported */
  virtual ::org::w3c::dom::RDocumentFragment createDocumentFragment();
  /** @bug not supported */
  virtual RText createTextNode(IN(RString) s);
  /** @bug not supported */
  virtual RComment createComment(IN(RString) s);
  /** @bug not supported */
  virtual RCDATASection createCDATASection(IN(RString) s) THROWS1(org::w3c::dom::RDOMException);
  /** @bug not supported */
  virtual RProcessingInstruction createProcessingInstruction(IN(RString) s, IN(RString) s1) THROWS1(org::w3c::dom::RDOMException);
  /** @bug not supported */
  virtual RAttr createAttribute(IN(RString) s) THROWS1(org::w3c::dom::RDOMException);
  /** @bug not supported */
  virtual REntityReference createEntityReference(IN(RString) s) THROWS1(org::w3c::dom::RDOMException);
  /** @bug not supported */
  virtual RNodeList getElementsByTagName(IN(RString) s);
// DOMImplementation
  virtual bool hasFeature(IN(RString) feature, IN(RString) version);
  virtual RDocument createDocument(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(RDocumentType) doctype);
  
  foreign RString toXML() { return XMLNode::toXML(); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_Document_h
