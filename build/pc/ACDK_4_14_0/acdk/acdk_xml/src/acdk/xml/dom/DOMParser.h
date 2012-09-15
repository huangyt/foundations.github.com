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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/DOMParser.h,v 1.8 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_DOMParser_h
#define acdk_xml_dom_DOMParser_h

#include "dom.h"
#include "XMLDocument.h"

#include <org/xml/sax/DocumentHandler.h>
#include <org/xml/sax/ErrorHandler.h>
#include "XMLElement.h"

namespace acdk {
namespace xml {
namespace dom {

USING_CLASS(org::xml::sax::, Locator);
USING_CLASS(org::xml::sax::, AttributeList);

ACDK_DECL_CLASS(DOMParser);


/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC DOMParser
: extends acdk::lang::Object
, implements org::xml::sax::DocumentHandler
, implements org::xml::sax::ErrorHandler
{
public:
  RLocator _locator;
  RXMLDocument _document;
  RNode _curNode;
  RText _curText;
  DOMParser() 
  : _document(Nil)
  {
    
  }
  /// deprecated
  RXMLDocument parse(IN(RString) text);
  RXMLDocument parse(IN(RbyteArray) text);
  virtual void setDocumentLocator(IN(RLocator) locator);
  virtual void startDocument() THROWS1(org::xml::sax::RSAXException);
  virtual void endDocument() THROWS1(org::xml::sax::RSAXException);
  virtual void startElement(IN(::acdk::lang::RString) s, IN(RAttributeList) attributelist) THROWS1(org::xml::sax::RSAXException);
  virtual void endElement(IN(::acdk::lang::RString) s) THROWS1(org::xml::sax::RSAXException);
  virtual void characters(IN(RString) str, int start, int length) THROWS1(org::xml::sax::RSAXException);
  
  
  
  virtual void ignorableWhitespace(IN(RString) str, int start, int length) THROWS1(org::xml::sax::RSAXException);
  virtual void processingInstruction(IN(::acdk::lang::RString) s, IN(::acdk::lang::RString) s1) 
      THROWS1(org::xml::sax::RSAXException);

  virtual void warning(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException);
  virtual void error(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException);
  virtual void fatalError(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException);

};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_DOMParser_h
