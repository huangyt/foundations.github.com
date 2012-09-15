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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/DefaultHandler.h,v 1.7 2005/02/05 10:45:38 kommer Exp $


#ifndef org_xml_sax_helpers_DefaultHandler_h
#define org_xml_sax_helpers_DefaultHandler_h

#include <acdk.h>
#include "../EntityResolver.h"
#include "../DTDHandler.h"
#include "../ContentHandler.h"
#include "../ErrorHandler.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(DefaultHandler);
/**
  this default handler for SAX parsing just does nothing in the callback methods
*/
class ACDK_ORG_XML_PUBLIC DefaultHandler
: extends acdk::lang::Object
, implements EntityResolver
, implements DTDHandler
, implements ContentHandler
, implements ErrorHandler
{
  ACDK_WITH_METAINFO(DefaultHandler)
public:
  virtual RInputSource resolveEntity(IN(acdk::lang::RString) publicId, IN(acdk::lang::RString) systemId) THROWS1(RSAXException) { return Nil; }
  virtual void notationDecl(IN(acdk::lang::RString) name, IN(acdk::lang::RString) publicId, IN(acdk::lang::RString) systemId) THROWS1(RSAXException) {}
  virtual void unparsedEntityDecl(IN(acdk::lang::RString) name, IN(acdk::lang::RString) publicId, IN(acdk::lang::RString) systemId, IN(acdk::lang::RString) notationName) THROWS1(RSAXException) {}
  virtual void setDocumentLocator(IN(RLocator) locator) THROWS1(RSAXException) {}
  virtual void startDocument() THROWS1(RSAXException) {}
  virtual void endDocument() THROWS1(RSAXException) {}
  virtual void startPrefixMapping(IN(acdk::lang::RString) prefix, IN(acdk::lang::RString) uri) THROWS1(RSAXException) {}
  virtual void endPrefixMapping(IN(acdk::lang::RString) prefix) THROWS1(RSAXException) {}
  virtual void startElement(IN(acdk::lang::RString) uri, IN(acdk::lang::RString) localName, IN(acdk::lang::RString) qName, IN(RAttributes) attributes) THROWS1(RSAXException) {}
  virtual void endElement(IN(acdk::lang::RString) uri, IN(acdk::lang::RString) localName, IN(acdk::lang::RString) qName) THROWS1(RSAXException) {}
  virtual void characters(IN(acdk::lang::RString) chars) THROWS1(RSAXException) {}
  virtual void ignorableWhitespace(IN(acdk::lang::RString) chars) THROWS1(RSAXException) {}

  virtual void processingInstruction(IN(acdk::lang::RString) target, IN(acdk::lang::RString) data) THROWS1(RSAXException) {}
  virtual void skippedEntity(IN(acdk::lang::RString) name) THROWS1(RSAXException) {}
  virtual void warning(IN(RSAXParseException) e) THROWS1(RSAXException) {}
  virtual void error(IN(RSAXParseException) e) THROWS1(RSAXException) {}
  virtual void fatalError(IN(RSAXParseException) e) THROWS1(RSAXException)
  {
    RSAXParseException ex = e;
    throw ex;
  }
};

} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers

#endif //org_xml_sax_helpers_DefaultHandler_h
