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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/XMLFilterImpl.h,v 1.7 2005/02/05 10:45:38 kommer Exp $

#ifndef org_xml_sax_helpers_XMLFilterImpl_h
#define org_xml_sax_helpers_XMLFilterImpl_h

#include <acdk.h>
#include "../EntityResolver.h"
#include "../DTDHandler.h"
#include "../ContentHandler.h"
#include "../ErrorHandler.h"
#include "../XMLFilter.h"

namespace org {
namespace xml {
namespace sax {
namespace helpers {

ACDK_DECL_CLASS(XMLFilterImpl);

class ACDK_ORG_XML_PUBLIC XMLFilterImpl
: extends Object
, implements XMLFilter
, implements EntityResolver
, implements DTDHandler
, implements ContentHandler
, implements ErrorHandler
{
  ACDK_WITH_METAINFO(XMLFilterImpl)
protected:
  RXMLReader _parent;
  RLocator _locator;
  REntityResolver _entityResolver;
  RDTDHandler _dtdHandler;
  RContentHandler _contentHandler;
  RErrorHandler _errorHandler;
public:
  XMLFilterImpl() {}
  XMLFilterImpl(IN(RXMLReader) parent)
    : _parent(parent)
  {}
  virtual void setParent(IN(RXMLReader) parent) { _parent = parent; }
  virtual RXMLReader getParent() { return _parent; }
  virtual void setFeature(IN(RString) name, bool state) THROWS2(RSAXNotRecognizedException, RSAXNotSupportedException)
  {
    if (_parent == Nil)
      THROW1(SAXNotRecognizedException, "Feature not recognized: " + name);
    _parent->setFeature(name, state);
  }
  virtual bool getFeature(IN(RString) name) THROWS2(RSAXNotRecognizedException, RSAXNotSupportedException)
  {
    if (_parent == Nil)
      THROW1(SAXNotRecognizedException, "Feature not recognized: " + name);
    return _parent->getFeature(name);
  }
  virtual void setProperty(IN(RString) name, IN(RObject) value) THROWS2(RSAXNotRecognizedException, RSAXNotSupportedException)
  {
    if (_parent == Nil)
      THROW1(SAXNotRecognizedException, "Property: " + name);
    _parent->setProperty(name, value);
  }

  virtual RObject getProperty(IN(RString) name) THROWS2(RSAXNotRecognizedException, RSAXNotSupportedException)
  {
    if (_parent == Nil)
      THROW1(SAXNotRecognizedException, "Property: " + name);
    return _parent->getProperty(name);
  }
  virtual void setEntityResolver(IN(REntityResolver) resolver)
  {
    _entityResolver = resolver;

  }
  virtual REntityResolver getEntityResolver() { return _entityResolver; }
  virtual void setDTDHandler(IN(RDTDHandler) handler)
  {
    _dtdHandler = handler;
  }
  virtual RDTDHandler getDTDHandler() { return _dtdHandler; }
  virtual void setContentHandler(IN(RContentHandler) handler)
  {
    _contentHandler = handler;
  }
  virtual RContentHandler getContentHandler() { return _contentHandler; }
  virtual void setErrorHandler(IN(RErrorHandler) handler)
  {
    _errorHandler = handler;
  }
  virtual RErrorHandler getErrorHandler() { return _errorHandler; }
  virtual void parse(IN(RInputSource) input) THROWS2(RSAXException, acdk::io::RIOException)
  {
    _setupParse();
    _parent->parse(input);
  }
  virtual void parse(IN(RString) systemId) THROWS2(RSAXException, acdk::io::RIOException)
  {
    _setupParse();
    _parent->parse(systemId);
  }
  virtual RInputSource resolveEntity(IN(RString) publicId, IN(RString) systemId) THROWS2(RSAXException, acdk::io::RIOException)
  {
    if (_entityResolver != Nil)
      return _entityResolver->resolveEntity(publicId, systemId);
    return Nil;
  }
  virtual void notationDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId) THROWS1(RSAXException)
  {
    if (_dtdHandler != Nil)
      _dtdHandler->notationDecl(name, publicId, systemId);
  }
  virtual void unparsedEntityDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId, IN(RString) notationName) THROWS1(RSAXException)
  {
    if (_dtdHandler != Nil)
      _dtdHandler->unparsedEntityDecl(name, publicId, systemId, notationName);
  }
  virtual void setDocumentLocator(IN(RLocator) locator)
  {
    _locator = locator;
	  if (_contentHandler != Nil) 
	    _contentHandler->setDocumentLocator(locator);
	}
  virtual void startDocument() THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->startDocument();
  }
  virtual void endDocument() THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->endDocument();
  }
  virtual void startPrefixMapping(IN(RString) prefix, IN(RString) uri) THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->startPrefixMapping(prefix, uri);
  }
  virtual void endPrefixMapping(IN(RString) prefix) THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->endPrefixMapping(prefix);
  }

  virtual void startElement(IN(RString) uri, IN(RString) localName, IN(RString) qName, IN(RAttributes) atts) THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->startElement(uri, localName, qName, atts);
  }
  virtual void endElement(IN(RString) uri, IN(RString) localName, IN(RString) qName)  THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->endElement(uri, localName, qName);
  }
  virtual void characters(IN(RString) chars)  THROWS1(RSAXException)
  {
    if (_contentHandler != Nil) 
	    _contentHandler->characters(chars);
  }
  virtual void ignorableWhitespace(IN(acdk::lang::RString) chars) THROWS1(RSAXException) 
  {
    if (_contentHandler != Nil) 
	    _contentHandler->ignorableWhitespace(chars);
  }
  void processingInstruction(IN(RString) target, IN(RString) data) THROWS1(RSAXException) 
  {
	  if (_contentHandler != Nil)
	    _contentHandler->processingInstruction(target, data);
  }
  void skippedEntity(IN(RString) name) THROWS1(RSAXException) 
  {
	  if (_contentHandler != Nil) 
	    _contentHandler->skippedEntity(name);
	}
  void warning(IN(RSAXParseException) e) THROWS1(RSAXException) 
  {
	  if (_errorHandler != Nil) 
	    _errorHandler->warning(e);
  }
  void error(IN(RSAXParseException) e) THROWS1(RSAXException) 
  {
	  if (_errorHandler != Nil) 
	    _errorHandler->error(e);
	}
  void fatalError (IN(RSAXParseException) e) THROWS1(RSAXException) 
  {
	  if (_errorHandler != Nil) 
	    _errorHandler->fatalError(e);
	
  }
private:
  void _setupParse()
  {
	  if (_parent == Nil)
	    THROW1(NullPointerException, "No parent for filter");
	  _parent->setEntityResolver(this); // ### @todo recursive reference
	  _parent->setDTDHandler(this);
	  _parent->setContentHandler(this);
	  _parent->setErrorHandler(this);
  }
};
} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers
#endif //org_xml_sax_helpers_XMLFilterImpl_h
