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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLDocumentBuilder.cpp,v 1.8 2005/02/05 10:45:36 kommer Exp $


#include "LibXMLDocumentBuilder.h"
#include <org/xml/sax/helpers/NamespaceSupport.h>
#include <org/xml/sax/helpers/LocatorImpl.h>
#include <acdk/io/FileReader.h>
#include <acdk/net/URL.h>
#include <acdk/net/MalformedURLException.h>

#include "../sax/NamedBufferReader.h"
#include "LibXMLDOMInternals.h"
#include "LibXMLNode.h"
#include "../sax/XMLReader.h"

#include "LibXMLDocument.h"

namespace acdk {
namespace xml {
namespace libxmldom {



acdk::xml::sax::RNamedBufferReader
_getInputStream(IN(org::xml::sax::RInputSource) input)
{
  acdk::io::RReader in = input->getByteStream();
  RString systemId = input->getSystemId ();
  if (in == Nil)
  {
    if (systemId == Nil)
      THROW1_FQ(acdk::io::, IOException, "No systemID given");
    try
    {
      in = acdk::net::URL(systemId).openStream();
    }
    catch (acdk::net::RMalformedURLException e)
    {
      in = new acdk::io::FileReader(systemId);
    }
  }
  return new acdk::xml::sax::NamedBufferReader(systemId, in);
}

org::w3c::dom::RDocument 
LibXMLDocumentBuilder::parse(IN(org::xml::sax::RInputSource) input) THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
{
  sax::XMLReader xmlReader;

  if (_errorHandler != Nil)
    xmlReader.setErrorHandler(_errorHandler);
  xmlReader.setExtendedFlags(_extendedFlags);
  
  
  return xmlReader.parseInternal(input);
  /*
  acdk::xml::sax::RNamedBufferReader in = _getInputStream(input);
  RString publicId = input->getPublicId();
  RString systemId = input->getSystemId();
  
  RString base = org::xml::sax::helpers::NamespaceSupport::getBaseURI(systemId);
  const char* basec = 0;
  if (base != Nil)
  {
    base = base->convert(CCUtf8);
    basec = base->c_str();
  }
  ReaderWrapper iowrapper(&in);
  */
  /*
  XML_PARSE_RECOVER = 1 : recover on errors
    XML_PARSE_NOENT = 2 : substitute entities
    XML_PARSE_DTDLOAD = 4 : load the external subset
    XML_PARSE_DTDATTR = 8 : default DTD attributes
    XML_PARSE_DTDVALID = 16 : validate with the DTD
    XML_PARSE_NOERROR = 32 : suppress error reports
    XML_PARSE_NOWARNING = 64 : suppress warning reports
    XML_PARSE_PEDANTIC = 128 : pedantic error reporting
    XML_PARSE_NOBLANKS = 256 : remove blank nodes
    XML_PARSE_SAX1 = 512 : use the SAX1 interface internally
    XML_PARSE_XINCLUDE = 1024 : Implement XInclude substitition
    XML_PARSE_NONET = 2048 : Forbid network access
    XML_PARSE_NODICT = 4096 : Do not reuse the context dictionnary
    XML_PARSE_NSCLEAN = 8192 : remove redundant namespaces declarations
    XML_PARSE_NOCDATA = 16384 : merge CDATA as text nodes
    XML_PARSE_NOXINCNODE = 32768 : do not generate XINCLUDE START/END nodes
*/
  /*
  int options = XML_PARSE_DTDVALID;
  xmlDocPtr doc = xmlReadIO(ReaderWrapper::readCB, ReaderWrapper::closeCB, (void*)&iowrapper, basec, 0, options);
  return (org::w3c::dom::RDocument)LibXMLNode::newInstance((xmlNodePtr)doc);
  */
  /*
  ### @todo implement me
  acdk::xml::sax::RNamedBufferReader in = _getInputStream(input);
  byte[] detectBuffer = in.getDetectBuffer();
  RString base = org::xml::sax::helpers::NamespaceSupport::getBaseURI(systemId);
  // Handle zero-length document
  if (detectBuffer == Nil)
  {
    throw new SAXParseException("No document element", publicId, systemId, 0, 0);
  }
  _seenFatalError = false;
  return parseStream(in, detectBuffer, publicId, systemId, base, validate, coalesce, expandEntities, _entityResolver != Nil, _errorHandler != Nil);
  */
  return Nil;
}

org::w3c::dom::RDocument 
LibXMLDocumentBuilder::_parseStream(IN(acdk::io::RReader) in, IN(RbyteArray) detectBuffer, IN(RString) publicId, IN(RString) systemId, IN(RString) base, bool validate,
                                      bool coalesce, bool expandEntities, bool entityResolver, bool errorHandler)
{
  
  THROW0(UnsupportedOperationException);
/*ReaderWrapper iowrapper(&in);
  
  xmlDocPtr doc = xmlReadIO(ReaderWrapper::readCB, ReaderWrapper::closeCB, (void*)&iowrapper, base->c_str(), 0, options);
  return (org::w3c::dom::RDocument)LibXMLNode::newInstance((xmlNodePtr)doc);
  */
  return Nil;
}


bool 
LibXMLDocumentBuilder::hasFeature(IN(RString) feature, IN(RString) version)
{
  if (feature->equals("XML") == true || feature->equals("Core") == true)
  {
    if (version == Nil || 
        version->equals("3.0") == true ||
        version->equals("2.0") == true ||
        version->equals("1.0") == true)
      return true;
    return false;
  }
  if (feature->equals("Stylesheets") == true)
    return false;
  if (feature->equals("HTML") == true || 
      feature->equals("CSS") == true || 
      feature->equals("Range") == true || 
      feature->equals("CSS2") == true || 
      feature->equals("Stylesheets") == true)
    return false;
  if (feature->equals("XPath") == true)
    return version == Nil || version->equals("3.0") == true;
  if (feature->equals("Traversal") == true)
    return version == Nil || version->equals("2.0") == true;
  return false;
}

// DOM Level 3

RObject 
LibXMLDocumentBuilder::getFeature(IN(RString) feature, IN(RString) version)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

org::w3c::dom::RDocument 
LibXMLDocumentBuilder::createDocument(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(org::w3c::dom::RDocumentType) doctype)
{
  return new LibXMLDocument((xmlNodePtr)xmlNewDoc((const unsigned char*)"1.0"));
}

org::w3c::dom::RDocumentType 
LibXMLDocumentBuilder::createDocumentType(IN(RString) qualifiedName, IN(RString) publicId, IN(RString) systemId)
{
  THROW0(UnsupportedOperationException);
  
  //return new StandaloneDocumentType(qualifiedName, publicId, systemId);
  return Nil;
}

// Callback hooks from JNI

void 
LibXMLDocumentBuilder::_setDocumentLocator(IN(RObject) ctx, IN(RObject) loc)
{
  // ignore
}

acdk::io::RReader 
LibXMLDocumentBuilder::_resolveEntity(IN(RString) publicId, IN(RString) systemId) THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
{
  if (_entityResolver == Nil)
    return Nil;
  
  org::xml::sax::RInputSource source = _entityResolver->resolveEntity(publicId, systemId);
  if (source == Nil)
    return Nil;
  return &_getInputStream(source);
}

void 
LibXMLDocumentBuilder::_warning(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (_seenFatalError == false && _errorHandler != Nil)
  {
    org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
    _errorHandler->warning(new org::xml::sax::SAXParseException(message, l));
  }
}

void 
LibXMLDocumentBuilder::_error(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (_seenFatalError == false && _errorHandler != Nil)
  {
    org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
    _errorHandler->error(new org::xml::sax::SAXParseException(message, l));
  }
}

void 
LibXMLDocumentBuilder::_fatalError(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (_seenFatalError == false && _errorHandler != Nil)
  {
    _seenFatalError = true;
    org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
    _errorHandler->fatalError(new org::xml::sax::SAXParseException(message, l));
  }
}


} // namespace libxmldom
} // namespace xml
} // namespace acdk
