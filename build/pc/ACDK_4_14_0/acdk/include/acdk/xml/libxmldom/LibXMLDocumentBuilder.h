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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLDocumentBuilder.h,v 1.6 2005/02/12 17:33:46 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLDocumentBuilder_h
#define acdk_xml_libxmldom_LibXMLDocumentBuilder_h

#include "libxmldom.h"
#include <org/xml/sax/helpers/DefaultHandler.h>
#include <org/w3c/dom/DOMImplementation.h>
#include <org/w3c/dom/Document.h>
#include <org/w3c/dom/DocumentType.h>
#include <org/xml/sax/EntityResolver.h>
#include <org/xml/sax/ErrorHandler.h>
#include <acdk/xml/parsers/DocumentBuilder.h>


namespace acdk {
namespace xml {
namespace libxmldom {

USING_CLASS(acdk::xml::parsers::, DocumentBuilder);

ACDK_DECL_CLASS(LibXMLDocumentBuilder);

class ACDK_XML_PUBLIC LibXMLDocumentBuilder
: extends acdk::xml::parsers::DocumentBuilder
, extends org::w3c::dom::DOMImplementation
{
  ACDK_WITH_METAINFO(LibXMLDocumentBuilder)
protected:
  bool _validate;
  bool _coalesce;
  bool _expandEntities;
  org::xml::sax::REntityResolver _entityResolver;
  org::xml::sax::RErrorHandler _errorHandler;
  bool _seenFatalError;
  int _extendedFlags;
public:
  LibXMLDocumentBuilder(bool validate = true, bool coalesce = false, bool expandEntities = false)
  : _validate(validate)
  , _coalesce(coalesce)
  , _expandEntities(expandEntities)
  , _errorHandler(Nil)
  , _seenFatalError(false)
  , _extendedFlags(0)
  {
  }
  org::w3c::dom::RDOMImplementation getDOMImplementation()
  {
    return this;
  }

  bool isNamespaceAware()
  {
    return true;
  }

  bool isValidating()
  {
    return _validate;
  }
  /**
    @param flags combination of acdk::xml::sax::XMLReaderFlags 
  */
  void setExtendedFlags(int flags) { _extendedFlags = flags; }
  /**
    @return flags combination of acdk::xml::sax::XMLReaderFlags 
  */
  int getExtendedFlags() { return _extendedFlags; }
  org::w3c::dom::RDocument newDocument()
  {
    return createDocument(Nil, Nil, Nil);
  }

  org::w3c::dom::RDocument parse(IN(org::xml::sax::RInputSource) input) THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  org::w3c::dom::RDocument parse(IN(acdk::io::RFile) file)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
  {
    return DocumentBuilder::parse(file);
  }
  org::w3c::dom::RDocument parse(IN(acdk::io::RReader) reader)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
  {
    return DocumentBuilder::parse(reader);
  }
  org::w3c::dom::RDocument parse(IN(acdk::io::RReader) reader, IN(RString) systemID)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
  {
    return DocumentBuilder::parse(reader, systemID);
  }
  org::w3c::dom::RDocument parse(IN(RString) uri)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException)
  {
    return DocumentBuilder::parse(uri);
  }
  

  org::w3c::dom::RDocument _parseStream(IN(acdk::io::RReader) in, IN(RbyteArray) detectBuffer, IN(RString) publicId, IN(RString) systemId, IN(RString) base, bool validate,
                                                bool coalesce, bool expandEntities, bool entityResolver, bool errorHandler);
  
  void setEntityResolver(IN(org::xml::sax::REntityResolver) resolver)
  {
    _entityResolver = resolver;
  }

  void setErrorHandler(IN(org::xml::sax::RErrorHandler) handler)
  {
    _errorHandler = handler;
  }

  bool hasFeature(IN(RString) feature, IN(RString) version);
  
  RObject getFeature(IN(RString) feature, IN(RString) version);
  
  org::w3c::dom::RDocument createDocument(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(org::w3c::dom::RDocumentType) doctype);

  org::w3c::dom::RDocumentType createDocumentType(IN(RString) qualifiedName, IN(RString) publicId, IN(RString) systemId);
  
    
  void _setDocumentLocator(IN(RObject) ctx, IN(RObject) loc);
  
  acdk::io::RReader _resolveEntity(IN(RString) publicId, IN(RString) systemId) THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  
  void _warning(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);

  void _error(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);

  void _fatalError(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLDocumentBuilder_h
