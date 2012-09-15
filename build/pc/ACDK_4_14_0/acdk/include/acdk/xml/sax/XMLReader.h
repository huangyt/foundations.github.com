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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/XMLReader.h,v 1.11 2005/02/13 03:25:57 kommer Exp $

#ifndef acdk_xml_sax_XMLReader_h
#define acdk_xml_sax_XMLReader_h

#include <acdk.h>
#include "../Config.h"
#include <org/xml/sax/XMLReader.h>
#include <org/xml/sax/DeclHandler.h>
#include <org/xml/sax/LexicalHandler.h>
#include <org/xml/sax/helpers/NamespaceSupport.h>
#include <org/xml/sax/helpers/StdErrorHandler.h>

#include <org/w3c/dom/Document.h>

namespace acdk {
namespace xml {
namespace sax {


enum XMLReaderFlags
{
  /** recover on errors */
  XMLRF_PARSE_RECOVER =     0x0001,
  /** substitute entities */
  XMLRF_PARSE_NOENT =       0x0002,
  /**  load the external subset */
  XMLRF_PARSE_DTDLOAD =     0x0004,
  /** default DTD attributes */
  XMLRF_PARSE_DTDATTR =     0x0008, 
  /** validate with the DTD */
  XMLRF_PARSE_DTDVALID =    0x0010,
  /** suppress error reports */
  XMLRF_PARSE_NOERROR =     0x0020, 
  /**  : suppress warning reports */
  XMLRF_PARSE_NOWARNING =   0x0040,
  /** pedantic error reporting */
  XMLRF_PARSE_PEDANTIC =    0x0080,
  /**  remove blank nodes */
  XMLRF_PARSE_NOBLANKS =    0x0100,
  /** use the SAX1 interface internally */
  XMLRF_PARSE_SAX1 =        0x0200,
  /**  : Implement XInclude substitition */
  XMLRF_PARSE_XINCLUDE =    0x0400,
  /**  Forbid network access */
  XMLRF_PARSE_NONET =       0x0800,
  /** Do not reuse the context dictionnary */
  XMLRF_PARSE_NODICT =      0x1000,
  /** remove redundant namespaces declarations */
  XMLRF_PARSE_NSCLEAN =     0x2000,
  /** merge CDATA as text nodes */
  XMLRF_PARSE_NOCDATA =     0x4000,
  /** do not generate XINCLUDE START/END nodes */
  XMLRF_PARSE_NOXINCNODE =  0x8000,
  /** do not use namespaces */
  XMLRF_PARSE_USE_NS       =  0x00010000,
  XMLRF_PARSE_USE_NS_PREFIX = 0x00020000,
  XMLRF_DEFAULTFLAGS =  XMLRF_PARSE_DTDLOAD,
  
  /** Parse HTML, not XML */
  XMLRF_PARSE_HTML          = 0x01000000,

  /** suppress error reports */
  HTML_PARSE_NOERROR = 32,
  HTML_PARSE_NOWARNING = 64,
  /** pedantic error reporting */
  HTML_PARSE_PEDANTIC = 128,
  /** remove blank nodes */
  HTML_PARSE_NOBLANKS = 256,
  /** Forbid network access */
  HTML_PARSE_NONET = 2048,

  XMLRF_PARSE_HTML_FLAGS = XMLRF_PARSE_HTML | HTML_PARSE_NOWARNING | HTML_PARSE_NONET | HTML_PARSE_NOERROR

  
};

ACDK_DECL_CLASS(XMLReader);

class ACDK_XML_PUBLIC XMLReader
: extends acdk::lang::Object
, implements org::xml::sax::XMLReader
{
  ACDK_WITH_METAINFO(XMLReader)
protected:
  org::xml::sax::REntityResolver _entityResolver;
  org::xml::sax::RDTDHandler _dtdHandler;
  org::xml::sax::RContentHandler _contentHandler;
  org::xml::sax::RErrorHandler _errorHandler;
  org::xml::sax::RDeclHandler _declarationHandler;

  org::xml::sax::RLexicalHandler _lexicalHandler;

  //bool _isStandalone;
  //bool _useNamespaces;
  //bool _useNamespacePrefixes;
  //bool _useValidation;
  bool _hasAlreadyFatal;
  org::xml::sax::helpers::RNamespaceSupport _ns;
  org::xml::sax::RLocator _locator;
public:
  bool _seenStartDocument;
protected:
  RString _baseURI;
  int _extendedFlags;
public:
  XMLReader(bool useNamespace = true, int xmlFlags = XMLRF_DEFAULTFLAGS)
  : _errorHandler(new org::xml::sax::helpers::StdErrorHandler())
  , _hasAlreadyFatal(false)
  , _ns(new org::xml::sax::helpers::NamespaceSupport())
  , _seenStartDocument(false)
  , _extendedFlags(0)
  {
  }
  ~XMLReader(){}
  virtual bool getFeature(IN(RString) name) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException);
  virtual void setFeature(IN(RString) name, bool value) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException);
  virtual RObject getProperty(IN(RString) name) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException);

  virtual void setProperty(IN(RString) name, IN(RObject) value) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException);
  virtual void setEntityResolver(IN(org::xml::sax::REntityResolver) resolver)
  {
    _entityResolver = resolver;
  }
  virtual org::xml::sax::REntityResolver getEntityResolver()
  {
    return _entityResolver;
  }
  virtual void setDTDHandler(IN(org::xml::sax::RDTDHandler) handler)
  {
    _dtdHandler = handler;
  }
  virtual org::xml::sax::RDTDHandler getDTDHandler() 
  {
    return _dtdHandler;
  }
  virtual void setContentHandler(IN(org::xml::sax::RContentHandler) handler)
  {
    _contentHandler = handler;
  }

  virtual org::xml::sax::RContentHandler getContentHandler()
  {
    return _contentHandler;
  }
  virtual void setErrorHandler(IN(org::xml::sax::RErrorHandler) handler)
  {
    _errorHandler = handler;
  }
  virtual org::xml::sax::RErrorHandler getErrorHandler()
  {
    return _errorHandler;
  }
  virtual void parse(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException);
  virtual void parse(IN(RString) systemId) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException);
  org::w3c::dom::RDocument parseInternal(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException);
  org::w3c::dom::RDocument parseInternal(IN(RString) systemId) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException);
  org::w3c::dom::RDocument parseInternalHtml(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException);
  
  RString getURI(IN(RString) prefix);
   /**
    @param flags combination of acdk::xml::sax::XMLReaderFlags 
  */
  void setExtendedFlags(int flags) { _extendedFlags = flags; }
  /**
    @return flags combination of acdk::xml::sax::XMLReaderFlags 
  */
  int getExtendedFlags() { return _extendedFlags; }
  bool useNamespace() { return _extendedFlags & XMLRF_PARSE_USE_NS; }
  bool isValidating() { return _extendedFlags & XMLRF_PARSE_DTDVALID; }
  
private:
  foreign void _checkFeatureName(IN(RString) name);
  void  _splitName(IN(RString) fqName, OUT(RString) localName, OUT(RString) prefix, OUT(RString) uri);
  foreign void _evaluteExtendedFlags();
public:
  foreign void _startDTD(IN(RString) name, IN(RString) externalID, IN(RString) systemID);
  foreign acdk::io::RReader _resolveEntity(IN(RString) publicId, IN(RString) systemId);
  void _startDocument(bool isStandalone);
  void _endDocument();
  void _internalEntityDecl(IN(RString) name, IN(RString) value);
  void _externalEntityDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId);
  void _notationDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId);
  void _attributeDecl(IN(RString) elementName, IN(RString) attributeName, IN(RString) type, IN(RString) mode, IN(RString) value);
  void _elementDecl(IN(RString) name, IN(RString) model);
  void _unparsedEntityDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId, IN(RString) notationName);
  void _startElement(IN(RString) name, IN(org::xml::sax::RAttributes) attrs);
  // key -> value, key -> value
  void _startElement(IN(RString) name, IN(RStringArray) attrs);
  void _setDocumentLocator(IN(org::xml::sax::RLocator) loc);

  void _endElement(IN(RString) name);
  void _startPrefixMapping(IN(RString) prefix, IN(RString) uri);
  void  _characters(IN(RString) text);
  void _ignorableWhitespace (IN(RString) text);
  void _processingInstruction(IN(RString) target, IN(RString) data);
  void _comment(IN(RString) text);
  void _cdataBlock(IN(RString) text);
  void _warning(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);
  void _error(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);
  void _fatalError(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId);
  bool hasAlreadyFatalError();
  bool setFatalError();
};

} // namespace sax
} // namespace xml
} // namespace acdk

#endif //acdk_xml_sax_XMLReader_h
