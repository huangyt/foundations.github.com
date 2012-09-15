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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/XMLReaderImpl.cpp,v 1.13 2005/02/13 03:25:57 kommer Exp $


#include "XMLReader.h"
#include "NamedBufferReader.h"
#include "AttributeListImpl.h"

#include <acdk/net/URL.h>
#include <acdk/io/MemWriter.h>
#include <org/xml/sax/helpers/LocatorImpl.h>
#include <org/xml/sax/helpers/AttributesImpl.h>

#include "LibXMLInternals.h"
#include "XmlLibLocator.h"
#include <org/xml/sax/helpers/LocatorImpl.h>
#include <org/xml/sax/helpers/NamespaceSupport.h>
#include "../libxmldom/LibXMLDocument.h"
#include "../../../libxml/HTMLparser.h"

#include <stdio.h> 
#include <stdarg.h>


namespace acdk {
namespace xml {
namespace sax {

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
#define DOUT(msg) std::cout << SBSTR(msg)->convert(CCUtf8)->c_str() << std::endl;
#else
#define DOUT(msg)
#endif

const char* feature_list[] = 
{
  "external-general-entities",
  "external-parameter-entities",
  "is-standalone",
  "lexical-handler/parameter-entities",
  "namespaces",
  "namespace-prefixes",
  "resolve-dtd-uris",
  "string-interning",
  "use-attributes2",
  "use-locator2",
  "use-entity-resolver2",
  "validation",
  0
};


bool containsInFeatureList(IN(RString) str)
{
  for (int i = 0; feature_list[i] != 0; ++i)
  {
    const char* feature = feature_list[i];
    if (str->equals(feature) == true)
      return true;
  }
  return false;
}

RString 
XMLReader::getURI(IN(RString) prefix)
{
  if (useNamespace() == true)
    return Nil;
  return _ns->getURI(prefix);
}
bool XMLReader::hasAlreadyFatalError() { return _hasAlreadyFatal; }
bool XMLReader::setFatalError() { return (_hasAlreadyFatal = true); }

bool 
XMLReader::getFeature(IN(RString) name) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException)
{
  _checkFeatureName(name);
  RString key = name->substring(strlen(FEATURES_PREFIX));
  if (key->equals("external-general-entities") == true ||
      key->equals("external-parameter-entities") == true ||
      key->equals("validation") == true
     )
  {
    return _extendedFlags & XMLRF_PARSE_DTDVALID;
  }
  else if (key->equals("is-standalone") == true)
  {
    return (_extendedFlags & XMLRF_PARSE_DTDLOAD) == false;
  }
  else if (key->equals("namespaces") == true)
  {
    return (_extendedFlags & XMLRF_PARSE_USE_NS);
  }
  else if (key->equals("namespace-prefixes") == true)
  {
    return(_extendedFlags & XMLRF_PARSE_USE_NS_PREFIX);
  }
  else if (key->equals("resolve-dtd-uris") == true)
  {
    return (_extendedFlags & XMLRF_PARSE_DTDLOAD);
  }
  return false;
}

void 
XMLReader::setFeature(IN(RString) name, bool value) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException)
{
  _checkFeatureName(name);
  RString key = name->substring(strlen(FEATURES_PREFIX));
  if (key->equals("namespaces") == true)
  {
    if (value == true)
      _extendedFlags |= XMLRF_PARSE_USE_NS;
    else
      _extendedFlags &= ~XMLRF_PARSE_USE_NS;
  }
  else if (key->equals("namespace-prefixes") == true)
  {
    if (value == true)
      _extendedFlags |= XMLRF_PARSE_USE_NS_PREFIX;
    else
      _extendedFlags &= ~XMLRF_PARSE_USE_NS_PREFIX;
  }
  else if (key->equals("validation") == true)
  {
    if (value == true)
      _extendedFlags |= XMLRF_PARSE_DTDVALID;
    else
      _extendedFlags &= ~XMLRF_PARSE_DTDVALID;
  }
}

void 
XMLReader::_checkFeatureName(IN(RString) name)
{
  if (name->startsWith(FEATURES_PREFIX) == false || 
      containsInFeatureList(name->substr(strlen(FEATURES_PREFIX))) == false)
    THROW1_FQ(org::xml::sax::, SAXNotRecognizedException, "Feature is not recognized: " + name);
}

RObject 
XMLReader::getProperty(IN(RString) name) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException)
{
  // ### @todo implement me
  return Nil;
}

void 
XMLReader::setProperty(IN(RString) name, IN(RObject) value) THROWS2(org::xml::sax::RSAXNotRecognizedException, org::xml::sax::RSAXNotSupportedException)
{
  // ### @todo implement me
}


enum SaxWarnLevel
{
  SWL_Warn,
  SWL_Error,
  SWL_FatalError
};


void invokeError(xmlParserCtxtPtr ctx, IN(RString) msg, SaxWarnLevel level)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(ctx->userData);
  RXMLReader target = sax->_xmlReader;
  org::xml::sax::RErrorHandler errorHandler = target->getErrorHandler();
  if (errorHandler == Nil || target->hasAlreadyFatalError() == true)
    return;
  
  xmlSAXLocatorPtr loc = (xmlSAXLocatorPtr)sax->loc;
  int lineNumber = loc->getLineNumber(ctx);
  int columnNumber = loc->getColumnNumber(ctx);
  RString publicId = XML2STR(loc->getPublicId(ctx));
  RString systemId = XML2STR(loc->getSystemId(ctx));
  org::xml::sax::RLocator locator = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
  try {
    org::xml::sax::RSAXParseException ex = new org::xml::sax::SAXParseException(msg, locator);
    if (level == SWL_FatalError)
    {
      if (target->_seenStartDocument == false)
        target->_startDocument(false);
      target->setFatalError();
    }
    if (level == SWL_Warn)
      errorHandler->warning(ex);
    else if (level == SWL_Error)
      errorHandler->error(ex);
    else
      errorHandler->fatalError(ex);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void checkValid(xmlParserCtxtPtr ctx)
{
  if (ctx->wellFormed == 0)
    invokeError(ctx, "Document is not weel-formed", SWL_FatalError);
  if (ctx->validate != 0 && ctx->valid == 0)
    invokeError(ctx, "Document is not valid", SWL_FatalError);
}
void checkValid(SaxParseContext* sctx)
{
  checkValid(sctx->ctx);
}

int	xmlInputCloseCB(void * context)
{
  return 1;
}


void	internalSubsetCB(void* vctx, const xmlChar * name, const xmlChar* externalId, const xmlChar* systemId)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2InternalSubset(sax->getXmlCtx(), name, externalId, systemId);

  RXMLReader target = sax->_xmlReader;
  target->_startDTD(XML2STR(name), XML2STR(externalId), XML2STR(systemId));
}


acdk::io::RReader getReader(IN(acdk::net::RURL) sourceUrl)
{
  return new NamedBufferReader(sourceUrl->toString(), sourceUrl->openStream());
}

void 
XMLReader::_startDTD(IN(RString) name, IN(RString) externalId, IN(RString) systemId)
{
  if (_lexicalHandler == Nil)
    return;
  _lexicalHandler->startDTD(name, externalId, org::xml::sax::helpers::NamespaceSupport::getAbsoluteURI(_baseURI, systemId));
}

void 
XMLReader::_startDocument(bool isStandalone)
{
  //_isStandalone = isStandalone;
  _seenStartDocument = true;
  if (_contentHandler == Nil)
    return;

  try {
    _contentHandler->startDocument();
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

xmlParserInputPtr
resolveEntityCB(void* vctx, const xmlChar* publicId, const xmlChar* systemId)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  RXMLReader target = sax->_xmlReader;
  
  acdk::io::RReader is = target->_resolveEntity(XML2STR(publicId), XML2STR(systemId));
  if (is == Nil)
    return 0;
  /** #### @todo implement me
  xmlParserCtxtPtr pctpr = xmlCreateIOParserCtxt(0, 0, xmlInputReadCB, xmlInputCloseCB, &is, XML_CHAR_ENCODING_NONE);
  xmlParserInputBufferPtr inputBuffer = xmlParserInputBufferCreateIO(xmlInputReadCB, xmlInputCloseCB, pctpr, XML_CHAR_ENCODING_NONE);
  

  xmlParserInputPtr inputPtr = xmlNewIOInputStream(sax->ctx, inputBuffer, XML_CHAR_ENCODING_NONE);
  return inputPtr;
  */
  return 0;
}

acdk::io::RReader 
XMLReader::_resolveEntity(IN(RString) publicId, IN(RString) systemId)
{
  if (_entityResolver == Nil)
    return Nil;

  try {
    org::xml::sax::RInputSource source = _entityResolver->resolveEntity(publicId, org::xml::sax::helpers::NamespaceSupport::getAbsoluteURI(_baseURI, systemId));
    if (source == Nil)
      return Nil;
    return source->getByteStream();
  } catch (RException ex) {
    if (instanceof(ex, org::xml::sax::SAXException) == true)
      THROW_INSTANCE(org::xml::sax::RSAXException(ex));
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
  return Nil;
}

void 
XMLReader::_internalEntityDecl(IN(RString) name, IN(RString) value)
{
  if (hasAlreadyFatalError() == true || _declarationHandler == Nil)
    return;
  try {
    _declarationHandler->internalEntityDecl(name, value);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void 
XMLReader::_externalEntityDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId)
{
  if (hasAlreadyFatalError() == true || _declarationHandler == Nil)
    return;
  try {
    _declarationHandler->externalEntityDecl(name, publicId, org::xml::sax::helpers::NamespaceSupport::getAbsoluteURI(_baseURI, systemId));
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
entityDeclCB(void* vctx, const xmlChar* name_, int type, const xmlChar* publicId, const xmlChar* systemId, xmlChar* content)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2EntityDecl(sax->getXmlCtx(), name_, type, publicId, systemId, content);
  
  RXMLReader target = sax->_xmlReader;

  checkValid(sax);
  RString name = XML2STR(name_);
  switch (type)
  {
  case XML_INTERNAL_GENERAL_ENTITY:
  case XML_INTERNAL_PARAMETER_ENTITY:
  case XML_INTERNAL_PREDEFINED_ENTITY:
    target->_internalEntityDecl(name, XML2STR(content));
    break;
  default:
    target->_externalEntityDecl(name, XML2STR(publicId), XML2STR(systemId));
    break;
  }
}

void 
XMLReader::_notationDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId)
{
  if (hasAlreadyFatalError() == true || _dtdHandler == Nil)
    return;
  try {
    _dtdHandler->notationDecl(name, publicId, org::xml::sax::helpers::NamespaceSupport::getAbsoluteURI(_baseURI, systemId));
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
notationDeclCB(void* vctx, const xmlChar* name, const xmlChar* publicId, const xmlChar* systemId)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2NotationDecl(sax->getXmlCtx(), name, publicId, systemId);
  
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);
  target->_notationDecl(XML2STR(name), XML2STR(publicId), XML2STR(systemId));
}


void 
XMLReader::_attributeDecl(IN(RString) elementName, IN(RString) attributeName, IN(RString) type, IN(RString) mode, IN(RString) value)
{
  if (hasAlreadyFatalError() == true || _declarationHandler == Nil)
    return;
  try {
    _declarationHandler->attributeDecl(elementName, attributeName, type, mode, value);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

RString
attributeTypeToName(int type)
{
  switch (type)
  {
  case XML_ATTRIBUTE_CDATA:
    return "CDATA";
  case XML_ATTRIBUTE_ID:
      return "ID";
  case XML_ATTRIBUTE_IDREF:
    return "IDREF";
  case XML_ATTRIBUTE_IDREFS:
    return "IDREFS";
  case XML_ATTRIBUTE_NMTOKEN:
    return "NMTOKEN";
  case XML_ATTRIBUTE_NMTOKENS:
    return "NMTOKENS";
  case XML_ATTRIBUTE_ENTITY:
   return "ID";
  case XML_ATTRIBUTE_ENTITIES:
    return "ID";
  default:
    return Nil;
  }
}

RString
attributeDefToName(int type)
{
  switch (type)
  {
  case XML_ATTRIBUTE_IMPLIED:
    return "#IMPLIED";
  case XML_ATTRIBUTE_REQUIRED:
    return "#REQUIRED";
  case XML_ATTRIBUTE_FIXED:
   return "#FIXED";
  default:
    return Nil;
  }
}


void
attributeDeclCB(void* vctx, const xmlChar* elem, const xmlChar* fullName, int type, int def, const xmlChar* defaultValue, xmlEnumerationPtr tree)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2AttributeDecl(sax->getXmlCtx(), elem, fullName, type, def, defaultValue, tree);
    
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);

  target->_attributeDecl(XML2STR(elem), XML2STR(fullName), attributeTypeToName(type), attributeDefToName(def), XML2STR(defaultValue));
}


void 
XMLReader::_elementDecl(IN(RString) name, IN(RString) model)
{
  if (hasAlreadyFatalError() == true || _declarationHandler == Nil)
    return;
  try {
    _declarationHandler->elementDecl(name, model);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
elementDeclCB(void* vctx, const xmlChar* name, int type, xmlElementContentPtr content)
{
  DOUT("elementDeclCB: " << name);

  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2ElementDecl(sax->getXmlCtx(), name, type, content);
  
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);
  target->_elementDecl(XML2STR(name), Nil); // ### @todo transform type to model name;
}

void 
XMLReader::_unparsedEntityDecl(IN(RString) name, IN(RString) publicId, IN(RString) systemId, IN(RString) notationName)
{
  if (hasAlreadyFatalError() == true || _dtdHandler == Nil)
    return;
  try {
    _dtdHandler->unparsedEntityDecl(name, publicId, org::xml::sax::helpers::NamespaceSupport::getAbsoluteURI(_baseURI, systemId), notationName);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
unparsedEntityDeclCB(void* vctx, const xmlChar* name, const xmlChar* publicId, const xmlChar* systemId, const xmlChar* notationName)
{
  DOUT("unparsedEntityDeclCB: " << name);
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2UnparsedEntityDecl(sax->getXmlCtx(), name, publicId, systemId, notationName);
    
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);

  target->_unparsedEntityDecl(XML2STR(name), XML2STR(publicId), XML2STR(systemId), XML2STR(notationName));
}

void
startDocumentCB(void* vctx)
{
  DOUT("startDocumentCB");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2StartDocument(sax->getXmlCtx());
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);
  target->_startDocument(sax->ctx->standalone);
}

void 
XMLReader::_endDocument()
{
  if (_contentHandler == Nil)
    return;
  try {
    _contentHandler->endDocument();
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
endDocumentCB(void* vctx)
{
  DOUT("endDocumentCB");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2EndDocument(sax->getXmlCtx());
  
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);
  target->_endDocument();
}

void 
XMLReader::_splitName(IN(RString) fqName, OUT(RString) localName, OUT(RString) prefix, OUT(RString) uri)
{
  int ci = fqName->lastIndexOf (':');
  if (ci < 1)
  {
    localName = fqName;
    prefix = Nil;
    uri = "";
    return;
  }
  localName = fqName->substr(ci + 1);
  prefix = fqName->substr(0, ci);
  if (prefix->equals("xml") == true)
  {
    if (localName->equals("lang") == true || 
        localName->equals("space") == true
       )
      uri = "http://www.w3.org/XML/1998/namespace";
    else
      uri = getURI(prefix);
  }
  else
  {
    uri = getURI(prefix);
  }
}

void 
XMLReader::_startPrefixMapping(IN(RString) prefix, IN(RString) uri)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil)
    return;
  _ns->declarePrefix(prefix, uri);
  _contentHandler->startPrefixMapping(prefix, uri);
}

void 
XMLReader::_startElement(IN(RString) name, IN(RStringArray) attrs)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil)
    return;
  try {
    RString localName;
    RString prefix;
    RString uri;
    _splitName(name, localName, prefix, uri);
    RStringArray newAttrs = attrs;
    if (useNamespace() == true)
    {
      _ns->pushContext();
      
      int len = (attrs == Nil) ? 0 : attrs->length();
      if (len > 0)
      {
        
        RStringArray filtered = new StringArray(0);

        for (int i = 0; i < len; i += 2)
        {
          RString attName = attrs[i];
          RString attValue = attrs[i + 1];
          if (attName->equals("xmlns") == true)
          {
            _startPrefixMapping("", attValue);
          }
          else if (attName->startsWith("xmlns:") == true)
          {
            _startPrefixMapping(attName->substr(6), attValue);
          }
          else
          {
            filtered->append(attName);
            filtered->append(attValue);
          }
        }
        newAttrs = filtered;
      }
    }
    org::xml::sax::helpers::RAttributesImpl atts  = new org::xml::sax::helpers::AttributesImpl();
    for (int i = 0; i < newAttrs->length(); i += 2)
    {
      RString attName = newAttrs[i];
      RString attValue = newAttrs[i + 1];
      RString localName;
      RString prefix;
      RString uri;
      _splitName(attName, localName, prefix, uri);
      atts->addAttribute(uri, localName, attName, "", attValue);
          
    }
    _contentHandler->startElement (uri, localName, name, &atts);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void 
XMLReader::_startElement(IN(RString) name, IN(org::xml::sax::RAttributes) attrs)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil)
    return;
  try {
    RString localName;
    RString prefix;
    RString uri;
    _splitName(name, localName, prefix, uri);
    _contentHandler->startElement(uri, localName, name, attrs);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
   
}

void startElementCB(void* vctx, const xmlChar* name, const xmlChar** attrs)
{
  DOUT("startElementCB");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2StartElement(sax->getXmlCtx(), name, attrs);

  RXMLReader target = sax->_xmlReader;
  checkValid(sax);

  //RAttributesImpl ali = new AttributesImpl();
  RStringArray sa = new StringArray(0);
  if (attrs && *attrs != 0) 
  {
    while (*attrs != 0) 
    {
      RString name = XML2STR(*attrs++);
      RString value = XML2STR(*attrs++);
      sa->append(name);
      sa->append(value);
      /*
      RString localName;
      RString prefix;
      RString uri;
      target->_splitName(name, localName, prefix, uri);
      ali->addAttribute(uri, localName, name, "", value);
      //ali->addAttribute(name, "", value);
      */
    } 
  }
  target->_startElement(XML2STR(name), sa);
}

void 
XMLReader::_endElement(IN(RString) name)
{
}

void
endElementCB (void* vctx, const xmlChar* name)
{
  DOUT("endElementCB");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2EndElement(sax->getXmlCtx(), name);
  
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);
  target->_endElement(XML2STR(name));
}



void 
XMLReader::_setDocumentLocator(IN(org::xml::sax::RLocator) loc)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil)
    return;
  try {
    _locator = loc;
    _contentHandler->setDocumentLocator(_locator);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
  
}

void
setDocumentLocatorCB(void* vctx, xmlSAXLocatorPtr loc)
{
  DOUT("setDocumentLocatorCB");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2SetDocumentLocator(sax->getXmlCtx(), loc);
  
  RXMLReader target = sax->_xmlReader;
  sax->loc = loc;
  target->_setDocumentLocator(new XmlLibLocator(sax->ctx, loc));
}

void 
XMLReader::_characters(IN(RString) text)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil || text == Nil)
    return;
  try {
    _contentHandler->characters(text);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
charactersCB(void* vctx, const xmlChar* ch, int len)
{
  DOUT("charactersCB: '" << XML2STR_LEN(ch, len) << "'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  
  xmlSAX2Characters(sax->getXmlCtx(), ch, len);
  RXMLReader target = sax->_xmlReader;
  
  checkValid(sax);

  xmlChar* dup = xmlStrndup(ch, len);
  StackSavedStr _stsafe(dup);

  target->_characters(XML2STR(dup));
}


void 
XMLReader::_ignorableWhitespace(IN(RString) text)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil || text == Nil)
    return;
  try {
    _contentHandler->ignorableWhitespace(text);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void ignorableWhitespaceCB(void* vctx, const xmlChar* ch, int len)
{
  DOUT("ignorableWhitespaceCB: \'" << XML2STR_LEN(ch, len) << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2IgnorableWhitespace(sax->getXmlCtx(), ch, len);

  RXMLReader target = sax->_xmlReader;
  checkValid(sax);

  xmlChar* dup = xmlStrndup(ch, len);
  StackSavedStr _stsafe(dup);
  target->_ignorableWhitespace(XML2STR(dup));
}

void 
XMLReader::_processingInstruction(IN(RString) target, IN(RString) data)
{
  if (hasAlreadyFatalError() == true || _contentHandler == Nil)
    return;
  try {
    RString tdata = data;
    if (tdata == Nil)
      tdata = "";
    _contentHandler->processingInstruction(target, tdata);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}


void
processingInstructionCB(void* vctx, const xmlChar* targ, const xmlChar* data)
{
  DOUT("processingInstructionCB: \'" << (const char*)targ << "\', '" << (const char*)data << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2ProcessingInstruction(sax->getXmlCtx(), targ, data);
  
  RXMLReader target = sax->_xmlReader;
  
  checkValid(sax);
  target->_processingInstruction(XML2STR(targ), XML2STR(data));
}

xmlEntityPtr
getEntityCB(void* vctx, const xmlChar* name)
{
  DOUT("getEntityCB: \'" << (const char*)name << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  // ### @todo implement me
  return 0;
}
void referenceCB(void* vctx, const xmlChar* name)
{
  DOUT("referenceCB: \'" << (const char*)name << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2Reference(sax->getXmlCtx(), name);
  // ### @todo implement me
}

void 
XMLReader::_comment(IN(RString) text)
{
   if (hasAlreadyFatalError() == true || _lexicalHandler == Nil || text == Nil)
    return;
  try {
    _lexicalHandler->comment(text);
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
commentCB(void* vctx, const xmlChar* value)
{
  DOUT("commentCB: \'" << (const char*)value << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);

  xmlSAX2Comment(sax->getXmlCtx(), value);
  
  RXMLReader target = sax->_xmlReader;
  
  checkValid(sax);
  target->_comment(XML2STR(value));
}

void 
XMLReader::_cdataBlock(IN(RString) text)
{
  if (hasAlreadyFatalError() == true || text == Nil)
    return;
  try {
    if (_lexicalHandler == Nil)
      _characters(text);
    else
    {
      _lexicalHandler->startCDATA();
      _characters(text);
      _lexicalHandler->endCDATA();
    }
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void
cdataBlockCB(void* vctx, const xmlChar* ch, int len)
{
  DOUT("cdataBlockCB: \'" << XML2STR_LEN(ch, len)  << "\'");
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2CDataBlock(sax->getXmlCtx(), ch, len);
  
  RXMLReader target = sax->_xmlReader;
  checkValid(sax);

  xmlChar* dup = xmlStrndup(ch, len);
  StackSavedStr _stsafe(dup);
  target->_cdataBlock(XML2STR(dup));
}

void 
XMLReader::_warning(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (hasAlreadyFatalError() == true || _errorHandler == Nil)
    return;
   try {
     org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
     _errorHandler->warning (new org::xml::sax::SAXParseException(message, l));
  } catch (org::xml::sax::RSAXException ex) {
     ex->throwException();
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void 
XMLReader::_error(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (hasAlreadyFatalError() == true || _errorHandler == Nil)
    return;
   try {
     org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
     _errorHandler->error(new org::xml::sax::SAXParseException(message, l));
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}

void 
XMLReader::_fatalError(IN(RString) message, int lineNumber, int columnNumber, IN(RString) publicId, IN(RString) systemId)
{
  if (hasAlreadyFatalError() == true)
    return;
  _hasAlreadyFatal = true;
  if (_errorHandler == Nil)
    return;
   try {
     if (_seenStartDocument == false)
       _startDocument(false);
     org::xml::sax::RLocator l = new org::xml::sax::helpers::LocatorImpl(lineNumber, columnNumber, publicId, systemId);
     _errorHandler->fatalError(new org::xml::sax::SAXParseException(message, l));
  } catch (org::xml::sax::RSAXException ex) {
    THROW_INSTANCE(ex);
  } catch (RException ex) {
    THROW1_FQ(org::xml::sax::, SAXException, ex);
  }
}



#if defined(_MSC_VER)
#define vsnprintf _vsnprintf
#endif


void
warningCB(void* vctx, const char *msg, ...)
{
  
  va_list args;

  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAXLocatorPtr loc = (xmlSAXLocatorPtr)sax->loc;
  RXMLReader target = sax->_xmlReader;
  
  // xmlParserWarning (vctx, msg, args); 

  int lineNumber = loc->getLineNumber(sax->ctx);
  int columnNumber = loc->getColumnNumber(sax->ctx);
  
  va_start(args, msg);
  
  char buffer[2048] = "";
  if (msg != 0)
  {
    vsnprintf(buffer, sizeof(buffer), msg, args);
  }
  DOUT("warningCB: \'" << (const char*)buffer << "\'");
  if (target != Nil)
    target->_warning(XML2STR(buffer), lineNumber, columnNumber, XML2STR(loc->getPublicId(sax->ctx)), XML2STR(loc->getSystemId(sax->ctx)));
  va_end(args);
}

void
errorCB(void* vctx, const char *msg, ...)
{
  va_list args;

  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAXLocatorPtr loc = (xmlSAXLocatorPtr)sax->loc;
  RXMLReader target = sax->_xmlReader;
  
  // xmlParserWarning (vctx, msg, args); 

  int lineNumber = loc->getLineNumber(sax->ctx);
  int columnNumber = loc->getColumnNumber(sax->ctx);
  
  va_start(args, msg);
  
  char buffer[2048] = "";
  if (msg != 0)
  {
    vsnprintf(buffer, sizeof(buffer), msg, args);
  }
  DOUT("errorCB: \'" << (const char*)buffer << "\'");
  if (target != Nil)
    target->_error(XML2STR(buffer), lineNumber, columnNumber, XML2STR(loc->getPublicId(sax->ctx)), XML2STR(loc->getSystemId(sax->ctx)));
  va_end(args);
}


void
fatalErrorCB(void* vctx, const char *msg, ...)
{
  va_list args;

  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAXLocatorPtr loc = (xmlSAXLocatorPtr)sax->loc;
  RXMLReader target = sax->_xmlReader;
  
  // xmlParserWarning (vctx, msg, args); 

  int lineNumber = loc->getLineNumber(sax->ctx);
  int columnNumber = loc->getColumnNumber(sax->ctx);
  
  va_start(args, msg);
  
  char buffer[2048] = "";
  if (msg != 0)
  {
    vsnprintf(buffer, sizeof(buffer), msg, args);
  }
  DOUT("fatalErrorCB: \'" << (const char*)buffer << "\'");
  if (target != Nil)
    target->_fatalError(XML2STR(buffer), lineNumber, columnNumber, XML2STR(loc->getPublicId(sax->ctx)), XML2STR(loc->getSystemId(sax->ctx)));
  va_end(args);
}

void structuredErrorCB(void *userData, xmlErrorPtr error)
{

}

void endElementNsSAX2CB(void *vctx, const xmlChar *localname, const xmlChar *prefix, const xmlChar* uri)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  //xmlSAX2CDataBlock(sax->getXmlCtx(), ch, len);
  xmlSAX2EndElementNs(sax->getXmlCtx(), localname, prefix, uri);
}

void startElementNsSAX2CB(void *vctx, const xmlChar *localname, const xmlChar *prefix, const xmlChar *uri, int nb_namespaces, const xmlChar **namespaces,
					                 int nb_attributes, int nb_defaulted, const xmlChar **attributes)
{
  SaxParseContext* sax = SaxParseContext::sctxFromCtx(vctx);
  xmlSAX2StartElementNs(sax->getXmlCtx(), localname, prefix, uri, nb_namespaces, namespaces, nb_attributes, nb_defaulted, attributes);
}

void 
XMLReader::parse(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException)
{
  parseInternal(input);
}

void 
XMLReader::_evaluteExtendedFlags()
{
  //_validate = _extendedFlags & XMLRF_PARSE_DTDVALID;
  //setFeature("http://xml.org/sax/features/validation", _validate);
  //_useNamespaces = _extendedFlags & XMLRF_PARSE_NONS;
  //setFeature("http://xml.org/sax/features/namespaces", _useNamespaces);
  //_isStandalone = _extendedFlags & XML_PARSE_NONET;
  //_useNamespacePrefixes = _useNamespaces;
}


org::w3c::dom::RDocument 
XMLReader::parseInternalHtml(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException)
{
  RString publicId = input->getPublicId();
  RString systemId = input->getSystemId();
  _baseURI = org::xml::sax::helpers::NamespaceSupport::getBaseURI(systemId);
  _evaluteExtendedFlags();
  
  const char* fileName = 0;
  
  if (systemId != Nil && systemId->length() > 0)
    fileName = (const char*)STR2XMLDUP(systemId);

  htmlDocPtr htmldoc = 0;
  if (fileName != 0 && false)
  {
    htmldoc = htmlReadFile(fileName, 0, (short)_extendedFlags);
  }
  else
  {
    bool useInMemory = true;
    if (useInMemory == true)
    {
      acdk::io::MemWriter mout;
      input->getByteStream()->trans(&mout);
      //mout.write(0);

      // ctxt->sax->startElement is not set on htmlReadDoc and htmlReadIO!!!!!
      // is set in htmlReadMemory htmlReadFile
      // not working htmldoc = htmlReadDoc(mout.getBuffer()->data(), fileName, 0, (short)_extendedFlags);

      htmldoc = htmlReadMemory((const char*)mout.getBuffer()->data(), mout.getBuffer()->length(), 0, 0, (short)_extendedFlags);
    }
    else
    {
      // code is buggy in libxml2
      SaxParseContext parseContext(this, publicId, systemId, input->getByteStream());
    
      htmldoc = htmlReadIO(SaxParseContext::readCB, SaxParseContext::closeCB, &parseContext,  fileName, 0, (short)_extendedFlags);
    }
  }
  return new libxmldom::LibXMLDocument((xmlNodePtr)htmldoc);
}

org::w3c::dom::RDocument 
XMLReader::parseInternal(IN(org::xml::sax::RInputSource) input) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException)
{
  if (_extendedFlags & XMLRF_PARSE_HTML)
  {
    return parseInternalHtml(input);
  }
  RString publicId = input->getPublicId();
  RString systemId = input->getSystemId();
  _baseURI = org::xml::sax::helpers::NamespaceSupport::getBaseURI(systemId);
  _evaluteExtendedFlags();
  
  int xmlFlags = _extendedFlags;

  //xmlParserCtxtPtr	xmlCreateIOParserCtxt	(xmlSAXHandlerPtr sax, 						 void * user_data, 						 xmlInputReadCallback ioread, 						 xmlInputCloseCallback ioclose, 						 void * ioctx, 						 xmlCharEncoding enc)
  
  xmlSAXHandlerPtr sax =  (xmlSAXHandlerPtr)xmlMalloc(sizeof(xmlSAXHandler));
  memset (sax, 0, sizeof (xmlSAXHandler));
  SaxParseContext parseContext(this, publicId, systemId, input->getByteStream());
  xmlParserCtxtPtr ctx = xmlCreateIOParserCtxt(sax, &parseContext, SaxParseContext::readCB, SaxParseContext::closeCB, &parseContext, XML_CHAR_ENCODING_NONE);
  parseContext.setCtx(ctx);
  //ctx->parseMode = XML_PARSE_DOM;
  if (_extendedFlags & XMLRF_PARSE_SAX1)
    ctx->sax2 = 0;
  else
    ctx->sax2 = 1;
  
  

  ctx->sax = sax;
  ctx->_private = &parseContext;
  ctx->userData = ctx;

  //wrong ctx->replaceEntities = (_extendedFlags & XMLRF_PARSE_NOENT) == 0;
  ctx->validate = isValidating();
  ctx->keepBlanks = (_extendedFlags & XMLRF_PARSE_NOBLANKS) == 0;
  ctx->pedantic = _extendedFlags & XMLRF_PARSE_PEDANTIC;
  ctx->options = _extendedFlags;
  //loadsubset
  if (systemId != Nil && systemId->length() > 0)
    ctx->input->filename = (const char*)STR2XMLDUP(systemId);
  if (_baseURI != Nil && _baseURI->length() > 0)
    ctx->input->directory = (const char*)STR2XMLDUP(_baseURI);
  
  xmlSAXVersion (sax, 2);

  if (_dtdHandler != Nil)
  {
    sax->internalSubset = &internalSubsetCB;
  }
  /* ###
  if (defaultLoader == 0)
  
    defaultLoader = xmlGetExternalEntityLoader ();
    xmlSetExternalEntityLoader (xmljExternalEntityLoader);
  }
  */
  if (_entityResolver != Nil)
  {
    sax->resolveEntity = &resolveEntityCB;
  }

  if (_declarationHandler != Nil)
  {
    sax->entityDecl = &entityDeclCB;
    sax->notationDecl = &notationDeclCB;
    sax->attributeDecl = &attributeDeclCB;
    sax->elementDecl = &elementDeclCB;
    sax->unparsedEntityDecl = &unparsedEntityDeclCB;
  }

  sax->setDocumentLocator = &setDocumentLocatorCB;
  
  if (_contentHandler != Nil)
  {
    sax->startDocument = &startDocumentCB;
    sax->endDocument = &endDocumentCB;
    sax->startElement = &startElementCB;
    sax->endElement = &endElementCB;
    sax->characters = &charactersCB;
    sax->ignorableWhitespace = &ignorableWhitespaceCB;
    sax->processingInstruction = &processingInstructionCB;
    sax->startElementNs = startElementNsSAX2CB;
    sax->endElementNs = endElementNsSAX2CB;
  
  }

  /* We always intercept getEntity */
  sax->getEntity = &getEntityCB;
  if (_lexicalHandler != Nil)
  {
    sax->reference = &referenceCB;
    sax->comment = &commentCB;
    sax->cdataBlock = &cdataBlockCB;
  }
  else if (_contentHandler != Nil)
  {
    sax->cdataBlock = &charactersCB;
  }

  if (_errorHandler != Nil)
  {
    sax->warning = &warningCB;
    sax->error = &errorCB;
    sax->fatalError = &fatalErrorCB;
    if (isValidating() == true)
    {
      ctx->vctxt.error = &errorCB;
      ctx->vctxt.warning = &warningCB;
    }
  }
  int parseRet = 0;
  parseRet = xmlParseDocument(ctx);
  if ((xmlNodePtr)ctx->myDoc == 0)
    return Nil;
  return new libxmldom::LibXMLDocument((xmlNodePtr)ctx->myDoc);
}

void 
XMLReader::parse(IN(RString) systemId) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException)
{
  parseInternal(systemId);
}

org::w3c::dom::RDocument 
XMLReader::parseInternal(IN(RString) systemId) THROWS2(acdk::io::RIOException, org::xml::sax::RSAXException)
{
  return Nil;
}



} // namespace sax
} // namespace xml
} // namespace acdk

