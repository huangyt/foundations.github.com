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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLDocument.cpp,v 1.11 2005/03/03 13:50:11 kommer Exp $


#include "LibXMLDocument.h"
#include "LibXMLDocumentType.h"
#include "LibXMLNamedNodeMap.h"
#include "LibXMLNotation.h"
#include "LibXMLEntity.h"
#include "LibXMLProcessingInstruction.h"

#include "LibXMLDOMInternals.h"
#include "LibXMLXPathExpression.h"
#include "LibXMLXPathNSResolver.h"
#include "LibXMLXPathResult.h"
#include "../dom/NodeIteratorWalker.h"
#include <org/w3c/dom/xpath/XPathException.h>
#include <acdk/lang/Boolean.h>

namespace acdk {
namespace xml {
namespace libxmldom {

using namespace org::w3c::dom;

/*
void 
LibXMLDocument::releaseNode()
{
}
*/

int	xmlOutputWriteCb(void* context, const char * buffer, int len)
{
  Object* o = (Object*)context;
  acdk::io::RWriter out = (acdk::io::RWriter)o;
  out->write((const byte*)buffer, 0, len);
  return len;
}
int	xmlOutputCloseCb(void * context)
{
  Object* o = (Object*)context;
  acdk::io::RWriter out = (acdk::io::RWriter)o;
  out->close();
  return 0;
}

xmlOutputBufferPtr 
LibXMLDocument::createWriterOutputBuffer(IN(acdk::io::RWriter) out)
{
  xmlOutputBufferPtr	outp = xmlAllocOutputBuffer(NULL);
  RObject o = (RObject)out;

  outp->context = o.impl();
  outp->writecallback = xmlOutputWriteCb;
  outp->closecallback = xmlOutputCloseCb;
  return outp;
}


  
void 
LibXMLDocument::finalize()
{
}
  
org::w3c::dom::RDocumentType
LibXMLDocument::getDoctype()
{
  xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
  xmlDtdPtr dtd = doc->extSubset;
  if (dtd == 0)
    dtd = doc->intSubset;

  return (org::w3c::dom::RDocumentType)LibXMLNode::newInstance((xmlNodePtr)dtd);
}
  
org::w3c::dom::RElement 
LibXMLDocument::getDocumentElement()
{
  return (org::w3c::dom::RElement)LibXMLNode::newInstance((xmlNodePtr)xmlDocGetRootElement((xmlDocPtr)_nodePtr._ptr()));
}


org::w3c::dom::RElement 
LibXMLDocument::createElement(IN(RString) tagName) THROWS1(org::w3c::dom::RDOMException)
{
  return createElementNS(Nil, tagName);
}

org::w3c::dom::RDocumentFragment 
LibXMLDocument::createDocumentFragment()
{
  return (org::w3c::dom::RDocumentFragment)LibXMLNode::newInstance((xmlNodePtr)xmlNewDocFragment((xmlDocPtr)_nodePtr._ptr()));
}

org::w3c::dom::RText 
LibXMLDocument::createTextNode(IN(RString) data)
{
  return (org::w3c::dom::RText)LibXMLNode::newInstance((xmlNodePtr)xmlNewDocText((xmlDocPtr)_nodePtr._ptr(), STR2XML(data)));
}


org::w3c::dom::RComment 
LibXMLDocument::createComment(IN(RString) data)
{
  return (org::w3c::dom::RComment)LibXMLNode::newInstance((xmlNodePtr)xmlNewDocComment((xmlDocPtr)_nodePtr._ptr(), STR2XML(data)));
}


org::w3c::dom::RCDATASection 
LibXMLDocument::createCDATASection(IN(RString) data) THROWS1(org::w3c::dom::RDOMException)
{
  RString rdata = data->convert(CCUtf8);
  const xmlChar* cd = (const xmlChar*)rdata->c_str();
  int len = xmlStrlen(cd);
  return (org::w3c::dom::RCDATASection)LibXMLNode::newInstance((xmlNodePtr)xmlNewCDataBlock((xmlDocPtr)_nodePtr._ptr(), cd, len));
}

org::w3c::dom::RProcessingInstruction 
LibXMLDocument::createProcessingInstruction(IN(RString) target, IN(RString) data) THROWS1(org::w3c::dom::RDOMException)
{
  return (org::w3c::dom::RProcessingInstruction)LibXMLNode::newInstance((xmlNodePtr)xmlNewPI(STR2XML(target), STR2XML(data)));
}

org::w3c::dom::RAttr 
LibXMLDocument::createAttribute(IN(RString) name) THROWS1(org::w3c::dom::RDOMException)
{
  return createAttributeNS(Nil, name);
}

org::w3c::dom::REntityReference 
LibXMLDocument::createEntityReference(IN(RString) name) THROWS1(org::w3c::dom::RDOMException)
{
  return (org::w3c::dom::REntityReference)LibXMLNode::newInstance((xmlNodePtr)xmlNewReference((xmlDocPtr)_nodePtr._ptr(), STR2XML(name)));
}

org::w3c::dom::RNodeList 
LibXMLDocument::getElementsByTagName(IN(RString) tagName)
{
  return _getElementsByTagName(_nodePtr, tagName);
}


org::w3c::dom::RNode 
LibXMLDocument::importNode(IN(org::w3c::dom::RNode) importedNode, bool deep) THROWS1(org::w3c::dom::RDOMException)
{
  xmlNodePtr node = _getNodePtr(importedNode);
  if (node == 0)
    THROW2(DOMException, NOT_FOUND_ERR, "");
  if (node->type == XML_DOCUMENT_NODE || node->type == XML_DOCUMENT_TYPE_NODE)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return &LibXMLNode::newInstance(xmlDocCopyNode (node, (xmlDocPtr)_nodePtr._ptr(), deep));
}


org::w3c::dom::RElement 
LibXMLDocument::createElementNS(IN(RString) namespaceURI, IN(RString) qualifiedName) THROWS1(org::w3c::dom::RDOMException)
{
  if (xmlValidateQName(STR2XML(qualifiedName), 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "");
  
  RString rqn = qualifiedName->convert(CCUtf8);
  xmlNsPtr ns = 0;
  if (namespaceURI != Nil)
  {
    RString ruri = namespaceURI->convert(CCUtf8);
  
    //const xmlChar* prefixc = _getPrefix((const xmlChar*)ruri->c_str());
    //const xmlChar* localNamec = _getLocalName((const xmlChar*)rqn->c_str());
    ns = xmlNewNs(_nodePtr, (const xmlChar*)ruri->c_str(), _getPrefix((const xmlChar*)rqn->c_str()));
  }
  return (org::w3c::dom::RElement)LibXMLNode::newInstance(xmlNewDocNode((xmlDocPtr)_nodePtr._ptr(), ns, (const xmlChar*)rqn->c_str(), 0));
}
  
org::w3c::dom::RAttr 
LibXMLDocument::createAttributeNS(IN(RString) namespaceURI, IN(RString) qualifiedName) THROWS1(org::w3c::dom::RDOMException)
{
  if (xmlValidateQName(STR2XML(qualifiedName), 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "");

  xmlNsPtr ns = 0;
  RString rqualifiedName = qualifiedName->convert(CCUtf8);
  if (namespaceURI != Nil)
  {
    const xmlChar* prefixc = _getPrefix((const xmlChar*)rqualifiedName->c_str());
    ns = xmlNewNs(_nodePtr, STR2XML(namespaceURI), prefixc);
  }
  xmlNodePtr attr = (xmlNodePtr)xmlNewNsProp(_nodePtr, ns, (const xmlChar*)rqualifiedName->c_str(), NULL);
  attr->parent = NULL;
  return (org::w3c::dom::RAttr)LibXMLNode::newInstance(attr);
}


org::w3c::dom::RNodeList 
LibXMLDocument::getElementsByTagNameNS(IN(RString) namespaceURI, IN(RString) localName)
{
  return _getElementsByTagNameNS(_nodePtr, namespaceURI, localName);
}

org::w3c::dom::RElement 
LibXMLDocument::getElementById(IN(RString) elementId)
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "LibXMLDocument::getElementById");
  return Nil; 
}


RString 
LibXMLDocument::getInputEncoding()
{
  xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
  if (doc->encoding == 0)
    return Nil;
  return XML2STR(doc->encoding);
}

RString 
LibXMLDocument::getXmlEncoding()
{
  // not getInputEncoding() ?
  
  return getInputEncoding();
}

bool 
LibXMLDocument::getXmlStandalone()
{
  return ((xmlDocPtr)_nodePtr.ptr())->standalone;
}


void 
LibXMLDocument::setXmlStandalone(bool xmlStandalone)
{
  ((xmlDocPtr)_nodePtr.ptr())->standalone = xmlStandalone;
}

RString 
LibXMLDocument::getXmlVersion()
{
  xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
  if (doc->version == 0)
    return Nil;
  return XML2STR(doc->version);
}

void 
LibXMLDocument::setXmlVersion(IN(RString) xmlVersion)
{
  xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
  if (xmlVersion == Nil)
    doc->version = 0; // memory leak?
  else
  {
    if (xmlVersion->equals("1.0") == false)
      THROW2(DOMException, NOT_SUPPORTED_ERR, "XML Version not supported: " + xmlVersion);
    doc->version = STR2XMLDUP(xmlVersion); 
  }
}

bool 
LibXMLDocument::getStrictErrorChecking()
{
  return _strictErrorChecking;
}

void 
LibXMLDocument::setStrictErrorChecking(bool strictErrorChecking)
{
  _strictErrorChecking = strictErrorChecking;
}
  
RString 
LibXMLDocument::getDocumentURI()
{
   xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
   if (doc->name == 0)
     return Nil;
   return XML2STR(doc->name);
}

void 
LibXMLDocument::setDocumentURI(IN(RString) documentURI)
{
  xmlDocPtr doc = (xmlDocPtr)_nodePtr.ptr();
   if (documentURI == Nil)
     doc->name = 0;
   else
      doc->name = (char*)STR2XMLDUP(documentURI);
}

org::w3c::dom::RNode 
LibXMLDocument::adoptNode(IN(org::w3c::dom::RNode) source) THROWS1(org::w3c::dom::RDOMException)
{
  if (source == Nil || instanceof(source, LibXMLNode) == false)
    return Nil;
  return _adoptNode(source);
}

org::w3c::dom::RNode 
LibXMLDocument::_adoptNode(IN(org::w3c::dom::RNode) source) THROWS1(org::w3c::dom::RDOMException)
{
  xmlNodePtr node = _getNodePtr(source);
  if (node == 0)
    THROW2(DOMException, NOT_FOUND_ERR, "");

  if (node->type == XML_DOCUMENT_NODE || node->type == XML_DOCUMENT_TYPE_NODE || node->type == XML_ENTITY_NODE || node->type == XML_NOTATION_NODE)
    THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  
  xmlUnlinkNode(node);
  xmlDocPtr doc = (xmlDocPtr)_nodePtr._ptr();
  node = xmlDocCopyNode(node, doc, 1);
  return &LibXMLNode::newInstance(node);
}

org::w3c::dom::RNode 
LibXMLDocument::renameNode(IN(org::w3c::dom::RNode) n, IN(RString) namespaceURI, IN(RString) qualifiedName)
{
  RString qName = qualifiedName->convert(CCUtf8);
  const xmlChar* qNamec = (const xmlChar*)qName->c_str();
  if (xmlValidateQName(qNamec, 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "");
  
  xmlNodeSetName(_nodePtr, qNamec);
  
  int length = 0;
  const xmlChar* prefix = xmlSplitQName3(qNamec, &length);
  xmlNsPtr ns = _nodePtr->ns;
  if (ns == 0)
  {
    if (namespaceURI != Nil)
    {
      ns = xmlNewNs(_nodePtr, STR2XML(namespaceURI), prefix);
      xmlSetNs(_nodePtr, ns);
    }
  }
  else
  {
    //crashes?: xmlFreeNs(_nodePtr->ns);
    _nodePtr->ns = 0;
    if (namespaceURI != Nil)
    {
      ns = xmlNewNs(_nodePtr, STR2XML(namespaceURI), prefix);
      xmlSetNs(_nodePtr, ns);
    }
  }
  return n;
}
 
bool _getBooleanValue(IN(RObject) value)
{
  if (instanceof(value, Boolean) == true)
    return ((RBoolean)value)->booleanValue();
  if (instanceof(value, String) == true)
    return Boolean((RString)value).booleanValue();
  return false;
}

void 
LibXMLDocument::setParameter(IN(RString) name, IN(RObject) value) THROWS1(org::w3c::dom::RDOMException)
{
  if (name->equals("canonical-form") == true)
  {
    _canonicalForm = _getBooleanValue(value);
  }
  else if (name->equals("element-content-whitespace") == true)
  {
    _elementContentWhitespace = _getBooleanValue(value);
  }
  else if (name->equals("check-character-normalization") == true) 
  {
    _checkCharacterNormalization = _getBooleanValue(value);
  }
  else if (name->equals("datatype-normalization") == true)
  {
    _datatypeNormalization = _getBooleanValue(value);
  }
  else if (name->equals("normalize-characters") == true)
  {
    _normalizeCharacters = _getBooleanValue(value);
  }
  else if (name->equals("validate") == true)
  {
    _validate = _getBooleanValue(value);
  }
  else if (name->equals("validate-if-schema") == true)
  {
    _validateIfSchema = _getBooleanValue(value);
  }
  else if (name->equals("well-formed") == true)
  {
    _wellFormed = _getBooleanValue(value);
  }
  else if (name->equals("cdata-sections") == true)
  {
    _cdataSections = _getBooleanValue(value);
  }
  
  else if (name->equals("comments") == true)
  {
    _comments = _getBooleanValue(value);
  }
  else if (name->equals("entities") == true)
  {
    _entities = _getBooleanValue(value);
  }
  else if (name->equals("error-handler") == true)
  {
    _errorHandler =(RDOMErrorHandler)value;
  }
  else if (name->equals("infoset") == true)
  {
    if (_getBooleanValue(value) == true)
    {
      _validateIfSchema = false;
      _entities = false;
      _datatypeNormalization = false;
      _cdataSections = false;
      _namespaceDeclarations = true;
      _wellFormed = true;
      _elementContentWhitespace = true;
      _comments = true;
      _namespaces = true;
    }
  }
  else if (name->equals("namespaces") == true)
  {
    _namespaces = _getBooleanValue(value);
  }
  else if (name->equals("namespace-declarations") == true)
  {
    _namespaceDeclarations = _getBooleanValue(value);
  }
  
  else if (name->equals("split-cdata-sections") == true)
  {
    _splitCdataSections = _getBooleanValue(value);
  }
  
  else
  {
    THROW2_FQ(org::w3c::dom::, DOMException, NOT_FOUND_ERR, name);
  }
}

RObject 
LibXMLDocument::getParameter(IN(RString) name) THROWS1(org::w3c::dom::RDOMException)
{
  if (name->equals("canonical-form") == true)
  {
    return new Boolean(_canonicalForm);
  }
  else if (name->equals("cdata-sections") == true)
  {
    return new Boolean(_cdataSections);
  }
  else if (name->equals("check-character-normalization") == true)
  {
    return new Boolean(_checkCharacterNormalization);
  }
  else if (name->equals("comments") == true)
  {
    return new Boolean(_comments);
  }
  else if (name->equals("datatype-normalization") == true)
  {
    return new Boolean(_datatypeNormalization);
  }
  else if (name->equals("element-content-whitespace") == true)
  {
    return new Boolean(_elementContentWhitespace);
  }
  else if (name->equals("entities") == true)
  {
    return new Boolean(_entities);
  }
  else if (name->equals("error-handler") == true)
  {
    return (RObject)_errorHandler;
  }
  else if (name->equals("infoset") == true)
  {
    if (_validateIfSchema == false && _entities == false &&
        _datatypeNormalization == false && _cdataSections == false &&
        _namespaceDeclarations == true && _wellFormed == true &&
        _elementContentWhitespace == true && _comments == true && _namespaces == true)
     return &Boolean::getTRUE();
    return &Boolean::getFALSE();
   
  }
  else if (name->equals("namespaces") == true)
  {
    if (_namespaces == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else if (name->equals("namespace-declarations") == true)
  {
    if (_namespaceDeclarations == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else if (name->equals("normalize-characters") == true)
  {
    if (_normalizeCharacters == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else if (name->equals("split-cdata-sections") == true)
  {
    if (_splitCdataSections == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
   
  }
  else if (name->equals("validate") == true)
  {
    if (_validate == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else if (name->equals("validate-if-schema") == true)
  {
    if (_validateIfSchema == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else if (name->equals("well-formed") == true)
  {
    if (_wellFormed == true)  
      return &Boolean::getTRUE();
    return  &Boolean::getFALSE();
  }
  else
  {
    THROW2_FQ(org::w3c::dom::, DOMException, NOT_FOUND_ERR, name);
  }
  return Nil;
}

bool 
LibXMLDocument::canSetParameter(IN(RString) name, IN(RObject) value)
{
  if (value == Nil)
    return true;
      
  return (name->equals("cdata-sections") ||
            name->equals("comments") ||
            name->equals("entities") ||
            name->equals("error-handler") ||
            name->equals("namespace-declarations") ||
            name->equals("split-cdata-sections"));
}
  
RStringArray 
LibXMLDocument::getParameterNames()
{
  RStringArray sa = new StringArray(0);
  sa->append("canonical-form");
  sa->append("cdata-sections");
  sa->append("check-character-normalization");
  sa->append("comments");
  sa->append("datatype-normalization");
  sa->append("element-content-whitespace");
  sa->append("entities");
  sa->append("error-handler");
  sa->append("infoset");
  sa->append("namespaces");
  sa->append("namespace-declarations");
  sa->append("normalize-characters");
  sa->append("split-cdata-sections");
  sa->append("validate");
  sa->append("validate-if-schema");
  sa->append("well-formed");
  return sa;
}

org::w3c::dom::xpath::RXPathExpression 
LibXMLDocument::createExpression(IN(RString) expression, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException)
{
  return new LibXMLXPathExpression(this, expression, resolver);
}

org::w3c::dom::xpath::RXPathNSResolver 
LibXMLDocument::createNSResolver(IN(org::w3c::dom::RNode) nodeResolver)
{
  return new LibXMLXPathNSResolver(this);
}


void xpathErrorCB(void *userData, xmlErrorPtr error)
{
  StringBuffer sb;
  sb << "Error in expression: '" << XML2STR(error->str1) << "' at position " << error->int1;
  
  THROW2_FQ(org::w3c::dom::xpath::, XPathException, error->code, sb.toString());
}

RObject 
LibXMLDocument::evaluate(IN(RString) expression, IN(org::w3c::dom::RNode) contextNode, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver, short type, IN(RObject) result) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException)
{
  xmlNodePtr contextNodePtr = _getNodePtr(contextNode);
  if (contextNodePtr == 0)
    return Nil;

  xmlXPathContextPtr ctx = xmlXPathNewContext(contextNodePtr->doc);
  ctx->node = contextNodePtr;
  ctx->error = xpathErrorCB;
  ctx->userData = this;
  //xmlXPathObjectPtr eval = xmlXPathEval(STR2XML(expression), ctx);
  xmlXPathObjectPtr eval = xmlXPathEvalExpression(STR2XML(expression), ctx);
  
  xmlXPathFreeContext(ctx);
  if (eval == 0)
    return Nil;
  return new LibXMLXPathResult(eval, contextNodePtr);
}

  
RString 
LibXMLDocument::toString()
{
  return SBSTR(getClass()->getName() << "[version=" << getXmlVersion() << ",standalone=" << getXmlStandalone() << "]");
}
  
org::w3c::dom::traversal::RNodeIterator 
LibXMLDocument::createNodeIterator(IN(org::w3c::dom::RNode) root, int whatToShow, IN(org::w3c::dom::traversal::RNodeFilter) filter, bool entityReferenceExpansion) THROWS1(org::w3c::dom::RDOMException)
{
  return new acdk::xml::dom::NodeIteratorWalker(root, filter, whatToShow, entityReferenceExpansion, false);
}

org::w3c::dom::traversal::RTreeWalker 
LibXMLDocument::createTreeWalker(IN(org::w3c::dom::RNode) root, int whatToShow, IN(org::w3c::dom::traversal::RNodeFilter) filter, bool entityReferenceExpansion) THROWS1(org::w3c::dom::RDOMException)
{
  return new acdk::xml::dom::NodeIteratorWalker(root, filter, whatToShow, entityReferenceExpansion, true);
}

org::w3c::dom::RNamedNodeMap 
LibXMLDocumentType::getEntities()
{
  return new LibXMLNamedNodeMap(_nodePtr, NNMTEntities);
}

org::w3c::dom::RNamedNodeMap 
LibXMLDocumentType::getNotations()
{
  return new LibXMLNamedNodeMap(_nodePtr, NNMTNotations);
}

RString 
LibXMLDocumentType::getPublicID()
{
  return XML2STR(((xmlDtdPtr)_nodePtr.ptr())->ExternalID);
}

RString 
LibXMLDocumentType::getSystemID()
{
  return XML2STR(((xmlDtdPtr)_nodePtr.ptr())->SystemID);
}

RString 
LibXMLDocumentType::getInternalSubset()
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return Nil;
}

RString 
LibXMLDocumentType::toString()
{
  RString publicId = getPublicID();
  StringBuffer sb;
  sb << getClass()->getName() << "[";
  if (publicId != Nil)
    sb << "publicId=" << publicId << ",";
        
  sb << "systemId=" << getSystemID() << "]";
  return sb.toString();
}

RString 
LibXMLNotation::getPublicID()
{
  xmlNotationPtr notation = (xmlNotationPtr)_nodePtr.ptr();
  if (notation->PublicID == 0)
    return Nil;
  return XML2STR(notation->PublicID);
}

RString 
LibXMLNotation::getSystemID()
{
  xmlNotationPtr notation = (xmlNotationPtr)_nodePtr.ptr();
  if (notation->SystemID == 0)
    return Nil;
  return XML2STR(notation->SystemID);
}

RString 
LibXMLNotation::toString()
{
  RString publicId = getPublicID();
  StringBuffer sb;
  sb << getClass()->getName() << "[";
  if (publicId != Nil)
    sb << "publicId=" << publicId << ",";
        
  sb << "systemId=" << getSystemID() << "]";
  return sb.toString();
}


RString 
LibXMLEntity::getPublicId()
{
  xmlEntityPtr notation = (xmlEntityPtr)_nodePtr.ptr();
  if (notation->ExternalID == 0)
    return Nil;
  return XML2STR(notation->ExternalID);
}

RString 
LibXMLEntity::getSystemId()
{
   xmlEntityPtr notation = (xmlEntityPtr)_nodePtr.ptr();
  if (notation->SystemID == 0)
    return Nil;
  return XML2STR(notation->SystemID);
}

RString 
LibXMLEntity::getNotationName()
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return Nil;
}

RString 
LibXMLEntity::getInputEncoding()
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return Nil;    
}
  
RString 
LibXMLEntity::getXmlEncoding()
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return Nil;
}
  
RString 
LibXMLEntity::getXmlVersion()
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "");
  return Nil;
}
  
RString 
LibXMLEntity::toString()
{
  RString publicId = getPublicId();
  StringBuffer sb(getClass()->getName());
  sb << "[";
  if (publicId != Nil)
    sb << "publicId=" << publicId << ",";
    
  sb << "systemId=" << getSystemId()/* << ",notationName=" << getNotationName()*/ << "]";
  return sb.toString();
}


RString 
LibXMLProcessingInstruction::getData()
{
  return XML2STR(xmlNodeGetContent(_nodePtr));
}

void 
LibXMLProcessingInstruction::setData(IN(RString) data) THROWS1(org::w3c::dom::RDOMException)
{
  xmlNodeSetContent(_nodePtr, STR2XML(data));
}

RString 
LibXMLProcessingInstruction::toString()
{
  return SBSTR(getClass()->getName() << "[data=" << getData() << "]");
}

RString 
LibXMLXPathNSResolver::lookupNamespaceURI(IN(RString) prefix)
{
  xmlDocPtr doc = (xmlDocPtr)_doc->getNodePtr();
  xmlNsPtr ns = xmlSearchNs(doc, (xmlNodePtr)doc, STR2XML(prefix));
  if (ns == 0)
    return Nil;
  return XML2STR(ns->href);
}

} // namespace libxmldom
} // namespace xml
} // namespace acdk

