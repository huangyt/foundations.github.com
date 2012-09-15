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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLDocument.h,v 1.6 2005/02/12 17:33:46 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLDocument_h
#define acdk_xml_libxmldom_LibXMLDocument_h

#include "LibXMLNode.h"
#include <org/w3c/dom/DOMConfiguration.h>
#include <org/w3c/dom/DOMErrorHandler.h>
#include <org/w3c/dom/xpath/XPathExpression.h>
#include <org/w3c/dom/xpath/XPathNSResolver.h>
#include <org/w3c/dom/xpath/XPathEvaluator.h>
#include <org/w3c/dom/traversal/DocumentTraversal.h>


namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLDocument);

class ACDK_XML_PUBLIC LibXMLDocument
: extends LibXMLNode
, implements org::w3c::dom::Document
, implements org::w3c::dom::DOMConfiguration
, implements org::w3c::dom::xpath::XPathEvaluator
, implements org::w3c::dom::traversal::DocumentTraversal
{
  ACDK_WITH_METAINFO(LibXMLDocument)
protected:
  org::w3c::dom::RDOMImplementation _domImpl;
  bool _canonicalForm;
  bool _cdataSections;
  bool _checkCharacterNormalization;
  bool _comments;
  bool _datatypeNormalization;
  bool _elementContentWhitespace;
  bool _entities;
  org::w3c::dom::RDOMErrorHandler _errorHandler;
  bool _namespaces;
  bool _namespaceDeclarations;
  bool _normalizeCharacters;
  bool _splitCdataSections;
  bool _validate;
  bool _validateIfSchema;
  bool _wellFormed;
  bool _strictErrorChecking;
  
public:
  foreign LibXMLDocument(xmlNodePtr np, FreeXmlNodePtrFuncPtr freeIt = 0)
  : LibXMLNode(np, freeIt)
  , _canonicalForm(false)
  , _cdataSections(true)
  , _checkCharacterNormalization(false)
  , _comments(true)
  , _datatypeNormalization(false)
  , _elementContentWhitespace(true)
  , _entities(true)
  , _errorHandler(Nil)
  , _namespaces(true)
  , _namespaceDeclarations(true)
  , _normalizeCharacters(false)
  , _splitCdataSections(true)
  , _validate(false)
  , _validateIfSchema(false)
  , _wellFormed(true)
  , _strictErrorChecking(true)
  {
  }
  //virtual void releaseNode();
  void finalize();
  
  org::w3c::dom::RDocumentType getDoctype();
  org::w3c::dom::RDOMImplementation getImplementation()
  {
    return _domImpl;
  }

  org::w3c::dom::RElement getDocumentElement();
  org::w3c::dom::RElement createElement(IN(RString) tagName) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RDocumentFragment createDocumentFragment();
  org::w3c::dom::RText createTextNode(IN(RString) data);
  org::w3c::dom::RComment createComment(IN(RString) data);
  org::w3c::dom::RCDATASection createCDATASection(IN(RString) data) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RProcessingInstruction createProcessingInstruction(IN(RString) target, IN(RString) data) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RAttr createAttribute(IN(RString) name) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::REntityReference createEntityReference(IN(RString) name) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNodeList getElementsByTagName(IN(RString) tagName);
  org::w3c::dom::RNode importNode(IN(org::w3c::dom::RNode) importedNode, bool deep) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RElement createElementNS(IN(RString) namespaceURI, IN(RString) qualifiedName) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RAttr createAttributeNS(IN(RString) namespaceURI, IN(RString) qualifiedName) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNodeList getElementsByTagNameNS(IN(RString) namespaceURI, IN(RString) localName);
  org::w3c::dom::RElement getElementById(IN(RString) elementId);
  RString getInputEncoding();
  RString getXmlEncoding();
  bool getXmlStandalone();
  void setXmlStandalone(bool xmlStandalone);
  RString getXmlVersion();
  void setXmlVersion(IN(RString) xmlVersion);
  bool getStrictErrorChecking();

  void setStrictErrorChecking(bool strictErrorChecking);
  RString getDocumentURI();
  void setDocumentURI(IN(RString) documentURI);
  org::w3c::dom::RNode adoptNode(IN(org::w3c::dom::RNode) source) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RDOMConfiguration getDomConfig() { return this; }

  void normalizeDocument() { normalize(); }

  org::w3c::dom::RNode renameNode(IN(org::w3c::dom::RNode) n, IN(RString) namespaceURI, IN(RString) qualifiedName);

  
  void setParameter(IN(RString) name, IN(RObject) value) THROWS1(org::w3c::dom::RDOMException);

  RObject getParameter(IN(RString) name) THROWS1(org::w3c::dom::RDOMException);

  bool canSetParameter(IN(RString) name, IN(RObject) value);

  RStringArray getParameterNames();

  
  org::w3c::dom::xpath::RXPathExpression createExpression(IN(RString) expression, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException);

  org::w3c::dom::xpath::RXPathNSResolver createNSResolver(IN(org::w3c::dom::RNode) nodeResolver);

  RObject evaluate(IN(RString) expression, IN(org::w3c::dom::RNode) contextNode, IN(org::w3c::dom::xpath::RXPathNSResolver) resolver, short type, IN(RObject) result) THROWS2(org::w3c::dom::xpath::RXPathException, org::w3c::dom::RDOMException);

  org::w3c::dom::traversal::RNodeIterator createNodeIterator(IN(org::w3c::dom::RNode) root, int whatToShow, IN(org::w3c::dom::traversal::RNodeFilter) filter, bool entityReferenceExpansion) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::traversal::RTreeWalker createTreeWalker(IN(org::w3c::dom::RNode) root, int whatToShow, IN(org::w3c::dom::traversal::RNodeFilter) filter, bool entityReferenceExpansion) THROWS1(org::w3c::dom::RDOMException);
  
  RString toString();
  foreign RString toXML() { return LibXMLNode::toXML(); }

  // internal only
  foreign static xmlOutputBufferPtr createWriterOutputBuffer(IN(acdk::io::RWriter) out);
protected:
  foreign org::w3c::dom::RNode _adoptNode(IN(org::w3c::dom::RNode) source) THROWS1(org::w3c::dom::RDOMException);
  
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLDocument_h
