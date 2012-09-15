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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLNode.cpp,v 1.11 2005/02/13 03:25:57 kommer Exp $

//#include "LibXMLNode.h"
#include "LibXMLNodeList.h"
#include "LibXMLElement.h"
#include "LibXMLComment.h"
#include "LibXMLText.h"
#include "LibXMLNamedNodeMap.h"
#include "LibXMLCDATASection.h"
#include "LibXMLAttr.h"
#include "LibXMLDocument.h"
#include "LibXMLNotation.h"
#include "LibXMLEntity.h"
#include "LibXMLEntityReference.h"
#include "LibXMLDOMInternals.h"
#include "LibXMLDocumentType.h"
#include "LibXMLProcessingInstruction.h"
#include "LibXMLDocumentFragment.h"
#include "LibXMLXPathResult.h"

#include <acdk/lang/NullPointerException.h>
#include <acdk/util/logging/Log.h>

#include <org/w3c/dom/xpath/XPathResult.h>
#include "../dom/NodeUtil.h"
#include "../../../libxml/HTMLtree.h"

#include <map>

namespace acdk {
namespace xml {
namespace libxmldom {

namespace { 

typedef std::map<xmlDocPtr, int> DocReferenceMap;

DocReferenceMap& 
getDocReferenceMap()
{
  static DocReferenceMap _map;
  return _map;
}


} // anon namespace

//static 
void 
XmlNodePtrHolder::registerNode(xmlNodePtr nodePtr)
{
  xmlDocPtr doc = nodePtr->doc;
  if (doc == 0)
    return ;
  DocReferenceMap& refmap = getDocReferenceMap();
  // ### @todo mt safe
  DocReferenceMap::iterator it = refmap.find(doc);
  if (it == refmap.end())
    refmap[doc] =  1;
  else
    ++it->second;

}

//static 
void 
XmlNodePtrHolder::unregisterNode(xmlNodePtr nodePtr)
{
   xmlDocPtr doc = nodePtr->doc;
  if (doc == 0)
    return ;
  DocReferenceMap& refmap = getDocReferenceMap();
  DocReferenceMap::iterator it = refmap.find(doc);
  if (it == refmap.end())
  {
    ACDK_NLOG("acdk.xml.libxmldom", Error, "freeing Unbound notePtr");
    return;
  }
  if ((--it->second) == 0)
  {
    refmap.erase(it);
    xmlFreeDoc(doc);
  }
}
// static 
void 
XmlNodePtrHolder::xmlFreeNode(xmlNodePtr cur)
{
  ::xmlFreeNode(cur);
}

using namespace org::w3c::dom;


LibXMLNode::LibXMLNode(xmlNodePtr np, FreeXmlNodePtrFuncPtr freeFuncPtr)
: _nodePtr(np, freeFuncPtr)
{
  _nodePtr->_private = (void*)this;
}

LibXMLNode::~LibXMLNode()
{
  if (_nodePtr->_private != 0)
  {
    if ((void*)_nodePtr->_private != this)
      ; // ooops
    _nodePtr->_private = 0;
  }
}

//foreign static 
RLibXMLNode 
LibXMLNode::newInstance(xmlDocPtr docPtr, xmlNodePtr nodePtr, int nodeType, bool ownsPtr)
{
  if (nodePtr == 0)
    return Nil;
  if (nodePtr->_private != 0)
  {
    LibXMLNode* np = (LibXMLNode*)nodePtr->_private;
    return np;
  }
  RLibXMLNode newInstance = Nil;
  switch(nodeType)
  {
  case XML_ELEMENT_NODE:
    newInstance = new LibXMLElement(nodePtr, ownsPtr);
    break;
  case XML_COMMENT_NODE:
    newInstance = new LibXMLComment(nodePtr, ownsPtr);
    break;
  case XML_TEXT_NODE:
    newInstance = new LibXMLText(nodePtr, ownsPtr);
    break;
  case XML_CDATA_SECTION_NODE:
    newInstance = new LibXMLCDATASection(nodePtr, ownsPtr);
    break;
  case XML_ATTRIBUTE_NODE:
    newInstance = new LibXMLAttr(nodePtr, ownsPtr);
    break;
  case XML_DOCUMENT_NODE:
    newInstance = new LibXMLDocument(nodePtr, 0);
    break;
  case XML_DOCUMENT_TYPE_NODE:
    newInstance = new LibXMLDocumentType(nodePtr, ownsPtr);
    break;
  case XML_NOTATION_NODE: //XML_DOCUMENT_NODE
    newInstance = new LibXMLNotation(nodePtr, ownsPtr);
    break;
  case XML_ENTITY_NODE:
    newInstance = new LibXMLEntity(nodePtr, ownsPtr);
    break;
  case XML_ENTITY_REF_NODE:
    newInstance = new LibXMLEntityReference(nodePtr, ownsPtr);
    break;
  case XML_PI_NODE: 
    newInstance = new LibXMLProcessingInstruction(nodePtr);
    break;
  case XML_DOCUMENT_FRAG_NODE:
    newInstance = new LibXMLDocumentFragment(nodePtr, ownsPtr);
    break;
  case XML_DTD_NODE:
    newInstance = new LibXMLDocumentType(nodePtr, ownsPtr); // ### @todo not sure, if this is correct
    break;
  
  default:
    newInstance = new LibXMLNode(nodePtr);
    break;
  }
  return newInstance;
}

//foreign static 
RLibXMLNode 
LibXMLNode::newInstance(xmlNodePtr nodePtr, bool ownsPtr)
{
  if (nodePtr == 0)
    return Nil;
  return newInstance(nodePtr->doc, nodePtr, nodePtr->type, ownsPtr);
}

RString 
LibXMLNode::getNodeName()
{
  return XML2STR(_nodePtr->name);
}





RString 
LibXMLNode::getBaseURI()
{
  return XML2STR(xmlNodeGetBase(_nodePtr->doc, _nodePtr));
}



const xmlChar* _getNodeValue(xmlNodePtr node)
{
  switch(node->type)
  {
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_COMMENT_NODE:
  case XML_ATTRIBUTE_NODE:
    return xmlNodeGetContent(node);
  default:
      return 0;
  }
}

RString 
LibXMLNode::getNodeValue() THROWS1(RDOMException)
{
  return XML2STR(_getNodeValue(_nodePtr));
}

void 
LibXMLNode::setNodeValue(IN(RString) nodeValue) THROWS1(RDOMException)
{
  switch(_nodePtr->type)
  {
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_COMMENT_NODE:
    xmlNodeSetContent(_nodePtr,(const unsigned char*)STR2XMLDUP(nodeValue));
    break;
  case XML_ATTRIBUTE_NODE:
  default:
      break;
  }
}

short 
LibXMLNode::getNodeType()
{
  return _mapLibXMLTypeToDomType(_nodePtr->type);
  /*
  if(_nodePtr->type == XML_DTD_NODE)
    return XML_DOCUMENT_TYPE_NODE;
  return _nodePtr->type;
  */
}


RNode 
LibXMLNode::getParentNode()
{
  return &newInstance(_nodePtr->parent);
}

RNodeList 
LibXMLNode::getChildNodes()
{
  return new LibXMLNodeList(_nodePtr);
}

RNode 
LibXMLNode::getFirstChild()
{
  return &newInstance(_nodePtr->children);
}

RNode 
LibXMLNode::getLastChild()
{
  return &newInstance(_nodePtr->last);
}

RNode 
LibXMLNode::getPreviousSibling()
{
  return &newInstance(_nodePtr->prev);
}

RNode 
LibXMLNode::getNextSibling()
{
  return &newInstance(_nodePtr->next);
}

RNamedNodeMap 
LibXMLNode::getAttributes()
{
  return new LibXMLNamedNodeMap(_nodePtr, NNMTAttributes);
}

RDocument 
LibXMLNode::getOwnerDocument()
{
  return(RDocument)newInstance((xmlNodePtr)_nodePtr->doc);
}

void
checkChildNode(xmlNodePtr parent, xmlNodePtr child, bool isNewNode)
{
  if(child == 0 || parent == 0)
    THROW2(DOMException, NOT_FOUND_ERR, "");
  if (child->doc != 0 || isNewNode == false)
  {
    if (child->doc != parent->doc)
      THROW2(DOMException, WRONG_DOCUMENT_ERR, "Child is not part of document");
  }
  switch(parent->type)
  {
  case XML_CDATA_SECTION_NODE:
  case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
  case XML_ENTITY_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_NOTATION_NODE:
  case XML_PI_NODE:
    THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Parent does not allow to contains children");
  case XML_ATTRIBUTE_NODE:
    if(child->type != XML_TEXT_NODE && child->type != XML_ENTITY_REF_NODE)
      THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Attributes can only contain text or entity references");
    break;
  case XML_DOCUMENT_FRAG_NODE:
  case XML_ELEMENT_NODE:
    if(child->type == XML_DTD_NODE || child->type == XML_DOCUMENT_TYPE_NODE ||
      child->type == XML_ENTITY_NODE || child->type == XML_NOTATION_NODE ||
      child->type == XML_PI_NODE)
      THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Parent node does not allow child of this type");
    // no break
  default:
    if(child->type == XML_ATTRIBUTE_NODE ||
      child->type == XML_DOCUMENT_NODE ||
      child->type == XML_DOCUMENT_FRAG_NODE)
      THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Node type is not allowed to be a child");
    break;
  }
  xmlNodePtr cur;
 for (cur = parent; cur != 0; cur = cur->parent)
  {
    if (cur == child)
      THROW2(DOMException, HIERARCHY_REQUEST_ERR, "child cannot be ancestor of itself");
  }
  if (parent->type == XML_DOCUMENT_NODE)
  {
    xmlNodePtr cur = parent->children;
    while(cur != 0)
    {
      if(cur->type == XML_DTD_NODE ||
          cur->type == XML_DOCUMENT_TYPE_NODE ||
         (cur->type == XML_ELEMENT_NODE && parent->type == XML_DOCUMENT_NODE))
      {
        if(child->type == cur->type && child != cur)
          THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Adding second doctype or root element");
      }
      cur = cur->next;
        
    }
  }
  
}

void
ensureChildNode(xmlNodePtr parent, xmlNodePtr child)
{
  if (child->parent != 0 && child->parent == parent)
    return;
  /*
  for (xmlNodePtr cur = parent->children; cur != 0; cur = cur->next)
  {
    if (cur == child)
      return;
  }*/
  THROW2(DOMException, NOT_FOUND_ERR, "Is not child");
}

RNode 
LibXMLNode::insertBefore(IN(RNode) newChild, IN(RNode) refChild) THROWS1(RDOMException)
{
  if(instanceof(newChild, LibXMLNode) == false || instanceof(refChild, LibXMLNode) == false)
    THROW2(DOMException, NOT_FOUND_ERR, "Node is not a LibXMLNode");
  
  RLibXMLNode nc =(RLibXMLNode)newChild;
  RLibXMLNode rc =(RLibXMLNode)refChild;
  if(rc->_nodePtr->parent != _nodePtr)
    THROW2(DOMException, NOT_FOUND_ERR, "RefNode is not child of this Node");
  checkChildNode(_nodePtr, nc->_nodePtr, true);
  ensureChildNode(_nodePtr, rc->_nodePtr);
  xmlNodePtr newChildNode = xmlAddPrevSibling(rc->_nodePtr, nc->_nodePtr);
  nc->_nodePtr.share();
  return &newInstance(newChildNode);
}

RNode 
LibXMLNode::replaceChild(IN(RNode) newChild, IN(RNode) oldChild) THROWS1(RDOMException)
{
  if(instanceof(newChild, LibXMLNode) == false || instanceof(oldChild, LibXMLNode) == false)
    THROW2(DOMException, NOT_FOUND_ERR, "Node is not a LibXMLNode");
  RLibXMLNode nc =(RLibXMLNode)newChild;
  RLibXMLNode oc =(RLibXMLNode)oldChild;

  checkChildNode(_nodePtr, nc->_nodePtr, true);
  checkChildNode(_nodePtr, oc->_nodePtr, false);
  ensureChildNode(_nodePtr, oc->_nodePtr);
  xmlNodePtr newChildNode = xmlReplaceNode(oc->_nodePtr, nc->_nodePtr);
  // TODO 
  return &newInstance(newChildNode, true);
}

RNode 
LibXMLNode::removeChild(IN(RNode) oldChild) THROWS1(RDOMException)
{
  if(instanceof(oldChild, LibXMLNode) == false)
    THROW2(DOMException, NOT_FOUND_ERR, "Node is not a LibXMLNode");
  RLibXMLNode oc =(RLibXMLNode)oldChild;
  
  ensureChildNode(_nodePtr, oc->_nodePtr);
  xmlUnlinkNode(oc->_nodePtr);
  oc->_unshare(xmlFreeNode);
  return oldChild;
}

RNode 
LibXMLNode::appendChild(IN(RNode) newChild) THROWS1(RDOMException)
{
  if(instanceof(newChild, LibXMLNode) == false)
    THROW2(DOMException, NOT_FOUND_ERR, "Node is not a LibXMLNode");
  
  RLibXMLNode nc =(RLibXMLNode)newChild;
  checkChildNode(_nodePtr, nc->_nodePtr, true);
  xmlNodePtr sch = nc->_nodePtr;
  xmlNodePtr newChildNode = xmlAddChild(_nodePtr, sch);
  nc->_nodePtr.share();
  return &newInstance(newChildNode);
}

bool 
LibXMLNode::hasChildNodes()
{
  return _nodePtr->children != 0;
}

int 
LibXMLNode::getChildCount()
{
  if (_nodePtr->children == 0)
    return 0;
  xmlNodePtr node = _nodePtr->children;
  int count = 0;
  for(count = 0; node != 0; count++)
  {
    node = node->next;
  }
  return count;
}

// from http://search.cpan.org/src/PHISH/XML-LibXML-1.50/dom.c
/*
xmlNodePtr
domAppendChild( xmlNodePtr self,
                xmlNodePtr newChild ){
    if ( self == NULL ) {
        return newChild;
    }
   
    if ( !(domTestHierarchy(self, newChild)
           && domTestDocument(self, newChild))){
        xs_warn("HIERARCHIY_REQUEST_ERR\n"); 
        return NULL;
    }
 
    if ( newChild->doc == self->doc ){
        xmlUnlinkNode( newChild );
    }
    else {
        //xs_warn("WRONG_DOCUMENT_ERR - non conform implementation\n"); 
        newChild= domImportNode( self->doc, newChild, 1 );
    }
 
    if ( self->children != NULL ) {
        domAddNodeToList( newChild, self->last, NULL );
    }
    else if (newChild->type == XML_DOCUMENT_FRAG_NODE ) {
	    newChild->children->parent = self;
        self->children = newChild->children;
        self->last     = newChild->last;
        domAddNodeToList( newChild, self->last, NULL );
    }
    else {
        self->children = newChild;
        self->last     = newChild;
        newChild->parent= self;
    }
    
    return newChild;
}
*/

/*
xmlNodePtr node = xmlDocCopyNode(
> 		xpath_obj->nodesetval->nodeTab[i],
>                  dest_doc, 1 with children );
> 	// no don't xmlUnlinkNode(node);
> 	xmlAddChild(insert_point->nodesetval->nodeNr[0],
> 		node);
*/


RNode 
LibXMLNode::cloneNode(bool deep)
{
  xmlNodePtr newNode = xmlCopyNode(_nodePtr, deep);
  return &newInstance(newNode, true);
}

void normalizeNode(xmlNodePtr _nodePtr)
{
  xmlNodePtr cNode = _nodePtr->children;
  xmlNodePtr lNode = 0;
  while(cNode != 0)
  {
    switch(cNode->type)
    {
    case XML_CDATA_SECTION_NODE:
    case XML_TEXT_NODE:
      if(xmlIsBlankNode(cNode))
      {
        xmlNodePtr next = cNode->next;
        xmlUnlinkNode(cNode);
        xmlFreeNode(cNode);
        cNode = next;
        continue;
      }
      if(lNode != 0)
      {
        lNode = xmlTextMerge(lNode, cNode);
        xmlUnlinkNode(cNode);
        xmlFreeNode(cNode);
        cNode = lNode;
      }
      else
      {
        lNode = cNode;
      }
      break;
    default:
      lNode = 0;
      normalizeNode(cNode);
    }
    cNode = cNode->next;
  }
}

void 
LibXMLNode::normalize()
{
  normalizeNode(_nodePtr);
}

bool 
LibXMLNode::isSupported(IN(RString) feature, IN(RString) version)
{
  // ### @todo implement me
  return false;
}

RString 
LibXMLNode::getNamespaceURI()
{
  if(_nodePtr->type != XML_ELEMENT_NODE && _nodePtr->type != XML_ATTRIBUTE_NODE)
    return Nil;
  if(_nodePtr->ns == 0)
    return Nil;
  return XML2STR(_nodePtr->ns->href);
}

RString 
LibXMLNode::getPrefix()
{
  if(_nodePtr->type != XML_ELEMENT_NODE && _nodePtr->type != XML_ATTRIBUTE_NODE)
    return Nil;
  if(_nodePtr->ns == 0)
    return Nil;
  return XML2STR(_nodePtr->ns->prefix);
}

void 
LibXMLNode::setPrefix(IN(RString) prefix) THROWS1(RDOMException)
{
  const xmlChar* xmlPrefix = STR2XMLDUP(prefix);
  if(xmlValidateName(xmlPrefix, 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "Prefix is not valid");
    
  if(_nodePtr->type != XML_ELEMENT_NODE && _nodePtr->type != XML_ATTRIBUTE_NODE)
    THROW2(DOMException, HIERARCHY_REQUEST_ERR, "Wrong node type to set Prefix");
  if(_nodePtr->ns == 0)
    THROW2(DOMException, NAMESPACE_ERR, "node has no namespace");
  if(_nodePtr->ns->prefix != 0)
    xmlFree((void*)_nodePtr->ns->prefix);
  _nodePtr->ns->prefix = xmlPrefix;
}

RString 
LibXMLNode::getLocalName()
{
  int qnameLen = 0;
  if(xmlSplitQName3(_nodePtr->name, &qnameLen) != 0)
    return XML2STR(_nodePtr->name + qnameLen);
  return XML2STR(_nodePtr->name);
}

bool 
LibXMLNode::hasAttributes()
{
  return _nodePtr->properties != 0;
}

RString 
LibXMLNode::lookupPrefix(IN(RString) namespaceURI)
{
  xmlNodePtr cnode = _nodePtr;
  xmlDocPtr doc = cnode->doc;
  if(cnode->type == XML_DOCUMENT_NODE)
  {
    doc =(xmlDocPtr)cnode;
    cnode = xmlDocGetRootElement(doc);
  }
  xmlNsPtr ns = xmlSearchNsByHref(doc, cnode, STR2XML(namespaceURI));
  if(ns == 0)
    return Nil;
  return XML2STR(ns->prefix);
}

  
bool 
LibXMLNode::isDefaultNamespace(IN(RString) namespaceURI)
{
  xmlNsPtr ns = xmlSearchNsByHref(_nodePtr->doc, _nodePtr, STR2XML(namespaceURI));
  if(ns == 0)
    return false;
  return ns->prefix == 0 || xmlStrlen(ns->prefix) == 0;
}
  
RString 
LibXMLNode::lookupNamespaceURI(IN(RString) prefix)
{
  xmlNodePtr cnode = _nodePtr;
  xmlDocPtr doc = cnode->doc;
  if(cnode->type == XML_DOCUMENT_NODE)
  {
    doc = (xmlDocPtr)cnode;
    cnode = xmlDocGetRootElement(doc);
  }
  xmlNsPtr ns = xmlSearchNs(doc, cnode, STR2XML(prefix));
  if(ns == 0)
    return Nil;
  return XML2STR(ns->href);
}

RString 
LibXMLNode::getTextContent() THROWS1(RDOMException)
{
  switch(getNodeType())
  {
  case ELEMENT_NODE:
  case ATTRIBUTE_NODE:
  case ENTITY_NODE:
  case ENTITY_REFERENCE_NODE:
  case DOCUMENT_FRAGMENT_NODE:
  {
    StringBuffer sb;
    RNodeList children = getChildNodes();
    int len = children->getLength();
    for(int i = 0; i < len; i++)
    {
      RNode child = children->item(i);
      RString textContent = child->getTextContent();
      if(textContent != Nil)
      {
        sb.append(textContent);
      }
    }
    return sb.toString();
  }
  case TEXT_NODE:
  case CDATA_SECTION_NODE:
  case COMMENT_NODE:
  case PROCESSING_INSTRUCTION_NODE:
    return getNodeValue();
  default:
    return Nil;
  }
}

void 
LibXMLNode::setTextContent(IN(RString) textContent) THROWS1(org::w3c::dom::RDOMException)
{
  switch (getNodeType ())
  {
  case ENTITY_REFERENCE_NODE:
    THROW2(DOMException, NO_MODIFICATION_ALLOWED_ERR, "");
  case ELEMENT_NODE:
  case ATTRIBUTE_NODE:
  case ENTITY_NODE:
  case DOCUMENT_FRAGMENT_NODE:
  {
    RNodeList children = getChildNodes();
    int len = children->getLength();
    for (int i = 0; i < len; i++)
    {
      RNode child = children->item(i);
      removeChild(child);
    }
    if (textContent != Nil)
    {
      RText text = getOwnerDocument()->createTextNode(textContent);
      appendChild(&text);
    }
    break;
  }
  case TEXT_NODE:
  case CDATA_SECTION_NODE:
  case COMMENT_NODE:
  case PROCESSING_INSTRUCTION_NODE:
    setNodeValue(textContent);
    break;
  }
}

bool _isEqualNode(xmlNodePtr node1, xmlNodePtr node2);

bool
_isEqualNodeList(xmlNodePtr node1, xmlNodePtr node2)
{
  while(node1 != 0)
  {
    if(_isEqualNode(node1, node2) == false)
      return false;
    node1 = node1->next;
    node2 = node2->next;
  }
  return true;
}
  
bool 
_isEqualNode(xmlNodePtr node1, xmlNodePtr node2)
{
  if(node1 == node2)
    return true;
  if(node1 == 0 || node2 == 0)
    return false;
  if(node1->type != node2->type)
    return false;
  if(xmlStrEqual(node1->name, node2->name) == 0)
    return false;
  if(node1->type == XML_ELEMENT_NODE || node1->type == XML_ATTRIBUTE_NODE)
  {
    xmlNsPtr ns1 = node1->ns;
    if(ns1 != 0)
    {
      xmlNsPtr ns2 = node2->ns;
      if(ns2 == NULL)
        return false;
      if(xmlStrEqual(ns1->href, ns2->href) == 0)
        return false;
      
    }
  }
  const xmlChar* val1 = _getNodeValue(node1);
  const xmlChar* val2 = _getNodeValue(node2);
  if(xmlStrEqual(val1, val2) == 0)
    return false;
  
  if(node1->type == XML_ELEMENT_NODE)
  {
    if(_isEqualNodeList((xmlNodePtr) node1->properties,(xmlNodePtr) node2->properties) == false)
      return false;
  }
  if(node1->type == XML_DOCUMENT_NODE)
  {
    xmlDocPtr doc1 =(xmlDocPtr) node1;
    xmlDocPtr doc2 =(xmlDocPtr) node2;

    if(_isEqualNode((xmlNodePtr)doc1->intSubset,(xmlNodePtr)doc2->intSubset) == false)
      return false;
    if(_isEqualNode((xmlNodePtr) doc1->extSubset,(xmlNodePtr) doc2->extSubset) == false)
      return false;
  }
  if(_isEqualNodeList(node1->children, node2->children) == false)
    return false;
  return true;
}

bool 
LibXMLNode::isEqualNode(IN(RNode) arg)
{
  if(instanceof(arg, LibXMLNode) == false)
    return false;
  return _isEqualNode(_nodePtr, RLibXMLNode(arg)->_nodePtr);
}

RNode 
LibXMLNode::selectNode(IN(RString) xpath)
{
  RLibXMLDocument doc = (RLibXMLDocument)getOwnerDocument();
  RObject obj = doc->evaluate(xpath, this, Nil, org::w3c::dom::xpath::ANY_UNORDERED_NODE_TYPE, Nil);
  if (obj == Nil)
    return Nil;
  if (instanceof(obj, LibXMLXPathResult) == true)
  {
    RLibXMLXPathResult lres = (RLibXMLXPathResult)obj;
    short resType = lres->getResultType();
    return lres->getSingleNodeValue();
  }
  return Nil;
}

RNodeList 
LibXMLNode::selectNodes(IN(RString) xpath)
{
  RLibXMLDocument doc = (RLibXMLDocument)getOwnerDocument();
  RObject obj = doc->evaluate(xpath, this, Nil, org::w3c::dom::xpath::ANY_UNORDERED_NODE_TYPE, Nil);
  if (instanceof(obj, LibXMLXPathResult) == true)
  {
    RLibXMLXPathResult lres = (RLibXMLXPathResult)obj;
    short resType = lres->getResultType();
    return lres->getSnapshotNodeList();
  }
  return Nil;
}

acdk::lang::RString 
LibXMLNode::selectText(IN(acdk::lang::RString) xpath)
{
  RLibXMLDocument doc = (RLibXMLDocument)getOwnerDocument();
  RObject obj = doc->evaluate(xpath, this, Nil, org::w3c::dom::xpath::ANY_UNORDERED_NODE_TYPE, Nil);
  if (instanceof(obj, LibXMLXPathResult) == true)
  {
    RLibXMLXPathResult lres = (RLibXMLXPathResult)obj;
    short resType = lres->getResultType();
    return lres->getStringValue();
  }
  return Nil;
}

acdk::lang::RObject 
LibXMLNode::selectObject(IN(acdk::lang::RString) xpath)
{
  RLibXMLDocument doc = (RLibXMLDocument)getOwnerDocument();
  RObject obj = doc->evaluate(xpath, this, Nil, org::w3c::dom::xpath::ANY_UNORDERED_NODE_TYPE, Nil);
  if (instanceof(obj, LibXMLXPathResult) == false)
    return Nil;

  RLibXMLXPathResult lres = (RLibXMLXPathResult)obj;
  return lres->getObjectValue();
}

acdk::lang::RBoolean 
LibXMLNode::selectBoolean(IN(acdk::lang::RString) xpath)
{
  RLibXMLDocument doc = (RLibXMLDocument)getOwnerDocument();
  RObject obj = doc->evaluate(xpath, this, Nil, org::w3c::dom::xpath::ANY_UNORDERED_NODE_TYPE, Nil);
  if (instanceof(obj, LibXMLXPathResult) == false)
    return Nil;

  RLibXMLXPathResult lres = (RLibXMLXPathResult)obj;
  return Boolean::valueOf(lres->getBooleanValue());
}

int 
LibXMLNode::_mapLibXMLTypeToDomType(int tp)
{
  switch(tp)
  {
  case XML_ELEMENT_NODE: return ELEMENT_NODE;
  case XML_ATTRIBUTE_NODE: return ATTRIBUTE_NODE;
  case XML_TEXT_NODE: return TEXT_NODE;
  case XML_CDATA_SECTION_NODE: return CDATA_SECTION_NODE;
  case XML_ENTITY_REF_NODE: return ENTITY_REFERENCE_NODE;
  case XML_ENTITY_NODE: return XML_ENTITY_NODE;
  case XML_PI_NODE: return PROCESSING_INSTRUCTION_NODE;
  case XML_COMMENT_NODE: return COMMENT_NODE;
  case XML_DOCUMENT_NODE: return DOCUMENT_NODE;
  case XML_DOCUMENT_TYPE_NODE: return DOCUMENT_TYPE_NODE;
  case XML_DOCUMENT_FRAG_NODE: return DOCUMENT_FRAGMENT_NODE;
  case XML_NOTATION_NODE: return NOTATION_NODE;
  case XML_HTML_DOCUMENT_NODE: 
    break;
  case XML_DTD_NODE: return DOCUMENT_TYPE_NODE;
  case XML_ELEMENT_DECL: 
    break;
  case XML_ATTRIBUTE_DECL:
    break;
  case XML_ENTITY_DECL:
    break;
  case XML_NAMESPACE_DECL:
    break;
  case XML_XINCLUDE_START:
    break;
  case XML_XINCLUDE_END:
    break;
  default:
    THROW2(DOMException, NOT_SUPPORTED_ERR, SBSTR("Unknown LibXML node type " << tp));
  }
  THROW2(DOMException, NOT_SUPPORTED_ERR, SBSTR("Cannot map LibXML node type " << tp));
  return 0;
}

int 
LibXMLNode::_mapDomTypeToLibXMLType(int tp)
{
  switch(tp)
  {
  case ATTRIBUTE_NODE: return XML_ATTRIBUTE_NODE;
  case CDATA_SECTION_NODE: return XML_CDATA_SECTION_NODE;
  case COMMENT_NODE: return XML_COMMENT_NODE;
  case DOCUMENT_FRAGMENT_NODE: return XML_DOCUMENT_FRAG_NODE;
  case DOCUMENT_NODE: return XML_DOCUMENT_NODE;
  case DOCUMENT_TYPE_NODE: return XML_DOCUMENT_TYPE_NODE;
  case ELEMENT_NODE: return XML_ELEMENT_NODE;
  case ENTITY_NODE: return XML_ENTITY_DECL;
  case ENTITY_REFERENCE_NODE: return XML_ENTITY_REF_NODE;
  case NOTATION_NODE: return XML_NOTATION_NODE;
  case PROCESSING_INSTRUCTION_NODE: return XML_PI_NODE;
  case TEXT_NODE: return XML_TEXT_NODE;
  default:
    THROW2(DOMException, NOT_SUPPORTED_ERR, SBSTR("Unknown DOM node type " << tp));
  }
  return 0;
}

RString 
LibXMLNode::toXML()
{
  return acdk::xml::dom::NodeUtil::toXml(this);
}

void 
LibXMLNode::write(IN(acdk::io::RWriter) out, int writeFlags, int indentLevel, IN(RString) encoding)
{
  xmlDocPtr doc = _nodePtr->doc;
  xmlOutputBufferPtr buffer = LibXMLDocument::createWriterOutputBuffer(out);
  if (encoding != Nil)
   
  {
    buffer->encoder = xmlFindCharEncodingHandler((const char*)STR2XML(encoding));
    // ### TODO encding check if acdk own 
  }
  int format = 0;
  if (writeFlags & NWFWithIndent)
    format = 1;
  if (doc->type == XML_HTML_DOCUMENT_NODE && ((writeFlags & NWFWHtmlAsXml) != NWFWHtmlAsXml))
  {
    if (((xmlNodePtr)doc) == _nodePtr)
      htmlDocContentDumpFormatOutput(buffer, _nodePtr->doc, (const char*)STR2XML(encoding), format);
    else
      htmlNodeDumpFormatOutput(buffer, _nodePtr->doc, _nodePtr, (const char*)STR2XML(encoding), format);
    //void	
  }
  else
  {
    xmlNodeDumpOutput(buffer, _nodePtr->doc, _nodePtr, indentLevel, format, (const char*)STR2XML(encoding));

  }
  xmlOutputBufferClose(buffer);
  out->flush();
}



RNode
LibXMLNodeList::item(int index)
{
  xmlNodePtr node = _nodePtr->children;
  int count = 0;
  for(count = 0; node != 0 && count < index; count++)
  {
    node = node->next;
  }
  return &LibXMLNode::newInstance(node);
}

int 
LibXMLNodeList::getLength()
{
  xmlNodePtr node = _nodePtr->children;
  int count = 0;
  for(count = 0; node != 0; count++)
  {
    node = node->next;
  }
  return count;
}

xmlAttrPtr
_getNamedItem(xmlNodePtr node, IN(RString) name)
{
  RString rname = name->convert(CCUtf8);
  xmlAttrPtr attr = node->properties;
  while (attr != 0)
  {
    if (_match((const xmlChar*)rname->c_str(), (xmlNodePtr)attr) == true)
      break;
    attr = attr->next;
  }
  return attr;
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::getNamedItem(IN(RString) name)
{
  if (_type == NNMTAttributes)
  {
    xmlAttrPtr  attr = _getNamedItem(_nodePtr, name);
    return &LibXMLNode::newInstance((xmlNodePtr)attr);
  }
  
  xmlDtdPtr dtd = (xmlDtdPtr) _nodePtr;
  xmlHashTablePtr hash = (xmlHashTablePtr)((_type == NNMTEntities) ? dtd->entities : dtd->notations);
  if (hash == 0)
    return Nil;
    
  xmlNodePtr ret = (xmlNodePtr)xmlHashLookup(hash, STR2XML(name));
  return &LibXMLNode::newInstance(ret);
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::setNamedItem(IN(org::w3c::dom::RNode) arg) THROWS1(org::w3c::dom::RDOMException)
{
  xmlNodePtr argNode = _getNodePtr(arg);
  
  if (argNode->doc != _nodePtr->doc)
    THROW2(DOMException, WRONG_DOCUMENT_ERR, "");
  checkChildNode(_nodePtr, argNode, false);  
  
  if (_type == NNMTAttributes)
  {
    if (argNode->parent != 0)
      THROW2(DOMException, INUSE_ATTRIBUTE_ERR, "");
    xmlAddChild(_nodePtr, argNode);
  }
  else
  {
    xmlDtdPtr dtd = (xmlDtdPtr)_nodePtr;
    xmlHashTablePtr hash = (xmlHashTablePtr) ((_type == NNMTEntities) ? dtd->entities : dtd->notations);
    if (hash == 0)
    {
      hash = xmlHashCreate(10);
      if (_type == NNMTEntities)
        dtd->entities = hash;
      else
        dtd->notations = hash;
    }
    xmlHashAddEntry(hash, argNode->name, argNode);
  }
  return arg;
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::removeNamedItem(IN(RString) name) THROWS1(org::w3c::dom::RDOMException)
{
  if (_type == NNMTAttributes)
  {
    xmlAttrPtr attr = _getNamedItem(_nodePtr, name);
    if (attr == 0)
      THROW2(DOMException, NOT_FOUND_ERR, "");
    xmlUnlinkNode((xmlNodePtr)attr);
    return &LibXMLNode::newInstance((xmlNodePtr)attr, true); 
  }
  
  xmlDtdPtr dtd = (xmlDtdPtr) _nodePtr;
  xmlHashTablePtr hash = (xmlHashTablePtr) ((_type == NNMTEntities) ? dtd->entities : dtd->notations);
  if (hash == 0)
    return Nil;
  xmlNodePtr ret = (xmlNodePtr)xmlHashLookup(hash, STR2XML(name));
  if (ret != 0)
  {
    xmlHashRemoveEntry(hash, STR2XML(name), NULL);
  }
  return &LibXMLNode::newInstance((xmlNodePtr)ret, true);
}

struct NodeData
{
  int index;
  int count;
  xmlNodePtr nodePtr;
  NodeData(int idx = 0)
    : index(idx)
    , count(0)
    , nodePtr(0)
  {
  }
};

void
_hashScanner(void* payload, void* vdata, xmlChar* name)
{
  NodeData* data = (NodeData*)vdata;
  if (data->count <= data->index)
    data->nodePtr = (xmlNodePtr)payload;
  data->count++;
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::item(int index)
{
  if (_type == NNMTAttributes)
  {
    xmlAttrPtr attr;
    int count = 0;
    if (_nodePtr->type == XML_ELEMENT_NODE)
    {
      xmlAttrPtr attr = _nodePtr->properties;
      for (count = 0; attr != 0 && count < index; count++)
      {
        attr = attr->next;
      }
      if (attr == 0)
        THROW1(NullPointerException, SBSTR("No attribute at indexs " << index));
      return &LibXMLNode::newInstance((xmlNodePtr)attr);
    }
    return Nil;
  }
  
  xmlDtdPtr dtd = (xmlDtdPtr)_nodePtr;
  xmlHashTablePtr hash = (xmlHashTablePtr) ((_type == NNMTEntities) ? dtd->entities : dtd->notations);
  if (hash == 0)
    return Nil;
  
  NodeData nodeData(index);
  xmlHashScan(hash, _hashScanner, &nodeData);
  return &LibXMLNode::newInstance(nodeData.nodePtr);
}

int 
LibXMLNamedNodeMap::getLength()
{
  if (_type == NNMTAttributes)
  {
    if (_nodePtr->type == XML_ELEMENT_NODE)
    {
      int count = 0;
      xmlAttrPtr attr = _nodePtr->properties;
      while (attr != NULL)
      {
        count++;
        attr = attr->next;
      }
      return count;
    }
    return -1;
  }
 
  xmlDtdPtr dtd = (xmlDtdPtr)_nodePtr;
  xmlHashTablePtr hash = (xmlHashTablePtr) ((_type == NNMTEntities) ? dtd->entities : dtd->notations);
  if (hash == 0)
    return 0;
  NodeData nodeData(-1);
  xmlHashScan(hash, _hashScanner, &nodeData);
  return nodeData.count;
}


xmlAttrPtr
_getNamedItemNS(xmlNodePtr node, IN(RString) uri, IN(RString) localName)
{
  RString ruri = uri->convert(CCUtf8);
  RString rlocalName = localName->convert(CCUtf8);

  const xmlChar* uric = (const xmlChar*)ruri->c_str();
  const xmlChar* lnc = (const xmlChar*)rlocalName->c_str();
 
  xmlAttrPtr attr = node->properties;
  while (attr != 0)
  {
    if (_matchNS(uric, lnc, (xmlNodePtr)attr))
      break;
    attr = attr->next;
  }
  return attr;
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::getNamedItemNS(IN(RString) namespaceURI, IN(RString) localName)
{
  if (_type == NNMTAttributes)
  {
    xmlAttrPtr attr = _getNamedItemNS (_nodePtr, namespaceURI, localName);
    return &LibXMLNode::newInstance((xmlNodePtr)attr);
  }
  return Nil;
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::setNamedItemNS(IN(org::w3c::dom::RNode) arg) THROWS1(org::w3c::dom::RDOMException)
{
  return setNamedItem(arg);
}

org::w3c::dom::RNode 
LibXMLNamedNodeMap::removeNamedItemNS(IN(RString) namespaceURI, IN(RString) localName)
{
  if (_type == NNMTAttributes)
  {
    xmlAttrPtr attr = _getNamedItemNS (_nodePtr, namespaceURI, localName);
    if (attr == 0)
      THROW2(DOMException, NOT_FOUND_ERR, "");
    xmlUnlinkNode((xmlNodePtr) attr);
    return &LibXMLNode::newInstance((xmlNodePtr)attr); // memory leak
  }
  return Nil;
}

bool 
LibXMLAttr::getSpecified()
{
  return ((xmlAttrPtr)_nodePtr.ptr())->atype != 0;
}

RString 
LibXMLAttr::getValue()
{
  xmlChar* text = xmlNodeGetContent(_nodePtr);
  if (text == 0)
    return Nil;
  RString ret = XML2STR(text);
  xmlFree(text);
  return ret;
}
  
void 
LibXMLAttr::setValue(IN(RString) value) THROWS1(org::w3c::dom::RDOMException)
{
  xmlNodeSetContent(_nodePtr, STR2XML(value));
}



} // namespace libxmldom
} // namespace xml
} // namespace acdk



