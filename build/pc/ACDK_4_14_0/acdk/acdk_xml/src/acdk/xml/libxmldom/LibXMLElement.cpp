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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLElement.cpp,v 1.8 2005/02/20 13:56:30 kommer Exp $


#include "LibXMLElement.h"
#include "LibXMLPathNodeList.h"

#include "LibXMLDOMInternals.h"



namespace acdk {
namespace xml {
namespace libxmldom {




using namespace org::w3c::dom;

RString 
LibXMLElement::getAttribute(IN(RString) name)
{
  xmlChar* value = xmlGetProp(_nodePtr, STR2XML(name));
  if (value == 0)
    return "";
  return XML2STR(value);
}

void 
LibXMLElement::setAttribute(IN(RString) name, IN(RString) value) THROWS1(org::w3c::dom::RDOMException)
{
  if (xmlValidateName(STR2XML(name), 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "Name of attribute is invalid");
  xmlSetProp(_nodePtr, STR2XML(name), STR2XML(value));
}
  
org::w3c::dom::RAttr 
LibXMLElement::getAttributeNode(IN(RString) name)
{
  xmlAttrPtr attr = xmlHasProp(_nodePtr, STR2XML(name));
  if (attr == 0)
    return Nil;
  return (org::w3c::dom::RAttr)LibXMLNode::newInstance((xmlNodePtr) attr);
}
  
xmlNodePtr _getNodePtr(IN(org::w3c::dom::RNode) node)
{
  if (instanceof(node, LibXMLNode) == false)
    THROW2(DOMException, NOT_FOUND_ERR, "Node is not a LibXMLNode");
  return RLibXMLNode(node)->getNodePtr();
}

void
_addAttribute(xmlNodePtr node, xmlAttrPtr attr)
{
  xmlAttrPtr cur = node->properties;
  if (cur == 0)
  {
    node->properties = attr;
    attr->prev = 0;
    attr->next = 0;
    attr->parent = node;
    attr->doc = node->doc;
  }
  else
  {
    while (cur->next != 0)
      cur = cur->next;
    cur->next = attr;
    attr->prev = cur;
    attr->next = NULL;
    attr->parent = node;
    attr->doc = node->doc;
  }
}

org::w3c::dom::RAttr 
LibXMLElement::setAttributeNode(IN(org::w3c::dom::RAttr) newAttr) THROWS1(org::w3c::dom::RDOMException)
{
  
  xmlAttrPtr new_attr = (xmlAttrPtr)_getNodePtr(&newAttr);
  if (new_attr->parent != 0)
    THROW2(DOMException, INUSE_ATTRIBUTE_ERR, "");
  if (new_attr->doc != _nodePtr->doc)
    THROW2(DOMException, WRONG_DOCUMENT_ERR, "");
  xmlAttrPtr old_attr = xmlHasProp(_nodePtr, new_attr->name);
  if (old_attr != 0)
      xmlUnlinkNode((xmlNodePtr)old_attr);
  _addAttribute(_nodePtr, new_attr);
  return (org::w3c::dom::RAttr)LibXMLNode::newInstance((xmlNodePtr) new_attr);
}

org::w3c::dom::RAttr 
LibXMLElement::removeAttributeNode(IN(org::w3c::dom::RAttr) oldAttr) THROWS1(org::w3c::dom::RDOMException)
{
  xmlAttrPtr attr = (xmlAttrPtr)_getNodePtr(&oldAttr);
  xmlUnlinkNode((xmlNodePtr)attr);
  return oldAttr;
}

xmlXPathContextPtr
_createContextPathPtr(xmlNodePtr node)
{
  xmlXPathContextPtr ctx = xmlXPathNewContext(node->doc);
  ctx->node = node;
  return ctx;
}

org::w3c::dom::RNodeList 
_getElementsByTagName(xmlNodePtr _nodePtr, IN(RString) name)
{
  RString format;
  if (name->equals("*") == true)
    format = "descendant-or-self::*[node-type()=1]";
  else
    format = SBSTR("descendant-or-self::*[name()='" << name << "']");

  RString rformat = STR2XMLSTR(format);
  
  xmlXPathContextPtr ctx = _createContextPathPtr(_nodePtr);
  xmlXPathObjectPtr eval = 0;
  if (ctx != 0)
  {
    eval = xmlXPathEval((const xmlChar*)rformat->c_str(), ctx);
    xmlXPathFreeContext(ctx);
  }
  return new LibXMLPathNodeList(eval);
}

org::w3c::dom::RNodeList 
LibXMLElement::getElementsByTagName(IN(RString) name)
{
  return _getElementsByTagName(_nodePtr, name);
}
  
RString 
LibXMLElement::getAttributeNS(IN(RString) namespaceURI, IN(RString) localName)
{
  RString value = Nil;
  if (namespaceURI == Nil)
    value = XML2STR(xmlGetNoNsProp(_nodePtr, STR2XML(localName)));
  else
    value = XML2STR(xmlGetNsProp(_nodePtr, STR2XML(localName), STR2XML(namespaceURI)));
  if (value == Nil)
    return "";
  return value;
}

const xmlChar*
_getLocalName(const xmlChar* fqName)
{
  xmlChar *prefix = 0;
  const xmlChar* localName = xmlSplitQName2(fqName, &prefix);
  if (localName == 0)
    return fqName;
  return localName;
}

const xmlChar*
_getPrefix(const xmlChar* fqName)
{
  xmlChar *prefix = 0;
  const xmlChar* localName = xmlSplitQName2(fqName, &prefix);
  if (localName == 0)
    return 0;
  return prefix;
}
  
void 
LibXMLElement::setAttributeNS(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(RString) value) THROWS1(org::w3c::dom::RDOMException)
{
  if (xmlValidateQName (STR2XML(qualifiedName), 0) != 0)
    THROW2(DOMException, INVALID_CHARACTER_ERR, "");
  if (namespaceURI == Nil)
      xmlSetProp(_nodePtr, STR2XML(qualifiedName), STR2XML(value));
  else
  {
    RString uvalue = STR2XMLSTR(value);
    RString uqname = STR2XMLSTR(qualifiedName);
    const xmlChar* prefixc = _getPrefix((const xmlChar*)uvalue->c_str());
    const xmlChar* localNamec = _getLocalName((const xmlChar*)uqname->c_str());
    xmlNsPtr ns = xmlNewNs(_nodePtr, STR2XML(namespaceURI), prefixc);
    xmlSetNsProp(_nodePtr, ns, prefixc, (const xmlChar*)uvalue->c_str());
  }
}

bool
_match(const xmlChar* name, xmlNodePtr node)
{
  switch (node->type)
  {
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
    return xmlStrcmp (node->name, name) == 0;
  default:
    return false;
  }
}

bool
_matchNS(const xmlChar* uri, const xmlChar* localName, xmlNodePtr node)
{
  switch (node->type)
  {
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
  {
    int len = 0; 
    const xmlChar *nodeLocalName = 0;
    if (xmlSplitQName3(node->name, &len) != 0)
    {
      nodeLocalName = node->name + len;
    }
    else
    {
      nodeLocalName = node->name;
    }
    xmlNsPtr ns = node->ns;
    if (ns == 0 || ns->href == 0)
    {
      if (uri != 0)
      {
        return true;
      }
      return xmlStrcmp (localName, nodeLocalName) == 0;
    }
    if (uri == 0)
      return true;
    return xmlStrcmp(localName, nodeLocalName) == 0 && xmlStrcmp(uri, ns->href) == 0;
  }
  default:
    return false;
  }
}

org::w3c::dom::RAttr 
LibXMLElement::getAttributeNodeNS(IN(RString) namespaceURI, IN(RString) localName)
{
  xmlAttrPtr attr = _nodePtr->properties;
  while (attr != 0)
  {
    if (namespaceURI == Nil)
    {
      if (_match(STR2XML(localName), (xmlNodePtr)attr) == false)
        break;
    }
    else
    {
      if (_matchNS(STR2XML(namespaceURI), STR2XML(localName), (xmlNodePtr)attr) == false)
        break;
    }
    attr = attr->next;
  }
  return (org::w3c::dom::RAttr)newInstance((xmlNodePtr)attr);
}

org::w3c::dom::RAttr 
LibXMLElement::setAttributeNodeNS(IN(org::w3c::dom::RAttr) newAttr) THROWS1(org::w3c::dom::RDOMException)
{
  xmlAttrPtr new_attr = (xmlAttrPtr) _getNodePtr(&newAttr);
  if (new_attr->parent != 0)
    THROW2(DOMException, INUSE_ATTRIBUTE_ERR, ""); 
  if (new_attr->doc != _nodePtr->doc)
    THROW2(DOMException, WRONG_DOCUMENT_ERR, ""); 

  const xmlChar* uri = (new_attr->ns != 0) ? new_attr->ns->href : 0;
  xmlAttrPtr old_attr = xmlHasNsProp(_nodePtr, new_attr->name, uri);
  if (old_attr != 0)
      xmlUnlinkNode((xmlNodePtr)old_attr);

  _addAttribute(_nodePtr, new_attr);
  if (old_attr == 0)
    return Nil;
  return (org::w3c::dom::RAttr)newInstance((xmlNodePtr)old_attr, true); 
}

org::w3c::dom::RNodeList 
_getElementsByTagNameNS(xmlNodePtr _nodePtr, IN(RString) namespaceURI, IN(RString) localName)
{
  if (_nodePtr == 0)
    return Nil;

  RString format;
  if (namespaceURI == Nil)
  {
    RString format;
    if (localName->equals("*") == true)
      format = "descendant-or-self::*[namespace-uri()='' and node-type()=1]";
    else
      format = SBSTR("descendant-or-self::*[namespace-uri()='' and local-name()='" << localName << "']");
  }
  else if (namespaceURI->equals("*") == true)
  {
    if (localName->equals("*") == true)
      format = "descendant-or-self::*[node-type()=1]";
    else
      format = SBSTR("descendant-or-self::*[local-name()='" << localName << "']");
  }
  else
  {
    if (localName->equals("*") == true)
      format = "descendant-or-self::*[namespace-uri()='%s' and node-type()=1]";
    else
      format = SBSTR("descendant-or-self::*[namespace-uri()='" << namespaceURI << "' and local-name()='" << localName << "']");
  }

  xmlXPathContextPtr ctx = _createContextPathPtr(_nodePtr);
  xmlXPathObjectPtr eval = 0;
  if (ctx != 0)
  {
    eval = xmlXPathEval(STR2XML(format), ctx);
    xmlXPathFreeContext(ctx);
  }
  return new LibXMLPathNodeList(eval);
}

org::w3c::dom::RNodeList 
LibXMLElement::getElementsByTagNameNS(IN(RString) namespaceURI, IN(RString) localName)
{
  return _getElementsByTagNameNS(_nodePtr, namespaceURI, localName);
}
  
bool 
LibXMLElement::hasAttribute(IN(RString) name)
{
  return xmlGetProp(_nodePtr, STR2XML(name)) != 0;
}

bool 
LibXMLElement::hasAttributeNS(IN(RString) namespaceURI, IN(RString) localName)
{
  if (namespaceURI == Nil)
    return xmlGetNoNsProp(_nodePtr, STR2XML(localName)) != 0;
  else
    return xmlGetNsProp(_nodePtr, STR2XML(localName), STR2XML(namespaceURI)) != 0;
}

/*
### @todo implement me
RTypeInfo getSchemaTypeInfo()
{
  return new LibXMLTypeInfo(id);
}
*/

void 
LibXMLElement::setIdAttributeNode(IN(org::w3c::dom::RAttr) isAddr, bool isId)
{
  // ### @todo implement me
  THROW0(UnsupportedOperationException);
}

void 
LibXMLElement::setIdAttributeNS(IN(RString) namespaceURI, IN(RString) localName, bool isId)
{
  // ### @todo implement me
  THROW0(UnsupportedOperationException);
}

int 
LibXMLElement::attributeCount()
{
  xmlAttrPtr cur = _nodePtr->properties;
  int count = 0;
  while (cur != 0)
  {
    cur = cur->next;
    ++count;
  }
  return count;
}

org::w3c::dom::RAttr 
LibXMLElement::attribute(int idx)
{
  if (idx < 0)
    THROW2(DOMException, NOT_FOUND_ERR, SBSTR("LibXMLElement::attribute idx out of bounds. Index: " << idx));
  xmlAttrPtr cur = _nodePtr->properties;
  int count = 0;
  while (cur != 0)
  {
    if (count == idx)
      return (org::w3c::dom::RAttr)LibXMLNode::newInstance((xmlNodePtr) cur);
    cur = cur->next;
    ++count;
  }
  THROW2(DOMException, NOT_FOUND_ERR, SBSTR("LibXMLElement::attribute idx out of bounds. Index " << idx << ", attributeCount: " << count));
  return Nil;
}

int 
LibXMLPathNodeList::getLength()
{
  if (_xpathPtr == 0 || _xpathPtr->nodesetval == 0)
    return 0;
  return _xpathPtr->nodesetval->nodeNr;
}

org::w3c::dom::RNode 
LibXMLPathNodeList::item(int index)
{
  if (_xpathPtr == 0 || _xpathPtr->nodesetval == 0)
    return Nil;
  if (_xpathPtr->nodesetval->nodeNr <= 0)
    return Nil;
  return &LibXMLNode::newInstance(_xpathPtr->nodesetval->nodeTab[index]);
}

void 
LibXMLPathNodeList::_free()
{
  if (_xpathPtr != 0)
  {
    xmlXPathFreeObject(_xpathPtr);
    _xpathPtr = 0;
  }
}


} // namespace libxmldom
} // namespace xml
} // namespace acdk


