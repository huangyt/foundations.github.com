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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLNode.cpp,v 1.14 2005/02/05 10:45:36 kommer Exp $

#include "XMLNode.h"
#include "XMLNodeList.h"
#include "NodeUtil.h"


namespace acdk {
namespace xml {
namespace dom {

XMLNode::~XMLNode()
{
}

RString 
XMLNode::toXML()
{
  return NodeUtil::toXml(this);
}


RNode 
XMLNode::getPreviousSibling()
{
  if (instanceof(_parent, XMLNode) == true)
  {
    RXMLNode p = (RXMLNode)_parent;
    RXMLNodeArray ca = p->_childs;
    int idx = ca->find(this);
    if (idx == -1 || idx == 0)
      return Nil;
    return &(ca[idx - 1]);
  }
  else
  {
    RNodeList nl = _parent->getChildNodes();
    int nll = nl->getLength();
    for (int i = 0; i < nll; ++i)
    {
      if (nl->item(i) == this)
      {
        if (i == 0)
          return Nil;
        return nl->item(i - 1);
      }
    }
    return Nil;
  }
}

RNode 
XMLNode::getNextSibling()
{
  if (instanceof(_parent, XMLNode)  == true)
  {
    RXMLNode p = (RXMLNode)_parent;
    RXMLNodeArray ca = p->_childs;
    int idx = ca->find(this);
    if (idx == -1 || idx == ca->length() - 1)
      return Nil;
    return &(ca[idx + 1]);
  }
  else
  {
    RNodeList nl = _parent->getChildNodes();
    int nll = nl->getLength();
    for (int i = 0; i < nll; ++i)
    {
      if (nl->item(i) == this)
      {
        if (i == nll - 1)
          return Nil;
        return nl->item(i + 1);
      }
    }
    return Nil;
  }
}

RString 
XMLNode::getNodeValue() THROWS1(RDOMException)
{
  return _value;
}

void 
XMLNode::setNodeValue(IN(RString) s) THROWS1(RDOMException)
{
  _value = s;
}

//virtual 
RNodeList 
XMLNode::getChildNodes()
{
  return new XMLNodeList(_childs);
}

//virtual 
RNode 
XMLNode::insertBefore(IN(RNode) newChild, IN(RNode) refChild) THROWS1(RDOMException)
{
  int idx = _childs->find((RXMLNode)refChild);
  if (idx == -1)
    idx = 0;
  _childs->insert(idx, (RXMLNode)newChild);
  return newChild;
}

//virtual 
RNode 
XMLNode::replaceChild(IN(RNode) newNode, IN(RNode) oldNode) THROWS1(RDOMException)
{
  int idx = _childs->find((RXMLNode)oldNode);
  if (idx == -1)
    return oldNode;
  _childs[idx] = (RXMLNode)newNode;
  return oldNode;
}
  
//virtual 
RNode 
XMLNode::removeChild(IN(RNode) node) THROWS1(RDOMException)
{
  RXMLNode xmlnode = (RXMLNode)node;
  int idx = _childs->find(xmlnode);
  if (idx == -1)
    THROW2(DOMException, NOT_FOUND_ERR, "cannot find Node");
  _sharedRefs.unregisterSharedObjectRefs(reinterpret_cast<RObject*>(_childs[idx]._ref_this()), 
                                         reinterpret_cast<RObject*>(xmlnode->_parent._ref_this()));
  
  //_sharedRefs.unregisterSharedObject(this,  reinterpret_cast<RObject*>(xmlnode->_parent._ref_this()));
  //_sharedRefs.unregisterSharedObject((RObject)xmlnode->_parent, (RObject*)_childs[idx]._ref_this());
  _childs->removeSameElement(xmlnode);
  xmlnode->_parent = Nil;
  return node;
}
  
//virtual 
RNode 
XMLNode::appendChild(IN(RNode) node) THROWS1(RDOMException)
{
  RXMLNode xmlnode = (RXMLNode)node;
  _childs->append(xmlnode);
  int idx = _childs->length() - 1;
  xmlnode->setParentNode(this);
  _sharedRefs.registerSharedObjectRefs(reinterpret_cast<RObject*>(xmlnode->_parent._ref_this()), reinterpret_cast<RObject*>(_childs[idx]._ref_this())
                                       );
  //_sharedRefs.registerSharedObject(this, reinterpret_cast<RObject*>(xmlnode._ref_this()), true);
  //_sharedRefs.registerSharedObject(&(RObject)node, reinterpret_cast<RObject*>(xmlnode->_parent._ref_this()), false);
  return node;
}

bool 
XMLNode::isEqualNode(IN(org::w3c::dom::RNode) arg)
{
  if (this == arg.impl())
    return true;
  // ### @todo check more
  return false;
}


//virtual 
RNode 
XMLNode::cloneNode(bool flag)
{
  return this;
}

RNode 
XMLNode::selectNode(IN(RString) xpath)
{
  return NodeUtil::selectNode(this, xpath);
}

RNodeList 
XMLNode::selectNodes(IN(RString) xpath) 
{
  return NodeUtil::selectNodes(this, xpath);
}

} // namespace dom
} // namespace xml
} // namespace acdk

//#endif //acdk_xml_dom_XMLNode_h
