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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLNode.h,v 1.14 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLNode_h
#define acdk_xml_dom_XMLNode_h

#include "../Config.h"
#include <acdk.h>
#include <acdk/lang/ref/SharedOwning.h>

#include <org/w3c/dom/Node.h>

namespace acdk {
namespace xml {
namespace dom {



using namespace org::w3c::dom;

ACDK_DECL_CLASS(XMLNode);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLNode
: extends acdk::lang::Object
, implements org::w3c::dom::Node
{
  ACDK_WITH_METAINFO(XMLNode)
protected:
  RString _name;
  RString _value;
  short _nodeType;
  
  foreign RNode _parent;
  acdk::lang::ref::SharedOwning _sharedRefs;
  RXMLNodeArray _childs;
public: 
  XMLNode(IN(RString) name, short type)
  : _name(name)
  , _value("")
  , _nodeType(type)
  , _parent(Nil)
  , _childs(new XMLNodeArray(0))
  {
  }
  ~XMLNode();
  foreign RString toXML();
  virtual RString getNodeName() { return _name; }

  virtual acdk::lang::RString getNamespaceURI() { return Nil; }
  virtual acdk::lang::RString getLocalName() { return Nil; }
  virtual acdk::lang::RString getBaseURI() { return Nil; }
  virtual acdk::lang::RString getPrefix() { return Nil; }
  virtual void setPrefix(IN(acdk::lang::RString) prefix) THROWS1(RDOMException) {  }

  virtual RString getNodeValue() THROWS1(RDOMException);
  virtual void setNodeValue(IN(RString) s) THROWS1(RDOMException);
  virtual short getNodeType() { return _nodeType; }
  virtual RNode getParentNode() { return _parent; }
  void setParentNode(IN(RNode) p) { _parent = &p; }
  virtual RNodeList getChildNodes();
  virtual RNode getFirstChild() 
  { 
    if (_childs->length() > 0)
      return (RNode)_childs[0]; 
    return Nil;
  }
  virtual RNode getLastChild()
  { 
    if (_childs->length() > 0)
      return (RNode)_childs[_childs->length() - 1]; 
    return Nil;
  }
  virtual RNode getPreviousSibling();
  virtual RNode getNextSibling();
  // is overwritten in XMLElement
  virtual RNamedNodeMap getAttributes() { return Nil; }
  virtual RDocument getOwnerDocument() 
  { 
    if (_parent != 0)
      return _parent->getOwnerDocument();
    return Nil;
  }
  virtual RNode insertBefore(IN(RNode) newChild, IN(RNode) refChild) THROWS1(RDOMException);
  virtual RNode replaceChild(IN(RNode) newChild, IN(RNode) oldChild) THROWS1(RDOMException);
  virtual RNode removeChild(IN(RNode) node) THROWS1(RDOMException);
  virtual RNode appendChild(IN(RNode) node) THROWS1(RDOMException);
  virtual bool hasChildNodes()
  {
    return _childs->length() != 0;
  }
  
  virtual RNode cloneNode(bool flag);

  virtual RNode selectNode(IN(RString) xpath);
  virtual RNodeList selectNodes(IN(RString) xpath);

  RString _childsToString()
  {
    StringBuffer sb;
    for (int i = 0; i < _childs->length(); ++i)
    {
      sb << "\n[" + _childs[i]->toString() + "]";
    }
    return sb.toString();
  }
  virtual bool isEqualNode(IN(org::w3c::dom::RNode) arg);
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLNode_h
