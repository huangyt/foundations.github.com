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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLNode.h,v 1.11 2005/02/13 03:25:57 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLNode_h
#define acdk_xml_libxmldom_LibXMLNode_h

#include "libxmldom.h"
#include <acdk/lang/NullPointerException.h>
#include <org/w3c/dom/Node.h>
#include <org/w3c/dom/Document.h>
#include <org/w3c/dom/DOMException.h>

struct _xmlNode;
typedef struct _xmlNode xmlNode;
typedef xmlNode *xmlNodePtr;

struct _xmlDoc;
typedef struct _xmlDoc xmlDoc;
typedef xmlDoc *xmlDocPtr;

struct _xmlOutputBuffer;
typedef struct _xmlOutputBuffer xmlOutputBuffer;
typedef xmlOutputBuffer* xmlOutputBufferPtr;

namespace acdk {
namespace xml {
namespace libxmldom {

typedef void (*FreeXmlNodePtrFuncPtr)(xmlNodePtr nodePtr);

foreign struct ACDK_XML_PUBLIC XmlNodePtrHolder
{
  xmlNodePtr _nodePtr;
  FreeXmlNodePtrFuncPtr _freeFunc;
  XmlNodePtrHolder(xmlNodePtr nodePtr, FreeXmlNodePtrFuncPtr freeIt = 0)
    : _nodePtr(nodePtr)
    , _freeFunc(freeIt)
  {
    if (_nodePtr != 0 && freeIt == 0)
      registerNode(_nodePtr);
  }
  ~XmlNodePtrHolder()
  {
    releaseNode();
  }
  void releaseNode()
  {
    if (_nodePtr != 0)
    {
      if (_freeFunc != 0)
        _freeFunc(_nodePtr);
      else
        unregisterNode(_nodePtr);
      _nodePtr = 0;
    }
  }
  operator xmlNodePtr () { return _nodePtr; }
  //operator xmlDocPtr () {  return (xmlDocPtr)_nodePtr; }
  xmlNodePtr operator->() { return ptr(); }
  xmlNodePtr ptr() 
  {
    if (_nodePtr == 0)
      THROW1(NullPointerException, "LibXMLNode _nodePtr is 0");
    return _nodePtr;
  }
  xmlNodePtr _ptr()  { return _nodePtr; }

  void unshare(FreeXmlNodePtrFuncPtr freeIt)
  {
    if (_freeFunc != 0)
      return;
    unregisterNode(_nodePtr);
    _freeFunc = freeIt;
  }
  void share()
  {
    if (_freeFunc == 0)
      return;
    _freeFunc = 0;
    registerNode(_nodePtr);
  }
  static void registerNode(xmlNodePtr nodePtr);
  static void unregisterNode(xmlNodePtr nodePtr);
  static void xmlFreeNode(xmlNodePtr cur);

};

//using namespace org::w3c::dom;

ACDK_DECL_CLASS(LibXMLNode);

class ACDK_XML_PUBLIC LibXMLNode
: extends acdk::lang::Object
, implements org::w3c::dom::Node
{
  ACDK_WITH_METAINFO(LibXMLNode)
protected:
  foreign XmlNodePtrHolder _nodePtr;
public:
  foreign LibXMLNode(xmlNodePtr np, FreeXmlNodePtrFuncPtr freeFuncPtr = 0);
  foreign ~LibXMLNode();
  //virtual void releaseNode();
  foreign static RLibXMLNode newInstance(xmlDocPtr docPtr, xmlNodePtr nodePtr, int nodeType, bool ownsPtr = false);
  foreign static RLibXMLNode newInstance(xmlNodePtr nodePtr, bool ownsPtr = false);
  RString getNodeName();
  
  RString getNamespaceURI();
  RString getLocalName();
  RString getBaseURI();
  RString getPrefix();
  void setPrefix(IN(RString) prefix) THROWS1(org::w3c::dom::RDOMException);

  RString getNodeValue()  THROWS1(org::w3c::dom::RDOMException);

  void setNodeValue(IN(RString) nodeValue) THROWS1(org::w3c::dom::RDOMException);

  short getNodeType();

  org::w3c::dom::RNode getParentNode();

  org::w3c::dom::RNodeList getChildNodes();

  org::w3c::dom::RNode getFirstChild();
  org::w3c::dom::RNode getLastChild();
  org::w3c::dom::RNode getPreviousSibling();
  org::w3c::dom::RNode getNextSibling();
  org::w3c::dom::RNamedNodeMap getAttributes();

  org::w3c::dom::RDocument getOwnerDocument();

  org::w3c::dom::RNode insertBefore(IN(org::w3c::dom::RNode) newChild, IN(org::w3c::dom::RNode) refChild) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNode replaceChild(IN(org::w3c::dom::RNode) newChild, IN(org::w3c::dom::RNode) oldChild) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNode removeChild(IN(org::w3c::dom::RNode) oldChild) THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNode appendChild(IN(org::w3c::dom::RNode) newChild) THROWS1(org::w3c::dom::RDOMException);

  bool hasChildNodes();

  org::w3c::dom::RNode cloneNode(bool deep);

  void normalize();

  bool isSupported(IN(RString) feature, IN(RString) version);



  bool hasAttributes();
  RString lookupPrefix(IN(RString) namespaceURI);
  
  bool isDefaultNamespace(IN(RString) namespaceURI);
  
  RString lookupNamespaceURI(IN(RString) prefix);
  
  virtual bool isEqualNode(IN(org::w3c::dom::RNode) arg);
  /** java 1.5  
  RObject getFeature(IN(RString) feature, IN(RString) version)
  {
    return getOwnerDocument()->getImplementation()->getFeature(feature, version);
  }
  */
  /** java 1.5
  RObject setUserData(IN(RString) key, IN(RObject) data, IN(RUserDataHandler) handler)
  {
    
  }
  */
  RObject getUserData(IN(RString) key)
  {
    /*
    if(userData == null)
      {
        return null;
      }
    return userData.get(key);
    */
    return Nil;
  }
  RString  getTextContent() THROWS1(org::w3c::dom::RDOMException);
  void setTextContent(IN(RString) textContent) THROWS1(org::w3c::dom::RDOMException);
  RString toString()
  {
    return SBSTR(getClass()->getName() << "[nodeName=" << getNodeName() << "]");
  }
  virtual org::w3c::dom::RNode selectNode(IN(RString) xpath);
  /**
    @see implementation in acdk::xml::dom::NodeUtil::selectNodes
  */
  virtual org::w3c::dom::RNodeList selectNodes(IN(RString) xpath);
  acdk::lang::RString selectText(IN(acdk::lang::RString) xpath);
  acdk::lang::RObject selectObject(IN(acdk::lang::RString) xpath);
  acdk::lang::RBoolean selectBoolean(IN(acdk::lang::RString) xpath);
  foreign xmlNodePtr getNodePtr() { return _nodePtr; }
  virtual int getChildCount();
  /** 
    @internal
    set internal free function
  */
  foreign void _unshare(FreeXmlNodePtrFuncPtr freeFuncPtr)
  {
    _nodePtr.unshare(freeFuncPtr);
  }
  foreign RString toXML();
  foreign void write(IN(acdk::io::RWriter) out, int writeFlags  = org::w3c::dom::NWFWDefaultFlags, int indentLevel = 0, IN(RString) encoding = Nil); 
protected:
  static int _mapLibXMLTypeToDomType(int tp);
  static int _mapDomTypeToLibXMLType(int tp);
};





} // namespace libxmldom
} // namespace xml
} // namespace acdk



#endif //acdk_xml_libxmldom_LibXMLNode_h
