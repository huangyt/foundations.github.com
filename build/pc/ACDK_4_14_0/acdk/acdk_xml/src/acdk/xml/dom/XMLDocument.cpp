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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLDocument.cpp,v 1.10 2005/03/08 18:50:42 kommer Exp $

#include "XMLDocument.h"
#include "XMLText.h"
#include "XMLComment.h"

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace xml {
namespace dom {


void 
XMLDocument::setDocumentRoot(IN(RXMLElement) el) 
{ 
  if (_childs->length() > 0 && el == _childs[0])
    return;
  if (_childs->length() > 0)
    _sharedRefs.unregisterSharedObjectRefs(reinterpret_cast<RObject*>(el->getOwnerDocumentRef()._ref_this()), reinterpret_cast<RObject*>(_childs[0]._ref_this()));
  
  if (_childs->length() == 0)
  {
    appendChild(&el);
    return;
  }
  el->setOwnerDocument(this);
  _childs[0] = &el;
  _sharedRefs.registerSharedObjectRefs(reinterpret_cast<RObject*>(el->getOwnerDocumentRef()._ref_this()), reinterpret_cast<RObject*>(_childs[0]._ref_this()));
}


//virtual 
RDocumentType 
XMLDocument::getDoctype()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
RDOMImplementation 
XMLDocument::getImplementation()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
  
  
//virtual 
RElement 
XMLDocument::createElement(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
{
  return new XMLElement(s);
}
  
//virtual 
RDocumentFragment 
XMLDocument::createDocumentFragment()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
 
//virtual 
RText 
XMLDocument::createTextNode(IN(RString) s)
{
  return (RText)new XMLText(s, TEXT_NODE);
}
 
//virtual 
RComment 
XMLDocument::createComment(IN(RString) s)
{
  return new XMLComment(s);
}

//virtual 
RCDATASection 
XMLDocument::createCDATASection(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
RProcessingInstruction 
XMLDocument::createProcessingInstruction(IN(RString) s, IN(RString) s1) THROWS1(org::w3c::dom::RDOMException)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
RAttr 
XMLDocument::createAttribute(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
 
//virtual 
REntityReference 
XMLDocument::createEntityReference(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
 
//virtual 
RNodeList 
XMLDocument::getElementsByTagName(IN(RString) s)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
bool 
XMLDocument::hasFeature(IN(RString) feature, IN(RString) version)
{
  return false;
}

RDocument 
XMLDocument::createDocument(IN(RString) namespaceURI, IN(RString) qualifiedName, IN(RDocumentType) doctype)
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

} // namespace dom
} // namespace xml
} // namespace acdk

