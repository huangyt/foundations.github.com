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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/Node.cpp,v 1.6 2005/02/05 10:45:37 kommer Exp $


#include "Node.h"
#include "DocumentType.h"
#include "Element.h"
#include "Comment.h"
#include "Document.h"

namespace org {
namespace w3c {
namespace dom {


int
Node::getChildCount()
{
  if (hasChildNodes() == false)
    return 0;
  RNode cn = getFirstChild();
  int count = 0;
  while (cn != Nil)
  {
    ++count;
    cn = cn->getNextSibling();
  }
  return count;
}

RNode 
Node::getChild(int idx)
{
  if (idx == -1)
    THROW2(DOMException, NOT_FOUND_ERR, "");
  RNode cn = getFirstChild();
  int count = 0;
  while (cn != Nil)
  {
    if (count == idx)
      return cn;
    ++count;
    cn = cn->getNextSibling();
  }
  THROW2(DOMException, NOT_FOUND_ERR, "");
  return Nil;
}

//virtual 
acdk::lang::RObject 
Node::selectObject(IN(acdk::lang::RString) xpath)
{
  // ### @todo implement me
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
acdk::lang::RString 
Node::selectText(IN(acdk::lang::RString) xpath)
{
  // ### @todo implement me
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
acdk::lang::RNumber 
Node::selectNumber(IN(acdk::lang::RString) xpath)
{
  RString t = selectText(xpath);
  if (t == Nil)
    return Nil;
  try {
    return Number::decodeToNumber(t);
  } catch (RNumberFormatException ex) {

  }
  return Nil;
}

acdk::lang::RBoolean 
Node::selectBoolean(IN(acdk::lang::RString) xpath)
{
  // ### @todo implement me
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
RNode 
Node::detach()
{
  return getParentNode()->removeChild(this);
}

void 
DocumentType::toXML(StringBuffer& sb)
{
  sb << "<!DOCTYPE " << getName();
  bool hasPublicID = false;
  RString publicID = getPublicID();
  
  if (publicID != Nil && publicID->length() > 0) 
  {
    sb << " PUBLIC \""  << publicID << "\"";
    hasPublicID = true;
  }
  
  RString systemID = getSystemID();
  if (systemID != Nil && systemID->length() > 0) 
  {
    if (hasPublicID == false) 
      sb << " SYSTEM";
    sb << " \"" << systemID << "\"";
  }
  /** not really
  RNamedNodeMap nnm = getEntities(); 
  int i = 0;
  for (i = 0; i < nnm->getLength(); ++i)
  {
    RNode sn = nnm->item(i);
    // ### @todo implement me
  }
  nnm = getNotations();
  */
  sb << ">";
}

RElement 
Branch::addElement(IN(RString) name)
{
  RElement newElem = getOwnerDocument()->createElement(name);
  appendChild(&newElem);
  return newElem;

}

RElement
Element::addComment(IN(RString) comment)
{
  RComment commentNode = getOwnerDocument()->createComment(comment);
  appendChild(&commentNode);
  return this;
}

RElement 
Element::addAttribute(IN(RString) name, IN(RString) value)
{
  if (value == Nil)
  {
    removeAttribute(name);
    return this;
  }
  setAttribute(name, value);
  return this;
}



RElement 
Element::addCDATA(IN(RString) cdata)
{
  RCDATASection cdataSec = getOwnerDocument()->createCDATASection(cdata);
  appendChild(&cdataSec);
  return this;
}

RElement 
Element::addEntity(IN(RString) name, IN(RString) text)
{
  THROW2(DOMException, NOT_SUPPORTED_ERR, "Element::addEntity()");
  return this;
}

RElement 
Element::addText(IN(RString) text)
{
  RText textEl = getOwnerDocument()->createTextNode(text);
  appendChild(&textEl);
  return this;
}

RDocument 
Document::addComment(IN(RString) comment)
{
  RComment commentNode = getOwnerDocument()->createComment(comment);
  appendChild(&commentNode);
  return this;
}



} // namespace dom
} // namespace w3c
} // namespace org

