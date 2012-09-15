// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright(C) 2000-2003 by Roger Rene Kommer / artefaktur, Kassel, Germany->
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE->	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details->

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www->acdk->de
// - http://www->artefaktur->com
// - http://acdk->sourceforge->net
// for more information->
// 
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLText.cpp,v 1.2 2005/02/05 10:45:36 kommer Exp $


#include "LibXMLText.h"


namespace acdk {
namespace xml {
namespace libxmldom {

using namespace org::w3c::dom;

org::w3c::dom::RText 
LibXMLText::splitText(int offset) THROWS1(org::w3c::dom::RDOMException)
{
  RString value = getNodeValue();
  RString part1 = value->substr(0, offset);
  RString part2 = value->substr(offset);
  RText text = getOwnerDocument()->createTextNode(part1);
  getParentNode()->insertBefore(&text, this);
  setNodeValue(part2);
  return text;
}

// DOM Level 3

bool 
LibXMLText::isElementContentWhitespace()
{
  return getTextContent()->trim()->length() == 0;
}

RString 
LibXMLText::getWholeText()
{
  RNode first = this;
  RNode node = getPreviousSibling();
  while(node != Nil && instanceof(node, Text) == true)
  {
    first = node;
    node = node->getPreviousSibling();
  }
  StringBuffer buf;
  node = first->getNextSibling();
  while(node != Nil && instanceof(node, Text) == true)
  {
    buf.append(node->getNodeValue());
    node = node->getNextSibling();
  }
  return buf.toString();
}

org::w3c::dom::RText 
LibXMLText::replaceWholeText(RString content) THROWS1(org::w3c::dom::RDOMException)
{
  bool isEmpty = (content == Nil || content->length() == 0);
  if(isEmpty == false)
    setNodeValue(content);
  
  RNode first = this;
  RNode node = getPreviousSibling();
  while(node != Nil && instanceof(node, Text) == true)
  {
    first = node;
    node = node->getPreviousSibling();
  }
  node = first->getNextSibling();
  RNode parent = getParentNode();
  if(first != this || isEmpty == true)
  {
    parent->removeChild(first);
  }
  while(node != Nil && instanceof(node, Text) == true)
  {
    RNode tmp = node;
    node = node->getNextSibling();
    if(tmp != this || isEmpty)
    {
      parent->removeChild(tmp);
    }
  }
  if (isEmpty == true)
    return Nil;
  return this;
}

} // namespace libxmldom
} // namespace xml
} // namespace acdk

