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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLElement.cpp,v 1.10 2005/02/05 10:45:36 kommer Exp $

#include "XMLElement.h"
#include "XMLNamedNodeMap.h"

namespace acdk {
namespace xml {
namespace dom {


RNamedNodeMap  
XMLElement::getAttributes()
{
  return new XMLNamedNodeMap(_attributes);
}

RString 
XMLElement::getAttribute(IN(RString) name)
{
  RXMLAttr attr = _getAttr(name);
  if (attr == Nil)
    return "";
  return attr->getValue();
}
void 
XMLElement::setAttribute(IN(RString) name, IN(RString) value) THROWS1(RDOMException)
{
  RXMLAttr attr = _getAttr(name);
  if (attr != Nil)
  {
    attr->setValue(value);
    return;
  }
  _attributes->append(new XMLAttr(name, true, value));
}

void 
XMLElement::removeAttribute(IN(RString) name) THROWS1(RDOMException)
{
  for (int i = 0; i < _attributes->length(); ++i)
  {
    if (_attributes[i]->getName()->equals(name) == true)
    {
      _attributes->remove(i);
      return;
    }
  }  
}

RAttr 
XMLElement::getAttributeNode(IN(RString) name)
{
  return &_getAttr(name);
}

RAttr 
XMLElement::setAttributeNode(IN(RAttr) newAttr) THROWS1(RDOMException)
{
   for (int i = 0; i < _attributes->length(); ++i)
  {
    if (_attributes[i]->getName()->equals(newAttr->getName()) == true)
    {
      _attributes[i] = RXMLAttr(newAttr);
      return newAttr;
    }
  }
  _attributes->append(RXMLAttr(newAttr));
  return newAttr;
}

RAttr 
XMLElement::removeAttributeNode(IN(RAttr) oldAttr) THROWS1(RDOMException)
{
  for (int i = 0; i < _attributes->length(); ++i)
  {
    if (_attributes[i]->getName()->equals(oldAttr->getName()) == true)
    {
      RXMLAttr sicattr = _attributes[i];
      _attributes->remove(i);
      return &sicattr;
    }
  }  
  return Nil;
}

RNodeList 
XMLElement::getElementsByTagName(IN(RString) name)
{
  return Nil;
}

//virtual 
void 
XMLElement::normalize()
{
}

RXMLAttr 
XMLElement::_getAttr(IN(RString) name)
{
  for (int i = 0; i < _attributes->length(); ++i)
  {
    if (_attributes[i]->getName()->equals(name) == true)
      return _attributes[i];
  }
  return Nil;
}

int 
XMLElement::attributeCount()
{
  return _attributes->length();
}


RAttr 
XMLElement::attribute(int idx)
{
  return (RAttr)_attributes[idx];
}

} // namespace dom
} // namespace xml
} // namespace acdk


