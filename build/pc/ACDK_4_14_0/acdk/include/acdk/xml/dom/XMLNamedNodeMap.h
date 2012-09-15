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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLNamedNodeMap.h,v 1.5 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLNamedNodeMap_h
#define acdk_xml_dom_XMLNamedNodeMap_h
#include "dom.h"

#include "XMLNode.h"
#include "XMLAttr.h"
#include <org/w3c/dom/Element.h>
#include <org/w3c/dom/NamedNodeMap.h>

namespace acdk {
namespace xml {
namespace dom {



//using namespace org::w3c::dom;
using org::w3c::dom::RDOMException;
USING_CLASS(org::w3c::dom::, Attr);

ACDK_DECL_CLASS(XMLNamedNodeMap);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLNamedNodeMap
: extends acdk::lang::Object
, implements org::w3c::dom::NamedNodeMap
{
  ACDK_WITH_METAINFO(XMLNamedNodeMap)
protected:
  RXMLAttrArray _attributes;
public:
  XMLNamedNodeMap(IN(RXMLAttrArray) attrs)
  : _attributes(attrs)
  {
  }
  virtual RNode getNamedItem(IN(acdk::lang::RString) name)
  {
    int attrl = _attributes->length();
    for (int i = 0; i < attrl; ++i)
    {
      if (name->equals(_attributes[i]->getName()) == true)
        return &_attributes[i];
    }
    return Nil;
  }
  virtual RNode setNamedItem(IN(RNode) arg) THROWS1(RDOMException)
  {
    if (instanceof(arg, Attr) == false)
      THROW2(DOMException, HIERARCHY_REQUEST_ERR, "wrong note type");
    if (instanceof(arg, XMLAttr) == false)
      THROW2(DOMException, WRONG_DOCUMENT_ERR, "wrong note type");

    int attrl = _attributes->length();
    RString nn = arg->getNodeName();
    for (int i = 0; i < attrl; ++i)
    {
      if (nn->equals(_attributes[i]->getName()) == true)
      {
        _attributes[i] = (RXMLAttr) arg;
        return arg;
      }
    }
    _attributes->append((RXMLAttr)arg);
    return arg;
  }
  virtual RNode removeNamedItem(IN(RString) name) THROWS1(RDOMException)
  {
     int attrl = _attributes->length();
    for (int i = 0; i < attrl; ++i)
    {
      if (name->equals(_attributes[i]->getName()) == true)
      {
        RNode ret = &_attributes[i];
        _attributes->remove(i);
        return ret;
      }
    }
    THROW2(DOMException, NOT_FOUND_ERR, "Element '" + name + "' not found");
    return Nil;
  }
  virtual RNode item(int index)
  {
    if (index < 0 || index >= _attributes->length())
      return Nil;
    return &_attributes[index];
  }
  virtual int getLength() { return _attributes->length(); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLNamedNodeMap_h
