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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLAttr.h,v 1.8 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLAttr_h
#define acdk_xml_dom_XMLAttr_h
#include "dom.h"

#include "XMLNode.h"
#include <org/w3c/dom/Attr.h>

namespace acdk {
namespace xml {
namespace dom {



using namespace org::w3c::dom;

ACDK_DECL_CLASS(XMLAttr);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLAttr
: extends XMLNode
, implements org::w3c::dom::Attr
{
  ACDK_WITH_METAINFO(XMLAttr)
protected:
  RString _name;
  bool _specified;
  RString _value;
public:
  XMLAttr(IN(RString) name, bool specified = false, IN(RString) value = "")
  : XMLNode(name, org::w3c::dom::ATTRIBUTE_NODE)
  , _name(name)
  , _specified(specified)
  , _value(value)
  {
  }
  RString toString() { return "XMLAttr: " + _name + "=\"" + _value + "\""; }
  virtual RString getName() { return _name; }
  virtual bool getSpecified() { return _specified; }
  virtual RString getValue() { return _value; }
  virtual void setValue(IN(RString) value) { _value = value; }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLAttr_h
