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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLCharacterData.h,v 1.10 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLCharacterData_h
#define acdk_xml_dom_XMLCharacterData_h
#include "dom.h"

#include "XMLNode.h"
#include <org/w3c/dom/CharacterData.h>

namespace acdk {
namespace xml {
namespace dom {


using namespace org::w3c::dom;

ACDK_DECL_CLASS(XMLCharacterData);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLCharacterData
: extends XMLNode
, implements org::w3c::dom::CharacterData
{
  ACDK_WITH_METAINFO(XMLCharacterData)
protected:
  RStringBuffer _data;
public:
  XMLCharacterData(IN(RString) str, short type)
  : XMLNode("", type)
  , _data(new StringBuffer(str))
  {
  }
  RString toString() 
  { 
    return "XMLCharacterData: \"" + _data->toString() + "\""; 
  }
  virtual RString getNodeValue() THROWS1(org::w3c::dom::RDOMException) { return getData(); }

  virtual RString getData() THROWS1(org::w3c::dom::RDOMException) 
  {
    return _data->toString();
  }
  virtual void setData(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
  {
    _data->set(s);
    _value = _data->toString();
  }
  virtual int getLength() 
  {
    return _data->length();
  }
  virtual RString subStringData(int start, int count) THROWS1(org::w3c::dom::RDOMException)
  {
    return _data->toString()->substr(start, count);
  }
  virtual void appendData(IN(RString) s) THROWS1(org::w3c::dom::RDOMException)
  {
    _data->append(s);
    _value = _data->toString();
  }
  virtual void insertData(int i, IN(RString) s) THROWS1(org::w3c::dom::RDOMException) 
  {
    _data->insert(i, s);
    _value = _data->toString();
  }
  virtual void deleteData(int start, int length) THROWS1(org::w3c::dom::RDOMException) 
  {
    _data->deleteRegion(start, length);
    _value = _data->toString();
  }
  virtual void replaceData(int start, int length, IN(RString) s) THROWS1(org::w3c::dom::RDOMException) 
  {
    _data->replace(start, length, s);
    _value = _data->toString();
  }
  foreign RString toXML() { return XMLNode::toXML(); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLCharacterData_h
