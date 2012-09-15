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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLCharacterData.h,v 1.5 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLCharacterData_h
#define acdk_xml_libxmldom_LibXMLCharacterData_h

#include "LibXMLNode.h"


namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLCharacterData);

class ACDK_XML_PUBLIC LibXMLCharacterData
: extends LibXMLNode
, implements org::w3c::dom::CharacterData
{
  ACDK_WITH_METAINFO(LibXMLCharacterData)
public:
  foreign LibXMLCharacterData(xmlNodePtr np, bool ownsPtr = false)
    : LibXMLNode(np, ownsPtr == true ? XmlNodePtrHolder::xmlFreeNode : 0)
  {
  }
  RString getData() THROWS1(org::w3c::dom::RDOMException)
  {
    return getNodeValue();
  }

  void setData(IN(RString) data) THROWS1(org::w3c::dom::RDOMException)
  {
    setNodeValue(data);
  }

  int getLength()
  {
    return getData()->length();
  }

  RString subStringData(int offset, int count) THROWS1(org::w3c::dom::RDOMException)
  {
    return getData()->substr(offset, offset + count);
  }

  void appendData(IN(RString) arg) THROWS1(org::w3c::dom::RDOMException)
  {
    setData(getData() + arg);
  }

  void insertData(int offset, IN(RString) arg) THROWS1(org::w3c::dom::RDOMException)
  {
    RString data = getData();
    setData(data->substr(0, offset) + arg + data->substr(offset));
  }

  void deleteData(int offset, int count) THROWS1(org::w3c::dom::RDOMException)
  {
    RString data = getData();
    setData(data->substr(0, offset) + data->substr(offset + count));
  }

  void replaceData(int offset, int count, IN(RString) arg) THROWS1(org::w3c::dom::RDOMException)
  {
    RString data = getData();
    setData(data->substr(0, offset) + arg + data->substr(offset + count));
  }

  RString toString()
  {
    return SBSTR(getClass()->getName() << "[data=" << getData() << "]");
  }
  foreign RString toXML() { return LibXMLNode::toXML(); }
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLCharacterData_h
