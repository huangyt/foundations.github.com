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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLNodeList.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLNodeList_h
#define acdk_xml_libxmldom_LibXMLNodeList_h

#include "LibXMLNode.h"

namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLNodeList);

class ACDK_XML_PUBLIC LibXMLNodeList
: extends acdk::lang::Object
, implements org::w3c::dom::NodeList
{
  ACDK_WITH_METAINFO(LibXMLNodeList)
private:
  foreign xmlNodePtr _nodePtr;
public:
  foreign LibXMLNodeList(xmlNodePtr np)
    : _nodePtr(np)
  {
  }
  org::w3c::dom::RNode item(int index);
  int getLength();
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk



#endif //acdk_xml_libxmldom_LibXMLNodeList_h
