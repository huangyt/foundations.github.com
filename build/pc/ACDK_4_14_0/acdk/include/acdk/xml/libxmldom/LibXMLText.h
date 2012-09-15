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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLText.h,v 1.3 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLText_h
#define acdk_xml_libxmldom_LibXMLText_h

#include "LibXMLCharacterData.h"


namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLText);

class ACDK_XML_PUBLIC LibXMLText
: extends LibXMLCharacterData
, implements org::w3c::dom::Text
{
  ACDK_WITH_METAINFO(LibXMLText)
public:
  foreign LibXMLText(xmlNodePtr np, bool ownsPtr = false)
  : LibXMLCharacterData(np, ownsPtr)
  {
  }
  org::w3c::dom::RText splitText(int offset) THROWS1(org::w3c::dom::RDOMException);
  bool isElementContentWhitespace();
  RString getWholeText();
  org::w3c::dom::RText replaceWholeText(RString content) THROWS1(org::w3c::dom::RDOMException);
  foreign RString toXML() { return LibXMLNode::toXML(); }
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLText_h
