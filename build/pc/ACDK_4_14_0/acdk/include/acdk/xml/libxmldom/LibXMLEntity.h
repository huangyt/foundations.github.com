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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/libxmldom/LibXMLEntity.h,v 1.4 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_libxmldom_LibXMLEntity_h
#define acdk_xml_libxmldom_LibXMLEntity_h

#include <org/w3c/dom/Entity.h>
#include "LibXMLNode.h"


namespace acdk {
namespace xml {
namespace libxmldom {


ACDK_DECL_CLASS(LibXMLEntity);

class ACDK_XML_PUBLIC LibXMLEntity
: extends LibXMLNode
, implements org::w3c::dom::Entity
{
  ACDK_WITH_METAINFO(LibXMLEntity)
public:
  foreign LibXMLEntity(xmlNodePtr np, bool ownsPtr = false)
  : LibXMLNode(np, ownsPtr == true ? XmlNodePtrHolder::xmlFreeNode : 0)
  {
  }
  RString getPublicId();
  RString getSystemId();
  RString getNotationName();
  RString getInputEncoding();
  RString getXmlEncoding();
  RString getXmlVersion();
  RString toString();
  foreign RString toXML() { return LibXMLNode::toXML(); }
};

} // namespace libxmldom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_libxmldom_LibXMLEntity_h
