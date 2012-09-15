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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLCDATASection.h,v 1.8 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLCDATASection_h
#define acdk_xml_dom_XMLCDATASection_h
#include "dom.h"

#include "XMLText.h"

#include <org/w3c/dom/CDATASection.h>

namespace acdk {
namespace xml {
namespace dom {



using namespace org::w3c::dom;

ACDK_DECL_CLASS(XMLCDATASection);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLCDATASection
: extends XMLText
, implements org::w3c::dom::CDATASection
{
  ACDK_WITH_METAINFO(XMLCDATASection)
public:
  XMLCDATASection(IN(RString) text)
  : XMLText(text, org::w3c::dom::CDATA_SECTION_NODE)
  {
  }
  foreign RString toXML() { return XMLNode::toXML(); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLCDATASection_h
