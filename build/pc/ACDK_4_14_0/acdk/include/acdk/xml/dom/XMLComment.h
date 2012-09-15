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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/XMLComment.h,v 1.9 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_XMLComment_h
#define acdk_xml_dom_XMLComment_h
#include "dom.h"

#include "XMLCharacterData.h"

#include <org/w3c/dom/Comment.h>

namespace acdk {
namespace xml {
namespace dom {



using namespace org::w3c::dom;

ACDK_DECL_CLASS(XMLComment);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC XMLComment
: extends XMLCharacterData
, implements org::w3c::dom::Comment
{
  ACDK_WITH_METAINFO(XMLComment)
public:
  XMLComment(IN(RString) text)
  : XMLCharacterData(text, COMMENT_NODE)
  {
  }
  RString toString() { return "XMLComment: " + _data->toString(); }
  foreign RString toXML() { return XMLNode::toXML(); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_XMLComment_h
