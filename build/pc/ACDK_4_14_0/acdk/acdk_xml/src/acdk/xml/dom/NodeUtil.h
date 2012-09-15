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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/NodeUtil.h,v 1.4 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_NodeUtil_h
#define acdk_xml_dom_NodeUtil_h

#include "../Config.h"

#include <org/w3c/dom/Node.h>
#include <org/w3c/dom/Document.h>

namespace acdk {
namespace xml {
namespace dom {





ACDK_DECL_CLASS(NodeUtil);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC NodeUtil
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(NodeUtil)
public:
  /**
    uses a (very small) subset of XPath to address nodes
    <code>
    "/"           Address the root XML Element node(s)
    "/main/sub"   Address Element via absolut path 
    "sub"         Address Element relativ to current node
    "sub/text()"  Address the Text below sub
    </code>
    
  */
  static org::w3c::dom::RNodeList selectNodes(IN(org::w3c::dom::RNode) node, IN(RString) xpath);
  static org::w3c::dom::RNodeList selectNodes(IN(org::w3c::dom::RDocument) doc, IN(RString) xpath)
  {
    return selectNodes(&doc->getDocumentElement(), xpath);
  }
  static org::w3c::dom::RNode selectNode(IN(org::w3c::dom::RNode) node, IN(RString) xpath);
  static org::w3c::dom::RNode selectNode(IN(org::w3c::dom::RDocument) doc, IN(RString) xpath)
  {
    return selectNode(&doc->getDocumentElement(), xpath);
  }
  static RString toXml(IN(org::w3c::dom::RNode) node);
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_NodeUtil_h
