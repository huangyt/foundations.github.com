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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/NodeIteratorWalker.h,v 1.2 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_NodeIteratorWalker_h
#define acdk_xml_dom_NodeIteratorWalker_h

#include "dom.h"

#include <org/w3c/dom/traversal/TreeWalker.h>
#include <org/w3c/dom/traversal/NodeIterator.h>

namespace acdk {
namespace xml {
namespace dom {


ACDK_DECL_CLASS(NodeIteratorWalker);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.2 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC NodeIteratorWalker
: extends acdk::lang::Object
, implements org::w3c::dom::traversal::TreeWalker
, implements org::w3c::dom::traversal::NodeIterator
{
  ACDK_WITH_METAINFO(NodeIteratorWalker)
private:
  org::w3c::dom::RNode _rootNode;
  org::w3c::dom::RNode _curNode;
  int _showFlags;
  org::w3c::dom::traversal::RNodeFilter _nodeFilter;
  bool _expandEntityReference;
  bool _walkMode;
public:
  NodeIteratorWalker(IN(org::w3c::dom::RNode) rootNode, IN(org::w3c::dom::traversal::RNodeFilter) nodeFilter, int showFlags,  bool expandEntity, bool walkMode);
  org::w3c::dom::RNode getRoot()
  {
    return _rootNode;
  }

  int getWhatToShow()
  {
    return _showFlags;
  }

  org::w3c::dom::traversal::RNodeFilter getFilter()
  {
    return _nodeFilter;
  }

  bool getExpandEntityReferences()
  {
    return _expandEntityReference;
  }

  org::w3c::dom::RNode nextNode() THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNode previousNode() THROWS1(org::w3c::dom::RDOMException);

  org::w3c::dom::RNode getCurrentNode()
  {
    return _curNode;
  }
  void setCurrentNode(IN(org::w3c::dom::RNode) curNode) THROWS1(org::w3c::dom::RDOMException);
  org::w3c::dom::RNode parentNode();
  org::w3c::dom::RNode firstChild();
  org::w3c::dom::RNode lastChild();
  org::w3c::dom::RNode previousSibling();
  org::w3c::dom::RNode nextSibling();
  void detach()
  {
    _rootNode = Nil;
  }
private:
  bool _noSkip(IN(org::w3c::dom::RNode) node);
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_NodeIteratorWalker_h
