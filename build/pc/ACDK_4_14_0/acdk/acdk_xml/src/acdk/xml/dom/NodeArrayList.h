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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/NodeArrayList.h,v 1.3 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_dom_NodeArrayList_h
#define acdk_xml_dom_NodeArrayList_h

#include "dom.h"
#include <org/w3c/dom/NodeList.h>

namespace acdk {
namespace xml {
namespace dom {

using namespace acdk::lang;
using namespace org::w3c::dom;

USING_CLASS(::org::w3c::dom::, Node);

ACDK_DECL_CLASS(NodeArrayList);

/** 
  API: org.w3c.dom<br>
  @author Roger Rene Kommer
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:36 $
*/
class ACDK_XML_PUBLIC NodeArrayList
: extends acdk::lang::Object
, implements org::w3c::dom::NodeList
{
  ACDK_WITH_METAINFO(NodeArrayList)
protected:
  
  RNodeArray _childs;
public: 
  NodeArrayList()
  : _childs(new ::org::w3c::dom::NodeArray(0))
  {
  }
  RNode item(int index) 
  {
    if (index < 0 || index >= _childs->length())
      return Nil;
    return _childs[index];
  }
  virtual int getLength() 
  {
    return _childs->length();
  }
  void append(IN(RNode) xmln) { _childs->append(xmln); }
};

} // namespace dom
} // namespace xml
} // namespace acdk

#endif //acdk_xml_dom_NodeArrayList_h
