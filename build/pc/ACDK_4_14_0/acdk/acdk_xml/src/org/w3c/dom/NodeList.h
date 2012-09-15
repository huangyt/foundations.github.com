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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/NodeList.h,v 1.12 2005/02/05 10:45:37 kommer Exp $

/*
  Documentation:
  Copyright (c) 2004 World Wide Web Consortium,
 
  (Massachusetts Institute of Technology, European Research Consortium for
  Informatics and Mathematics, Keio University). All Rights Reserved. This
  work is distributed under the W3C(r) Software License [1] in the hope that
  it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 
  [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
*/

#ifndef org_w3c_dom_NodeList_h
#define org_w3c_dom_NodeList_h

#include "dom.h"
#include <acdk/util/Iterator.h>
#include <acdk/util/NoSuchElementException.h>

namespace org {
namespace w3c {
namespace dom {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(NodeList);

/**
 * The <code>NodeList</code> interface provides the abstraction of an ordered 
 * collection of nodes, without defining or constraining how this collection 
 * is implemented. <code>NodeList</code> objects in the DOM are live.
 * <p>The items in the <code>NodeList</code> are accessible via an integral 
 * index, starting from 0.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407'>Document Object Model (DOM) Level 3 Core Specification</a>.
 
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_ORG_XML_PUBLIC NodeList
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(NodeList)
public: 
  /**
     * Returns the <code>index</code>th item in the collection. If 
     * <code>index</code> is greater than or equal to the number of nodes in 
     * the list, this returns <code>null</code>.
     * @param index Index into the collection.
     * @return The node at the <code>index</code>th position in the 
     *   <code>NodeList</code>, or <code>null</code> if that is not a valid 
     *   index.
     */
  virtual RNode item(int index) = 0;
  /**
     * The number of nodes in the list. The range of valid child node indices 
     * is 0 to <code>length-1</code> inclusive.
     */
  virtual int getLength() = 0;
  /**
    API acdk extension
  */
  acdk::util::RIterator iterator();
};

ACDK_DECL_CLASS(NodeListIterator);

/** 
  internal implementation for NodeList::iterator() method
*/
class ACDK_ORG_XML_PUBLIC NodeListIterator
: extends acdk::lang::Object
, implements acdk::util::Iterator
{
  ACDK_WITH_METAINFO(NodeListIterator)
protected:
  RNodeList _list;
  int _idx;
public:
  NodeListIterator(IN(RNodeList) list)
    : _list(list)
    , _idx(-1)
  {
  }
  bool hasNext()
  {
    if (_idx + 1 <  _list->getLength())
      return true;
    return false;
  }
  acdk::lang::RObject next()
  {
    if (hasNext() == false)
      THROW0_FQ(acdk::util::, NoSuchElementException);
    return (acdk::lang::RObject)_list->item(++_idx);
  }
  acdk::lang::RObject element()
  {
    if (_idx >=  _list->getLength())
      THROW0_FQ(acdk::util::, NoSuchElementException);
    return (acdk::lang::RObject)_list->item(_idx);
  }
  void remove()
  {
    THROW0_FQ(acdk::lang::, UnsupportedOperationException);
  }
};

inline 
acdk::util::RIterator 
NodeList::iterator()
{
  return new NodeListIterator(this);
}

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_NodeList_h
