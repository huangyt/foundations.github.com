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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractSequentialList.h,v 1.13 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractSequentialList_h
#define acdk_util_AbstractSequentialList_h

#include "AbstractList.h"
#include "ListIterator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(AbstractSequentialList);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC AbstractSequentialList 
: extends AbstractList
{
  ACDK_WITH_METAINFO(AbstractSequentialList)
public:

  ~AbstractSequentialList() { }
  
  virtual RListIterator listIterator(int index = 0) = 0;

  virtual void add(int index, IN(RObject) o) 
  {
    RListIterator i = listIterator(index);
    i->add(o);
  }
  virtual bool add(IN(RObject) o) { return AbstractList::add(o); }
  virtual bool addAll(int index, IN(RCollection) c);
  virtual bool addAll(IN(RCollection) c) { return AbstractList::addAll(c); }
  virtual RObject get(int index);
  virtual RIterator iterator() { return (RIterator)listIterator(); }
  virtual RObject remove(int index);
  virtual bool remove(IN(RObject) obj) { return AbstractList::remove(obj); }
  virtual RObject set(int index, IN(RObject) o);

};


} // util
} // acdk

#endif //acdk_util_AbstractSequentialList_h

