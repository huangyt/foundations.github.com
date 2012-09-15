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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TListIterator.h,v 1.4 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TListIterator_h
#define acdk_util_TListIterator_h

#include <acdk.h>
#include "TIterator.h"

namespace acdk {
namespace util {


/**
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class TListIterator
: implements TIterator<T>
{
  
public :
  typedef T RValueType;
  typedef TListIterator<RValueType> ListIteratorType;
  typedef InterfaceHolder<ListIteratorType> RListIteratorType;
  typedef RListIteratorType RefType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  

  virtual void add(IN(RValueType) o) = 0;
  virtual bool hasNext() = 0;
  virtual bool hasPrevious() = 0;
  virtual RValueType next() = 0;
  virtual RValueType element() = 0;
  virtual int nextIndex() = 0;
  virtual RValueType previous()  = 0;
  virtual int previousIndex() = 0;
  virtual void remove() = 0;
  virtual void set(IN(RValueType) o) = 0;
};


} // util
} // acdk

#endif //acdk_util_TListIterator_h

