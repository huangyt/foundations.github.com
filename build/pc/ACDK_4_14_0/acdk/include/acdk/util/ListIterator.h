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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ListIterator.h,v 1.9 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_ListIterator_h
#define acdk_util_ListIterator_h

#include <acdk.h>
#include "Iterator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(ListIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC ListIterator
: implements Iterator
{
  ACDK_WITH_METAINFO(ListIterator)
public :
  virtual void add(IN(RObject) o) = 0;
  virtual bool hasNext() = 0;
  virtual bool hasPrevious() = 0;
  virtual RObject next() = 0;
  virtual RObject element() = 0;
  virtual int nextIndex() = 0;
  virtual RObject previous()  = 0;
  virtual int previousIndex() = 0;
  virtual void remove() = 0;
  virtual void set(IN(RObject) o) = 0;
};


} // util
} // acdk

#endif //acdk_util_ListIterator_h

