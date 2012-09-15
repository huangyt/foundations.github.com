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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/EmptyCollectionIterator.h,v 1.4 2005/02/05 10:45:05 kommer Exp $

#ifndef acdk_util_EmptyCollectionIterator_h
#define acdk_util_EmptyCollectionIterator_h

#include <acdk.h>
#include "Iterator.h"
#include "NoSuchElementException.h"

namespace acdk {
namespace util {


ACDK_DECL_CLASS(EmptyCollectionIterator);

/**
  EmptyCollectionIterator implements an iterator for a collection with no elements
  It behaves the same, like a collection with no element

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:05 $
  
*/
class ACDK_CORE_PUBLIC EmptyCollectionIterator
: extends acdk::lang::Object
, implements Iterator
{
  ACDK_WITH_METAINFO(EmptyCollectionIterator)
public :
  EmptyCollectionIterator()
  {
  }
  virtual bool hasNext() { return false; }
  virtual RObject next() 
  { 
    THROW0(NoSuchElementException);
    return Nil;
  }
  virtual RObject element() { THROW0(NoSuchElementException); return Nil; }
  virtual void remove() { THROW0(UnsupportedOperationException); }
};

} // util
} // acdk

#endif //acdk_util_EmptyCollectionIterator_h

