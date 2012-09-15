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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SingleObjectIterator.h,v 1.3 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_SingleObjectIterator_h
#define acdk_util_SingleObjectIterator_h

#include <acdk.h>
#include "Iterator.h"

namespace acdk {
namespace util {


ACDK_DECL_CLASS(SingleObjectIterator);

/**
  SingleObjectIterator wrapps a single object with a iterator.
  It behaves the same, like a collection with only one value

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
class ACDK_CORE_PUBLIC SingleObjectIterator
: extends acdk::lang::Object
, implements Iterator
{
  ACDK_WITH_METAINFO(SingleObjectIterator)
protected:
  RObject _object;
  bool _endOfCol;
public :
  SingleObjectIterator(IN(RObject) obj)
  : _object(obj)
  , _endOfCol(false)
  {
  }
  virtual bool hasNext() { return _endOfCol == false; }
  virtual RObject next() 
  { 
    if (_endOfCol == true)
      THROW0(NoSuchElementException);
    _endOfCol = true; 
    return _object; 
  }

  /***
    API: Extension, may not supported by some Containers 
    returns current Element, whithout forward to next element
  */
  virtual RObject element() { THROW0(UnsupportedOperationException); return Nil; }

  virtual void remove() { THROW0(UnsupportedOperationException); }

};

} // util
} // acdk

#endif //acdk_util_SingleObjectIterator_h

