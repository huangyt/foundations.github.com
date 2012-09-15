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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ArrayIterator.h,v 1.8 2005/03/03 09:41:25 kommer Exp $

#ifndef acdk_util_ArrayIterator_h
#define acdk_util_ArrayIterator_h

#include "ListIterator.h"
#include "NoSuchElementException.h"

namespace acdk {
namespace util {



ACDK_DECL_INTERFACE(ArrayIterator);

/**
  An Iterator for standard ObjectArrays.
  Different to standard Collection Iterator implementations
  this Iterator doesn't check for modifications of the underlying
  ObjectArray. If you have an ObjectArray, and an ArrayIterator
  you must not use modifing functions of ObjectArray, otherwise
  you receive undefined behavior.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/03/03 09:41:25 $
  
*/
class ACDK_CORE_PUBLIC ArrayIterator
: extends ::acdk::lang::Object
, implements ListIterator
{
  ACDK_WITH_METAINFO(ArrayIterator)
  RObjectArray _array;
  foreign ObjectArray::array_iterator _it;
public :
  ArrayIterator(IN(RObjectArray) arr)
  : _array(arr)
  {
    _it = _array->begin();
  }
  virtual void add(IN(RObject) o)
  {
    int idx = _it - _array->begin();
    _array->insert(idx, o);
    _it = _array->begin() + idx;
  }
  virtual bool hasNext()
  {
    return _it < _array->end();
  }

  virtual RObject next()
  {
    if (_it >= _array->end())
      THROW0(NoSuchElementException);
    RObject obj = *_it;
    ++_it;
    return obj;
  }
  bool hasPrevious()
  {
    return _it > _array->begin();
  }
  virtual RObject previous()
  {
    if (_it <= _array->begin())
      THROW0(NoSuchElementException);
    --_it;
    return *_it;
  }
  
  virtual RObject element() 
  {
    if (_it < _array->end())
      return *_it;
    THROW0(NoSuchElementException);
    return Nil;
  }
  
  virtual void remove() 
  {
    if (_it >= _array->end())
      THROW0(NoSuchElementException);
    int idx = _it - _array->begin();
    _array->remove(idx);
    _it = _array->begin() + idx;
  }
  virtual int nextIndex()
  {
    return _it - _array->begin();
  }
  virtual int previousIndex()
  {
    return _it - _array->begin() - 1;
  }
  virtual void set(IN(RObject) o) 
  {
    if (_it >= _array->end())
      THROW0(NoSuchElementException);
    _array[int(_it - _array->begin())] = o;
  }
};

} // util
} // acdk

#endif //acdk_util_ArrayIterator_h

