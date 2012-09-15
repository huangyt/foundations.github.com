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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ArrayList.cpp,v 1.18 2005/03/08 12:45:45 kommer Exp $



#include <acdk.h>
#include "ArrayList.h"
#include <acdk/lang/System.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/ObjectArrayImpl.h>

#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/IndexOutOfBoundsException.h>
#include "ListIterator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;  

const int ArrayList::DEFAULT_CAPACITY = 16;

ArrayList::ArrayList(int capacity)
: _size(0)
{
  _data = new (allocator()) ObjectArray(capacity);
}

ArrayList::ArrayList(IN(RCollection) other)
: _size(0)
{
  _data = new (allocator()) ObjectArray(other->size() + 10);
  addAll(other);
}

ArrayList::ArrayList(IN(RObjectArray) array, bool copy)
: _size(0)
{
  if (array == Nil)
     _data = new (allocator()) ObjectArray(ArrayList::DEFAULT_CAPACITY);
  else
  {
    _size = array->length();
    if (copy == false)
    {
      _data = array;
    }
    else
    {
       _data = new (allocator()) ObjectArray(_size);
       ObjectArrayBaseImpl::array_iterator si = array->begin();
       ObjectArrayBaseImpl::array_iterator se = array->end();
       ObjectArrayBaseImpl::array_iterator ti = array->end();
       for (; si != se; ++si, ++ti)
         *ti = *si;
    }
  }

}

//virtual 
ArrayList::~ArrayList()
{
}

//virtual 
void 
ArrayList::ensureCapacity(int mincap)
{
  _data->resize(mincap);  
}

//virtual 
bool 
ArrayList::add(IN(RObject) o)
{
  ensureCapacity(_size + 1);
  _data[_size++] = o;
  _modCount++;
  return true;
}

//virtual 
RObject 
ArrayList::remove(int index)
{
  if (index >= _size)
    THROW0(IndexOutOfBoundsException);
  RObject res = _data[index];
  if (index != --_size)
    System::arraycopy(_data, (index + 1), _data, index,  (_size - index));
  _modCount++;
  _data[_size] = Nil;
  return res;
}


//virtual 
bool 
ArrayList::remove(IN(RObject) o)
{
  int idx = indexOf(o);
  if (idx == -1)
    return false;
  remove(idx);
  return true;
}

//virtual 
void 
ArrayList::removeRange(int fromIndex, int toIndex) 
  
{
  if (fromIndex >= _size || toIndex > _size) 
    THROW0(IndexOutOfBoundsException);
  if (fromIndex > toIndex)
    THROW0(IllegalArgumentException);
  
  if (fromIndex != toIndex) {
    int dif = toIndex - fromIndex;
    System::arraycopy(_data, (fromIndex + dif), _data, fromIndex, (_size - fromIndex - dif));
    _modCount++;
    for (int i = (fromIndex + dif); i < _size; i++)
      _data[i] = Nil;
      _size -= dif;
  }
}


//virtual 
void 
ArrayList::add(int index, IN(RObject) obj) 
{
  if (index > _size)
  THROW0(IndexOutOfBoundsException);
  ensureCapacity(_size + 1);
  System::arraycopy(_data, index, _data, (index + 1), (_size - index));
  _data[index] = obj;
  _size++;
  _modCount++;
}


//virtual 
bool 
ArrayList::addAll(IN(RCollection) coll)
{
  int length = coll->size();
  if (length <= 0)
    return false;
  ensureCapacity(_size + length);
  _modCount++;
  RIterator it = coll->iterator();
  while (it->hasNext() == true)
    _data[_size++] = it->next();
  return true;
}

//virtual 
bool 
ArrayList::addAll(int index, IN(RCollection) coll) 
{
  if (index > _size)
      THROW0(IndexOutOfBoundsException);  
  int length = coll->size();
  if (length <= 0)
    return false;
  ensureCapacity(_size + length);
  System::arraycopy(_data, index, _data,  (index + length), (_size - index));
  _modCount++;
  _size += length;
  RIterator it = coll->iterator();
  while (it->hasNext() == true)
    _data[index++] = it->next();
  return true;
}


//virtual 
RObject 
ArrayList::clone(acdk::lang::sys::Allocator* alloc)
{
  RArrayList thisarray(const_cast<ArrayList*>(this));
  RCollection coll = (RCollection)thisarray;
  RArrayList narr = new (alloc)ArrayList(coll);
  return (RObject)narr;
}


//virtual 
int 
ArrayList::indexOf(IN(RObject) obj)
{
  for (int i = 0; i < _size; i++) {
    if (_isEqual(obj, _data[i]))
      return i;
  }
  return -1;
}

//virtual 
int 
ArrayList::lastIndexOf(IN(RObject) obj)
{
  for (int i = _size - 1; i >= 0; i--) {
    if (_isEqual(obj, _data[i]))
      return i;
  }
  return -1;
}

//virtual 
void 
ArrayList::clear()
{
  if (_size <= 0)
    return;
  _modCount++;
  _size = 0;
  for (int i = 0; i < _size; i++)
    _data[i] = Nil;
}

//virtual 
RObject 
ArrayList::set(int index, IN(RObject) obj) 
{
  if (index >= _size)
      THROW0(IndexOutOfBoundsException); 
  RObject res = _data[index];
  _modCount++;
  _data[index] = obj;
  return res;
}

//virtual 
RObjectArray 
ArrayList::toArray()
{
  RObjectArray erg = new ObjectArray(_size);
  System::arraycopy(_data, 0, erg, 0, _size);
  return erg;
}

//virtual 
RObjectArray 
ArrayList::toArray(IN(RObjectArray) oarray)
{
  oarray->resize(_size);
  System::arraycopy(_data, 0, oarray, 0, _size);
  return oarray;
}

//virtual 
void 
ArrayList::trimToSize()
{
  if (_size == _data->length())
    return;
  RObjectArray erg = new (allocator()) ObjectArray(_size);
  System::arraycopy(_data, 0, erg, 0, _size);
  _modCount++;
  _data = erg;
}


} // util
} // acdk
