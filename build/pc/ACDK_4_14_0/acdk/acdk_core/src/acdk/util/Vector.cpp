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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Vector.cpp,v 1.12 2005/03/08 12:45:46 kommer Exp $


#include <acdk.h>

#include "Vector.h"
#include <acdk/lang/System.h>
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include <acdk/lang/IndexOutOfBoundsException.h>

namespace acdk {
namespace util {

//using namespace std;

/*
ACDK_DECL_CLASS(VectorIterator);

class VectorIterator 
: public acdk::lang::Object,
  public Iterator
{
private:
  RVector _vector;
  mutable size_t _curPos;
public:
  VectorIterator(RVector vec)
  : Object(),
    _vector(vec),
    _curPos(0)
  {
  }
  virtual bool hasNext() 
  {
    return _curPos < _vector->size();
  }
  virtual RObject next()
  {
    if (hasNext() == false)
      THROW1(IndexOutOfBoundsException, "Iterator::next()");
    return _vector->elementAt(_curPos++);
  }
  virtual RObject element() 
  {
    if (hasNext() == false)
      THROW1(IndexOutOfBoundsException, "Iterator::element()");
    return _vector->elementAt(_curPos);
  }

  virtual void remove()
  {
    if (hasNext() == false)
      THROW1(IndexOutOfBoundsException, "Iterator::element()");
    _vector->remove(_curPos);
  }
};
*/



Vector::Vector(int initcapac, int capacInc)
: _elementCount(0),
  _capacityIncrement(capacInc),
  _modCount(0),
  _elementData(new (allocator()) ObjectArrayImpl<RObject>(initcapac))
{
}
 
Vector::Vector(IN(RCollection) col)
: _elementCount(0),
  _capacityIncrement(0),
  _modCount(0),
  _elementData()
{
  addAll(col);
}

//virtual 
Vector::~Vector()
{

}


void 
Vector::trimToSize() 
{
  if (_elementCount == _elementData->length()) 
    return;
  _elementData->resize(_elementCount);
}

//virtual 
RObject 
Vector::clone(sys::Allocator* alc) 
{
  RCollection rcoll(const_cast<Vector*>(this));
  SYNCTHIS();
  return new (alc) Vector(rcoll);
}

//virtual 
void 
Vector::ensureCapacity(int minCapacity)
{
  SYNCTHIS();
  _elementData->resize(minCapacity);
}

//virtual 
void 
Vector::setSize(int newSize) 
{
  SYNCTHIS();
  if (newSize == _elementCount)
    return;

  if (newSize < _elementCount) {
    _modCount++;
    _elementCount = newSize;
  }
  _elementData->resize(_elementCount);
}


//virtual 
int 
Vector::indexOf(IN(RObject) object, int index)
{
  SYNCTHIS();
  for (int i = index; i < _elementCount; i++)  {
    if (_elementData[i]->equals(object) == true) 
      return i;
  }
  return -1;
}

//virtual 
int 
Vector::lastIndexOf(IN(RObject) object, int index)
{
  SYNCTHIS();
  if (index == -1)
    index = _elementCount - 1;
  if (index > _elementCount - 1) 
    THROW1(ArrayIndexOutOfBoundsException, index);
  for (int i = index; i >= 0; i--) {
    if ((_elementData[i] == object) || _elementData[i]->equals(object)) 
      return i;
  }
  return -1;
}



//virtual 
void 
Vector::setElementAt(IN(RObject) obj, int index) 
  THROWS1( RArrayIndexOutOfBoundsException)
{
  SYNCTHIS();
  if ((index < 0) || (index >= _elementCount)) 
    THROW1(ArrayIndexOutOfBoundsException, index);
  _modCount++;
  _elementData[index] = obj;
}

//virtual 
RObject 
Vector::set(int index, IN(RObject) obj)  THROWS1( RArrayIndexOutOfBoundsException)
{
  SYNCTHIS();
  if (index >= _elementCount)
    THROW1(ArrayIndexOutOfBoundsException, index);
  _modCount++;
  RObject temp = _elementData[index];
  _elementData[index] = obj;
  return temp;
}


//virtual 
void 
Vector::removeElementAt(int index)  THROWS1( RArrayIndexOutOfBoundsException)
{
  SYNCTHIS();
  if (index >= _elementCount) 
    THROW1(ArrayIndexOutOfBoundsException, index);    
  _modCount++;
  _elementCount--;
  if (index < _elementCount) 
    System::arraycopy(_elementData, index + 1, _elementData, index,  _elementCount - index);
  _elementData[_elementCount] = Nil;
}

//virtual 
void 
Vector::insertElementAt(IN(RObject) obj, int index)  THROWS1( RArrayIndexOutOfBoundsException)
{
  SYNCTHIS();
  if ((index < 0) || (index > _elementCount)) 
    THROW1(ArrayIndexOutOfBoundsException, index);
  if (index == _elementCount - 1) {
      addElement(obj);
  } else {
    ensureCapacity(++_elementCount);
    _modCount++;
    System::arraycopy(_elementData, index, _elementData, index + 1,  (_elementCount - index) - 1);
    _elementData[index] = obj;
  }
}

//virtual 
void 
Vector::addElement(IN(RObject) obj)
{

  SYNCTHIS();
  ensureCapacity(_elementCount+1);
  _modCount++;
  _elementData[_elementCount++] = obj;
}


//virtual 
bool 
Vector::removeElement(IN(RObject) obj) 
{
  SYNCTHIS();
  int ix = indexOf(obj);
  if (ix == -1) 
    return false;
  removeElementAt(ix);
  return true;
}

//virtual 
void 
Vector::removeAllElements() 
{
  SYNCTHIS();
  if (size() == 0) 
    return;
  _modCount++;
  for (int i = 0; i < _elementData->length(); i++) {
    _elementData[i] = Nil;
  }
  _elementCount = 0;
}

//virtual
RObjectArray 
Vector::toArray()
{
  RObjectArray newArray = new ObjectArray(_elementCount);
  copyInto(newArray);
  return newArray;
}  


//virtual 
RObjectArray 
Vector::toArray(IN(RObjectArray) array) 
{
  SYNCTHIS();
  if (array->length() >= _elementCount) {
    copyInto(array);
    if (array->length() > _elementCount) {
      array[_elementCount] = Nil;
    }
    return array;
  } else {
    array->resize(_elementCount);
    copyInto(array);
    return array;
  }
}

//virtual 
RObject 
Vector::remove(int index) 
{
  SYNCTHIS();
  _modCount++;
  RObject temp = _elementData[index];
  removeElementAt(index);
  return temp;
}  

//virtual 
bool 
Vector::containsAll(IN(RCollection) coll)
{
  SYNCTHIS();
  for (RIterator it = coll->iterator(); it->hasNext(); ) {
    if (contains(it->next()) == false) 
      return false;
  }
  return true;
}

//virtual 
bool 
Vector::addAll(IN(RCollection) coll) 
{
  SYNCTHIS();
  _modCount++;
  ensureCapacity(size() + coll->size());
  for (RIterator it = coll->iterator(); it->hasNext();) {
    addElement(it->next());
  }
  return true; 
}

//virtual 
bool 
Vector::addAll(int index, IN(RCollection) coll) 
{
  SYNCTHIS();
  _modCount++;
  int idx = index;
  ensureCapacity(size() + coll->size());
  if (index < _elementCount) {
    System::arraycopy(_elementData, index, _elementData,  index + coll->size(), _elementCount - index);
  }
  for (RIterator it = coll->iterator(); it->hasNext();) {
    _elementData[idx++] = it->next();
  }
  _elementCount = size() + coll->size();
  return true; 
}

//virtual 
bool 
Vector::removeAll(IN(RCollection) coll) 
{
  SYNCTHIS();
  bool result = false;
  for (RIterator it = coll->iterator(); it->hasNext();) {
    result = remove(it->next()) || result;
  }
  if (result == true) 
    _modCount++;
  return result;
}

//virtual 
bool 
Vector::retainAll(IN(RCollection) coll) 
{
  SYNCTHIS();
  bool result = false;
  for (RIterator it = iterator(); it->hasNext();) {
    RObject temp = it->next();
    if (coll->contains(temp) == false) {
      it->remove();
      result = true;
    }
  }
  if (result) 
    _modCount++;
  return result;
}

//virtual 
RString 
Vector::toString()
{
  SYNCTHIS();
  if (_elementCount == 0)
    return "[]";
  StringBuffer buffer("[");
  for (int i = 0; i < _elementCount; i++) 
  {
    buffer.append(_elementData[i]->toString());
    buffer.append(", ");
  }
  int x = buffer.length();
  buffer.deleteRegion(x - 3, x);
  buffer.append("]");
  return buffer.toString();
}

void 
Vector::copyInto(IN(RObjectArray) array)
{
  SYNCTHIS();
  System::arraycopy(_elementData, 0, array, 0, _elementCount);
}


} // util
} // acdk
