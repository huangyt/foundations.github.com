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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Vector.h,v 1.15 2005/04/09 19:26:58 kommer Exp $

#ifndef acdk_util_Vector_h
#define acdk_util_Vector_h

#include <acdk.h>

#include "AbstractList.h"

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

using namespace acdk::io;

ACDK_DECL_CLASS(Vector);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:58 $
  
*/
class ACDK_CORE_PUBLIC Vector 
: extends AbstractList,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Vector)
protected:
  int _elementCount;
  int _capacityIncrement;
  int _modCount;
  RObjectArray _elementData;
public:
  static RObject create_instance() { return new Vector(0, 0); }
  Vector(int initcapac = 10, int capacInc = 0);
  Vector(IN(RCollection) col);
  virtual ~Vector();
  
  virtual void ensureCapacity(int minCapacity);
  virtual void setSize(int newSize);

  virtual int capacity() 
  {
    return _elementData->length();
  }
  foreign virtual int size()
  {
    return _elementCount;
  }
  foreign virtual bool isEmpty()
  {
    SYNCTHIS();
    return _elementCount == 0;
  }
  foreign virtual int indexOf(IN(RObject) object, int index);
  foreign virtual int indexOf(IN(RObject) o) { return AbstractList::indexOf(o); }
  foreign virtual bool contains(IN(RObject) object)
  {
    SYNCTHIS();
    return indexOf(object, 0) != -1;
  }
  foreign virtual int lastIndexOf(IN(RObject) o) { return AbstractList::lastIndexOf(o); }
  foreign virtual int lastIndexOf(IN(RObject) object, int index);
  
  foreign virtual RObject elementAt(int index) 
  {
    SYNCTHIS();
    if (index >= _elementCount) 
      THROW1(ArrayIndexOutOfBoundsException, index);
    return _elementData[index];
  }
  virtual RObject firstElement() 
  {
    SYNCTHIS();
    if (isEmpty() == true) 
      THROW0(NoSuchElementException);
    return elementAt(0);
  }
  virtual RObject lastElement() 
  {
    SYNCTHIS();
    if (isEmpty() == true) 
      THROW0(NoSuchElementException);
    return elementAt(_elementCount - 1);
  }
  virtual void setElementAt(IN(RObject) obj, int index) THROWS1( RArrayIndexOutOfBoundsException);
  virtual RObject set(int index, IN(RObject) obj) THROWS1( RArrayIndexOutOfBoundsException);
  virtual void removeElementAt(int index) THROWS1( RArrayIndexOutOfBoundsException);
  virtual void insertElementAt(IN(RObject) obj, int index) THROWS1( RArrayIndexOutOfBoundsException);
  virtual void addElement(IN(RObject) obj);
  virtual bool removeElement(IN(RObject) obj);
  virtual void removeAllElements();
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(acdk::lang::sys::Allocator* alloc);
  foreign virtual RObjectArray toArray();
  foreign virtual RObjectArray toArray(IN(RObjectArray)  a);
  foreign virtual RObject get(int index) 
  { 
    return elementAt(index); 
  }
  foreign virtual bool remove(IN(RObject) o) 
  {
    return removeElement(o);
  }
  foreign virtual bool add(IN(RObject) o) 
  { 
    SYNCTHIS();
    add(size(), o); 
    return true;
  }
  foreign virtual void add(int index, IN(RObject) element) 
  {
    SYNCTHIS();
    _modCount++;
    insertElementAt(element, index);
  }
  foreign virtual RObject remove(int index);
  foreign virtual void clear() 
  {
    removeAllElements();
  }
  foreign virtual bool containsAll(IN(RCollection) coll);
  foreign virtual bool addAll(IN(RCollection) coll);
  foreign virtual bool addAll(int index, IN(RCollection) coll);
  foreign virtual bool removeAll(IN(RCollection) coll);
  foreign virtual bool retainAll(IN(RCollection) c);
  foreign virtual RString toString();

  void trimToSize();
  void copyInto(IN(RObjectArray) array);
};


} // util
} // acdk


#endif //acdk_util_Vector_h

