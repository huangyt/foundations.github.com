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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TArrayList.h,v 1.6 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TArrayList_h
#define acdk_util_TArrayList_h

#include <acdk.h>
#include "TAbstractList.h"
#include <acdk/lang/Cloneable.h>
#include <acdk/lang/System.h>
#include <acdk/io/Serializable.h>


#include <acdk/lang/IndexOutOfBoundsException.h>

namespace acdk {
namespace util {

#define USE_TARRAYLIST(ClassName) \
  typedef ::acdk::util::TArrayList<R##ClassName> ClassName##ArrayList; \
  typedef RefHolder<ClassName##ArrayList> R##ClassName##ArrayList



//ACDK_DECL_CLASS(ArrayList);

/**
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:06 $
  isfinal
*/
template <class T>
class TArrayList
: extends TAbstractList<T>
, implements acdk::lang::Cloneable
, implements acdk::io::Serializable
{
public:
  typedef T RValueType;
  typedef T RElementType;
  typedef typename RElementType::Type ElementType;
  typedef TArrayList<RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef ThisType ArrayListType;
  typedef RThisType RArrayListType;

  typedef RThisType RefType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef TListIterator<RValueType> ListIteratorType;
  typedef typename ListIteratorType::RefType RListIteratorType;
  
  typedef TAbstractList<RValueType> AbstractListType;
  typedef typename AbstractListType::RefType RAbstractListType;
  
  typedef ObjectArrayImpl<RValueType> ValueTypeArrayType;
  typedef RObjectArrayImpl<RValueType> RValueTypeArrayType;

protected:
  int _size;
  RValueTypeArrayType _data;
public:
  static RObject create_instance() { return new ArrayListType(); }
  TArrayList(int capacity = ArrayList::DEFAULT_CAPACITY)
  : _size(0)
  {
    _data = new (TArrayList<T>::allocator()) ValueTypeArrayType(capacity);
  }
  TArrayList(IN(RCollectionType) other)
  : _size(0)
  {
     _data = new (TArrayList<T>::allocator()) ValueTypeArrayType(other->size() + 10);
    addAll(other);
  }
  
  
  virtual void ensureCapacity(int mincap)
  {
    _data->ensureCapacity(mincap);  
  }
  foreign virtual bool add(IN(RValueType) o)
  {
    ensureCapacity(_size + 1);
    _data[_size++] = o;
    TArrayList<T>::_modCount++;
    return true;
  }
  foreign virtual bool containsAll(IN(RCollectionType) c)  { return AbstractListType::containsAll(c);  }
  
  foreign RValueType get(int index)
  {
    if (index >= _size)
      THROW0(IndexOutOfBoundsException);
    return _data[index];
  }
  foreign int size()
  {
    return _size;
  }
  foreign bool isEmpty() { return size() == 0; }
  foreign RValueType remove(int index)
  {
    if (index >= _size)
    THROW0(IndexOutOfBoundsException);
    RValueType res = _data[index];
    if (index != --_size)
      ::acdk::lang::System::arraycopy(_data, (index + 1), _data, index,  (_size - index));
    TArrayList<T>::_modCount++;
    _data[_size] = Nil;
    return res;
  }
  foreign bool removeAll(IN(RCollectionType) c) { return AbstractListType::removeAll(c); }
  foreign bool retainAll(IN(RCollectionType) c) { return AbstractListType::retainAll(c); }
  foreign bool remove(IN(RValueType) o)
  {
    int idx = indexOf(o);
    if (idx == -1)
      return false;
    remove(idx);
    return true;
  }
  foreign void removeRange(int fromIndex, int toIndex)
  {
    if (fromIndex >= _size || toIndex > _size) 
      THROW0(IndexOutOfBoundsException);
    if (fromIndex > toIndex)
      THROW0(IllegalArgumentException);
    if (fromIndex != toIndex) 
    {
      int dif = toIndex - fromIndex;
      System::arraycopy(_data, (fromIndex + dif), _data, fromIndex, (_size - fromIndex - dif));
      TArrayList<T>::_modCount++;
      for (int i = (fromIndex + dif); i < _size; i++)
        _data[i] = Nil;
        _size -= dif;
    }
  }
  foreign void add(int index, IN(RValueType) obj)
  {
    if (index > _size)
      THROW0(IndexOutOfBoundsException);
    ensureCapacity(_size + 1);
    System::arraycopy(_data, index, _data, (index + 1), (_size - index));
    _data[index] = obj;
    _size++;
    TArrayList<T>::_modCount++;
  }
  foreign bool addAll(IN(RCollectionType) coll)
  {
    int length = coll->size();
    if (length <= 0)
      return false;
    ensureCapacity(_size + length);
    TArrayList<T>::_modCount++;
    RIteratorType it = coll->iterator();
    while (it->hasNext() == true)
      _data[_size++] = it->next();
    return true;
  }
  foreign bool addAll(int index, IN(RCollectionType) coll)
  {
    if (index > _size)
      THROW0(IndexOutOfBoundsException);  
    int length = coll->size();
    if (length <= 0)
      return false;
    ensureCapacity(_size + length);
    ::acdk::lang::System::arraycopy(_data, index, _data,  (index + length), (_size - index));
    TArrayList<T>::_modCount++;
    _size += length;
    RIteratorType it = coll->iterator();
    while (it->hasNext() == true)
      _data[index++] = it->next();
    return true;
  }
  foreign RObject clone() { return clone(TArrayList<T>::allocator()); }
  foreign RObject clone(sys::Allocator* alc)
  {
    RArrayListType thisarray(this);
    RCollectionType coll = (RCollectionType)thisarray;
    RArrayListType narr = new (alc) ArrayListType(coll);
    return (RObject)narr;
  }
  
  foreign bool contains(IN(RValueType) obj)
  {
    return (indexOf(obj) != -1);
  }
  foreign int indexOf(IN(RValueType) obj)
  {
    for (int i = 0; i < _size; i++) 
    {
      if (_isEqual(obj, _data[i]))
        return i;
    }
    return -1;
  }
  foreign int lastIndexOf(IN(RValueType) obj)
  {
    for (int i = _size - 1; i >= 0; i--) 
    {
      if (_isEqual(obj, _data[i]))
      return i;
    }
    return -1;
  }
  foreign RListIteratorType listIterator(int index = 0) 
  {
    return TAbstractList<RValueType> ::listIterator(index); 
  }
  foreign void clear()
  {
    if (_size <= 0)
      return;
    TArrayList<T>::_modCount++;
    _size = 0;
    for (int i = 0; i < _size; i++)
      _data[i] = Nil;
  }
  foreign RValueType set(int index, IN(RValueType) obj)
  {
    if (index >= _size)
      THROW0(IndexOutOfBoundsException); 
    RValueType res = _data[index];
    TArrayList<T>::_modCount++;
    _data[index] = obj;
    return res;
  }
  foreign RValueTypeArrayType toArray()
  {
    RValueTypeArrayType erg = new ValueTypeArrayType(_size);
    System::arraycopy(_data, 0, erg, 0, _size);
    return erg;
  }
  foreign RValueTypeArrayType toArray(IN(RValueTypeArrayType) oarray)
  {
    oarray->resize(_size);
    System::arraycopy(_data, 0, oarray, 0, _size);
    return oarray;
  }

  foreign void trimToSize()
  {
    if (_size == _data->length())
      return;
    RObjectArray erg = new (ThisType::allocator()) ObjectArray(_size);
    System::arraycopy(_data, 0, erg, 0, _size);
    TArrayList<T>::_modCount++;
    _data = erg;
  }
private:
  inline static bool _isEqual(IN(RValueType) o1, IN(RValueType) o2)
  {
    return o1 == Nil ? o2 == Nil : o1->equals(o2);
  }

};



} // util
} // acdk

#endif // acdk_util_TArrayList_h

