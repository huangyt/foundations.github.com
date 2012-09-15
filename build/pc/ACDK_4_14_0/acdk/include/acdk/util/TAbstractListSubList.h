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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractListSubList.h,v 1.6 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TAbstractListSubList_h
#define acdk_util_TAbstractListSubList_h

#include "TAbstractList.h"
//#include "TListIterator.h"

#include "ConcurrentModificationException.h"
#include <acdk/lang/IndexOutOfBoundsException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

template <class T> class TAbstractListSubListListIterator;

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class T>
class TAbstractListSubList
: extends TAbstractList<T>
{
public:
  typedef T RValueType;
  typedef TAbstractListSubList<RValueType> AbstractListSubListType;
  typedef RefHolder<AbstractListSubListType> RAbstractListSubListType;
  typedef RAbstractListSubListType RefType;

  typedef TAbstractList<RValueType> AbstractListType;
  typedef typename AbstractListType::RefType RAbstractListType;

  typedef typename AbstractListType::RCollectionType RCollectionType;
  typedef typename AbstractListType::RListIteratorType RListIteratorType;

  typedef TAbstractListSubListListIterator<RValueType> AbstractListSubListListIteratorType;
  typedef typename AbstractListSubListListIteratorType::RefType RAbstractListSubListListIteratorType;
private:
  int _offset;
  int _size;
  int _modCount;
  RAbstractListType _list;
public:
  TAbstractListSubList(IN(RAbstractListType) list, int fromIndex, int toIndex)
  : _offset(fromIndex)
  , _size(toIndex - fromIndex)
  , _modCount(list->_modCount)
  {
  }
  

public:
  virtual int size()  
  {
    _checkMod();
    return _size;
  }
  
  foreign RListIteratorType listIterator(int index = 0)
  {
    _checkMod();
    _checkBoundsInclusive(index);
    return new AbstractListSubListListIteratorType(this, index, _offset);
  }
  foreign virtual RValueType set(int index, IN(RValueType) o)
  {
    _checkMod();
    _checkBoundsExclusive(index);
    RValueType reto = _list->set(index + _offset, o);
    _modCount = _list->_modCount;
    return reto;
  }
  foreign virtual RValueType get(int index)
  {
    _checkMod();
    _checkBoundsExclusive(index);
    return _list->get(index + _offset);
  }
  foreign virtual void add(int index, IN(RValueType) o)
  {
     _checkMod();
    _checkBoundsInclusive(index);
    _list->add(index + _offset, o);
    _modCount = _list->_modCount;
    _size++;
  }
  foreign virtual RValueType remove(int index)
  {
    _checkMod();
    _checkBoundsExclusive(index);
    RValueType o = _list->remove(index + _offset);
    _modCount = _list->_modCount;
    _size--;
    return o;
  }
  foreign virtual void removeRange(int fromIndex2, int toIndex2)
  {
    _checkMod();
    _checkBoundsExclusive(fromIndex2);
    _checkBoundsInclusive(toIndex2);
  
    // this call will catch the toIndex2 < fromIndex2 condition
    _list->removeRange(_offset + fromIndex2, _offset + toIndex2);
    _modCount = _list->_modCount;
    _size -= toIndex2 - fromIndex2;
  }
  foreign virtual bool addAll(int index, IN(RCollectionType) c)
  {
     _checkMod();
    _checkBoundsInclusive(index);
    int s = _list->size();
    bool result = _list->addAll(_offset + index, c);
    _modCount = _list->_modCount;
    _size += _list->size() - s;
    return result;
  }
private:
  void _checkMod()
  {
    if (_modCount != _list->_modCount) 
      THROW0(ConcurrentModificationException);
  }
  void _checkBoundsInclusive(int index)
  {
    if (index < 0 || index > _size) 
      THROW0(IndexOutOfBoundsException);
  }
  void _checkBoundsExclusive(int index)
  {
    if (index < 0 || index >= _size) 
      THROW0(IndexOutOfBoundsException);
  }  
  friend class TAbstractListSubListListIterator<T>;
};

template <class T>
typename TAbstractList<T>::RListType 
TAbstractList<T>::subList(int fromIndex, int toIndex)
{
  return new (TAbstractList<T>::allocator()) AbstractListSubListType(this, fromIndex, toIndex);
}

template <class T>
class TAbstractListSubListListIterator
: extends acdk::lang::Object
, implements TListIterator<T>
{
public:
  typedef T RValueType;
  typedef TAbstractListSubListListIterator<RValueType> AbstractListSubListListIteratorType;
  typedef RefHolder<AbstractListSubListListIteratorType> RAbstractListSubListListIteratorType;
  typedef RAbstractListSubListListIteratorType RefType;

  typedef TListIterator<RValueType> ListIteratorType;
  typedef typename ListIteratorType::RListIteratorType RListIteratorType;
  
  typedef TAbstractList<RValueType> AbstractListType;
  typedef typename AbstractListType::RefType RAbstractListType;

  typedef TAbstractListSubList<RValueType> AbstractListSubListType;
  typedef RefHolder<AbstractListSubListType> RAbstractListSubListType;

private:
  RListIteratorType _it;
  RAbstractListSubListType _list;
  int _position;
public:
  TAbstractListSubListListIterator(IN(RAbstractListSubListType) list, int index, int offset)
  : _it(list->listIterator(index + offset))
  , _list(list)
  , _position(index)
  {
  }
  foreign virtual bool hasNext()  
  {
    _list->_checkMod();
    return _position < _list->_size;
  }
  
  foreign virtual bool hasPrevious()
  {
    _list->_checkMod();
    return _position > 0;
  }
  
  foreign virtual RValueType next() 
  {
    if (_position < _list->_size) 
    {
      RValueType o = _it->next();
      _position++;
      return o;
    } else {
      THROW0(NoSuchElementException);
    }
    return Nil; //never reached
  }
  foreign virtual RValueType element()
  {
    if (_position < _list->_size) 
      return _it->next();
    THROW0(NoSuchElementException);
    return Nil; //never reached
  }
  foreign virtual RValueType previous() 
  {
    if (_position > 0) 
    {
      RValueType o = _it->previous();
      _position--;
      return o;
    } else {
      THROW0(NoSuchElementException);
    }
    return Nil; //never reached
  }
  
  foreign virtual int nextIndex() 
  {
    return _list->_offset + _it->nextIndex();
  }
  
  foreign virtual int previousIndex() 
  {
    return _list->_offset + _it->previousIndex();
  }
  foreign virtual void remove() 
  {
    _it->remove();
    //((AbstractListSubListType*)&_list)->_modCount = ((AbstractListType*)&_list)->_modCount;
    _list->AbstractListSubListType::_modCount = _list->AbstractListType::_modCount;
    _list->_size--;
    _position = nextIndex();
  }
  foreign virtual void set(IN(RValueType) o) 
  {
    _it->set(o);
  }
  foreign virtual void add(IN(RValueType) o) 
  {
    _it->add(o);
    _list->AbstractListSubListType::_modCount = _list->AbstractListType::_modCount;
    _list->_size++;
    _position++;
  }
};

} // util
} // acdk



#endif //acdk_util_TAbstractListSubList_h

