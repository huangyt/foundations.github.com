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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractListListIterator.h,v 1.7 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TAbstractListListIterator_h
#define acdk_util_TAbstractListListIterator_h

#include "TListIterator.h"
#include "TAbstractList.h"

#include "ConcurrentModificationException.h"
#include <acdk/lang/IllegalStateException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {



/**
  this is just a private implementation of the Iterator for AbstractList *
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/09 19:26:57 $

*/

template <class T>
class TAbstractListListIterator
: extends acdk::lang::Object,
  implements TListIterator<T>
{
public:
  typedef T RValueType;

  typedef TList<RValueType> ListType;
  typedef typename ListType::RefType RListType;

  typedef TListIterator<RValueType> ListIteratorType;
  typedef typename ListIteratorType::RefType RListIteratorType;
  typedef TAbstractList<RValueType> AbstractListType;
  typedef typename AbstractListType::RefType RAbstractListType;

  typedef TAbstractListListIterator<RValueType> AbstractListListIteratorType;
  typedef RefHolder<AbstractListListIteratorType> RAbstractListListIteratorType;
  typedef RAbstractListListIteratorType RefType;

private:
  RAbstractListType _list;
  int _knownMod;
  int _position;
  transient int _lastReturned;

  void _checkMod()
  {
    if (_knownMod != _list->_modCount)
      THROW0(ConcurrentModificationException);
  }
public:
  TAbstractListListIterator(IN(RAbstractListType) list, int index)
  : Object(),
    _list(list),
    _knownMod(list->_modCount),
    _position(index),
    _lastReturned(-1)
  {
  }
  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _list->size();
  }

  foreign virtual bool hasPrevious()
  {
    _checkMod();
    return _position > 0;
  }
  foreign virtual RValueType next()
  {
    _checkMod();
    if (hasNext() == false) {
      THROW0(NoSuchElementException);
    }
    _lastReturned = _position++;
    return _list->get(_lastReturned);
  }
  foreign virtual RValueType element()
  {
    _checkMod();
    if (hasNext() == false) {
      THROW0(NoSuchElementException);
    }
    _lastReturned = _position;
    return _list->get(_lastReturned);
  }
  foreign virtual RValueType previous()
  {
    _checkMod();
    if (hasPrevious() == false) {
      THROW0(NoSuchElementException);
    }
    _lastReturned = --_position;
    return _list->get(_lastReturned);
  }
  foreign virtual int nextIndex()
  {
    _checkMod();
    return _position;
  }
  foreign virtual int previousIndex()
  {
    _checkMod();
    return _position - 1;
  }
  foreign virtual void remove()
  {
    _checkMod();
    if (_lastReturned < 0) {
      THROW0(IllegalStateException);
    }
    _list->remove(_lastReturned);
    _knownMod = _list->_modCount;
    _position = _lastReturned;
    _lastReturned = -1;
  }

  foreign virtual void set(IN(RValueType) o)
  {
    _checkMod();
    if (_lastReturned < 0) {
      THROW0(IllegalStateException);
    }
    _list->set(_lastReturned, o);
    _knownMod = _list->_modCount;
  }
  foreign virtual void add(IN(RValueType) o)
  {
    _checkMod();
    _list->add(_position++, o);
    _lastReturned = -1;
    _knownMod = _list->_modCount;
  }
};


template <class T>
inline
typename TAbstractList<T>::RListIteratorType 
TAbstractList<T>::listIterator(int index)
{
  typedef TAbstractListListIterator<RValueType> AbstractListListIteratorType;
   return new (TAbstractList<T>::allocator()) AbstractListListIteratorType(this, index);
}

} // util
} // acdk



#endif //acdk_util_TAbstractListListIterator_h

