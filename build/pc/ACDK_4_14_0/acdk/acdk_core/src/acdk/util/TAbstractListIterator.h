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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractListIterator.h,v 1.8 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TAbstractListIterator_h
#define acdk_util_TAbstractListIterator_h


#include "ConcurrentModificationException.h"
#include <acdk/lang/IllegalStateException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

//template <class T> class TAbstractList;

/**
  this is just a private implementation of the Iterator for AbstractList
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:57 $

*/
template <class T>
class TAbstractListIterator
: extends acdk::lang::Object
, implements TIterator<T>
{
public:
  typedef T RValueType;

  typedef TAbstractListIterator<RValueType> AbstractListIteratorType;
  typedef RefHolder<AbstractListIteratorType> RAbstractListIteratorType;
  typedef RAbstractListIteratorType RefType;

  typedef TAbstractList<RValueType> AbstractListType;
  typedef RefHolder<AbstractListType> RAbstractListType;

private:
  RAbstractListType _list;
  mutable int _knownMod;
  int _position;
  mutable bool _removed;

  void _checkMod()
  {
    if (_knownMod != _list->_modCount)
      THROW0(ConcurrentModificationException);
  }
public:
  TAbstractListIterator(IN(RAbstractListType) list)
  : Object(),
    _list(list),
    _knownMod(list->_modCount),
    _position(0),
    _removed(false)
  {
  }

  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _list->size();
  }

  foreign virtual RValueType next()
  {
    _checkMod();
    _removed = false;
    try
    {
      return _list->get(_position++);
    }
    catch (RIndexOutOfBoundsException )
    {
      THROW0(NoSuchElementException);
    }
    return RValueType();
  }

  foreign virtual void remove()
  {
    _checkMod();
    if (_removed == true) {
      THROW0(IllegalStateException);
    }
    _list->remove(--_position);
    _knownMod = _list->_modCount;
    _removed = true;
  }
  foreign virtual RValueType element()
  {
    if (_position == 0)
      THROW0(IllegalStateException);
    try {
      return _list->get(_position - 1);
    } catch (RIndexOutOfBoundsException) {
      THROW0(NoSuchElementException);
    }
    return RValueType();
  }
};

template <class T>
inline
typename TAbstractList<T>::RIteratorType 
TAbstractList<T>::iterator()
{
  typedef TAbstractListIterator<T> AbstractListIteratorType;
  //typedef typename AbstractListIteratorType::RefType RAbstractListIteratorType;
  return new (TAbstractList<T>::allocator()) AbstractListIteratorType(this);
}


} // util
} // acdk

#endif //acdk_util_TAbstractListIterator_h

