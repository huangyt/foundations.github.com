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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractListIterator.h,v 1.13 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractListIterator_h
#define acdk_util_AbstractListIterator_h


#include "ConcurrentModificationException.h"
#include <acdk/lang/IllegalStateException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractListIterator);

/**
  this is just a private implementation of the Iterator for AbstractList
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:56 $

*/
class ACDK_CORE_PUBLIC AbstractListIterator
: extends acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(AbstractListIterator)
private:
  RAbstractList _list;
  mutable int _knownMod;
  int _position;
  mutable bool _removed;

  void _checkMod()
  {
    if (_knownMod != _list->_modCount)
      THROW0(ConcurrentModificationException);
  }
public:
  AbstractListIterator(IN(RAbstractList) list)
  : Object(),
    _list(list),
    _knownMod(list->_modCount),
    _position(0),
    _removed(false)
  {
  }

  virtual bool hasNext()
  {
    _checkMod();
    return _position < _list->size();
  }

  virtual RObject next()
  {
    _checkMod();
    _removed = false;
    try {
      return _list->get(_position++);
    } catch (RIndexOutOfBoundsException ) {
      THROW0(NoSuchElementException);
    }
    return Nil; // never reached
  }

  virtual void remove()
  {
    _checkMod();
    if (_removed == true) {
      THROW0(IllegalStateException);
    }
    _list->remove(--_position);
    _knownMod = _list->_modCount;
    _removed = true;
  }
  virtual RObject element()
  {
    if (_position == 0)
      THROW0(IllegalStateException);
    try {
      return _list->get(_position - 1);
    } catch (RIndexOutOfBoundsException) {
      THROW0(NoSuchElementException);
    }
    return Nil; // never reached
  }
};

} // util
} // acdk

#endif //acdk_util_AbstractListIterator_h

