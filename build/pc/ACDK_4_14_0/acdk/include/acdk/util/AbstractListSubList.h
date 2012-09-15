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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractListSubList.h,v 1.16 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractListSubList_h
#define acdk_util_AbstractListSubList_h

#include "AbstractList.h"
#include "ListIterator.h"

#include "ConcurrentModificationException.h"
#include <acdk/lang/IndexOutOfBoundsException.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractListSubList);


/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC AbstractListSubList
: extends AbstractList 
{
  ACDK_WITH_METAINFO(AbstractListSubList)
private:
  int _offset;
  int _size;
  int _modCount;
  RAbstractList _list;
public:
  AbstractListSubList(IN(RAbstractList) list, int fromIndex, int toIndex)
  : AbstractList(),
    _offset(fromIndex),
    _size(toIndex - fromIndex),
    _modCount(list->_modCount)
  {
  }
  

public:
  virtual int size()  
  {
    _checkMod();
    return _size;
  }
  
  foreign RListIterator listIterator(int index = 0);
  foreign virtual RObject set(int index, IN(RObject) o);
  foreign virtual RObject get(int index) ;
  virtual void add(int index, IN(RObject) o);
  virtual RObject remove(int index);
  virtual void removeRange(int fromIndex2, int toIndex2);
  virtual bool addAll(int index, IN(RCollection) c);
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
  friend class AbstractListSubListListIterator;
};



ACDK_DECL_CLASS(AbstractListSubListListIterator);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CORE_PUBLIC AbstractListSubListListIterator
: extends acdk::lang::Object,
  implements ListIterator
{
  ACDK_WITH_METAINFO(AbstractListSubListListIterator)
private:
  RListIterator _it;
  RAbstractListSubList _list;
  int _position;
public:
  AbstractListSubListListIterator(IN(RAbstractListSubList) list, int index, int offset)
  : ListIterator(),
  _it(list->listIterator(index + offset)),
  _list(list),
  _position(index)
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
  
  foreign virtual RObject next() 
  {
    if (_position < _list->_size) {
      RObject o = _it->next();
      _position++;
      return o;
    } else {
      THROW0(NoSuchElementException);
    }
    return Nil; //never reached
  }
  foreign virtual RObject element()
  {
    if (_position < _list->_size) {
      RObject o = _it->next();
      return o;
    } else {
      THROW0(NoSuchElementException);
    }
    return Nil; //never reached
  }
  foreign virtual RObject previous() 
  {
    if (_position > 0) {
      RObject o = _it->previous();
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
    _list->AbstractListSubList::_modCount = _list->AbstractList::_modCount;
    _list->_size--;
    _position = nextIndex();
  }
  foreign virtual void set(IN(RObject) o) 
  {
    _it->set(o);
  }
  foreign virtual void add(IN(RObject) o) 
  {
    _it->add(o);
    _list->AbstractListSubList::_modCount = _list->AbstractList::_modCount;
    _list->_size++;
    _position++;
  }
};

} // util
} // acdk



#endif //acdk_util_AbstractListSubList_h

