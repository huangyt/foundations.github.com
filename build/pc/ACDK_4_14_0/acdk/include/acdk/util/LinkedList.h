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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/LinkedList.h,v 1.20 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_LinkedList_h
#define acdk_util_LinkedList_h

#include <acdk/lang/IllegalStateException.h>
#include "AbstractSequentialList.h"

#include "ConcurrentModificationException.h"
#include "NoSuchElementException.h"

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(LinkedListEntry);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC LinkedListEntry 
: extends acdk::lang::Object
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LinkedListEntry)  
public:
  RObject data;
  RLinkedListEntry next;
  RLinkedListEntry previous;
public:
  LinkedListEntry(IN(RObject) o, IN(RLinkedListEntry) n, IN(RLinkedListEntry) p) 
    : data(o),
      next(n),
      previous(p)
  {
  }
  LinkedListEntry() 
  {
    next = previous = this;
  }
  RObject remove() 
  {
    previous->next = next;
    next->previous = previous;
    return data;
  }
};



ACDK_DECL_CLASS(LinkedList);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:57 $
*/
class ACDK_CORE_PUBLIC LinkedList
: extends AbstractSequentialList
, implements acdk::io::Serializable
, implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(LinkedList)
private:
  RLinkedListEntry _tail;
  int _size;
public:
  static RObject create_instance() { return new LinkedList(); }
  LinkedList();
  LinkedList(IN(RCollection) coll);
  virtual ~LinkedList();
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  foreign virtual RObject getFirst() 
  {
    if (_size == 0) 
      THROW0(NoSuchElementException);
    return _tail->next->data;
  }
  foreign virtual RObject getLast() 
  {
    if (_size == 0) 
      THROW0(NoSuchElementException);
    return _tail->previous->data;
  }
  foreign virtual RObject removeFirst() 
  {
    if (_size == 0) 
      THROW0(NoSuchElementException);
    _size--;
    _modCount++;
    return _tail->next->remove();
  }
  foreign virtual RObject removeLast() 
  {
    if (_size == 0) 
      THROW0(NoSuchElementException);
    _size--;
    _modCount++;
    return _tail->previous->remove();
  }
  foreign virtual void addFirst(IN(RObject) object);
  foreign virtual void addLast(IN(RObject) object);
  foreign virtual int size() {  return _size; }
  foreign virtual void removeRange(int fromIndex, int toIndex) 
  {
    subList(fromIndex, toIndex)->clear();
  }
  foreign virtual void clear();
  foreign virtual RListIterator listIterator(int index = 0);
  foreign virtual RList subList(int fromIndex, int toIndex);
protected:
  static RLinkedListEntry getEntry(int start, int size, IN(RLinkedListEntry) head, IN(RLinkedListEntry) tail);
  friend class LinkedListIterator;
  friend class SubLinkedList;
};


ACDK_DECL_CLASS(SubLinkedList);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SubLinkedList 
: extends AbstractSequentialList 
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SubLinkedList)
private:
  RLinkedList _list;
  RLinkedListEntry _head; // entry before the beginning
  RLinkedListEntry _tail; // entry after the end

  int _size;
  int _knownMod;
public:
  SubLinkedList(IN(RLinkedList) list, IN(RLinkedListEntry) head, IN(RLinkedListEntry) tail, int size) 
  : _list(list),
    _head(head),
    _tail(tail),
    _size(size),
    _knownMod(list->_modCount)
  {
  }
  foreign virtual int size()
  {
    _checkMod();
    return _size;
  }
  foreign virtual RListIterator listIterator(int index = 0);
  foreign virtual void clear();
  foreign virtual RList subList(int fromIndex, int toIndex);
private:
  void _checkMod()
  {
    if (_knownMod != _list->_modCount)
      THROW0(ConcurrentModificationException);
  }
};


ACDK_DECL_CLASS(LinkedListIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC LinkedListIterator 
: extends acdk::lang::Object,
  implements ListIterator,
  implements Iterator
{
  ACDK_WITH_METAINFO(LinkedListIterator)
private:  
  int _position;
  int _size;
  RLinkedListEntry _next;
  RLinkedListEntry _previous;
  RLinkedListEntry _recent;
  RLinkedList _list;
  int _knownMod;
public:
  LinkedListIterator(IN(RLinkedList) list, IN(RLinkedListEntry) entry, int index, int size, int modCount) 
  : _position(index),
    _size(size),
    _next(entry),
    _previous(entry->previous),
    _recent(entry),
    _list(list),
    _knownMod(list->_modCount)
  {
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
  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _size;
  }
  foreign virtual bool hasPrevious()
  {
    _checkMod();
    return _position > 0;
  }
  foreign virtual RObject next();
  foreign virtual RObject element();

  foreign virtual RObject previous();
  foreign virtual void remove();
  foreign virtual void add(IN(RObject) o);
  foreign virtual void set(IN(RObject) o);
private:
  void _checkMod()
  {
    if (_knownMod != _list->_modCount)
      THROW0(ConcurrentModificationException);
  }
};


} // util
} // acdk

#endif //acdk_util_LinkedList_h

