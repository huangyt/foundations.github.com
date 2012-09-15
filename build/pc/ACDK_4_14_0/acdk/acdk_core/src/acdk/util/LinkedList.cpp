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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/LinkedList.cpp,v 1.14 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>

#include "LinkedList.h"

#include <acdk/lang/IllegalStateException.h>
#include <acdk/lang/IndexOutOfBoundsException.h>
#include "NoSuchElementException.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {
    
using namespace acdk::lang;
using namespace acdk::io;
    

LinkedList::LinkedList()
: _tail(new LinkedListEntry()),
  _size(0)
{
}

LinkedList::LinkedList(IN(RCollection) coll)
: _tail(new LinkedListEntry()),
  _size(0)
{
  AbstractCollection::addAll(coll);
}

//virtual 
RObject 
LinkedList::clone(sys::Allocator* alc)
{
  THROW1(UnsupportedOperationException, "LinkedList::clone() not implemented");
  return Nil;
}

//virtual 
LinkedList::~LinkedList()
{
  RLinkedListEntry l = _tail->next;
  while (l != _tail) {
    RLinkedListEntry tl = l->next;
    l->next = Nil;
    l->previous = Nil;
    l = tl;
  }
  _tail->next = _tail->previous = Nil;
  _tail = Nil;
}

//virtual 
void 
LinkedList::addFirst(IN(RObject) object) 
{
  _tail->next = new LinkedListEntry(object, _tail->next, _tail);
  _tail->next->previous = _tail->next;
  _size++;
  _modCount++;
}

//virtual 
void 
LinkedList::addLast(IN(RObject) object) 
{
  add(size(), object);
  //add(object);
  /*
  _tail->previous = new LinkedListEntry(object, _tail, _tail->previous);
  _tail->previous->next = _tail->previous;
  _tail = _tail->previous;
  _size++;
  _modCount++;
  */
}

//virtual 
void 
LinkedList::clear() 
{
  RLinkedListEntry l = _tail->next;
  while (l != _tail) {
    RLinkedListEntry tl = l->next;
    l->data = Nil;
    l->next = Nil;
    l->previous = Nil;
    l = tl;
  }
  _tail->data = Nil;
  _tail->next = _tail->previous = _tail;
  _modCount++;
  _size = 0;
}


//virtual 
RListIterator 
LinkedList::listIterator(int index) 
{
  if (index < 0 || index > _size) 
    THROW0(IndexOutOfBoundsException);
  return new LinkedListIterator(this, getEntry(index, _size, _tail, _tail),  index, _size, _modCount);
}

//virtual 
RList 
LinkedList::subList(int fromIndex, int toIndex)
{
  return new SubLinkedList(this, getEntry(fromIndex - 1, _size, _tail, _tail),
                                  getEntry(toIndex, _size, _tail, _tail),
                                  toIndex - fromIndex);
}


//static 
RLinkedListEntry 
LinkedList::getEntry(int start, int size, IN(RLinkedListEntry) head_, IN(RLinkedListEntry) tail_) 
{
  RLinkedListEntry head = head_;
  RLinkedListEntry tail = tail_;
  if (start < size >> 1) {
    while (start-- >= 0) {
      head = head->next;
    }
    return head;
  } else {
    while (++start <= size) {
      tail = tail->previous;
    }
    return tail;
  }
}

//virtual 
RListIterator 
SubLinkedList::listIterator(int index) 
{
  _checkMod();
  if (index < 0 || index > _size) 
    THROW0(IndexOutOfBoundsException);
  return new LinkedListIterator(_list, LinkedList::getEntry(index, _size, _head, _tail),  index, _size, _modCount);
}

//virtual 
void 
SubLinkedList::clear() 
{
  _checkMod();
  _head->next = _tail;
  _tail->previous = _head;
  _list->_size -= _size;
  _size = 0;

  _knownMod++;
  _list->_modCount++;
  _list->_size--;
}

//virtual
RList 
SubLinkedList::subList(int fromIndex, int toIndex) 
{
  _checkMod();    
  if (fromIndex > toIndex || fromIndex < 0 || toIndex > _size) 
    THROW0(IndexOutOfBoundsException);
  return new SubLinkedList(_list, LinkedList::getEntry(fromIndex - 1, _size, _head, _tail), 
                                  LinkedList::getEntry(toIndex, _size, _head, _tail), 
                                  toIndex - fromIndex);
}


//virtual 
RObject 
LinkedListIterator::next()
{
  _checkMod();
  if (_position >= _size) 
    THROW0(NoSuchElementException);
  _position++;
  _recent = _previous = _next;
  _next = _recent->next;
  return _recent->data;
}

//virtual 
RObject 
LinkedListIterator::previous()
{
  _checkMod();
  if (_position <= 0) 
    THROW0(NoSuchElementException);
  _position--;
  _recent = _next = _previous;
  _previous = _recent->previous;
  return _recent->data;
}

//virtual 
RObject 
LinkedListIterator::element()
{
  _checkMod();
  return _recent->data;
}

//virtual 
void 
LinkedListIterator::remove()
{
  _checkMod();
  if (_recent == Nil)
    THROW0(IllegalStateException);
  if (_recent == _previous) 
    _position--;
  _next = _recent->previous->next = _recent->next;
  _previous = _recent->next->previous = _recent->previous;
  _size--;
  ++_knownMod; 
  ++_list->_modCount;
  _list->_size--;
  _recent = Nil;
}

//virtual 
void 
LinkedListIterator::add(IN(RObject) o) 
{
  _checkMod();
  _previous->next = _next->previous = new LinkedListEntry(o, _next, _previous);
  _previous = _next;
  _next = _previous->next;
  _position++;
  _size++;
  _list->_size++;
  ++_knownMod; ++_list->_modCount;
  _recent = Nil;
}

//virtual 
void 
LinkedListIterator::set(IN(RObject) o) 
{
  _checkMod();
  if (_recent == Nil) 
    THROW0(IllegalStateException);
  _recent->data = o;
}


} // util
} // acdk

