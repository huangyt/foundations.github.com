// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
/* Parts of this class are ported from the of GNU Classpath project 
  (http://www.gnu.org/software/classpath/classpath.html)
   with following copyright statement:
*/
// Collections.java -- Utility class with methods to operate on collections
//
// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published
// by the Free Software Foundation, version 2. (see COPYING.LIB)
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA



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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Collections.cpp,v 1.12 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Exception.h>
#include <acdk/lang/IndexOutOfBoundsException.h>

#include "Collections.h"
#include "Arrays.h"
#include "SynchronizedCollections.h"
#include "AbstractList.h"
#include "AbstractSet.h"


namespace acdk {
namespace util {

RList Collections::_EMPTY_LIST;
RSet Collections::_EMPTY_SET;

foreign
class EmptyList
: extends AbstractList
{
public:
  virtual int size() { return 0; }
  virtual RObject get(int index) { 
    THROW0(IndexOutOfBoundsException); 
    return Nil; 
  }
};

foreign
class EmptySet
: extends AbstractSet
{
public:
  virtual int size() { return 0; }
  virtual RIterator iterator() {
    return Collections::get_EMPTY_LIST()->iterator(); 
  }
};

//static 
RList 
Collections::get_EMPTY_LIST()
{
  if (_EMPTY_LIST == Nil) {
    _EMPTY_LIST = new EmptyList();
    System::registerStaticReference(_EMPTY_LIST);
  }
  return _EMPTY_LIST;
}



//static 
RSet 
Collections::get_EMPTY_SET()
{
  if (_EMPTY_SET == Nil) {
    _EMPTY_SET = new EmptySet();
    System::registerStaticReference(_EMPTY_SET);
  }
  return _EMPTY_SET;
}  


void 
Collections::addAll(IN(RCollection) coll, IN(RObjectArray) array)
{
  if (array == Nil)
    return;
  for (int i = 0; i < array->length(); i++)
    coll->add(array[i]);
}


//static 
int 
Collections::binarySearch(IN(RList) list, IN(RObject) key, IN(RComparator) comparator)
{  
  return _defaultSearch(list, key, comparator);
}

//static 
void 
Collections::copy(IN(RList) dest, IN(RList) source) 
{
  RIterator i1 = source->iterator();
  RListIterator i2 = dest->listIterator();
  while (i1->hasNext() == true) {
    i2->next();
    i2->set(i1->next());
  }
}


//static 
REnumeration 
Collections::enumeration(IN(RCollection) coll)
{
  return new IteratorEnumeration(coll->iterator());
}


//static 
void 
Collections::fill(IN(RList) list, IN(RObject) val) 
{
  RListIterator it = list->listIterator();
  while (it->hasNext() == true) {
    it->next();
    it->set(val);
  }
}


//static 
RObject 
Collections::max(IN(RCollection) coll) 
{
  RIterator it = coll->iterator();
  RComparable max = RComparable(it->next()); 
  while (it->hasNext()) {
    RObject o = it->next();
    if (max->compareTo(o) < 0) {
      max = RComparable(o);
    }
  }
  return RObject(max);
}

//static 
RObject 
Collections::max(IN(RCollection) coll, IN(RComparator) order) 
{
  RIterator it = coll->iterator();
  RObject max = it->next(); // throws NoSuchElementException
  while (it->hasNext()) {
    RObject o = it->next();
    if (order->compare(max, o) < 0) {
      max = o;
    }
  }
  return max;
}


//static 
RObject 
Collections::min(IN(RCollection) coll) 
{
  RIterator it = coll->iterator();
  RComparable min = RComparable(it->next());
  while (it->hasNext()) {
    RObject o = it->next();
    if (min->compareTo(o) > 0) {
      min = RComparable(o);
    }
  }
  return RObject(min);
}

//static 
RObject 
Collections::min(IN(RCollection) coll, IN(RComparator) order) 
{
  RIterator it = coll->iterator();
  RObject min = it->next(); // throws NoSuchElementExcception
  while (it->hasNext()) {
    RObject o = it->next();
    if (order->compare(min, o) > 0) {
      min = o;
    }
  }
  return min;
}

//static 
void 
Collections::reverse(IN(RList) list) 
{
  RListIterator i1 = list->listIterator();
  RListIterator i2 = list->listIterator(list->size());
  while (i1->nextIndex() < i2->previousIndex()) {
    RObject o = i1->next();
    i1->set(i2->previous());
    i2->set(o);
  }
}

class ReverseOrderComparator
: extends Object
, implements Comparator
{
public:
  virtual int compare(IN(RObject) a, IN(RObject) b)
  {
    return RComparable(a)->compareTo(b) * (-1);
  }
};

//static 
RComparator 
Collections::reverseOrder() 
{
  return new ReverseOrderComparator();
}

//static 
void 
Collections::shuffle(IN(RList) list)
{
  shuffle(list, new Random());
}

//static 
void 
Collections::shuffle(IN(RList) list, IN(RRandom) rnd)
{
  RObjectArray a = list->toArray(); 
  RListIterator it = list->listIterator(list->size());
  while (it->hasPrevious()) {
    int swap = rnd->nextInt(it->nextIndex());
    RObject o = it->previous();
    it->set(a[swap]);
    a[swap] = o;
  }
}

//static 
RSet 
Collections::singleton(IN(RObject) o)
{
  THROW1(Exception, "Not implemented yet");
  return Nil;
}
//static 
void 
Collections::sort(IN(RList) list, IN(RComparator) comparator)
{
  RObjectArray temparray = list->toArray();
  ArraysImpl::sort(temparray, comparator);
  RListIterator it = list->listIterator();
  for (int pos = 0; pos < temparray->length(); pos++) {
    it->next();
    it->set(temparray[pos]);
  }
}

//static 
RCollection 
Collections::synchronizedCollection(IN(RCollection) coll, IN(RObject) lock)
{
  return new SynchronizedCollection(coll, lock);
}

//static 
RList 
Collections::synchronizedList(IN(RList) list, IN(RObject) lock)
{
  return new SynchronizedList(list, lock);
}

//static 
RSet 
Collections::synchronizedSet(IN(RSet) set, IN(RObject) lock)
{
  return new SynchronizedSet(set, lock);
}

//static 
RMap
Collections::synchronizedMap(IN(RMap) map, IN(RObject) lock)
{
  return new SynchronizedMap(map, lock);
}
//static 
RSortedSet 
Collections::synchronizedSortedSet(IN(RSortedSet) sortedset, IN(RObject) lock)
{
  return new SynchronizedSortedSet(sortedset, lock);
}

//static 
RSortedMap 
Collections::synchronizedSortedMap(IN(RSortedMap) sortedmap, IN(RObject) lock)
{
  return new SynchronizedSortedMap(sortedmap, lock);
}

//static 
int 
Collections::_defaultSearch(IN(RList) list, IN(RObject) key, IN(RComparator) comparator) 
{
  int pos = 0;
  if (instanceof(list, AbstractSequentialList) == true) {
    RListIterator it = list->listIterator();
    while (it->hasNext()) {
      int d = _compare(key, it->next(), comparator);
      if (d == 0) {
        return pos;
      } else if (d < 0) {
        return -pos - 1;
      }
      pos++;
    }
    
    // We assume the list is random-access, and use a binary search
  } else {
    int low = 0;
    int hi = list->size() - 1;
    while (low <= hi) {
      pos = (low + hi) >> 1;
      int d = _compare(key, list->get(pos), comparator);
      if (d == 0) {
        return pos;
      } else if (d < 0) {
        hi = pos - 1;
      } else {
        low = ++pos; // This gets the insertion point right on the last loop
      }
    }
  }
  return -pos - 1;
}

//static 
RCollection 
Collections::unmodifiableCollection(IN(RCollection) c) //## not implemented yet
{
  return c;
}

//static 
RList 
Collections::unmodifiableList(IN(RList) list) //## not implemented yet
{
  return list;
}

//static 
RMap 
Collections::unmodifiableMap(IN(RMap) m) //## not implemented yet
{
  return m;
}

//static 
RSet 
Collections::unmodifiableSet(IN(RSet) s) //## not implemented yet
{
  return s;
}

//static 
RSortedMap 
Collections::unmodifiableSortedMap(IN(RSortedMap) m) //## not implemented yet
{
  return m;
}

//static 
RSortedSet 
Collections::unmodifiableSortedSet(IN(RSortedSet) s) //## not implemented yet
{
  return s;
}


} // Util
} // acdk

