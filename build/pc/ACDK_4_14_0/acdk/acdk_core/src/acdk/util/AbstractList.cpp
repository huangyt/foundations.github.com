// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
// Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
// END

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractList.cpp,v 1.18 2005/03/08 12:45:44 kommer Exp $



#include <acdk.h>
#include "AbstractList.h"
#include "AbstractListIterator.h"
#include "AbstractListListIterator.h"
#include "AbstractListSubList.h"


#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/IndexOutOfBoundsException.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

//virtual 
RObjectArray 
AbstractList::toArray() 
{ 
  return AbstractCollection::toArray(); 
}

//virtual 
RObjectArray 
AbstractList::toArray(IN(RObjectArray) a) 
{ 
  return AbstractCollection::toArray(&a); 
}
  
//virtual 
void 
AbstractList::add(int index, IN(RObject) o) 
{
  THROW0(UnsupportedOperationException);
}

//virtual 
bool 
AbstractList::add(IN(RObject) o) 
{
  add(size(), o);
  return true;
}


//virtual 
bool 
AbstractList::addAll(int index, IN(RCollection) c) 
{
  RIterator i = c->iterator();
  if (i->hasNext()) {
    do {
      add(index++, i->next());
    } while (i->hasNext());
    return true;
  } else {
    return false;
  }
}


//virtual 
void 
AbstractList::clear() 
{
  removeRange(0, size());
}

//virtual 
bool 
AbstractList::equals(IN(RObject) o)
{
  if (o.impl() == (const Object*)this) {
    return true;
  } else if (instanceof(o, List) == false) {
    return false;
  } else {
    RIterator i1 = iterator();
    RIterator i2 = RList(o)->iterator();
    while (i1->hasNext() == true) {
      if (i2->hasNext() == false) {
        return false;
      } else {
        RObject e1 = i1->next();
        RObject e2 = i2->next();
        if (e1 == Nil ? e2 != Nil : e1->equals(e2) == false) {
          return false;
        }
      }
    }
    if (i2->hasNext()) {
      return false;
    } else {
      return true;
    }
  }
}

//virtual 
int 
AbstractList::hashCode()  
{
  int hashCode = 1;
  RIterator i = const_cast<AbstractList*>(this)->iterator();
  while (i->hasNext()) {
    RObject obj = i->next();
    hashCode = 31 * hashCode + (obj == Nil ? 0 : obj->hashCode());
  }
  return hashCode;
}

//virtual 
int 
AbstractList::indexOf(IN(RObject) o)
{
  int index = 0;
  RListIterator i = listIterator();
  if (o == Nil) {
    while (i->hasNext()) {
      if (i->next() == Nil) {
        return index;
      }
      index++;
    }
  } else {
    while (i->hasNext()) {
      if (o->equals(i->next())) {
        return index;
      }
      index++;
    }
  }
  return -1;
}



//virtual
RIterator 
AbstractList::iterator() 
{
  return new (allocator()) AbstractListIterator(this);
}


//virtual 
int 
AbstractList::lastIndexOf(IN(RObject) o)  
{
  int index = size();
  RListIterator i = listIterator(index);
  if (o == Nil) {
    while (i->hasPrevious() == true) {
      if (i->previous() == Nil) {
        return index;
      }
      index--;
    }
  } else {
    while (i->hasPrevious() == true) {
      if (o->equals(i->previous()) == true) {
        return index;
      }
      index--;
    }
  }
  return -1;
}

RListIterator 
AbstractList::listIterator(int index) 
{
  return new (allocator())AbstractListListIterator(this, index);
}


//virtual 
RObject 
AbstractList::remove(int index) 
{
  THROW0(UnsupportedOperationException);
  return Nil;
} 

void 
AbstractList::removeRange(int fromIndex, int toIndex) 
{
  if (fromIndex > toIndex) {
    THROW0(IllegalArgumentException);
  } else if (fromIndex < 0 || toIndex > size()) {
    THROW0(IndexOutOfBoundsException);
  } else {
    RListIterator i = listIterator(fromIndex);
    for (int index = fromIndex; index < toIndex; index++) {
      i->next();
      i->remove();
    }
  }
}

//virtual 
RObject 
AbstractList::set(int index, IN(RObject) o) 
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

//virtual 
int 
AbstractList::size()
{
  THROW0(UnsupportedOperationException);
  return 0;
}

//virtual 
RList 
AbstractList::subList(int fromIndex, int toIndex)
{
  return new (allocator()) AbstractListSubList(this, fromIndex, toIndex);
}

  } // util
} // acdk
