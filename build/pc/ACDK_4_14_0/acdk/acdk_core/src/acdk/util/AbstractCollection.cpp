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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractCollection.cpp,v 1.13 2005/02/07 11:40:52 kommer Exp $



#include <acdk.h>
#include "AbstractCollection.h"

#include <acdk/lang/ObjectArrayImpl.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

//virtual
bool
AbstractCollection::addAll(IN(RCollection) c)
{
  RIterator i = c->iterator();
  bool modified = false;
  while (i->hasNext()) {
    modified |= add(i->next());
  }
  return modified;
}

//virtual
void
AbstractCollection::clear()
{
  RIterator i = iterator();
  while (i->hasNext()) {
    i->next();
    i->remove();
  }
}


//virtual
bool
AbstractCollection::contains(IN(RObject) o)
{
  RIterator i = iterator();
  if (o == Nil) {
    while (i->hasNext() == true) {
      if (i->next() == Nil) {
        return true;
      }
    }
  } else {
    while (i->hasNext() == true) {
      if (o->equals(i->next()) == true) {
        return true;
      }
    }
  }
  return false;
}

//virtual
bool
AbstractCollection::containsAll(IN(RCollection) c)
{
  RIterator i = c->iterator();
  while (i->hasNext() == true) {
    if (contains(i->next()) == false) {
      return false;
    }
  }
  return true;
}

//virtual
bool
AbstractCollection::remove(IN(RObject) o)
{
  RIterator i = iterator();
  if (o == Nil) {
    while (i->hasNext() == true) {
      if (i->next() == Nil) {
        i->remove();
        return true;
      }
    }
  } else {
    while (i->hasNext() == true) {
      if (o->equals(i->next()) == true) {
        i->remove();
        return true;
      }
    }
  }
  return false;
}

//virtual
bool
AbstractCollection::removeAll(IN(RCollection) c)
{
  RIterator i = iterator();
  bool changed = false;
  while (i->hasNext() == true) {
    if (c->contains(i->next()) == true) {
      i->remove();
      changed = true;
    }
  }
  return changed;
}

//virtual
bool
AbstractCollection::retainAll(IN(RCollection) c)
{
  RIterator i = iterator();
  bool changed = false;
  while (i->hasNext() == true) {
    if (c->contains(i->next()) == false) {
      i->remove();
      changed = true;
    }
  }
  return changed;
}

//virtual
RObjectArray
AbstractCollection::toArray()
{
  RObjectArray a(new ObjectArray(size()));
  RIterator i = iterator();
  for (int pos = 0; pos < a->length(); pos++) {
    a[pos] = i->next();
  }
  return a;
}

//virtual
RObjectArray
AbstractCollection::toArray(IN(RObjectArray) a)
{
  int n = size();
  if (a->length() < n) {
    a->resize(n);
  }
  RIterator i = iterator();
  for (int pos = 0; pos < n; pos++) {
    a[pos] = i->next();
  }
  if (a->length() > n) {
    a[n] = Nil;
  }
  return a;
}

//virtual
RString
AbstractCollection::toString()
{
  StringBuffer s;
  s.append('[');
  RIterator i = const_cast<AbstractCollection*>(this)->iterator();
  bool more = i->hasNext();
  while (more == true)
  {
    s.append(i->next());
    if ((more = i->hasNext()) == true)
    {
      s.append(", ");
    }
  }
  s.append(']');
  return s.toString();
}


} // Util
} // acdk
