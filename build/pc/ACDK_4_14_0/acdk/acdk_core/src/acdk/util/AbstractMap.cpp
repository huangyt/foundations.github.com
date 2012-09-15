// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:
// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com),
//                       Geoff Berry (gcb@cs.duke.edu)
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractMap.cpp,v 1.16 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include "AbstractMap.h"

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

//virtual 
void 
AbstractMap::clear()
{
  entrySet()->clear();
}

//virtual 
bool 
AbstractMap::containsKey(IN(RObject) key)
{
  RObject k;
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() ==  true) {
    k = RMapEntry(entries->next())->getKey();
    if (key == Nil ? k == Nil : key->equals(k))
      return true;
  }
  return false;
}

//virtual 
bool 
AbstractMap::containsValue(IN(RObject) value)
{
  RObject v;
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() ==  true) {
    v = RMapEntry(entries->next())->getValue();
    if (value == Nil ? v == Nil : value->equals(v))
      return true;
  }
  
  return false; 
}


//virtual 
bool 
AbstractMap::equals(IN(RObject) o)
{
  if (o.impl() == this)
    return true;
  if (o == Nil || instanceof(o, Map) == false)
    return false;
  RMap m = RMap(o);
  if (m->size() != size())
    return false;
  RObject key, value1, value2;
  RMapEntry entry;
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() ==  true) {
    entry = RMapEntry(entries->next());
    key = entry->getKey();
    value1 = entry->getValue();
    value2 = m->get(key);
    if (((value1 == Nil && value2 == Nil) || value1->equals(value2)) == false)
      return false;
  }
  
  return true;    
}

//virtual 
RObject 
AbstractMap::get(IN(RObject) key)
{
  RObject k;
  RMapEntry entry;
  RIterator entries = entrySet()->iterator();
  
  while (entries->hasNext()) {
    entry = RMapEntry(entries->next());
    k = entry->getKey();
    if ((key == Nil ? k == Nil : key->equals(k) == true))
      return entry->getValue();
  }
  
  return Nil;
}


//virtual 
int 
AbstractMap::hashCode()
{
  int hashcode = 0;
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() == true) {
    RObject o = entries->next();
    if (o != RObject(this))
      hashcode += o->hashCode();
  }
  
  return hashcode;
}

//virtual 
bool 
AbstractMap::isEmpty()  
{
  return size() == 0;
}

    

//virtual 
RSet 
AbstractMap::keySet()
{
  return new AbstractMapCachedKeySet(this);
  /*
  if (_cachedKeySet == Nil)
    _cachedKeySet = new AbstractMapCachedKeySet(this);
  return _cachedKeySet;
  */
}

//virtual 
RObject 
AbstractMap::put(IN(RObject) key, IN(RObject) value)
{
  THROW0(UnsupportedOperationException);
  return Nil;
} 

//virtual 
void 
AbstractMap::putAll(IN(RMap) m)
{
  RMapEntry entry;
  RIterator entries = m->entrySet()->iterator();
  while (entries->hasNext()) {
    entry = RMapEntry(entries->next());
    put(entry->getKey(), entry->getValue());
  }
}


//virtual 
RObject 
AbstractMap::remove(IN(RObject) key)
{
  RObject k, value;
  RMapEntry entry;
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() == true) {
    entry = RMapEntry(entries->next());
    k = entry->getKey();
    if (key == Nil ? k == Nil : key->equals(k ) == true) {
      value = entry->getValue();
      entries->remove();
      return value;
    }
  }
  return Nil;    
}

//virtual 
int 
AbstractMap::size()
{
  return entrySet()->size();
}

//virtual 
RString 
AbstractMap::toString()
{
  RMapEntry entry;
  StringBuffer rep(1024);
  rep << "AbstractMap< ";
  RIterator entries = entrySet()->iterator();
  while (entries->hasNext() == true) 
  {
    RObject obj = entries->next();
    entry = RMapEntry(obj);
    rep.append(entry->getKey()->toString());
    rep.append(" -> ");
    RObject val = entry->getValue();
    if (val == Nil)
      rep.append("Nil");
    else
      rep.append(val->toString());
    rep.append(", ");
  }
  rep << " >";
  return rep.toString();
}

//virtual 
RCollection 
AbstractMap::values()
{
  return new AbstractMapCachedValuesCollection(this);
  /*
  if (_cachedValues == Nil)
    _cachedValues = new AbstractMapCachedValuesCollection(this);
  return _cachedValues;
  */
}


//virtual
RIterator 
AbstractMapCachedKeySet::iterator()
{
  return new AbstractMapCachedKeySetIterator(_map);
}

//virtual 
RIterator 
AbstractMapCachedValuesCollection::iterator()
{  
  return new AbstractMapCachedValuesCollectionIterator(_map);
}

} // Util
} // acdk
