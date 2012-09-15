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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TreeSet.cpp,v 1.12 2005/03/03 09:38:01 kommer Exp $


#include <acdk.h>
#include "TreeSet.h"

#include <acdk/io/Serializable.h>
#include <acdk/lang/Cloneable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


TreeSet::TreeSet(IN(RComparator) comp)
: _map(new (allocator()) TreeMap(comp))
{
}

TreeSet::TreeSet(IN(RCollection) coll)
:  _map(new (allocator()) TreeMap())

{
  if (coll != Nil)
    addAll(coll);
}

TreeSet::TreeSet(IN(RSortedSet) sortedSet)
: _map(new (allocator()) TreeMap(sortedSet->comparator()))
{
  RTreeMap map = RTreeMap(_map);
  int i = 0;
  RMapEntryArray entries = new (allocator()) MapEntryArray(sortedSet->size());
  RIterator it = sortedSet->iterator();
  while (it->hasNext() == true)
    entries[i++] = new (allocator()) BasicMapEntry(it->next(), (RObject)Boolean::getTRUE());
  map->_size = i;
  map->putAllLinear(entries);
}


TreeSet::TreeSet(IN(RSortedMap) map)
: _map(map)
{
}


//virtual 
bool 
TreeSet::add(IN(RObject) object)
{
  if (_map->containsKey(object) == true) 
    return false;

  _add(object);
  return true;
}

//virtual 
bool 
TreeSet::addAll(IN(RCollection) coll)
{
  bool result = true;
  RIterator it = coll->iterator();
  while (it->hasNext() == true)
  {
    bool res = add(it->next());
    result = result && res;
  }
  return result;
}


//virtual 
RObject 
TreeSet::clone(sys::Allocator* alc)
{
  return new (alc) TreeSet(RSortedSet(this)); 
}

//virtual 
RSortedSet 
TreeSet::subSet(IN(RObject) from, IN(RObject) to)
{
  RTreeSet t(new TreeSet(_map->subMap(from, to)));
  return (RSortedSet) t;
}


} // util
} // acdk

