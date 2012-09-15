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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/WeakHashMap.cpp,v 1.8 2005/02/05 10:45:07 kommer Exp $


#include <acdk.h>
#include "WeakHashMap.h"
#include "NoSuchElementException.h"
#include "BasicMapEntry.h"
namespace acdk {
namespace util {


RObject 
WeakHashMap::remove(IN(RObject) key)
{
  _clearQueue();
  WeakHashMapKey tempkey(key);
  RObject obj = _hashMap->remove(&tempkey);
  if (obj == Nil)
    return Nil;
  return RWeakHashMapKey(obj)->get();
}


//virtual 
RSet 
WeakHashMap::entrySet()
{
  return new WeakHashMapEntrySet(_hashMap->entrySet());
}


bool 
WeakHashMapKey::equals(IN(RObject) other) 
{
  if (other.impl() == this)
    return true;
  if (instanceof(other, WeakHashMapKey) == false)
    return false;
  RWeakHashMapKey wother = (RWeakHashMapKey)other;
  RObject o1 = get();
  RObject o2 = wother->get();
  if (o1 == o2) 
    return true;
  if (o1 == Nil || o2 == Nil) 
    return false;
  return o1->equals(o2);
}
  

bool 
WeakHashMapEntrySetEntry::equals(IN(RObject) o) 
{
  if (instanceof(o, MapEntry) == false)
    return false;
  RMapEntry ome(o);
  RObject okey = ome->getKey();
  if (_key != okey) 
  {
    if (_key == Nil || okey == Nil)
      return false;
    if (_key->equals(okey) == false)
      return false;
  }
  RObject val = getValue();
  RObject oval = ome->getValue();
  if (val == oval)
    return true;
  if (val == Nil || oval == Nil)
    return false;
  return val->equals(oval);
}


int 
WeakHashMapEntrySetEntry::hashCode() 
{
  int ret = 0;
  if (_key != Nil)
    ret = _key->hashCode();
  RObject o = getValue();
  if (o == Nil)
    return ret;
  return ret ^ o->hashCode();
}


bool 
WeakHashMapEntrySetIterator::hasNext()
{
  while (_whmesit->hasNext() == true)
  {
    RMapEntry me = (RMapEntry)_whmesit->next();
    RObject realkey;
    if (me != Nil && (realkey = RWeakHashMapKey(me)->get()) == Nil)
      continue;
    _cachedNext = new WeakHashMapEntrySetEntry(me, realkey);
    return true;
  }
  return false;
}


RObject 
WeakHashMapEntrySetIterator::next() 
{
  if (_cachedNext == Nil)
  {
    if (hasNext() == false)
      THROW0(NoSuchElementException);
  }
  RMapEntry ret = _cachedNext;
	_cachedNext = Nil;
  return RObject(ret);
}

RObject
WeakHashMapEntrySetIterator::element()
{
  if (_cachedNext == Nil)
  {
    if (hasNext() == false)
      THROW0(NoSuchElementException);
  }
  return RObject(_cachedNext);
}


} // util
} // acdk


