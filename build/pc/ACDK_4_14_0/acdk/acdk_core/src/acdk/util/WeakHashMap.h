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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/WeakHashMap.h,v 1.9 2005/02/05 10:45:07 kommer Exp $

#ifndef acdk_util_WeakHashMap_h
#define acdk_util_WeakHashMap_h

#include "AbstractMap.h"
#include "HashMap.h"
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/lang/ref/ReferenceQueue.h>

namespace acdk {
namespace util {


ACDK_DECL_CLASS(WeakHashMap);
ACDK_DECL_CLASS(WeakHashMapKey);
using ::acdk::lang::ref::WeakReference;

class ACDK_CORE_PUBLIC WeakHashMapKey
: extends ::acdk::lang::ref::WeakReference
{
  ACDK_WITH_METAINFO(WeakHashMapKey)
public:
  WeakHashMapKey(IN(RObject) k)
    : WeakReference(k)
  {
  }
  WeakHashMapKey(IN(RObject) k, IN(::acdk::lang::ref::RReferenceQueue) refqueue)
    : WeakReference(k, refqueue)
  {
  }
  bool equals(IN(RObject) other);
  int hashCode() 
  { 
    RObject o = get();
    if (o == Nil)
      return 0;
    return get()->hashCode(); 
  }

};
/**
  Corresponds to the Java API class
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:07 $
  
*/

class ACDK_CORE_PUBLIC WeakHashMap
: extends AbstractMap
{
  ACDK_WITH_METAINFO(WeakHashMap)

public:
  static RObject create_instance() { return new WeakHashMap(); }
  
private:
  /**
    Contains the WeakHashMapKey and Values
  */
  RHashMap _hashMap;
  /**
    Used to synchronized cleaning
  */
  ::acdk::lang::ref::RReferenceQueue _refQueue;
  
public:
  WeakHashMap(int initialCapacity = HashMap::DEFAULT_CAPACITY, float initialLoadFactor = HashMap::DEFAULT_LOAD_FACTOR)
  : AbstractMap()
  , _hashMap(new (allocator()) HashMap(initialCapacity, initialLoadFactor))
  , _refQueue(new ::acdk::lang::ref::ReferenceQueue())
  {

  }
  virtual ~WeakHashMap()
  {
  }

  foreign virtual int size() { return _hashMap->size(); }
  foreign virtual bool isEmpty() { return _hashMap->isEmpty(); }
  foreign virtual void clear()  { _hashMap->clear(); }
  /*
  foreign virtual RSet keySet();
  */
  foreign virtual RSet entrySet();
  /*
  foreign virtual RCollection values();
  foreign virtual bool containsKey(IN(RObject) key) { return (_get(key) != Nil); }
  foreign virtual bool containsValue(IN(RObject) value);
  */
  foreign virtual RObject get(IN(RObject) key) 
  {
    _clearQueue();
    WeakHashMapKey tempkey(key);
    return _hashMap->get(&tempkey);
  }
  foreign virtual RObject put(IN(RObject) key, IN(RObject) value)
  {
    _clearQueue();
    return _hashMap->put(new (allocator()) WeakHashMapKey(key, _refQueue), value);
  }
  //foreign virtual void putAll(IN(RMap) t);
  foreign virtual RObject remove(IN(RObject) key);
  
  int capacity() { return _hashMap->capacity(); }
  float loadFactor() { return _hashMap->loadFactor(); }

  //foreign virtual RIterator iterator();

private:
  void _clearQueue()
  {
    RWeakHashMapKey wk;
	  while ((wk = (RWeakHashMapKey)_refQueue->poll()) != Nil) 
    {
	    _hashMap->remove(&wk);
  	}
  }
};

class ACDK_CORE_PUBLIC WeakHashMapEntrySet
: extends AbstractSet
{
private:
  RSet _entrySet;
public:
  WeakHashMapEntrySet(IN(RSet) entryset)
  : AbstractSet()
  , _entrySet(entryset)
  {
  }
  int size() { return _entrySet->size(); }
  RIterator iterator();
};

class ACDK_CORE_PUBLIC WeakHashMapEntrySetEntry
: extends ::acdk::lang::Object
, implements MapEntry
{
private:
  RMapEntry _weakHashMapEntry;
  RObject _key;
public:
  WeakHashMapEntrySetEntry(IN(RMapEntry) me, IN(RObject) key)
  : Object()
  , _weakHashMapEntry(me)
  , _key(key)
  {
  }
  RObject getKey() { return _weakHashMapEntry->getKey(); }
  RObject getValue() { return _weakHashMapEntry->getValue(); }
  RObject setValue(IN(RObject) value) 
  {
	  return _weakHashMapEntry->setValue(value);
	}

	bool equals(IN(RObject) o);

	int hashCode() ;

};

class ACDK_CORE_PUBLIC WeakHashMapEntrySetIterator
: extends ::acdk::lang::Object
, implements Iterator
{
private:
  RIterator _whmesit;
  RMapEntry _cachedNext;
public:
  WeakHashMapEntrySetIterator(IN(RIterator) it)
  : Object()
  , _whmesit(it)
  , _cachedNext(Nil)
  {
  }
  bool hasNext();
  RObject next();
  RObject element();
  void remove() 
  {
	  _whmesit->remove();
  }

};

inline 
RIterator 
WeakHashMapEntrySet::iterator()
{
  return new WeakHashMapEntrySetIterator(_entrySet->iterator());
}

} // util
} // acdk

#endif // acdk_util_WeakHashMap_h

