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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SynchronizedCollections.h,v 1.21 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_SynchronizedCollections_h
#define acdk_util_SynchronizedCollections_h

#include <acdk.h>
#include <acdk/io/Serializable.h>

#include "ListIterator.h"
#include "List.h"
#include "Collection.h"
#include "Set.h"
#include "SortedSet.h"
#include "Map.h"
#include "SortedMap.h"
#include "AbstractSequentialList.h"



namespace acdk {
namespace util {


ACDK_DECL_CLASS(SynchronizedIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  @todo check which function of the Synchronized group need virtual methods

*/
class ACDK_CORE_PUBLIC SynchronizedIterator 
: extends acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(SynchronizedIterator)
protected:
  RObject _lock;
  RIterator _it;
public:
  SynchronizedIterator(IN(RIterator) it, IN(RObject) thelock) 
  :  _lock(thelock),
    _it(it)
  {
  }
  ~SynchronizedIterator() { }
  foreign virtual RObject next() 
  {
    SYNCTHIS();
    return _it->next();
  }
  foreign virtual bool hasNext()
  {
    SYNCTHIS();
    return _it->hasNext();
  }
  foreign virtual void remove() 
  {
    SYNCTHIS();
    _it->remove();
  }
  foreign virtual RObject element()
  {
    SYNCTHIS();
    return _it->element();
  }
};


ACDK_DECL_CLASS(SynchronizedListIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedListIterator 
: extends SynchronizedIterator,
  implements ListIterator
{
  ACDK_WITH_METAINFO(SynchronizedListIterator)
private:
   RListIterator _lit;
public:
  SynchronizedListIterator(IN(RListIterator) lit, IN(RObject) sync)
  : SynchronizedIterator(RIterator(lit), sync),
    _lit(lit)
  {
  }
  foreign virtual bool hasNext()
  {
    return SynchronizedIterator::hasNext();
  }
  foreign virtual RObject next()
  {
    return SynchronizedIterator::next();
  }
  foreign virtual RObject element()
  {
    return SynchronizedIterator::element();
  }
  foreign virtual void remove() 
  {
    SynchronizedIterator::remove();
  }
  foreign virtual bool hasPrevious()
  {
    SYNCTHIS();
    return _lit->hasPrevious();
  }
  foreign virtual RObject previous() 
    {
    SYNCTHIS();
    return _lit->previous();
  }
  
  foreign virtual int nextIndex() 
  {
    SYNCTHIS();
    return _lit->nextIndex();
    
  }
  foreign virtual int previousIndex()
    {
    SYNCTHIS();
    return _lit->previousIndex();
  }
  foreign virtual void add(IN(RObject) o) 
  {
    SYNCTHIS();
    _lit->add(o);
  }
  foreign virtual void set(IN(RObject) o) 
  {
    SYNCTHIS();
    _lit->set(o);
  }
};

ACDK_DECL_CLASS(SynchronizedCollection);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedCollection 
: extends acdk::lang::Object
, implements Collection,
  implements acdk::io::Serializable 
{
  ACDK_WITH_METAINFO(SynchronizedCollection)
protected:
  mutable RObject _lock;
  RCollection _coll;
public:
  static RObject create_instance() { return new SynchronizedCollection(Nil, Nil); }

  SynchronizedCollection(IN(RCollection) coll, IN(RObject) sync = Nil) 
  :  _lock(sync),
    _coll(coll)
  {
  }
  foreign virtual void lock()
  {
    if (_lock != Nil)
      _lock->lock();
    else
      Object::lock();
  }
  foreign virtual void unlock()
  {
    if (_lock != Nil)
      _lock->unlock();
    else
      Object::unlock();
  }
  foreign virtual bool add(IN(RObject) o) 
  {
    SYNCTHIS();
    return _coll->add(o);
  }
  foreign virtual bool addAll(IN(RCollection) col) 
  {
    SYNCTHIS();
    return _coll->addAll(col);
  }
  foreign virtual void clear() 
  {
    SYNCTHIS();
    _coll->clear();
  }
  foreign virtual bool contains(IN(RObject) o)
  {
    SYNCTHIS();
    return _coll->contains(o);
  }
  foreign virtual bool containsAll(IN(RCollection) c1)
  {
    SYNCTHIS();
    return _coll->containsAll(c1);
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    SYNCTHIS();
    return _coll->equals(o);
  }
  foreign virtual int hashCode()
  {
    SYNCTHIS();
    return _coll->hashCode();
  }
  foreign virtual bool isEmpty()
  {
    SYNCTHIS();
    return _coll->isEmpty();
  }
  foreign virtual RIterator iterator() 
  {
    SYNCTHIS();
    return new SynchronizedIterator(_coll->iterator(), _lock);
  }
  foreign virtual bool remove(IN(RObject) o) 
  {
    SYNCTHIS();
    return _coll->remove(o);
  }
  foreign virtual bool removeAll(IN(RCollection) col) 
  {
    SYNCTHIS();
    return _coll->removeAll(col);
  }
  foreign virtual bool retainAll(IN(RCollection) col) 
  {
    SYNCTHIS();
    return _coll->retainAll(col);
  }
  foreign virtual int size()
  {
    SYNCTHIS();
    return _coll->size();
  }
  foreign virtual ::acdk::lang::RObjectArray toArray();
  foreign virtual ::acdk::lang::RObjectArray toArray(IN(::acdk::lang::RObjectArray) array);
};



ACDK_DECL_CLASS(SynchronizedList);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedList
: extends SynchronizedCollection
, implements List
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedList)
private:
  RList _list;
public:
  SynchronizedList(IN(RList) list, IN(RObject) sync = Nil) 
    : SynchronizedCollection(RCollection(list), sync),
    _list(list)
   {
  }
  foreign virtual int size() 
  {
    return SynchronizedCollection::size();
  }
  foreign virtual bool isEmpty()
  {  
    return SynchronizedCollection::isEmpty();
  }
  foreign virtual void clear()
  {
    SynchronizedCollection::clear();
  }
  foreign virtual bool contains(IN(RObject) o)
  {
    return SynchronizedCollection::contains(o);
  }
  foreign virtual bool containsAll(IN(RCollection) c)
  {
    return SynchronizedCollection::containsAll(c);
  }
  foreign virtual RIterator iterator()
  {
    return SynchronizedCollection::iterator();
  }
  foreign virtual bool add(IN(RObject) o)
  {
    return SynchronizedCollection::add(o);
  }
  foreign virtual bool addAll(IN(RCollection) c)
  {
    return SynchronizedCollection::addAll(c);
  }
  foreign virtual bool removeAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::removeAll(c);
  }
  foreign virtual bool retainAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::retainAll(c);
  }
  foreign virtual RObjectArray toArray();
  foreign virtual RObjectArray toArray(IN(RObjectArray) array);
  foreign virtual void add(int index, IN(RObject) o) 
   {
    SYNCTHIS();
    _list->add(index, o);
  }
  foreign virtual bool addAll(int index, IN(RCollection) c) 
  {
    SYNCTHIS();
    return _list->addAll(index, c);
    
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    SYNCTHIS();
    return _list->equals(o);
  }
  foreign virtual RObject get(int index) 
  {
    SYNCTHIS();
    return _list->get(index);
  }
  foreign virtual int hashCode()
  {
    SYNCTHIS();
    return _list->hashCode();
  }
  foreign virtual int indexOf(IN(RObject) o)
  {
    SYNCTHIS();
    return _list->indexOf(o);
  }
  foreign virtual int lastIndexOf(IN(RObject) o)  
  {
    SYNCTHIS();
    return _list->lastIndexOf(o);
  }
  foreign virtual RListIterator listIterator(int index = 0) 
  {
    SYNCTHIS();
    return new SynchronizedListIterator(_list->listIterator(index), _lock);
  }
  foreign virtual RObject remove(int index) 
  {
    SYNCTHIS();
    return _list->remove(index);
  }
  foreign virtual bool remove(IN(RObject) o) 
  {
    SYNCTHIS();
    return _list->remove(o);
  }
  foreign virtual RObject set(int index, IN(RObject) o) 
  {
    SYNCTHIS();
    return _list->set(index, o);
  }
  foreign virtual RList subList(int fromIndex, int toIndex);
};


inline 
RList 
SynchronizedList::subList(int fromIndex, int toIndex) 
{
  SYNCTHIS();
  return new SynchronizedList(_list->subList(fromIndex, toIndex), _lock);
}

ACDK_DECL_CLASS(SynchronizedSet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedSet
: extends SynchronizedCollection
, implements Set
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedSet)
private:
public:
  SynchronizedSet(IN(RSet) set, IN(RObject) sync = Nil) 
  : SynchronizedCollection(RCollection(RObject(set)), sync)
  {
  }
  foreign virtual int size()  
  {
    return SynchronizedCollection::size();
  }
  foreign virtual bool isEmpty()
  {  
    return SynchronizedCollection::isEmpty();
  }
  foreign virtual void clear()
  {
    SynchronizedCollection::clear();
  }
  foreign virtual bool contains(IN(RObject) o)
  {
    return SynchronizedCollection::contains(o);
  }
  foreign virtual bool containsAll(IN(RCollection) c)
  {
    return SynchronizedCollection::containsAll(c);
  }
  foreign virtual RIterator iterator()
  {
    return SynchronizedCollection::iterator();
  }
  foreign virtual bool add(IN(RObject) o)
  {
    return SynchronizedCollection::add(o);
  }
  foreign virtual bool addAll(IN(RCollection) c)
  {
    return SynchronizedCollection::addAll(c);
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    return SynchronizedCollection::equals(o);
  }
  foreign virtual bool removeAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::removeAll(c);
  }
  foreign virtual bool retainAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::retainAll(c);
  }
  foreign virtual int hashCode()
  {
    return SynchronizedCollection::hashCode();
  }
  foreign virtual ::acdk::lang::RObjectArray toArray()
  {
    return SynchronizedCollection::toArray();
  }
  foreign virtual RObjectArray toArray(IN(RObjectArray) array)
  {
    return SynchronizedCollection::toArray(array);
  }
  foreign virtual bool remove(IN(RObject) o)
  {
    return SynchronizedCollection::remove(o);
  }
  
};

ACDK_DECL_CLASS(SynchronizedSortedSet);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedSortedSet
: extends SynchronizedSet
, implements SortedSet
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedSortedSet)
private:
  RSortedSet _sortedSet;
public:
  SynchronizedSortedSet(IN(RSortedSet) ss, IN(RObject) sync = Nil) 
  : SynchronizedSet(RSet(ss), sync),
    _sortedSet(ss)
  {
  }
  foreign virtual int size()  
  {
    return SynchronizedCollection::size();
  }
  foreign virtual bool isEmpty()
  {  
    return SynchronizedCollection::isEmpty();
  }
  foreign virtual void clear()
  {
    SynchronizedCollection::clear();
  }
  foreign virtual bool contains(IN(RObject) o)
  {
    return SynchronizedCollection::contains(o);
  }
  foreign virtual bool containsAll(IN(RCollection) c)
  {
    return SynchronizedCollection::containsAll(c);
  }
  foreign virtual RIterator iterator()
  {
    return SynchronizedCollection::iterator();
  }
  foreign virtual bool add(IN(RObject) o)
  {
    return SynchronizedCollection::add(o);
  }
  foreign virtual bool addAll(IN(RCollection) c)
  {
    return SynchronizedCollection::addAll(c);
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    return SynchronizedCollection::equals(o);
  }
  foreign virtual bool removeAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::removeAll(c);
  }
  foreign virtual bool retainAll(IN(RCollection) c) 
  {
    return SynchronizedCollection::retainAll(c);
  }
  foreign virtual int hashCode()
  {
    return SynchronizedCollection::hashCode();
  }
  foreign virtual RObjectArray toArray()
  {
    return SynchronizedCollection::toArray();
  }
  foreign virtual RObjectArray toArray(IN(RObjectArray) array)
  {
    return SynchronizedCollection::toArray(array);
  }
  foreign virtual bool remove(IN(RObject) o)
  {
    return SynchronizedCollection::remove(o);
  }
  foreign virtual RComparator comparator() 
  {
    SYNCTHIS();
    return _sortedSet->comparator();
  }
  foreign virtual RObject first() 
  {
    SYNCTHIS();
    return _sortedSet->first();
  }
  foreign virtual RObject last() 
  {
    SYNCTHIS();
    return _sortedSet->last();
  }
  foreign virtual RSortedSet headSet(IN(RObject) toElement) 
  {
    SYNCTHIS();
    return new SynchronizedSortedSet(_sortedSet->headSet(toElement), _lock);
  }
  foreign virtual RSortedSet tailSet(IN(RObject) fromElement) 
  {
    SYNCTHIS();
    return new SynchronizedSortedSet(_sortedSet->tailSet(fromElement), _lock);
  }
  RSortedSet subSet(IN(RObject) fromElement, IN(RObject) toElement) 
  {
    SYNCTHIS();
    return new SynchronizedSortedSet(_sortedSet->subSet(fromElement, toElement), _lock);
  }
};


ACDK_DECL_CLASS(SynchronizedMap);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedMap
: extends acdk::lang::Object
, implements Map
, implements acdk::io::Serializable 
{
  ACDK_WITH_METAINFO(SynchronizedMap)
protected:
  RObject _lock;
  RMap _map;
public:
  static RObject create_instance() { return new SynchronizedMap(Nil, Nil); }
  SynchronizedMap(IN(RMap) map, IN(RObject) sync = Nil) 
  :  _lock(sync),
    _map(map)
  {
  }
  foreign virtual void lock()
  {
    if (_lock != Nil)
      _lock->lock();
    else
      Object::lock();
  }
  foreign virtual void unlock()
  {
    if (_lock != Nil)
      _lock->unlock();
    else
      Object::unlock();
  }
  
  foreign virtual void clear() 
  {
    SYNCTHIS();
    _map->clear();
  }
  foreign virtual bool containsKey(IN(RObject) key)
  {
    SYNCTHIS();
    return _map->containsKey(key);
  }
  foreign virtual bool containsValue(IN(RObject) value)
  {
    SYNCTHIS();
    return _map->containsValue(value);
  }
  foreign virtual RSet entrySet(); 
  foreign virtual bool equals(IN(RObject) o)
  {
    SYNCTHIS();
    return _map->equals(o);
  }
  foreign virtual RObject get(IN(RObject) key) 
  {
    SYNCTHIS();
    return _map->get(key);
  }
  foreign virtual RObject put(IN(RObject) key, IN(RObject) value) 
  {
    SYNCTHIS();
    return _map->put(key, value);
  }
  foreign virtual int hashCode()
  {
    SYNCTHIS();
    return _map->hashCode();
  }
  foreign virtual bool isEmpty()
  {
    SYNCTHIS();
    return _map->isEmpty();
  }
  foreign virtual RSet keySet() 
  {
    SYNCTHIS();
    return new SynchronizedSet(_map->keySet(), _lock);
  }
  foreign virtual void putAll(IN(RMap) map) 
  {
    SYNCTHIS();
    _map->putAll(map);
  }
  foreign virtual RObject remove(IN(RObject) o) 
  {
    SYNCTHIS();
    return _map->remove(o);
  }
  foreign virtual int size()
  {
    SYNCTHIS();
    return _map->size();
  }
  foreign virtual RCollection values() 
  {
    SYNCTHIS();
    return new SynchronizedCollection(_map->values(), _lock);
  }  
};


ACDK_DECL_CLASS(SynchronizedMapEntry);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedMapEntry
: extends acdk::lang::Object
, implements MapEntry
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedMapEntry)
private:
  RSynchronizedMap _map;  
  RMapEntry _entry;
public:
  SynchronizedMapEntry(IN(RSynchronizedMap) map, IN(RMapEntry) entry)
  : _map(map),
    _entry(entry)
  {
  }
  foreign virtual RObject getKey() 
  {
    SYNCHRONIZEOBJECT(_map);
    return _entry->getKey();
  }
  foreign virtual RObject getValue() 
  {
    SYNCHRONIZEOBJECT(_map);
    return _entry->getValue();
  }
  foreign virtual RObject setValue(IN(RObject) value) 
  {
    SYNCHRONIZEOBJECT(_map);
    return _entry->setValue(value);
  }
  foreign virtual int hashCode()
  {
    SYNCHRONIZEOBJECT(_map);
    return _entry->hashCode();
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    SYNCHRONIZEOBJECT(_map);
    return _entry->equals(o);
  }
};

ACDK_DECL_CLASS(SynchronizedMapEntySetIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedMapEntySetIterator
: extends SynchronizedIterator
{
  ACDK_WITH_METAINFO(SynchronizedMapEntySetIterator)
private:
  RSynchronizedMap _map;
  RIterator _it;
public:
  SynchronizedMapEntySetIterator(IN(RSynchronizedMap) map, IN(RIterator) it, IN(RObject) thelock)
  : SynchronizedIterator(it, thelock),
    _map(map)
  {
  }
  foreign virtual RObject next() 
  {
    RMapEntry e = RMapEntry(SynchronizedIterator::next());
    return new SynchronizedMapEntry(_map, e);
  }
};

ACDK_DECL_CLASS(SynchronizedMapEntySet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SynchronizedMapEntySet
: extends SynchronizedSet
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedMapEntySet)
protected:
  RObject _lock;
  RSynchronizedMap _map;
public:
  SynchronizedMapEntySet(IN(RSynchronizedMap) map, IN(RObject) thelock)
  : SynchronizedSet(map->entrySet(), thelock),
    _map(map)
  {
  }
  foreign virtual RIterator iterator() 
  {
    return new SynchronizedMapEntySetIterator(_map, _coll->iterator(), _lock);
  }

};


inline
//virtual 
RSet 
SynchronizedMap::entrySet()
{
  return new SynchronizedMapEntySet(this, _lock);
}

ACDK_DECL_CLASS(SynchronizedSortedMap);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
//obsolet: ACDK_CLASSATTRIBUTE(acdk.tools.mc.DmiProxyGenerator(:parentOnly = true))
class ACDK_CORE_PUBLIC SynchronizedSortedMap 
: extends SynchronizedMap
, implements SortedMap
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SynchronizedSortedMap)
private:
  //RSortedMap _sortedMap;
public:
  SynchronizedSortedMap(IN(RSortedMap) sm, IN(RObject) sync = Nil) 
  : SynchronizedMap(RMap(sm), sync)
  //, _sortedMap(sm)
  {
  }
  
  foreign virtual void clear() 
  {
    SynchronizedMap::clear();
  }
  foreign virtual bool containsKey(IN(RObject) key)
  {
    return SynchronizedMap::containsKey(key);
  }
  foreign virtual bool containsValue(IN(RObject) value)
  {
    return SynchronizedMap::containsValue(value);
  }
  foreign virtual RSet entrySet()
  {
    return SynchronizedMap::entrySet();
  }
  foreign virtual bool equals(IN(RObject) o)
  {
    return SynchronizedMap::equals(o);
  }
  foreign virtual RObject get(IN(RObject) key) 
  {
    return SynchronizedMap::get(key);
  }
  foreign virtual RObject put(IN(RObject) key, IN(RObject) value) 
  {
    return SynchronizedMap::put(key, value);
  }
  foreign virtual int hashCode()
  {
    return SynchronizedMap::hashCode();
  }
  foreign virtual bool isEmpty()
  {
    return SynchronizedMap::isEmpty();
  }
  foreign virtual RSet keySet() 
  {
    return SynchronizedMap::keySet();
  }
  foreign virtual void putAll(IN(RMap) map) 
  {
    SynchronizedMap::putAll(map);
  }
  foreign virtual RObject remove(IN(RObject) o) 
  {
    return SynchronizedMap::remove(o);
  }
  foreign virtual int size()
  {
    return SynchronizedMap::size();
  }
  foreign virtual RCollection values() 
  {
    return SynchronizedMap::values();
  }  
  foreign virtual RComparator comparator() 
  {
    SYNCTHIS();
    return RSortedMap(_map)->comparator();
  }
  foreign virtual RObject firstKey() 
  {
    SYNCTHIS();
      return RSortedMap(_map)->firstKey();
  }
  foreign virtual RObject lastKey() 
  {
    SYNCTHIS();
    return RSortedMap(_map)->lastKey();
  }
  foreign virtual RSortedMap headMap(IN(RObject) toKey) 
  {
    return new SynchronizedSortedMap(RSortedMap(_map)->headMap(toKey), _lock);
  }
  foreign virtual RSortedMap tailMap(IN(RObject) fromKey) 
  {
    return new SynchronizedSortedMap(RSortedMap(_map)->tailMap(fromKey), _lock);
  }
  foreign virtual RSortedMap subMap(IN(RObject) fromKey, IN(RObject) toKey) 
  {
    return new SynchronizedSortedMap(RSortedMap(_map)->subMap(fromKey, toKey), _lock);
  }
};


inline
//foreign virtual 
acdk::lang::RObjectArray 
SynchronizedCollection::toArray()
{
  SYNCTHIS();
  return _coll->toArray();
}

inline
//foreign virtual 
::acdk::lang::RObjectArray 
SynchronizedCollection::toArray(IN(::acdk::lang::RObjectArray) array)
{
  SYNCTHIS();
  return _coll->toArray(array);
}

inline
//foreign virtual 
RObjectArray 
SynchronizedList::toArray()
{
  return SynchronizedCollection::toArray();
}

inline
//foreign virtual 
RObjectArray 
SynchronizedList::toArray(IN(RObjectArray) array)
{
  return SynchronizedCollection::toArray(array);
}


} // util
} // acdk

#endif //acdk_util_SynchronizedCollections_h

