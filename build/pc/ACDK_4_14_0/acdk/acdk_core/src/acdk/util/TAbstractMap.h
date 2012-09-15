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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractMap.h,v 1.15 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TAbstractMap_h
#define acdk_util_TAbstractMap_h

#include "TSet.h"
#include "TMap.h"
#include "TAbstractSet.h"
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

//ACDK_DECL_CLASS(TAbstractMap);

template <class K, class V> class TAbstractMapCachedKeySetIterator;
template <class K, class V> class TAbstractMapCachedKeySet;
template <class K, class V> class TAbstractMapCachedValuesCollection;
template <class K, class V> class TAbstractMapCachedValuesCollectionIterator;

/**
  @internal
  Basic abstract implementation of a Map.
  @see acdk::util::TMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TAbstractMap
: extends acdk::lang::Object
, implements TMap<K, V>
{
  
public:
  
  typedef K RKeyType;
  typedef V RValueType;
  typedef TAbstractMap<K, V> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef TAbstractMap<RKeyType, RValueType> AbstractSetType;
  typedef RefHolder<AbstractSetType> RAbstractSetType;
  typedef RAbstractSetType RefType;
  

  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef TIterator<RKeyType> KeyIteratorType;
  typedef typename KeyIteratorType::RefType RKeyIteratorType;

  typedef TIterator<RValueType> ValueIteratorType;
  typedef typename ValueIteratorType::RefType RValueIteratorType;

  typedef TCollection<RValueType> ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;

  
  typedef TMapEntry<RKeyType, RValueType> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef typename MapType::MapEntrySetType MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;

  typedef typename MapType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;
  
  typedef TIterator<RMapEntryType> MapEntryIteratorType;
  typedef typename MapEntryIteratorType::RefType RMapEntryIteratorType;

  TAbstractMap()
  : Object()
  {
  }
  foreign virtual void clear()
  {
    entrySet()->clear();
  }
  foreign virtual bool containsKey(IN(RKeyType) key)
  {
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    while (entries->hasNext() ==  true) 
    {
      RKeyType k = entries->next()->getKey();
      // old: if (key == Nil ? k == Nil : key->equals(k))
      if (acdk_equals(key, k) == true)
        return true;
    }
    return false;
  }
  foreign virtual bool containsValue(IN(RValueType) value)
  {
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    while (entries->hasNext() ==  true) 
    {
      RValueType v = entries->next()->getValue();
      if (acdk_equals(value, v) == true)
      //if (value == Nil ? v == Nil : value->equals(v))
        return true;
    }
    return false; 
  }
  foreign virtual RMapEntrySetType entrySet() = 0;
  foreign virtual bool equals(IN(RObject) o)
  {
    if (o.impl() == this)
      return true;
    if (o == Nil || instanceof(o, MapType) == false)
      return false;
    RMapType m = RMapType(o);
    if (m->size() != size())
      return false;
    
    RMapEntryType entry;
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    while (entries->hasNext() ==  true) 
    {
      entry = entries->next();
      RKeyType key = entry->getKey();
      RValueType value1 = entry->getValue();
      RValueType value2 = m->get(key);
      //old: if (((value1 == Nil && value2 == Nil) || value1->equals(value2)) == false)
      if (acdk_equals(value1, value2) == false)
        return false;
      

    }
    return true;    
  }
  foreign virtual RValueType get(IN(RKeyType) key)
  {
    RMapEntryType entry;
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    
    while (entries->hasNext()) 
    {
      entry = entries->next();
      RKeyType k = entry->getKey();
      // old: if ((key == Nil ? k == Nil : key->equals(k) == true))
      if (acdk_equals(key, k) == true)
        return entry->getValue();
    }
    return Nil;
  }
  foreign virtual int hashCode()
  {
    int hashcode = 0;
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    RMapEntryType o;
    while (entries->hasNext() == true) 
    {
      o = entries->next();
      if (o != RThisType(this))
        hashcode += o->hashCode();
    }
    return hashcode;
  }
  foreign virtual bool isEmpty()
  {
    return size() == 0;
  }
  foreign virtual RKeySetType keySet()
  {
    return new TAbstractMapCachedKeySet<RKeyType, RValueType>(this);
  }
  foreign virtual RValueType put(IN(RKeyType) key, IN(RValueType) value)
  {
    THROW0(UnsupportedOperationException);
    return Nil;
  }

  foreign virtual void putAll(IN(RMapType) m)
  {
    RMapEntryType entry;
    typename MapEntrySetType::RIteratorType entries = m->entrySet()->iterator();
    while (entries->hasNext()) 
    {
      entry = entries->next();
      put(entry->getKey(), entry->getValue());
    }
  }
  foreign virtual RValueType remove(IN(RKeyType) key)
  {
    RMapEntryType entry;
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    while (entries->hasNext() == true) 
    {
      entry = entries->next();
      RKeyType k = entry->getKey();
      // old: if (key == Nil ? k == Nil : key->equals(k) == true) 
      if (acdk_equals(key, k) == true)
      {
        RValueType value = entry->getValue();
        entries->remove();
        return value;
      }
    }
    return Nil;    
  }
  foreign virtual int size()
  {
     return entrySet()->size();
  }
  foreign virtual RString toString()
  {
    RMapEntryType entry;
    StringBuffer rep(1024);
    rep.append("AbstractMap< ");
    typename MapEntrySetType::RIteratorType entries = entrySet()->iterator();
    while (entries->hasNext() == true) 
    {
      entry = entries->next();
      rep.append(acdk_toString(entry->getKey()));
      rep.append(" -> ");
      rep.append(acdk_toString(entry->getValue()));
      rep.append(", ");
    }
    rep.append(" >");
    return rep.toString();
  }
  foreign virtual RValueCollectionType values();

  //friend template <class T> class TAbstractMapCachedKeySetIterator;
};


//ACDK_DECL_CLASS(TAbstractMapCachedKeySet);

/**
  @internal
  
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TAbstractMapCachedKeySet
:  extends TAbstractSet<K>
{
  
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TAbstractMap<RKeyType, RValueType> AbstractMapType;
  typedef typename AbstractMapType::RefType RAbstractMapType;

  typedef TIterator<RKeyType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
private:
  RAbstractMapType _map;
public:
  
  TAbstractMapCachedKeySet(IN(RAbstractMapType) map)
  : _map(map)
  {
  }
  foreign virtual bool isEmpty() { return size() == 0; }

  foreign virtual int size() { return _map->size(); }
  foreign virtual RIteratorType iterator();

};



//ACDK_DECL_CLASS(TAbstractMapCachedKeySetIterator);

/**
  @internal
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/

template <class K, class V>
class TAbstractMapCachedKeySetIterator
: extends acdk::lang::Object
, implements TIterator<K>
{
  
public:
  typedef K RKeyType;
  typedef V RValueType;
  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef TAbstractMap<RKeyType, RValueType> AbstractMapType;
  typedef typename AbstractMapType::RefType RAbstractMapType;

  typedef TMapEntry<RKeyType, RValueType> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef TIterator<RMapEntryType> MapEntryIteratorType;
  typedef typename MapEntryIteratorType::RefType RMapEntryIteratorType;
private:
  RMapEntryIteratorType _it;
public:
  TAbstractMapCachedKeySetIterator(INP(RMapType) map)
  : _it(map->entrySet()->iterator())
  {
  }
  foreign virtual bool hasNext() 
  {
    return _it->hasNext(); 
  }
  
  foreign virtual RKeyType next() 
  {
    RMapEntryType me = _it->next();
    return me->getKey(); 
  }
  foreign virtual RKeyType element() 
  {
    return _it->element()->getKey(); 
  }
  foreign virtual void remove() { _it->remove(); }
};

template <class K, class V>
typename TAbstractMapCachedKeySet<K, V>::RIteratorType 
TAbstractMapCachedKeySet<K, V>::iterator()
{
  return new TAbstractMapCachedKeySetIterator<RKeyType, RValueType>(&_map);
}

//ACDK_DECL_CLASS(TAbstractMapCachedValuesCollection);

/**
  @internal
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TAbstractMapCachedValuesCollection
: extends TAbstractCollection<V>
{
  
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef TAbstractMapCachedValuesCollection<RKeyType, RValueType> ThisCollectionType;
  typedef RefHolder<ThisCollectionType> RThisCollectionType;
  
  typedef TAbstractMap<RKeyType, RValueType> AbstractMapType;
  typedef typename AbstractMapType::RefType RAbstractMapType;

  typedef TMapEntry<RKeyType, RValueType> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef TSet<RMapEntryType> MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;

  //typedef TIterator<RMapEntryType> MapEntryIteratorType;
  //typedef typename MapEntryIteratorType::RefType RMapEntryIteratorType;
  
  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
private:
  RMapType _map;
public:
  RMapType theMap() { return _map; }
  TAbstractMapCachedValuesCollection(IN(RMapType) map)
  : _map(map)
  {
  }
  foreign virtual int size() 
  {
    return _map->size(); 
  }
  foreign virtual RMapEntrySetType entrySet() 
  {
    return _map->entrySet(); 
  }
  foreign virtual RIteratorType iterator();
// collection
  foreign bool equals(IN(RObject) o) 
  { 
    if (instanceof(o, ThisCollectionType) == false)
      return false;
    return _map->equals((RObject)RThisCollectionType(o)->theMap());
  }
  foreign int hashCode() { return _map->hashCode(); }
  
};

template <typename K, typename V>
typename TAbstractMap<K, V>:: RValueCollectionType 
TAbstractMap<K, V> ::values()
{
  return new TAbstractMapCachedValuesCollection<RKeyType, RValueType>(this);
}

/**
  @internal
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TAbstractMapCachedValuesCollectionIterator
: extends  acdk::lang::Object
, implements TIterator<V>
{
  
public:
  typedef K RKeyType;
  typedef V RValueType;
  typedef TAbstractMap<RKeyType, RValueType> AbstractMapType;
  typedef typename AbstractMapType::RefType RAbstractMapType;
  
  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef typename AbstractMapType::MapEntryType MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef TIterator<RMapEntryType> MapEntryIteratorType;
  typedef typename MapEntryIteratorType::RefType RMapEntryIteratorType;
  

private:
  RMapEntryIteratorType _it;
public:
  TAbstractMapCachedValuesCollectionIterator(IN(RMapType) map)
  : Object(),
    _it(map->entrySet()->iterator())
  {
  }
  foreign virtual bool hasNext() 
  {
    return _it->hasNext(); 
  }
  foreign virtual RValueType next() 
  {
    return _it->next()->getValue();
  }
  foreign virtual RValueType element() 
  {
    return _it->element()->getValue(); 
  }
  foreign virtual void remove() 
  {
    _it->remove(); 
  }
};

template <class K, class V>
typename TAbstractMapCachedValuesCollection<K, V>::RIteratorType 
TAbstractMapCachedValuesCollection<K, V>::iterator()
{
  return new TAbstractMapCachedValuesCollectionIterator<RKeyType, RValueType>(&_map);
}

} // util
} // acdk

#endif //acdk_util_TAbstractMap_h


