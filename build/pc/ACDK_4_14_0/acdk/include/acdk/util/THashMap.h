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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/THashMap.h,v 1.21 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_THashMap_h
#define acdk_util_THashMap_h

#include "HashMap.h"

#include "TAbstractMap.h"
#include "TBucket.h"
#include "NoSuchElementException.h"

#include <acdk/lang/Math.h>
#include <acdk/lang/Error.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

#include "ConcurrentModificationException.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/ExtObject.h>
#include <acdk/lang/IllegalStateException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;  


template <class K, class V> class THashMapValuesCollection;

/**
  @internal
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class THashMapNilEntry 
: extends acdk::lang::Object
{
};


/**
  defines a concret hashmap with key and value types
*/
#define ACDK_DECL_HASHMAP(Type1, RType1, Type2, RType2) \
  ACDK_DECL_MAP(Type1, RType1, Type2, RType2); \
  typedef ::acdk::util::THashMap<RType1, RType2> Type1##To##Type2##HashMap; \
  typedef Type1##To##Type2##HashMap::RefType R##Type1##To##Type2##HashMap

/**
  A unsorted typed Map implemented via HashCodes.
  The elements stored in a THashMap should implement the method
  hashCode(). 

  @see @ref tmap
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/HashMap.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class THashMap
: extends TAbstractMap<K, V>
, implements acdk::lang::Cloneable
, implements acdk::io::Serializable
{
  DECL_ACDK_DEFAULT_METACLASS(Object)
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> ThisContainerType;
  typedef RefHolder<ThisContainerType> RThisContainerType;
  typedef THashMap<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisContainerType RefType;

  typedef TBucket<RKeyType, RValueType> BucketType;
  typedef typename BucketType::RefType RBucketType;

  typedef TBucketNode<RKeyType, RValueType> BucketNodeType;
  typedef typename BucketNodeType::RefType RBucketNodeType;

  typedef ObjectArrayImpl<RBucketType> BucketArrayType;
  typedef RObjectArrayImpl<RBucketType> RBucketArrayType;
  
  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef typename MapType::MapEntryType MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef typename MapType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;

  typedef typename MapType::MapEntrySetType MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;
  
  typedef typename MapType::ValueCollectionType ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;
  

  typedef TIterator<RMapEntryType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
  
  
public:
  static RObject create_instance() { return new THashMap(); }

  //static THashMap.Nil Nil_KEY = new THashMap.Nil();

friends_private:
  int _capacity;
  int _size;
  float _loadFactor;
  int _threshold;
  int _modCount;
  RBucketArrayType _buckets;

public:
  THashMap(int initialCapacity = HashMap::DEFAULT_CAPACITY, float initialLoadFactor = HashMap::DEFAULT_LOAD_FACTOR)
  : _capacity(0),
    _size(0),
    _loadFactor(0),
    _threshold(0),
    _modCount(0)
  {
    if (initialCapacity < 0 || initialLoadFactor <= 0 || initialLoadFactor > 1) {
    
      THROW1_FQ(::acdk::lang::, IllegalArgumentException, 
          SBSTR("wrong initial values: intialCapacity=[" << initialCapacity << "]; initialLoadFactor=["
          << initialLoadFactor << "];"));
    }
    _init(initialCapacity, initialLoadFactor);
  }
  THashMap(IN(RMapType) other)
  : _capacity(0),
    _size(0),
    _loadFactor(0),
    _threshold(0),
    _modCount(0)
  {
    int mapSize = other->size() * 2;
    _init(((mapSize > HashMap::DEFAULT_CAPACITY) ? mapSize : HashMap::DEFAULT_CAPACITY), HashMap::DEFAULT_LOAD_FACTOR);
    putAll(other);
  }

  foreign virtual int size() { return _size; }
  foreign virtual bool isEmpty() { return _size == 0; }
  foreign virtual void clear()
  {
    _size = 0;
    _modCount++;
    _buckets = new (Object::allocator()) BucketArrayType(_capacity);
  }

  foreign virtual RObject clone() { return clone(Object::allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc)
  {
    typename MapEntrySetType::RIteratorType it = entrySet()->iterator();
    RThisContainerType erg = new (alc) ThisContainerType(_capacity, _loadFactor);
    RMapEntryType entry;
    while (it->hasNext() == true) 
    {
      entry = it->next();
      erg->_put(entry->getKey(), entry->getValue());
    }
    return (RObject)erg;
  }

  foreign virtual RKeySetType keySet();
  foreign virtual RMapEntrySetType entrySet();
  foreign virtual RValueCollectionType values();
  foreign virtual bool containsKey(INP(RKeyType) key) 
  { 
    return (_get(key) != Nil);
  }
  foreign virtual bool containsValue(INP(RValueType) value)
  {
    int i;
    RBucketType list;
    for (i = 0; i < _capacity; i++) {
      list = _buckets[i];
      if (list != Nil && list->containsValue(value))
        return true;
    }
    return false;
  }
  /**
    @todo definition of nil by T = int
  */
  foreign virtual RValueType get(INP(RKeyType) key) 
  {
    RMapEntryType res = _get(key);
    if (res == Nil)
      return Nil;
    return res->getValue();
  }
  foreign virtual RValueType put(IN(RKeyType) key, IN(RValueType) value)
  {
    return _put(key, value);
  }
  foreign virtual void putAll(IN(RMapType) t)
  {
    RMapEntryType entry;
    typename MapEntrySetType::RIteratorType it = t->entrySet()->iterator();
    while (it->hasNext() == true) 
    {
      entry = it->next();
      put(entry->getKey(), entry->getValue());
    }
  }
  /**
    @todo definition of nil by T = int
  */
  foreign virtual RValueType remove(INP(RKeyType) key)
  {
    if (_size <= 0)
      return Nil;
    RBucketType list;
    int index;
    RValueType result = Nil;
    if (acdk_isNil(key) == true)
      return Nil;
    index = _hash(key == Nil ? nilEntry() : key);
    list = _buckets[index];
    if (list != Nil) {
      result = list->removeByKey(key);
      if (acdk_isNil(result) == false) 
      {
        _size--;
        _modCount++;
        if (list->_first == Nil)
          _buckets[index] = Nil;
      }
    }
    return result;
  }
  
  int capacity() { return _capacity; }
  float loadFactor() { return _loadFactor; }

  foreign virtual RIteratorType iterator();

  //friend class THashMapIterator;
  //friend class THashMapSet;
  //friend class THashMapKey;

friends_private:
  void _init(int initialCapacity, float initialLoadFactor)
  {
    _capacity = initialCapacity;
    _loadFactor = initialLoadFactor;
    _threshold = (int) ((float) _capacity * _loadFactor);
    _buckets = new (Object::allocator()) BucketArrayType(_capacity);
  }
  template <typename OT>
  int _hash(INP(OT) t) 
  {
    if (_capacity == 0)
      return 0;
    int h = acdk_hashCode(t) % _capacity;
    return h < 0 ? -h : h;
  }
  /**
    @todo definition of nil by T = int
  */
  RValueType _put(INP(RKeyType) k, INP(RValueType) value)
  {
    RKeyType key = k;
    
    _modCount++;
    if (_size == _threshold)
      _rehash();
    key = (key == Nil) ? nilEntry() : key;
    
    RMapEntryType oldentry = _get(key);
    if (oldentry != Nil) 
    {
      RValueType oldvalue = oldentry->getValue();
      oldentry->setValue(value);
      return oldvalue;
    } 
    RBucketNodeType entry = new (Object::allocator()) BucketNodeType(key, value);
    int hashIndex = _hash(key);
    RBucketType list = _buckets[hashIndex];
    if (list == Nil) 
    {
      list = new (Object::allocator()) BucketType();
      _buckets[hashIndex] = list;
    }
    RBucketNodeType res = list->add(entry);
    if (res == Nil) 
    {
      _size++;
      return Nil;
    } 
    THROW1_FQ(::acdk::lang::, Error, "Unexpected state in THashMap");
    return Nil;//res->getValue();
  }
  void _rehash()
  {
    int i;
    RBucketArrayType data = _buckets;
    _modCount++;
    _capacity = (_capacity * 2) + 1;
    _size = 0;
    _threshold = (int) ((float) _capacity * _loadFactor);
    _buckets = new (Object::allocator()) BucketArrayType(_capacity);
    RBucketNodeType node;
    for (i = 0; i < data->length(); i++) 
    {
      if (data[i] != Nil) {
        node = data[i]->_first;
        while (node != Nil) 
        {
          _put(node->getKey(), node->getValue());
          node = node->next();
        }
      }
    }
  }
  RMapEntryType _get(IN(RKeyType) key)
  {
    if (_size == 0)
      return Nil;
    if (key == Nil)
      return Nil;
    
    RBucketType list = _buckets[_hash(key == Nil ? nilEntry() : key)];
    return (list == Nil) ? RMapEntryType(Nil) : RMapEntryType(list->getEntryByKey(key));
  }
  bool _containsEntry(IN(RMapEntryType) entry)
  {
    if (entry == Nil)
      return false;
    RMapEntryType oInternalEntry = _get(entry->getKey());
    return (oInternalEntry != Nil && oInternalEntry->equals(entry));
  }
  
  RKeyType nilEntry()
  {
    static RKeyType _nilEntry = new typename RKeyType::Type();
    return _nilEntry;
  }
};

/*
//ACDK_DECL_CLASS(THashMapKey);


template <class K, class V>
class THashMapKey
: extends TBucketNode<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> HashMapType;
public:
  THashMapKey(IN(RKeyType) key, IN(RValueType) value)
  : BucketNode(key, value)
  {
  }
  foreign virtual RKeyType getKey()
  {
    RKeyType res = BucketNode::getKey();
    if (res == HashMapType::nilEntry())
      return Nil;
    return res;
  }
};

*/


/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class THashMapKeyIterator 
: extends acdk::lang::Object,
  implements TIterator<K>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;

  
  typedef typename HashMapType::BucketNodeType BucketNodeType;
  typedef typename HashMapType::RBucketNodeType RBucketNodeType;

  typedef typename HashMapType::BucketType BucketType;
  typedef typename HashMapType::RBucketType RBucketType;

public:
  int _knownMods;
  int _position;
  int _bucketIndex;
  RBucketNodeType _currentNode;
  RKeyType _currentKey;
  RHashMapType _hashMap;
public:
  THashMapKeyIterator(IN(RHashMapType) hashMap)
  : Object(),
    _knownMods(hashMap->_modCount),
    _position(0),
    _bucketIndex(-1),
    _currentNode(Nil),
    _currentKey(Nil),
    _hashMap(hashMap)
  {
  }
  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _hashMap->size();
  }
  foreign virtual RKeyType next()
  {
    RBucketType list = Nil;
    RKeyType result;
    _checkMod();      
    try {
      while (_currentNode == Nil) 
      {
        while (list == Nil)
          list = _hashMap->_buckets[++_bucketIndex];
        _currentNode = list->first();
      }
      _currentKey = _currentNode->getKey();
      result = _currentKey;
      _currentNode = _currentNode->next();
    } catch (RException) {
      THROW0_FQ(::acdk::util::, NoSuchElementException);
    }
    _position++;
    return result;
  }
  foreign virtual void remove()
  {
    _checkMod();
    if (acdk_isNil(_currentKey) == true) 
      THROW0_FQ(::acdk::lang::, IllegalStateException);
    _hashMap->remove(_currentKey);
    _knownMods++;
    _currentKey = Nil;
  }
  foreign virtual RKeyType element()
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);  
    return RKeyType();
  }
private:
  void _checkMod()  
  {
    if (_knownMods != _hashMap->_modCount)
      THROW0_FQ(::acdk::util::, ConcurrentModificationException);
  }
};


/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class THashMapValueIterator 
: extends acdk::lang::Object,
  implements TIterator<V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;

  typedef typename HashMapType::BucketNodeType BucketNodeType;
  typedef typename HashMapType::RBucketNodeType RBucketNodeType;

  typedef typename HashMapType::BucketType BucketType;
  typedef typename HashMapType::RBucketType RBucketType;
public:
  int _knownMods;
  int _position;
  int _bucketIndex;
  RBucketNodeType _currentNode;
  RKeyType _currentKey;
  RHashMapType _hashMap;
public:
  THashMapValueIterator(IN(RHashMapType) hashMap)
  : Object(),
    _knownMods(hashMap->_modCount),
    _position(0),
    _bucketIndex(-1),
    _currentNode(Nil),
    _currentKey(Nil),
    _hashMap(hashMap)
  {
  }
  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _hashMap->size();
  }
  foreign virtual RValueType next()
  {
    RBucketType list = Nil;
    RValueType result;
    _checkMod();      
    try {
      while (_currentNode == Nil) 
      {
        while (list == Nil)
          list = _hashMap->_buckets[++_bucketIndex];
        _currentNode = list->first();
      }
      _currentKey = _currentNode->getKey();
      result = _currentNode->getValue();
      _currentNode = _currentNode->next();
    } catch (acdk::lang::RException) {
      THROW0_FQ(::acdk::util::, NoSuchElementException);
    }
    _position++;
    return result;
  }
  foreign virtual void remove()
  {
    _checkMod();
    if (acdk_isNil(_currentKey) == true) 
      THROW0_FQ(::acdk::lang::, IllegalStateException);
    _hashMap->remove(_currentKey);
    _knownMods++;
    _currentKey = Nil;
  }
  foreign virtual RValueType element()
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);  
    return RValueType();
  }
private:
  void _checkMod()  
  {
    if (_knownMods != _hashMap->_modCount)
      THROW0_FQ(::acdk::util::, ConcurrentModificationException);
  }
};


/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class THashMapEntriesIterator 
: extends acdk::lang::Object
, implements TIterator<typename THashMap<K, V>::RMapEntryType>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;

  typedef typename HashMapType::BucketType BucketType;
  typedef typename HashMapType::RBucketType RBucketType;
  
  typedef typename HashMapType::BucketNodeType BucketNodeType;
  typedef typename HashMapType::RBucketNodeType RBucketNodeType;

  typedef typename HashMapType::RMapEntryType RMapEntryType;
  
public:
  int _knownMods;
  int _position;
  int _bucketIndex;
  RBucketNodeType _currentNode;
  RKeyType _currentKey;
  RHashMapType _hashMap;
public:
  THashMapEntriesIterator(IN(RHashMapType) hashMap)
  : Object(),
    _knownMods(hashMap->_modCount),
    _position(0),
    _bucketIndex(-1),
    _currentNode(Nil),
    _currentKey(Nil),
    _hashMap(hashMap)
  {
  }
  foreign virtual bool hasNext()
  {
    _checkMod();
    return _position < _hashMap->size();
  }
  foreign virtual RMapEntryType next()
  {
    RMapEntryType result;
    RBucketType list = Nil;
    _checkMod();
    try {
      while (_currentNode == Nil) 
      {
        while (list == Nil)
          list = _hashMap->_buckets[++_bucketIndex];
        _currentNode = list->first();
      }
      _currentKey = _currentNode->getKey();
      result = (RMapEntryType)_currentNode;
      _currentNode = _currentNode->next();
    } catch (acdk::lang::RException) {
      THROW0_FQ(::acdk::util::, NoSuchElementException);
    }
    _position++;
    return result;
  }
  foreign virtual void remove()
  {
    _checkMod();
    if (acdk_isNil(_currentKey) == true) 
      THROW0_FQ(::acdk::lang::, IllegalStateException);
    _hashMap->remove(_currentKey);
    _knownMods++;
    _currentKey = Nil;
  }
  foreign virtual RMapEntryType element()
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);  
    return Nil;
  }
private:
  void _checkMod()  
  {
    if (_knownMods != _hashMap->_modCount)
      THROW0_FQ(::acdk::util::, ConcurrentModificationException);
  }
};

/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class THashMapEntriesSet 
: extends TAbstractSet<typename TMapEntry<K, V>::RefType > 
{
public:
  typedef K RHashMapKeyType;
  typedef V RHashMapValueType;

  typedef TMapEntry<RHashMapKeyType, RHashMapValueType> ValueType;
  typedef typename ValueType::RefType RValueType;

  typedef THashMapEntriesSet<RHashMapKeyType, RHashMapValueType> ThisCollectionType;
  typedef RefHolder<ThisCollectionType> RThisCollectionType;
  typedef RThisCollectionType RefType;

  typedef THashMap<RHashMapKeyType, RHashMapValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef typename CollectionType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
private:
  RHashMapType _hashMap;
public:
  THashMapEntriesSet(IN(RHashMapType) hashMap)
  : _hashMap(hashMap)
  {
  }
  foreign virtual bool add(IN(RValueType) o) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual bool addAll(IN(RCollectionType) c) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual void clear()
  {
    _hashMap->clear();
  }
  foreign virtual bool contains(IN(RValueType) o)
  {
    return _hashMap->_containsEntry(o);
  }
  foreign virtual bool isEmpty()
  {
    return _hashMap->isEmpty();
  }
  foreign virtual bool remove(IN(RValueType) o)
  {
    return acdk_isNil(_hashMap->remove((o)->getKey())) == false;
  }
  foreign virtual int size()
  {
    return _hashMap->size();
  }
  foreign virtual RIteratorType iterator()
  {
    return new THashMapEntriesIterator<RHashMapKeyType, RHashMapValueType>(_hashMap);
  }
};

/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class THashMapKeySet 
: extends TAbstractSet<K>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef THashMap<RKeyType, RValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;
  
  typedef THashMapKeySet<RKeyType, RValueType> ThisCollectionType;
  typedef RefHolder<ThisCollectionType> RThisCollectionType;
  typedef RThisCollectionType RefType;

  typedef TCollection<RKeyType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;

  typedef typename CollectionType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

private:
  RHashMapType _hashMap;
public:
  THashMapKeySet(IN(RHashMapType) hashMap)
  : _hashMap(hashMap)
  {
  }
  foreign virtual bool add(IN(RKeyType) o) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual bool addAll(IN(RCollectionType) c) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual void clear()
  {
    _hashMap->clear();
  }
  foreign virtual bool contains(IN(RKeyType) o)
  {
    return _hashMap->containsKey(o);
  }
  foreign virtual bool isEmpty()
  {
    return _hashMap->isEmpty();
  }
  foreign virtual bool remove(IN(RKeyType) o)
  {
    return (acdk_isNil(_hashMap->remove(o)) == false);
  }
  foreign virtual int size()
  {
    return _hashMap->size();
  }
  foreign virtual RIteratorType iterator()
  {
    return new THashMapKeyIterator<RKeyType, RValueType>(_hashMap);
  }
};




/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class THashMapValuesCollection
: extends TAbstractCollection<V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef THashMap<RKeyType, RValueType> HashMapType;
  typedef typename HashMapType::RefType RHashMapType;
  typedef THashMapValuesCollection<RKeyType, RValueType> ThisCollection;
  typedef RefHolder<ThisCollection> RThisCollection;

  // defined in Collection
  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef typename CollectionType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
private:
  RHashMapType _hashMap;
public:
  THashMapValuesCollection(IN(RHashMapType) hashMap)
  : _hashMap(hashMap)
  {
  }
  foreign virtual bool add(IN(RValueType) o) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual bool addAll(IN(RCollectionType) c) 
  {
    THROW0_FQ(::acdk::lang::, UnsupportedOperationException);
    return false;
  }
  foreign virtual void clear()
  {
    _hashMap->clear();
  }
  foreign virtual bool contains(IN(RValueType) o)
  {
    return _hashMap->containsValue(o);
  }
  foreign virtual bool isEmpty()
  {
    return _hashMap->isEmpty();
  }
  foreign virtual int size()
  {
    return _hashMap->size();
  }
  foreign virtual RIteratorType iterator()
  {
    return new THashMapValueIterator<RKeyType, RValueType>(_hashMap);
  }
  foreign virtual bool equals(IN(RObject) c)  
  {
    return _hashMap->equals(c);
  }
  foreign virtual int hashCode()
  {
    return _hashMap->hashCode();
  }
};

template <class K, class V>
typename THashMap<K, V>::RKeySetType
THashMap<K, V>::keySet()
{
  typedef THashMapKeySet<K, V> HashMapKeySetType;
  // (typename HashMapKeySetType::RHashMapType)
  return new (Object::allocator()) HashMapKeySetType(this);
}

template <class K, class V>
typename THashMap<K, V>::RMapEntrySetType 
THashMap<K, V>::entrySet()
{
  return new (Object::allocator()) THashMapEntriesSet<RKeyType, RValueType>(this);
}

template <class K, class V>
typename THashMap<K, V>::RValueCollectionType 
THashMap<K, V>::values()
{
  typedef THashMapValuesCollection<RKeyType, RValueType> HashMapValuesCollectionType;
  return new (Object::allocator()) HashMapValuesCollectionType((typename HashMapValuesCollectionType::RHashMapType)this);
}

template <class K, class V>
typename THashMap<K, V>::RIteratorType
THashMap<K, V>::iterator()
  {
    typedef THashMapEntriesIterator<RKeyType, RValueType> LocalHashMapEntriesIteratorType;
    return new (Object::allocator()) LocalHashMapEntriesIteratorType(this);
  }

} // util
} // acdk

#endif // acdk_util_THashMap_h

