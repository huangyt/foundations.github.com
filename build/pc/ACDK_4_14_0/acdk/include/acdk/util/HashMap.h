// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*-
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

// Copyright (c) 1998 by Jon A. Zeppieri (jon@eease.com),
//                    Free Software Foundation, Inc.
// Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004
//                Free Software Foundation, Inc.

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/HashMap.h,v 1.27 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_HashMap_h
#define acdk_util_HashMap_h

#include "AbstractMap.h"
#include "Bucket.h"
#include <acdk/lang/Math.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

#include "ConcurrentModificationException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(HashMap);


enum HashMapSetType
{
  HMSTKeys = 0,
  HMSTValues,
  HMSTEntries
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, HashMapSetType);


typedef int (*ObjectHashFunc)(IN(RObject) obj);

inline int _standardHashFunc(IN(RObject) obj)
{
  return obj == Nil ? 0 : obj->hashCode();
}

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class HashMapNilEntry
: extends acdk::lang::Object
{
};

/**
  Implements a unsorted map using hashing
  Parts of this class are inspired from the GNU Classpath implementation
  
  @author of the orignal Classpath implementation: 
          Copyright (c) 1998 by Jon A. Zeppieri (jon@eease.com),
          Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004
                        Free Software Foundation, Inc.
                                                  
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class ACDK_CORE_PUBLIC HashMap
: extends AbstractMap,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(HashMap)
public:
  static RObject create_instance() { return new HashMap(); }

  static const int DEFAULT_CAPACITY;
  static const float DEFAULT_LOAD_FACTOR;
  //static HashMap.Nil Nil_KEY = new HashMap.Nil();

private:
  int _capacity;
  int _size;
  float _loadFactor;
  int _threshold;
  int _modCount;
  RBucketArray _buckets;
  foreign ObjectHashFunc _hashFunc;
public:
  HashMap(int initialCapacity = DEFAULT_CAPACITY, float initialLoadFactor = DEFAULT_LOAD_FACTOR);
  HashMap(IN(RMap) other);
  virtual ~HashMap();

  foreign virtual int size() { return _size; }
  foreign virtual bool isEmpty() { return _size == 0; }
  foreign virtual void clear();

  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alloc);
  foreign virtual RSet keySet();
  foreign virtual RSet entrySet();
  foreign virtual RCollection values();
  foreign virtual bool containsKey(IN(RObject) key) { return (_get(key) != Nil); }
  foreign virtual bool containsValue(IN(RObject) value);
  foreign virtual RObject get(IN(RObject) key)
  {
    RMapEntry res = _get(key);
    if (res == Nil)
      return Nil;
    return res->getValue();
  }
  foreign virtual RObject put(IN(RObject) key, IN(RObject) value)
  {
    return _put(key, value);
  }
  foreign virtual void putAll(IN(RMap) t);
  foreign virtual RObject remove(IN(RObject) key);

  int capacity() { return _capacity; }
  float loadFactor() { return _loadFactor; }

  foreign virtual RIterator iterator();
  foreign ObjectHashFunc get_hashFunc() { return _hashFunc; }
  foreign void set_hashFunc(ObjectHashFunc func) { _hashFunc = func; }
  friend class HashMapIterator;
  friend class HashMapSet;
  friend class HashMapKey;

private:
  void _init(int initialCapacity, float initialLoadFactor);
  inline int _hash(IN(RObject) key)
  {
    if (_capacity == 0)
      return 0;
#if defined(abs)
# undef abs
#endif
    return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Math)::abs(_hashFunc(key) % _capacity);
  }
  RObject _put(IN(RObject) key, IN(RObject) value);
  void _rehash();
  RMapEntry _get(IN(RObject) key);
  bool _containsEntry(IN(RMapEntry) entry);

  static RObject _nilEntry;
  static RObject nilEntry();
  // serial
  //void writeObject(ObjectOutputStream s)
  // void readObject(ObjectInputStream s)

};

ACDK_DECL_CLASS(HashMapKey);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class ACDK_CORE_PUBLIC HashMapKey
: extends BucketNode
, implements acdk::io::Serializable
{
  /** internal class */
  ACDK_WITH_METAINFO(HashMapKey)
public:
  HashMapKey(IN(RObject) key, IN(RObject) value)
  : BucketNode(key, value)
  {
  }
  foreign virtual RObject getKey()
  {
    RObject res = BucketNode::getKey();
    if (res == HashMap::nilEntry())
      return Nil;
    return res;
  }
};


ACDK_DECL_CLASS(HashMapIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class ACDK_CORE_PUBLIC HashMapIterator
: extends acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(HashMapIterator)
private:
  HashMapSetType _type;
  int _knownMods;
  int _position;
  int _bucketIndex;
  RBucketNode _currentNode;
  RObject _currentKey;
  RHashMap _hashMap;
public:
  HashMapIterator(IN(RHashMap) hashMap, HashMapSetType type)
  : _type(type),
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
  foreign virtual RObject next();
  foreign virtual void remove();
  foreign virtual RObject element();
private:
  void _checkMod()
  {
    if (_knownMods != _hashMap->_modCount)
      THROW0(ConcurrentModificationException);
  }
};

ACDK_DECL_CLASS(HashMapSet);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class ACDK_CORE_PUBLIC HashMapSet
: extends AbstractSet
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(HashMapSet)
private:
  RHashMap _hashMap;
  HashMapSetType _setType;
public:
  HashMapSet(IN(RHashMap) hashMap, HashMapSetType type)
  : AbstractSet(),
    _hashMap(hashMap),
    _setType(type)
  {
  }
  foreign virtual bool add(IN(RObject) o)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  foreign virtual bool addAll(IN(RCollection) c)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  foreign virtual void clear()
  {
    _hashMap->clear();
  }
  foreign virtual bool contains(IN(RObject) o)
  {
    if (_setType == HMSTKeys)
      return _hashMap->containsKey(o);
    if (instanceof(o, MapEntry) == false)
      return false;
    return _hashMap->_containsEntry((RMapEntry)o);
  }
  foreign virtual bool isEmpty()
  {
    return _hashMap->isEmpty();
  }
  foreign virtual bool remove(IN(RObject) o)
  {
    if (_setType == HMSTKeys)
      return (_hashMap->remove(o) != Nil);
    if (instanceof(o, MapEntry) == false)
      return false;
    return _hashMap->remove(((RMapEntry)o)->getKey()) != Nil;
  }
  foreign virtual int size()
  {
    return _hashMap->size();
  }
  foreign virtual RIterator iterator()
  {
    return new HashMapIterator(_hashMap, _setType);
  }
};


ACDK_DECL_CLASS(HashMapCollection);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/04/09 19:26:57 $

*/

class ACDK_CORE_PUBLIC HashMapCollection
: extends AbstractCollection
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(HashMapCollection)
private:
  RHashMap _hashMap;
public:
  HashMapCollection(IN(RHashMap) hashMap)
  : AbstractCollection(),
    _hashMap(hashMap)
  {
  }
  foreign virtual bool add(IN(RObject) o)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  foreign virtual bool addAll(IN(RCollection) c)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  foreign virtual void clear()
  {
    _hashMap->clear();
  }
  foreign virtual bool contains(IN(RObject) o)
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
  foreign virtual RIterator iterator()
  {
    return new HashMapIterator(_hashMap, HMSTValues);
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

} // util
} // acdk

#endif // acdk_util_HashMap_h

