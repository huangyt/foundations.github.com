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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/THashSet.h,v 1.3 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_THashSet_h
#define acdk_util_THashSet_h

#include "THashMap.h"
#include "TAbstractSet.h"

namespace acdk {
namespace util {



/**
  Helper macro to define typed hashset
  ACDK_DECL_SET(String, RString);
  introduces the names StringHashSet, RStringHashSet.
*/
#define ACDK_DECL_HASHSET(Type1, RType1) \
  ACDK_DECL_SET(Type1, RType1); \
  typedef ::acdk::util::THashSet<RType1> Type1##HashSet; \
  typedef Type1##To##Type2##HashMap::RefType R##Type1##HashSet

/**
  A unsorted typed Set implemented via HashCodes.
  The elements stored in a THashSet should implement the method
  hashCode(). 

  @see @ref tmap
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/HashMap.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K>
class THashSet
: extends TAbstractSet<K>
{
  DECL_ACDK_DEFAULT_METACLASS(Object)
public:
  typedef K RKeyType;
  typedef RKeyType RValueType;

  typedef RObject RMapValueType;

  typedef THashSet<RKeyType> ThisContainerType;
  typedef RefHolder<ThisContainerType> RThisContainerType;
  typedef THashSet<RKeyType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisContainerType RefType;

  typedef THashMap<RKeyType, RMapValueType> HashMapType;

  typedef TBucket<RKeyType, RMapValueType> BucketType;
  typedef typename BucketType::RefType RBucketType;

  typedef TBucketNode<RKeyType, RMapValueType> BucketNodeType;
  typedef typename BucketNodeType::RefType RBucketNodeType;

  typedef ObjectArrayImpl<RBucketType> BucketArrayType;
  typedef RObjectArrayImpl<RBucketType> RBucketArrayType;
  
  typedef TMap<RKeyType, RMapValueType> MapType;
  typedef typename MapType::RefType RMapType;

  typedef typename MapType::MapEntryType MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  typedef typename MapType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;

  typedef typename MapType::MapEntrySetType MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;
  
  typedef typename MapType::ValueCollectionType ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;
  

  typedef typename TAbstractSet<K>::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
  
  
public:
  static RObject create_instance() { return new THashSet(); }

  HashMapType _map;

public:
  THashSet(int initialCapacity = HashMap::DEFAULT_CAPACITY, float initialLoadFactor = HashMap::DEFAULT_LOAD_FACTOR)
  : _map(initialCapacity, initialLoadFactor)
  {
  }
  
  virtual int size() { return _map.size(); }
  virtual bool isEmpty() { return _map.isEmpty(); }


  virtual bool add(IN(RValueType) o) {  return _map.put(o, Nil) != Nil; }
  foreign virtual void clear() {  _map.clear(); }
  foreign virtual bool contains(IN(RValueType) o)
  {
    return _map.containsKey(o);
  }
  foreign virtual RIteratorType iterator()
  {
    return _map.keySet()->iterator();
  }
  foreign virtual bool remove(IN(RValueType) o)
  {
    return (_map.remove(o) != Nil);
  }
  foreign virtual RObject clone() { return clone(Object::allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc)
  {
    RIteratorType it = iterator();
    RThisContainerType erg = new (alc) ThisContainerType(_map.capacity(), _map.loadFactor());
    RValueType val;
    while (it->hasNext() == true) 
    {
      erg->add(it->next());
    }
    return (RObject)erg;
  }
};


} // util
} // acdk

#endif // acdk_util_THashSet_h

