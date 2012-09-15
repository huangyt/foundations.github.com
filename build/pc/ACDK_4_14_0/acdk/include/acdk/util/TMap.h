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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TMap.h,v 1.9 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TMap_h
#define acdk_util_TMap_h

#include "TCollection.h"
#include "TSet.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

/**
  @internal 
  Interface for a element in a Map
*/
template <class K, class V>
class TMapEntry
      ACDK_INTERFACEBASE      
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TMapEntry<RKeyType, RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef ThisType MapEntryType;
  typedef RThisType RMapEntryType;
  
  virtual bool equals(IN(RThisType) o) = 0;
  virtual bool equals(IN(RObject) o) 
  {
    if (instanceof(o, ThisType) == false)
      return false;
    return equals(RThisType(o));
  }
  virtual ::acdk::lang::RString toString() = 0;//{ return ""; }
  virtual RKeyType getKey() = 0;
  virtual RValueType getValue() = 0;
  virtual int hashCode() = 0;
  virtual RValueType setValue(IN(RValueType) value) = 0;
};

#define ACDK_DECL_MAP(Type1, RType1, Type2, RType2) \
  ACDK_DECL_TCONTAINER2(Map, Type1, RType1, Type2, RType2); \
  typedef Type1##To##Type2##Map::RMapEntrySetType R##Type1##To##Type2##MapEntrySet; \
  typedef Type1##To##Type2##Map::RKeySetType R##Type1##Set; \
  typedef Type1##To##Type2##Map::RValueSetType R##Type2##Set; \
  typedef ::acdk::util::TIterator<RType1> Type1##Iterator; \
  typedef Type1##Iterator::RefType R##Type1##Iterator; \
  typedef ::acdk::util::TIterator<RType2> Type2##Iterator; \
  typedef Type2##Iterator::RefType R##Type2##Iterator; \
  typedef ::acdk::util::TMapEntry<RType1, RType2> Type1##To##Type2##MapEntry; \
  typedef Type1##To##Type2##MapEntry::RefType R##Type1##To##Type2##MapEntry

/**
  A TMap is a container where elements are accessed via 
  key objects.

  @see  acdk::util::Map
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/Map.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

template <class K, class V>
class TMap
      ACDK_INTERFACEBASE
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TMap<RKeyType, RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef ThisType MapType;
  typedef RThisType RMapType;

  typedef TIterator<RKeyType> KeyIteratorType;
  typedef typename KeyIteratorType::RefType RKeyIteratorType;
  
  typedef TIterator<RValueType> ValueIteratorType;
  typedef typename ValueIteratorType::RefType RValueIteratorType;

  
  typedef TMapEntry<RKeyType, RValueType> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  
  typedef TSet<RKeyType> KeySetType;
  typedef typename KeySetType::RefType RKeySetType;
  
  typedef TSet<RValueType> ValueSetType;
  typedef typename ValueSetType::RefType RValueSetType;

  typedef TSet<RMapEntryType> SetType;
  typedef typename SetType::RefType RSetType;

  typedef TSet<RMapEntryType> MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;
  
  typedef TCollection<RMapEntryType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef TCollection<RValueType> ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;
  
  

  virtual void clear() = 0;
  virtual bool containsKey(IN(RKeyType) key) = 0;
  virtual bool containsValue(IN(RValueType) value) = 0;
  virtual RMapEntrySetType entrySet() = 0;
  virtual bool equals(IN(RObject) o) = 0;
  virtual RValueType get(IN(RKeyType) key) = 0;
  virtual int hashCode() = 0;
  virtual bool isEmpty() = 0;
  virtual RKeySetType keySet() = 0;
  virtual RValueType put(IN(RKeyType) key, IN(RValueType) value) = 0;
  virtual void putAll(IN(RMapType) m) = 0;
  virtual RValueType remove(IN(RKeyType) k) = 0;
  virtual int size() = 0;
  virtual RValueCollectionType values() = 0;

};



} // util
} // acdk

#endif //acdk_util_TMap_h


