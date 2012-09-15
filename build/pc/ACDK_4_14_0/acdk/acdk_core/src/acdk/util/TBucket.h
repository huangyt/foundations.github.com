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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TBucket.h,v 1.11 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TBucket_h
#define acdk_util_TBucket_h

#include "TMap.h"
#include "TBasicMapEntry.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

//ACDK_DECL_CLASS(TBucketNode);

/**
  @internal
  Used by THashMap.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TBucketNode
: public TBasicMapEntry<K, V>
{
  
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TBucketNode<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TBasicMapEntry<RKeyType, RValueType> SuperType;
  
friends_private:
  RThisType _next;
public:
  
  TBucketNode(IN(RKeyType) key, IN(RValueType) value)
  : SuperType(key, value)
  {
  }
  foreign virtual RThisType next() { return _next; }
  //friend template <class K, class T> class TBucket;
  //friend TBucket;
};


//ACDK_DECL_CLASS(TBucket);

/**
  @internal
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TBucket
: extends acdk::lang::Object
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TBucket<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TBucketNode<RKeyType, RValueType> BucketNodeType;
  typedef typename BucketNodeType::RefType RBucketNodeType;
  
friends_private:
  RBucketNodeType _first;
public:
  TBucket()
  : Object()
  {
  }
  RBucketNodeType add(IN(RBucketNodeType) newNode)
  {
    RKeyType testKey = newNode->getKey();
    RBucketNodeType it = _first;
    RBucketNodeType prev = Nil;
    if (it == Nil) 
    {
      _first = newNode;
      return Nil;
    } 
    while (it != Nil)   
    {
      RKeyType key = it->getKey();
      if (acdk_equals(key, testKey) == true) 
      {
        if (prev != Nil)
          prev->next() = newNode;
        else
          _first = newNode;
        newNode->_next = it->next();
        return it;
      }
      prev = it;  
      it = it->next();
    }
    prev->_next = newNode; 
    return Nil;
  }
  /**
    @todo definition of nil by T = int
  */
  RValueType removeByKey(INP(RKeyType) key)
  {
    RBucketNodeType prev = Nil;
    RBucketNodeType it = _first;
    while (it != Nil) 
    {
      RKeyType oEntryKey = it->getKey();
      if (acdk_equals(oEntryKey, key) == true) 
      {
        if (prev == Nil) 
          _first = it->next();
        else
          prev->_next = it->next();
        return it->getValue();
      } else {
        prev = it;
        it = it->next();
      }
    }
    return Nil;
  }
  /**
    @todo definition of nil by T = int
  */
  RValueType getValueByKey(INP(RKeyType) key)
  {
    RBucketNodeType entry = getEntryByKey(key);
    if (entry == Nil)
      return Nil;
    return entry->getValue();
  }
  RBucketNodeType getEntryByKey(IN(RKeyType) key)
  {
    RBucketNodeType it = _first;
    while (it != Nil) 
    {
      RKeyType oEntryKey = it->getKey();
      if (acdk_equals(oEntryKey, key) == true)
        return it;
      it = it->next();
    }
    return Nil;
  }
  
  bool containsValue(IN(RValueType) value)
  {
    RBucketNodeType it = _first;
    while (it != Nil) 
    {
      RValueType oEntryValue = it->getValue();
      if (acdk_equals(oEntryValue, value) == true)
        return true;
      it = it->next();
    }
    return false;
  }
  RBucketNodeType first() { return _first; }
  //friend template <class K, class V> class THashMap;
};


} // util
} // acdk

#endif // #define acdk_util_TBucket_h


