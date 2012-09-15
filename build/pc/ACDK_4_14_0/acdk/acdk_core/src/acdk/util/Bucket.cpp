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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Bucket.cpp,v 1.8 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>

#include "Bucket.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

RMapEntry 
Bucket::add(IN(RBucketNode) newNode)
{
  RObject key;
  RObject testKey = newNode->getKey();
  RBucketNode it = _first;
  RBucketNode prev = Nil;
  if (it == Nil) {
    _first = newNode;
    return Nil;
  } 
  while (it != Nil)   {
    key = it->getKey();
    if ((key == Nil) ? (testKey == Nil) : key->equals(testKey)) {
      if (prev != Nil)
        prev->next() = newNode;
      else
        _first = newNode;
      newNode->_next = it->next();
      return (RMapEntry)it;
    }
    prev = it;  
    it = it->next();
  }
  prev->_next = newNode; 
  return Nil;
    
}

RObject 
Bucket::removeByKey(IN(RObject) key)
{
  RObject oEntryKey;
  RBucketNode prev = Nil;
  RBucketNode it = _first;
  while (it != Nil) {
    oEntryKey = it->getKey();
    if ((oEntryKey == Nil) ? (key == Nil) : oEntryKey->equals(key)) {
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

RBucketNode 
Bucket::getEntryByKey(IN(RObject) key)
{
  RObject oEntryKey;
  RBucketNode it = _first;
  while (it != Nil) {
    oEntryKey = it->getKey();
    if ((oEntryKey == Nil) ? (key == Nil) : oEntryKey->equals(key))
        return it;
    it = it->next();
  }
  return Nil;
}


bool 
Bucket::containsValue(IN(RObject) value) 
{
  RObject oEntryValue;
  RBucketNode it = _first;
  while (it != Nil) {
    oEntryValue = it->getValue();
    if ((oEntryValue == Nil) ? (value == Nil) : oEntryValue->equals(value))
        return true;
    it = it->next();
  }
  return false;
}


} // Util
} // acdk

