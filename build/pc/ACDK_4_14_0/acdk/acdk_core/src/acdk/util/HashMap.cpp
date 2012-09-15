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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/HashMap.cpp,v 1.21 2005/03/08 12:45:45 kommer Exp $



#include <acdk.h>

#include "HashMap.h"
#include <acdk/lang/System.h>
#include <acdk/lang/Error.h>

#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/IllegalStateException.h>
#include "NoSuchElementException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

const int HashMap::DEFAULT_CAPACITY = 11;
const float HashMap::DEFAULT_LOAD_FACTOR = 0.75F;

//static HashMap.Nil Nil_KEY = new HashMap.Nil();

RObject HashMap::_nilEntry = Nil;


  

HashMap::HashMap(int initialCapacity, float initialLoadFactor)
: _capacity(0),
  _size(0),
  _loadFactor(0),
  _threshold(0),
  _modCount(0)
, _hashFunc(_standardHashFunc)
{
  if (initialCapacity < 0 || initialLoadFactor <= 0 || initialLoadFactor > 1) {
    
    THROW1(IllegalArgumentException, 
        SBSTR("wrong initial values: intialCapacity=[" << initialCapacity << "]; initialLoadFactor=["
        << initialLoadFactor << "];"));
  }
  _init(initialCapacity, initialLoadFactor);
}


HashMap::HashMap(IN(RMap) other)
: _capacity(0),
  _size(0),
  _loadFactor(0),
  _threshold(0),
  _modCount(0)
, _hashFunc(_standardHashFunc)
{
  int mapSize = other->size() * 2;
  _init(((mapSize > DEFAULT_CAPACITY) ? mapSize : DEFAULT_CAPACITY), DEFAULT_LOAD_FACTOR);
  putAll(other);
}

//virtual 
HashMap::~HashMap()
{

}

//virtual 
void 
HashMap::clear()
{
  _size = 0;
  _modCount++;
  _buckets = new (allocator()) BucketArray(_capacity);
}


//virtual 
RObject 
HashMap::clone(sys::Allocator* alc)
{
  RIterator it = entrySet()->iterator();
  RHashMap erg = new (alc) HashMap(_capacity, _loadFactor);
  RMapEntry entry;
  while (it->hasNext() == true) {
    entry = RMapEntry(it->next());
    erg->_put(entry->getKey(), entry->getValue());
  }
  return (RObject)erg;
}

//virtual 
RSet 
HashMap::keySet()
{
  return new (allocator()) HashMapSet(this, HMSTKeys);
}

//virtual 
RSet 
HashMap::entrySet()
{
  return new (allocator()) HashMapSet(this, HMSTEntries);
}

RCollection 
HashMap::values()
{
  return new (allocator()) HashMapCollection(this);
}

bool 
HashMap::_containsEntry(IN(RMapEntry) entry)
{
  if (entry == Nil)
    return false;
  RMapEntry oInternalEntry = _get(entry->getKey());
  return (oInternalEntry != Nil && oInternalEntry->equals((RObject)entry));
}


//virtual 
bool 
HashMap::containsValue(IN(RObject) value)
{
  int i;
  RBucket list;
  for (i = 0; i < _capacity; i++) {
    list = _buckets[i];
    if (list != Nil && list->containsValue(value))
        return true;
  }
  return false;
}

//virtual 
RIterator
HashMap::iterator() 
{ 
  return new (allocator()) HashMapIterator(this, HMSTEntries);
}

//virtual 
void 
HashMap::putAll(IN(RMap) t)
{
  RMapEntry entry;
  RIterator it = t->entrySet()->iterator();
  while (it->hasNext() == true) {
    entry = RMapEntry(it->next());
    put(entry->getKey(), entry->getValue());
  }
}

//virtual 
RObject 
HashMap::remove(IN(RObject) key)
{
  if (_size <= 0)
    return Nil;
  RBucket list;
  int index;
  RObject result = Nil;
  if (key == Nil) 
    return Nil;
  index = _hash(key == Nil ? nilEntry() : key);
  list = _buckets[index];
  if (list != Nil) {
    result = list->removeByKey(key);
    if (result != Nil) {
      _size--;
      _modCount++;
      if (list->_first == Nil)
        _buckets[index] = Nil;
    }
  }
  return result;
}


void 
HashMap::_init(int initialCapacity, float initialLoadFactor)
{
  _capacity = initialCapacity;
  _loadFactor = initialLoadFactor;
  _threshold = (int) ((float) _capacity * _loadFactor);
  _buckets = new (allocator()) BucketArray(_capacity);
}

RObject 
HashMap::_put(IN(RObject) k, IN(RObject) value)
 {
   
  RMapEntry res;
  RObject key = k;

  _modCount++;
  if (_size == _threshold)
    _rehash();
  key = (key == Nil) ? nilEntry() : key;
  
  RMapEntry oldentry = _get(key);
  if (oldentry != Nil) {
    RObject oldvalue = oldentry->getValue();
    oldentry->setValue(value);
    return oldvalue;
  } 
  RBucketNode entry = new (allocator()) BucketNode(key, value);
  int hashIndex = _hash(key);
  RBucket list = _buckets[hashIndex];
  if (list == Nil) {
    list = new (allocator()) Bucket();
    _buckets[hashIndex] = list;
  }
  res = list->add(entry);
  if (res == Nil) {
    _size++;
    return Nil;
  } 
  THROW1(Error, "Unexpected");// error here
  return Nil;//res->getValue();
}

void 
HashMap::_rehash()
{
  int i;
  RBucketArray data = _buckets;
  _modCount++;
  _capacity = (_capacity * 2) + 1;
  _size = 0;
  _threshold = (int) ((float) _capacity * _loadFactor);
  _buckets = new (allocator()) BucketArray(_capacity);
  RBucketNode node;
  for (i = 0; i < data->length(); i++) {
    if (data[i] != Nil) {
      node = data[i]->_first;
      while (node != Nil) {
        _put(node->getKey(), node->getValue());
        node = node->next();
      }
    }
  }
}

RMapEntry 
HashMap::_get(IN(RObject) key)
{
  if (_size == 0)
    return Nil;
  if (key == Nil)
    return Nil;

  RBucket list = _buckets[_hash(key == Nil ? nilEntry() : key)];
  return (list == Nil) ? RMapEntry(Nil) : (RMapEntry)list->getEntryByKey(key);
}


//static
RObject 
HashMap::nilEntry()
{
  if (_nilEntry == Nil) {
    _nilEntry = new HashMapNilEntry();
    System::registerStaticReference(_nilEntry);
  }
  return _nilEntry;
}

//virtual 
RObject 
HashMapIterator::next()
{
  RBucket list = Nil;
  RObject result;
  _checkMod();      
  try {
    while (_currentNode == Nil) {
      while (list == Nil)
        list = _hashMap->_buckets[++_bucketIndex];
      _currentNode = list->first();
    }
    _currentKey = _currentNode->getKey();
    if (_type == HMSTKeys) {
      result = _currentKey;
    } else if (_type == HMSTValues) {
      result = _currentNode->getValue();
    } else { 
      result = (RObject)_currentNode;
    }
    _currentNode = _currentNode->next();
  } catch (RException) {
    THROW0(NoSuchElementException);
  }
  _position++;
  return result;
}

//virtual 
RObject 
HashMapIterator::element() // ### @todo to be implemented
{
  THROW0(UnsupportedOperationException);  
  return Nil;
}

//virtual 
void 
HashMapIterator::remove()
{
  _checkMod();
  if (_currentKey == Nil) 
    THROW0(IllegalStateException);
  _hashMap->remove(_currentKey);
  _knownMods++;
  _currentKey = Nil;
}



} // Util
} // acdk
