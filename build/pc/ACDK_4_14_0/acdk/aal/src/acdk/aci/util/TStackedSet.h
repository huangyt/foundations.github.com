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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/util/TStackedSet.h,v 1.6 2005/02/05 10:44:51 kommer Exp $


#ifndef acdk_aci_util_TStackedSet_h
#define acdk_aci_util_TStackedSet_h

#include <acdk.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/HashSet.h>
#include <acdk/util/TIterator.h>

namespace acdk {
namespace aci {
namespace util {

/**
  Flags which are used in connection with
  TStackedSet und TStackedMap
*/
foreign enum StackedContainerFlags
{
  ScfNoFlags                = 0x0000,
  /**
    allow read also in parent nodes
  */
  ScfParentRead             = 0x0001,
  /*
    disallow read in parent nodes
  */
  ScfNoParentRead           = 0x0002,
  
  /*
    When modifiing a parameter
    copy parameter in current scope if needed.
    Default is true.
  */
  ScfParentWrite            = 0x0004,
  /**
    Disallow in parent nodes
  */
  ScfNoParentWrite          = 0x0008

};


/**
  Helper for StackedSet and StackedMap
*/
template <class StackedColl, class Coll>
class TStackedCollectionHelper
{
public:
  typedef StackedColl StackedCollectionType;
  typedef ::RefHolder<StackedCollectionType> RStackedCollectionType;
  typedef ::ObjectArrayImpl<RStackedCollectionType> StackedCollectionArrayType; 
  typedef ::RObjectArrayImpl<RStackedCollectionType> RStackedCollectionArrayType;

  typedef Coll CollectionType;

protected:
  /**
    combination of StackedContainerFlags
    These flags are default flags and may be overwritten
    by single calls (like get(), put(), etc)
  */
  int _flags;
  RStackedCollectionArrayType _parents;
public:
  /**
    @param flags combination of StackedContainerFlags
  */
  TStackedCollectionHelper(int flags)
  : _flags(flags)
  , _parents(new StackedCollectionArrayType(0))
  {
  }
  RStackedCollectionArrayType getDirectParentCollection() 
  {
    return _parents;
  }
  /**
    Add a parent
  */
  void addParentRef(IN(RStackedCollectionType) tt)
  {
    _parents->append(tt);
  }
  /**
    opens a new scope
    this will become parent of a new empty
    StackedCollection
    @return the new StackedCollection
  */
  RStackedCollectionType pushScope()
  {
    RStackedCollectionType tt = new StackedCollectionType(_flags);
    tt->addParentRef((StackedCollectionType*)this);
    return tt;
  }
  /**
    close a scope
    @return the first parent of this collection
  */
  RStackedCollectionType popScope()
  {
    RStackedCollectionType ret;
    if (_parents->length() >= 1)
    {
      ret = _parents[0];
    }
    return ret;
  }
  /**
    remove the reference from direct parents
    @return true if p was a direct parent
  */
  bool removeParentRef(IN(RStackedCollectionType) p)
  {
    return removeSameElement(p) != Nil;
  }
  /**
    @return the sum of all element size of the parents
  */
  int parentSize(int flags = ScfNoFlags)
  {
    int s = 0;
    if (_readParent(flags) == false)
      return s;
    for (int i = 0; i < _parents->length(); ++i)
      s += _parents[i]->size();
    return s;
  }
  /**
    return true if all parent are empty
  */
  bool isParentEmpty(int flags = ScfNoFlags) 
  {
    if (_readParent(flags) == false)
      return false;
    for (int i = 0; i < _parents->length(); ++i)
      if (_parents[i]->isEmpty() == false)
        return false;
    return false;
  }
  /**
    return true if parent elements can be read
  */
  inline bool _readParent(int flags) 
  { 
    if (flags & ScfNoParentRead)
      return false;
    if (flags & ScfParentRead)
      return true;
    return (_flags & ScfNoParentRead) != ScfNoParentRead;
  }
  /**
    return true if parent elements can be written
  */
  
  inline bool _writeParent(int flags) 
  { 
    if (flags & ScfNoParentWrite)
      return false;
    if (flags & ScfParentWrite)
      return true;
    return (_flags & ScfNoParentWrite) != ScfNoParentWrite;
  }
};


/**
  Template, which enables stacked container
*/
template <class Set>
class TStackedSet
: extends Set
, public TStackedCollectionHelper<TStackedSet<Set>, Set>
{
public:
  typedef TStackedSet<Set> ThisType;
  typedef ::RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef ::ObjectArrayImpl<RThisType> ThisArrayType; 
  typedef ::RObjectArrayImpl<RThisType> RThisArrayType;

  
  typedef Set SetType;
  typedef typename Set::RThisType RSetType;

  //typedef typename SetType::ValueType ValueType;
  typedef typename SetType::RValueType RValueType;
  
  typedef ::ObjectArrayImpl<RSetType> SetArrayType; 
  typedef ::RObjectArrayImpl<RSetType> RSetArrayType;

  typedef TStackedCollectionHelper<TStackedSet<Set>, Set> HelperType;

  typedef typename Set::IteratorType IteratorType;
  typedef typename Set::RIteratorType RIteratorType;
public:
  /**
    @param flags combination of StackedContainerFlags
  */
  TStackedSet(int flags = ScfNoParentWrite)
  : HelperType(flags)
  {
  }
  

  bool contains(IN(RValueType) el, int flags = ScfNoFlags)
  {
    if (SetType::contains(el) == true)
      return true;
    if (_readParent(flags) == false)
      return false;
    for (int i = 0; i < _parents->length(); ++i)
      if (_parents[i]->contains(el) == true)
        return true;
    return false;
  }
  bool isEmpty(int flags = ScfNoFlags) 
  {
    if (SetType::isEmpty() == false)
      return false;
    return HelperType::isParentEmpty(flags);
  }
  virtual int size(int flags = ScfNoFlags)
  {
    return SetType::size() + HelperType::parentSize(flags);
  }
  virtual RIteratorType iterator();
};

/**
  This iterator works on StackedSet's
  First of current scope will be traversed
  then recursivally all of first parent, and then
  through all other parent
*/
template <class StackedSet>
class StackedSetIterator
: extends acdk::lang::Object
, implements acdk::util::TIterator<typename StackedSet::RValueType>
{
public:
  typedef StackedSet StackedSetType;
  typedef typename StackedSetType::RefType RStackedSetType;

  typedef typename StackedSetType::RValueType RValueType;
  //typedef typename ValueType::RefType RValueType;
  
  typedef typename StackedSetType::IteratorType IteratorType;
  typedef typename StackedSetType::RIteratorType RIteratorType;
  
  // the underlying set
  typedef typename StackedSetType::SetType SetType;
  typedef typename StackedSetType::RSetType RSetType;

  RStackedSetType _container;
  //acdk::lang::sys::core_vector<int> _childNums;
  int _parentNum;
  RStackedSetType _curContainer;
  RIteratorType _curIterator;

  StackedSetIterator(IN(RStackedSetType) cont)
  : _container(cont)
  , _parentNum(-1)
  {
    _curIterator = _container->SetType::iterator();
    _seekNext();
  }
  bool hasNext()
  {
    return _curIterator->hasNext();
  }
  RValueType next()
  {
    RValueType ret = _curIterator->next();
    _seekNext();
    return ret;
  }
  RValueType element()
  {
    return _curIterator->element();
  }
  virtual void remove()
  {
    _curIterator->remove();
  }
  void _seekNext()
  {
    while (_curIterator->hasNext() == false)
    {
      ++_parentNum;
      if (_container->getDirectParentCollection()->length() > _parentNum)
        _curIterator = _container->getDirectParentCollection()[_parentNum]->iterator();
      else
        break;
    }
  }
};

template <class Set>
typename TStackedSet<Set>::RIteratorType 
TStackedSet<Set>::iterator()
{
  return new StackedSetIterator<ThisType>(this);
}


/**
  Template which provides a template stacked map
  which implements the scope pattern
*/
template <class Map>
class TStackedMap
: extends Map
, public TStackedCollectionHelper<TStackedMap<Map>, Map>
{
public:
  typedef Map MapType;
  typedef typename MapType::RThisType RMapType;
  typedef typename MapType::RKeyType RKeyType;
  typedef typename MapType::RValueType RValueType;
  typedef TStackedMap<Map> ThisType;
  typedef ::RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef  TStackedCollectionHelper<TStackedMap<Map>, Map> HelperType;

  typedef typename acdk::util::TArrayType<RThisType>::Type ThisArrayType; 
  typedef typename acdk::util::TArrayType<RThisType>::RefType RThisArrayType;
  
  typedef typename MapType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;
  
  typedef typename KeySetType::IteratorType KeySetIteratorType;
  typedef typename KeySetIteratorType::RefType RKeySetIteratorType;
  
  typedef typename MapType::ValueSetType ValueSetType;
  typedef typename ValueSetType::RefType RValueSetType;

  typedef typename ValueSetType::IteratorType ValueSetIteratorType;
  typedef typename ValueSetIteratorType::RefType RValueSetIteratorType;
    
  typedef typename acdk::util::TArrayType<RKeyType>::Type KeyArrayType;
  typedef typename acdk::util::TArrayType<RKeyType>::RefType RKeyArrayType;
  
  typedef typename acdk::util::TArrayType<RValueType>::Type ValueArrayType;
  typedef typename acdk::util::TArrayType<RValueType>::RefType RValueArrayType;

  RThisArrayType _parents;
  int _flags;

  TStackedMap(int flags = ScfNoParentWrite)
  : HelperType(flags)
  , _parents(new ThisArrayType(0))
  , _flags(flags)
  {
  }
  int size(int flags = ScfNoFlags)
  {
    return MapType::size() + HelperType::parentSize(flags);
  }
  bool containsKey(IN(RKeyType) key, int flags = ScfNoFlags)
  {
    if (MapType::containsKey(key) == true)
      return true;
    if (_readParent(flags) == false)
      return false;
    for (int i = 0; i < _parents->length(); ++i)
      if (_parents[i]->containsKey(key, flags) == true)
        return true;
    return false;
  }
  bool containsValue(IN(RValueType) value, int flags = ScfNoFlags) 
  {
    if (MapType::containsValue(value) == true)
      return true;
    if (_readParent(flags) == false)
      return false;
    for (int i = 0; i < _parents->length(); ++i)
      if (_parents[i]->containsValue(value, flags) == true)
        return true;
    return false;
  }
  /**
    find the container in this an parent, which contains the key
    return Nil if none is found
  */
  RThisType getContainingMap(IN(RKeyType) key)
  {
    if (MapType::containsKey(key) == true)
      return this;
    for (int i = 0; i < _parents->length(); ++i)
    {
      RThisType erg = _parents[i]->getContainingMap(key);
      if (erg != Nil)
        return erg;
    }
    return Nil;
  }

  virtual RValueType put(IN(RKeyType) key, IN(RValueType) val, int flags = ScfNoFlags)
  {
    if (_writeParent(flags) == false || MapType::containsKey(key) == true)
      return MapType::put(key, val);
    RThisType erg = getContainingMap(key);
    if (erg == Nil)
      return MapType::put(key, val);
    return erg->MapType::put(key, val);
  }
  /**
    makes a deep first search
  */
  virtual RValueType get(IN(RKeyType) key, int flags = ScfNoFlags)
  {
    RValueType erg = MapType::get(key);
    if (erg != Nil || _readParent(flags) == false)
      return erg;
    for (int i = 0; i < _parents->length(); ++i)
    {
      RValueType erg = _parents[i]->get(key, flags);
      if (erg != Nil)
        return erg;
    }
    return Nil;
  }
  bool isEmpty(int flags = ScfNoFlags) 
  {
    if (MapType::isEmpty() == false)
      return false;
    return HelperType::isParentEmpty(flags);
  }
  virtual RValueType remove(IN(RKeyType) k)
  {
    RValueType vt = MapType::remove(k);
    if (vt != Nil)
      return vt;
    return Nil;
  }

  void _collectKeys(IN(RKeyArrayType) ks)
  {
    typename KeySetType::RIteratorType ksit = MapType::keySet()->iterator();
    while (ksit->hasNext() == true)
    {
      RKeyType k = ksit->next();
      if (ks->find(k) == -1)
        ks->append(k);
    }
    for (int i = 0; i < _parents->length(); ++i)
    {
      _parents[i]->_collectKeys(ks);
    }
  }
  RKeyArrayType getKeys()
  {
    RKeyArrayType ks = new KeyArrayType(0);
    _collectKeys(ks);
    return ks;
  }
  void _collectValues(IN(RValueArrayType) ks)
  {
    typename ValueSetType::RIteratorType vsit = MapType::ValueSet()->iterator();
    while (vsit->hasNext() == true)
    {
      RValueType k = vsit->next();
      if (ks->find(k) == -1)
        ks->append(k);
    }
    for (int i = 0; i < _parents->length(); ++i)
    {
      _parents[i]->_collectValues(ks);
    }
  }
  RValueArrayType getValues()
  {
    RValueArrayType ks = new ValueArrayType(0);
    _collectValues(ks);
    return ks;
  }
  void _collectValues(IN(RValueArrayType) ks, IN(RKeyType) key)
  {
    RValueType v = MapType::get(key);
    if (v != Nil)
      ks->append(v);
    for (int i = 0; i < _parents->length(); ++i)
    {
      _parents[i]->_collectValues(ks, key);
    }
  }
  RValueArrayType getValues(IN(RKeyType) key)
  {
    RValueArrayType va = new ValueArrayType(0);
    _collectValues(va, key);
    return va;
  }

};



} // util
} // aci
} // acdk


#endif //acdk_aci_util_TStackedSet_h

