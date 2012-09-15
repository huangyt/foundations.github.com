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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TSet.h,v 1.9 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TSet_h
#define acdk_util_TSet_h

#include "TCollection.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

//ACDK_DECL_INTERFACE1(TSet, T);

#define ACDK_DECL_SET(Type1, RType1) \
  typedef ::acdk::util::TSet<RType1> Type1##Set; \
  typedef Type1##Set::RefType R##Type1##Set; \
  typedef Type1##Set::IteratorType Type1##SetIterator; \
  typedef Type1##Set::RIteratorType R##Type1##SetIterator; \

/**
  A set is a collection with values, where each value
  can have only element in the set.

  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/Set.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class TSet
      ACDK_INTERFACEBASE
{
  
public:
  typedef T RValueType;

  typedef TSet<RValueType> ThisCollectionType;
  typedef InterfaceHolder<ThisCollectionType > RThisCollectionType;
  typedef RThisCollectionType RefType;
  typedef ThisCollectionType SetType;
  typedef RThisCollectionType RSetType;

  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  /** does not work with T != ObjectType */
  typedef RObjectArrayImpl<RValueType> RValueArrayType;
  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  virtual bool add(IN(RValueType) o) = 0;
  virtual bool addAll(IN(RCollectionType) c) = 0;
  virtual void clear() = 0;
  virtual bool contains(IN(RValueType) o) = 0;
  virtual bool containsAll(IN(RCollectionType) c) = 0;
  virtual bool equals(IN(RObject) o) = 0;
  virtual int hashCode() = 0;
  virtual bool isEmpty() = 0;
  virtual RIteratorType iterator() = 0;
  
  virtual bool remove(IN(RValueType) o) = 0;
  virtual bool removeAll(IN(RCollectionType) c) = 0;
  virtual bool retainAll(IN(RCollectionType) c) = 0;
  virtual int size() = 0;
  virtual RValueArrayType toArray() = 0;
  virtual RValueArrayType toArray(IN(RValueArrayType) array) = 0;
};


} // util
} // acdk

#endif //acdk_util_TSet_h

