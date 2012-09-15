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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TCollection.h,v 1.8 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TCollection_h
#define acdk_util_TCollection_h


#include <acdk.h>
#include "TIterator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

/** To define a standard T<ContainerName> */
#define ACDK_DECL_TCONTAINER1(ContainerName, TypeName, RTypeName) \
  typedef ::acdk::util::T##ContainerName<RTypeName> TypeName##ContainerName; \
  typedef TypeName##ContainerName::RefType R##TypeName##ContainerName

/** To define a standard T<ContainerName> */
#define ACDK_DECL_TCONTAINER2(ContainerName, KeyName, RKeyName, ValueName, RValueName) \
  typedef ::acdk::util::T##ContainerName<RKeyName, RValueName> KeyName##To##ValueName##ContainerName; \
  typedef KeyName##To##ValueName##ContainerName::RefType R##KeyName##To##ValueName##ContainerName


/**
  Typed Collection following Java Model.

  @see @ref tcollection
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/Collection.html
  
  Requirements:
  - Element created with default constructor T() has to be implemented.
    acdk_isNil(T()) == true
  - Copy constructor has to be implemented.

  - ::acdk_equals(INP(T) t); has to be implemented
  - ::acdk_isNil(INP(T) t); has to be implemented

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class  TCollection
      ACDK_INTERFACEBASE
{
public:
  typedef T RValueType;
  
  typedef T RElementType;
  typedef typename RElementType::Type ElementType;

  typedef TCollection<T> CollectionType;
  typedef InterfaceHolder<CollectionType> RCollectionType;
  typedef RCollectionType RefType;

  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
  typedef RObjectArrayImpl<RValueType> RValueArrayType;

  /**
    Add an element to collection
  */
  virtual bool add(INP(T) o) = 0;
  /**
    add all elements of c to this collection
  */
  virtual bool addAll(INP(RCollectionType) c) = 0;
  /**
    remove all elements from this collection
  */
  virtual void clear() = 0;
  /**
    return the number of elements in this collection
  */
  virtual int size() = 0;
  /**
    return true if this collection doesn't contains any elements
  */
  virtual bool isEmpty() = 0;
  /**
  */
  virtual bool contains(INP(RValueType) o) = 0;
  virtual bool containsAll(INP(RCollectionType) c) = 0;

  virtual RIteratorType iterator() = 0;
  virtual bool remove(IN(RValueType) o) = 0;
  virtual bool removeAll(IN(RCollectionType) c) = 0;
  virtual bool retainAll(IN(RCollectionType) c) = 0;
  virtual bool equals(IN(RObject) c) = 0;
  virtual int hashCode() = 0;
  virtual RValueArrayType toArray() = 0;
  virtual RValueArrayType toArray(INP(RValueArrayType) array) = 0;
          
};

/**
  ACDK_DECL_COLLECTION(int);
  defines
  intCollection, RintCollection, RintCollectionIterator
*/
#define ACDK_DECL_COLLECTION(Type, RType) \
  ACDK_DECL_TCONTAINER1(Collection, Type, RType); \
  typedef Type##Collection::IteratorType Type##Iterator; \
  typedef Type##Collection::RIteratorType R##Type##Iterator


} // util
} // acdk


#endif //acdk_util_TCollection_h

