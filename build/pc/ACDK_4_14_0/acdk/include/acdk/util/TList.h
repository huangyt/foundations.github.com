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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TList.h,v 1.5 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TList_h
#define acdk_util_TList_h

#include "TCollection.h"
#include "TListIterator.h"

namespace acdk {
namespace util {


#define USE_TLIST(ClassName) \
  typedef ::acdk::util::TList<R##ClassName> ClassName##List; \
  typedef RefHolder<ClassName##List> R##ClassName##List

/**
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class  TList
: implements TCollection<T>
{
public:
  typedef T RValueType;
  typedef TList<RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;

  typedef RValueType RElementType;
  typedef TCollection<T> ThisCollectionType;
  typedef InterfaceHolder<TCollection<T> > RThisCollectionType;

  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
   
  typedef TListIterator<RValueType> ListIteratorType;
  typedef typename ListIteratorType::RefType RListIteratorType;
  
  typedef ObjectArrayImpl<RValueType> ValueTypeArray;
  typedef RObjectArrayImpl<RValueType>  RValueTypeArray;
public:
  // Collection:
  overwrite bool add(IN(RValueType) o) = 0;
  overwrite bool addAll(IN(RCollectionType) c) = 0;
  overwrite void clear() = 0;
  overwrite int size() = 0;
  overwrite bool isEmpty() = 0;
  overwrite bool contains(IN(RValueType) o) = 0;
  overwrite bool containsAll(IN(RCollectionType) c) = 0;
  overwrite RIteratorType iterator() = 0;
  overwrite bool remove(IN(RValueType) o) = 0;
  overwrite bool removeAll(IN(RCollectionType) c) = 0;
  overwrite bool retainAll(IN(RCollectionType) c) = 0;
  overwrite bool equals(IN(RObject) c) = 0;
  overwrite int hashCode() = 0;
  overwrite RValueTypeArray toArray() = 0;
  overwrite RValueTypeArray toArray(IN(RValueTypeArray) array) = 0;
  
  // List
  virtual void add(int index, IN(RValueType) element) = 0;
  virtual bool addAll(int index, IN(RCollectionType) c) = 0;
  virtual RValueType get(int index) = 0;
  virtual int indexOf(IN(RValueType) o) = 0;
  virtual int lastIndexOf(IN(RValueType) o)  = 0; 
  virtual RListIteratorType listIterator(int index = 0) = 0;
  virtual RValueType remove(int index) = 0;
  virtual RValueType set(int index, IN(RValueType) element) = 0;
  virtual RThisType subList(int fromIndex, int toIndex) = 0;
};


} // util
} // acdk

#endif //acdk_util_TList_h

