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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractList.h,v 1.7 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TAbstractList_h
#define acdk_util_TAbstractList_h

#include <acdk.h>
#include <acdk/lang/IndexOutOfBoundsException.h>

#include "TAbstractCollection.h"
#include "TList.h"
#include "TListIterator.h"


namespace acdk {
namespace util {

using namespace acdk::lang;

template <class T> class TAbstractListIterator;
template <class T> class TAbstractListListIterator;
template <class T> class TAbstractListSubList;

/**
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class  TAbstractList 
: extends TAbstractCollection<T>
, implements TList<T>
{
public:
  typedef T RValueType;
  typedef TAbstractList<RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;

  typedef RValueType RElementType;

  typedef TList<RValueType> ListType;
  typedef typename ListType::RefType RListType;
  typedef TAbstractList<RValueType> ThisCollectionType;
  typedef RefHolder<TAbstractCollection<RValueType> > RThisCollectionType;
  typedef TAbstractCollection<RValueType> AbstractCollectionType;
  typedef typename AbstractCollectionType::RefType RAbstractCollectionType;

  
  typedef TIterator<T> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;
  
  typedef ObjectArrayImpl<T> ValueArrayType;
  typedef RObjectArrayImpl<T> RValueArrayType;

  typedef TListIterator<RValueType> ListIteratorType;
  typedef typename ListIteratorType::RefType RListIteratorType;

  typedef TAbstractListSubList<RValueType> AbstractListSubListType;
  typedef RefHolder<AbstractListSubListType> RAbstractListSubListType;

protected:
  mutable int _modCount;
public:
  TAbstractList()
  : _modCount(0)
  {
  }
  
public:
  foreign virtual RValueType get(int index) = 0;
  foreign virtual void add(int index, IN(RValueType) o)
  {
    THROW0(UnsupportedOperationException);
  }

  foreign virtual bool add(IN(RValueType) o)
  {
    add(size(), o);
    return true;
  }
  foreign virtual bool addAll(int index, IN(RCollectionType) c)
  {
    RIteratorType i = c->iterator();
    if (i->hasNext()) 
    {
      do {
        add(index++, i->next());
      } while (i->hasNext());
      return true;
    } else {
      return false;
    }
  }
  foreign virtual void clear() { removeRange(0, size()); }
  foreign virtual bool equals(IN(RObject) o)
  {
    if (o.impl() == (const Object*)this) 
      return true;
    if (instanceof(o, ListType) == false) 
      return false;
    RIteratorType i1 = iterator();
    RIteratorType i2 = RListType(o)->iterator();
    while (i1->hasNext() == true) 
    {
      if (i2->hasNext() == false) 
        return false;
      RValueType e1 = i1->next();
      RValueType e2 = i2->next();
      if (e1 == Nil ? e2 != Nil : e1->equals(e2) == false) 
        return false;
    }
    if (i2->hasNext()) 
      return false;
    return true;
  }
  foreign virtual int hashCode()
  {
    int hashCode = 1;
    RIteratorType i = iterator();
    while (i->hasNext()) 
    {
      RValueType obj = i->next();
      hashCode = 31 * hashCode + (obj == Nil ? 0 : obj->hashCode());
    }
    return hashCode;
  }
  foreign virtual int indexOf(IN(RValueType) o)
  {
    int index = 0;
    RListIteratorType i = listIterator();
    if (o == Nil) 
    {
      while (i->hasNext()) 
      {
        if (i->next() == Nil) 
        {
          return index;
        }
        index++;
      }
    } 
    else 
    {
      while (i->hasNext()) 
      {
        if (o->equals(i->next())) 
        {
          return index;
        }
        index++;
      }
    }
    return -1;
  }
  foreign virtual RIteratorType iterator();

  foreign virtual RListIteratorType listIterator(int index = 0);

  foreign virtual int lastIndexOf(IN(RValueType) o)
  {
    int index = size();
    RListIteratorType i = listIterator(index);
    //RefHolder<TListIterator<T> > i = listIterator(index);
    if (o == Nil) 
    {
      while (i->hasPrevious() == true) 
      {
        if (i->previous() == Nil) 
          return index;
        index--;
      }
    } 
    else 
    {
      while (i->hasPrevious() == true) 
      {
        if (o->equals(i->previous()) == true) 
        {
          return index;
        }
        index--;
      }
    }
    return -1;
  }
 

  foreign virtual RValueType remove(int index)
  {
    THROW0(UnsupportedOperationException);
    return RValueType();
  }
  foreign virtual RValueType set(int index, IN(RValueType) o)
  {
    THROW0(UnsupportedOperationException);
    return RValueType();
  }
  foreign virtual int size()  { THROW0(UnsupportedOperationException); return 0; }
  /// see AbstractListSubList
  foreign virtual RListType subList(int fromIndex, int toIndex);

  foreign virtual bool isEmpty() { return AbstractCollectionType::isEmpty(); }
  foreign virtual bool contains(IN(RValueType) obj) { return AbstractCollectionType::contains(obj); }
  foreign virtual bool containsAll(IN(RCollectionType) c) { return AbstractCollectionType::containsAll(c); }
  foreign virtual bool remove(IN(RValueType) obj) { return AbstractCollectionType::remove(obj); }
  foreign virtual bool removeAll(IN(RCollectionType) c) { return AbstractCollectionType::removeAll(c); }
  foreign virtual bool addAll(IN(RCollectionType) c) { return AbstractCollectionType::addAll(c); }
  foreign virtual bool retainAll(IN(RCollectionType) c) { return AbstractCollectionType::retainAll(c); }
  foreign virtual RValueArrayType toArray() { return AbstractCollectionType::toArray(); }
  foreign virtual RValueArrayType toArray(IN(RValueArrayType) a) { return AbstractCollectionType::toArray(a); }
  foreign virtual RString toString() { return AbstractCollectionType::toString(); }

  friend class TAbstractListIterator<T>;
  friend class TAbstractListListIterator<T>;
  friend class TAbstractListSubList<T>;
  
protected:
  virtual void removeRange(int fromIndex, int toIndex)
  {
    if (fromIndex > toIndex) {
      THROW0(IllegalArgumentException);
    } 
    else if (fromIndex < 0 || toIndex > size()) 
    {
      THROW0(IndexOutOfBoundsException);
    } 
    else 
    {
      RListIteratorType i = listIterator(fromIndex);
      for (int index = fromIndex; index < toIndex; index++) 
      {
        i->next();
        i->remove();
      }
    }
  }
};



} // util
} // acdk

#include "TAbstractListIterator.h"
#include "TAbstractListListIterator.h"
#include "TAbstractListSubList.h"

#endif //acdk_util_TAbstractList_h

