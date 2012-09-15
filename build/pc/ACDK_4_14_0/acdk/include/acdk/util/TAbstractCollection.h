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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractCollection.h,v 1.13 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TAbstractCollection_h
#define acdk_util_TAbstractCollection_h

#include "TCollection.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


//ACDK_DECL_CLASS(TAbstractCollection);

/**
  @internal
  Basic abstract implementation of an collection.

  @see acdk::util::TCollection
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class TAbstractCollection 
: extends ::acdk::lang::Object
, implements TCollection<T>
{
  
public:

  typedef T RValueType;
  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef RValueType RElementType;

  typedef TAbstractCollection<RValueType> ThisCollectionType;
  typedef RefHolder<TAbstractCollection<RValueType> > RThisCollectionType;
  typedef ThisCollectionType AbstractCollectionType;
  typedef RThisCollectionType RAbstractCollectionType;

  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef ObjectArrayImpl<RValueType> ValueArrayType;
  typedef RObjectArrayImpl<RValueType> RValueArrayType;
  
  // already defined in Collection
  //typedef TCollection<T> CollectionType;
  //typedef SharedInterfaceRef<TCollection<T> > RCollectionType;

  /// reimplemented from Collection
  foreign virtual RIteratorType iterator() = 0;

  /// reimplemented from Collection
  foreign virtual bool add(INP(RValueType) o) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }

  /// reimplemented from Collection
  foreign virtual bool addAll(IN(RCollectionType) c)
  {
    RIteratorType i = c->iterator();
    bool modified = false;
    while (i->hasNext()) 
    {
      modified |= add(i->next());
    }
    return modified;
  }

  /// reimplemented from Collection
  foreign virtual void clear()
  {
    RIteratorType i = iterator();
    while (i->hasNext()) 
    {
      i->next();
      i->remove();
    }
  }

  /// reimplemented from Collection
  foreign virtual bool contains(IN(RValueType) o)
  {
    RIteratorType i = iterator();
    if (acdk_isNil(o) == true) 
    {
      while (i->hasNext() == true) 
      {
        if (acdk_isNil(i->next()) == true) 
          return true;
        }
    } else {
      while (i->hasNext() == true) 
      {
        RElementType t = i->next();
        if (acdk_equals(o, t) == true)
          return true;
        }
      }
    return false;
  }

  /// reimplemented from Collection
  foreign virtual bool containsAll(IN(RCollectionType) c)
  {
    RIteratorType i = c->iterator();
    while (i->hasNext() == true) 
    {
      if (contains(i->next()) == false) {
        return false;
      }
    }
    return true;
  }

  /// reimplemented from Collection
  foreign virtual bool isEmpty() { return this->size() == 0; }

  /// reimplemented from Collection
  foreign virtual bool remove(IN(RValueType) o)
  {
    RIteratorType i = iterator();
    if (acdk_isNil(o) == true) 
    {
      while (i->hasNext() == true) 
      {
        if (acdk_isNil(i->next()) == true) 
        {
          i->remove();
          return true;
        }
      }
    } else {
      while (i->hasNext() == true) 
      {
        if (acdk_equals(o, i->next()) == true) {
        // old: if (o->equals((RObject)i->next()) == true) {
          i->remove();
          return true;
        }
      }
    }
    return false;
  }

  /// reimplemented from Collection
  foreign virtual bool removeAll(IN(RCollectionType) c)
  {
    RIteratorType i = iterator();
    bool changed = false;
    while (i->hasNext() == true) {
      if (c->contains(i->next()) == true) {
        i->remove();
        changed = true;
      }
    }
    return changed;
  }

  /// reimplemented from Collection
  foreign virtual bool retainAll(IN(RCollectionType) c)
  {
    RIteratorType i = iterator();
    bool changed = false;
    while (i->hasNext() == true) 
    {
      if (c->contains(i->next()) == false) {
        i->remove();
        changed = true;
      }
    }
    return changed;
  }

  /// reimplemented from Collection
  foreign virtual RValueArrayType toArray()
  {
    RValueArrayType a(new ValueArrayType(this->size()));
    RIteratorType i = iterator();
    for (int pos = 0; pos < a->length(); pos++) 
    {
      a[pos] = i->next();
    }
    return a;
  }

  /// reimplemented from Collection
  foreign virtual RValueArrayType toArray(IN(RValueArrayType) a)
  {
    int n = this->size();
    if (a->length() < n) {
      a->resize(n);
    }
    RIteratorType i = iterator();
    for (int pos = 0; pos < n; pos++) {
      a[pos] = i->next();
    }
    if (a->length() > n) {
      a[n] = Nil;
    }
    return a;
  }


  foreign virtual RString toString()
  {
    StringBuffer s;
    s.append('[');
    RIteratorType i = iterator();
    bool more = i->hasNext();
    while (more == true) 
    {
      s.append(acdk_toString(i->next()));
      if ((more = i->hasNext()) == true) {
        s.append(", ");
      }
    }
    s.append(']');
    return s.toString();
  }
};


} // util
} // acdk



#endif //acdk_util_TAbstractCollection_h

