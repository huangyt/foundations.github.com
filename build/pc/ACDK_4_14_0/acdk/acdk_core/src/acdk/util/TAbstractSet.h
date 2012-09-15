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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TAbstractSet.h,v 1.10 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TAbstractSet_h
#define acdk_util_TAbstractSet_h

#include <acdk.h>
#include <acdk/lang/NullPointerException.h>
#include "TSet.h"
#include "TAbstractCollection.h"
#include "Arrays.h"

namespace acdk {
namespace util {


using namespace acdk::lang;


//ACDK_DECL_CLASS(TAbstractSet);

/**
  @internal
  Basis abstract implementation for a Set.
  
  @see acdk::util::TSet
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class  TAbstractSet
: extends TAbstractCollection<T>
, implements TSet<T>
{
  
public:
  typedef T RValueType;
  
  typedef TAbstractSet<RValueType> ThisCollectionType;
  typedef RefHolder<TAbstractSet<RValueType> > RThisCollectionType;
  typedef RThisCollectionType RefType;
  
  typedef TAbstractCollection<RValueType> SuperType;
  typedef TSet<RValueType> SetType;
  typedef typename SetType::RefType RSetType;

  typedef typename SuperType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef RObjectArrayImpl<RValueType> RValueArrayType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;

  foreign virtual bool equals(IN(RObject) o)
  {
     if (o.impl() == (const Object*)this) {
      return true;
      } else if (instanceof(o, SetType) && RSetType(o)->size() == size()) {
        return containsAll((RCollectionType)o);
      } else {
      return false;
    }
  }
  foreign virtual int hashCode()
  {
    int hash = 0;
    RIteratorType i = iterator();
    while (i->hasNext()) 
    {
      try {
        hash += acdk_hashCode(i->next());
#if __GNUC__ == 3 && __GNUC_MINOR__ == 3// && __GNUC_PATHLEVEL__ == 1
      } catch (RThrowable) {
#else
      } catch (RNullPointerException) {
#endif
        // gcc 3.3.2 troubles: } catch (::acdk::lang::RNullPointerException ) {
      }
    }
    return hash;
  }
  foreign virtual bool add(IN(RValueType) o) { return SuperType::add(o); }
  foreign virtual bool addAll(IN(RCollectionType) c) { return SuperType::addAll(c); }
  foreign virtual void clear() { SuperType::clear(); }
  foreign virtual bool contains(IN(RValueType) o) { return SuperType::contains(o); }
  foreign virtual bool containsAll(IN(RCollectionType) c) { return SuperType::containsAll(c); }
  foreign virtual bool isEmpty() { return SuperType::isEmpty(); }
  foreign virtual RIteratorType iterator() = 0;
  foreign virtual bool remove(IN(RValueType) o) { return SuperType::remove(o); }
  foreign virtual bool removeAll(IN(RCollectionType) c) { return SuperType::removeAll(c); }
  foreign virtual bool retainAll(IN(RCollectionType) c) { return SuperType::retainAll(c); }
  
  foreign virtual int size() = 0;

  foreign virtual RValueArrayType toArray() { return SuperType::toArray(); }
  foreign virtual RValueArrayType toArray(IN(RValueArrayType) array) { return SuperType::toArray(array); }

};


} // util
} // acdk

#endif //acdk_util_TAbstractSet_h

