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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ArrayList.h,v 1.16 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_ArrayList_h
#define acdk_util_ArrayList_h
#include <acdk.h>
#include "AbstractList.h"
#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>


#include <acdk/lang/IndexOutOfBoundsException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
//using namespace acdk::io;  


ACDK_DECL_CLASS(ArrayList);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  isfinal
*/
class ACDK_CORE_PUBLIC ArrayList
: extends AbstractList,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(ArrayList)
public:
  static const int DEFAULT_CAPACITY;
  int _size;
  RObjectArray _data;
public:
  static RObject create_instance() { return new ArrayList(); }
  ArrayList(int capacity = DEFAULT_CAPACITY);
  ArrayList(IN(RCollection) other);
  /**
    create ArrayList from array
    @param array the array
    @param copy if false take over the reference of array as internal
           data. Modification to this ArrayList will also be reflected
           in the array parameter.
  */
  ArrayList(IN(RObjectArray) array, bool copy);
  virtual ~ArrayList();
  
  virtual void ensureCapacity(int mincap);
  foreign virtual bool add(IN(RObject) o);
  foreign virtual bool containsAll(IN(RCollection) c) { return AbstractCollection::containsAll(c); }
  
  foreign RObject get(int index)
  {
    if (index >= _size)
      THROW0(IndexOutOfBoundsException);
    return _data[index];
  }
  foreign int size()
  {
    return _size;
  }
  foreign bool isEmpty() { return size() == 0; }
  
  foreign RObject remove(int index);
  foreign bool removeAll(IN(RCollection) c) { return AbstractCollection::removeAll(c); }
  foreign bool retainAll(IN(RCollection) c) { return AbstractCollection::retainAll(c); }
  foreign bool remove(IN(RObject) o);
  foreign void removeRange(int fromIndex, int toIndex);
  foreign void add(int index, IN(RObject) obj);
  foreign bool addAll(IN(RCollection) coll);
  foreign bool addAll(int index, IN(RCollection) coll);
  foreign RObject clone() { return clone(allocator()); }
  foreign RObject clone(sys::Allocator* alc);
  
  foreign bool contains(IN(RObject) obj)
  {
    return (indexOf(obj) != -1);
  }
  foreign int indexOf(IN(RObject) obj);
  foreign int lastIndexOf(IN(RObject) obj);
  foreign RListIterator listIterator(int index = 0) 
  {
    return AbstractList::listIterator(index); 
  }
  foreign void clear();
  foreign RObject set(int index, IN(RObject) obj);
  foreign RObjectArray toArray();
  foreign RObjectArray toArray(IN(RObjectArray) oarray);
  foreign void trimToSize();
private:
  inline static bool _isEqual(IN(RObject) o1, IN(RObject) o2)
  {
    return o1 == Nil ? o2 == Nil : o1->equals(o2);
  }

};

} // util
} // acdk

#endif // acdk_util_ArrayList_h

