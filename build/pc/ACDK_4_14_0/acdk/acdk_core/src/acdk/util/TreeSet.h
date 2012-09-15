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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TreeSet.h,v 1.10 2005/04/09 19:26:58 kommer Exp $

#ifndef acdk_util_TreeSet_h
#define acdk_util_TreeSet_h

#include "AbstractCollection.h"
#include "AbstractSet.h"
#include "TreeMap.h"
#include "SortedSet.h"

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

#include <acdk/lang/Boolean.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(AbstractSet);

ACDK_DECL_CLASS(TreeSet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:58 $
  
*/
class ACDK_CORE_PUBLIC TreeSet 
: extends AbstractSet,
  implements SortedSet, 
  implements acdk::lang::Cloneable, 
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(TreeSet)
private:
  RSortedMap _map;
public:
  static RObject create_instance() { return new TreeSet(RCollection(Nil)); }
  TreeSet(IN(RComparator) comp = Nil);
  TreeSet(IN(RCollection) coll);
  TreeSet(IN(RSortedSet) sortedSet);
  TreeSet(IN(RSortedMap) map);

  foreign virtual bool add(IN(RObject) object);
  foreign virtual bool addAll(IN(RCollection) coll);

  foreign virtual void clear()
  {
    _map->clear();
  }
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  foreign virtual RComparator comparator()
  {
    return _map->comparator();
  }
  
  foreign virtual bool contains(IN(RObject) object)
  {
    return _map->containsKey(object);
  }
  foreign virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  foreign virtual int size()
  {
    return _map->size();
  }

  foreign virtual bool remove(IN(RObject) object)
  {
    return _map->remove(object) != Nil;
  }

  foreign virtual RObject first()
  {
    return _map->firstKey();
  }

  foreign virtual RObject last()
  {
    return _map->lastKey();
  }

  
  foreign virtual RSortedSet subSet(IN(RObject) from, IN(RObject) to);

  foreign virtual RSortedSet headSet(IN(RObject) to)
  {
    return new TreeSet(_map->headMap(to));
  }

  foreign virtual RSortedSet tailSet(IN(RObject) from)
  {
    return new TreeSet(_map->tailMap(from));
  }

  foreign virtual RIterator iterator()
  {
    return _map->keySet()->iterator();
  }
  foreign virtual bool containsAll(IN(RCollection) coll)
  {
    return AbstractSet::containsAll(coll);
  }
  foreign virtual bool equals(IN(RObject) other)
  {
    return AbstractSet::equals(other);
  }
  foreign virtual int hashCode()
  {
    return AbstractSet::hashCode();
  }
  foreign virtual bool removeAll(IN(RCollection) coll)
  {
    return AbstractSet::removeAll(coll);
  }
  foreign virtual bool retainAll(IN(RCollection) coll)
  {
    return AbstractSet::retainAll(coll);
  }
  foreign virtual RObjectArray toArray()
  {
    return AbstractSet::toArray();
  }
  foreign virtual RObjectArray toArray(IN(RObjectArray) array)
  {
    return AbstractSet::toArray(array);
  }
protected:
  void _add(IN(RObject) object)
  {
    _map->put(object, (RObject)Boolean::getTRUE());
  }
};

} // util
} // acdk

#endif //acdk_util_TreeSet_h

