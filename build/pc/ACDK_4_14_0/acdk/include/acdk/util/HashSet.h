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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/HashSet.h,v 1.14 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_HashSet_h
#define acdk_util_HashSet_h

#include <acdk.h>
#include "AbstractSet.h"
#include "HashMap.h"
#include <acdk/lang/Boolean.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(HashSet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.14 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

class ACDK_CORE_PUBLIC HashSet
: extends AbstractSet
, implements acdk::lang::Cloneable
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(HashSet)
private:
  RHashMap _map;
public:
  static RObject create_instance() { return new HashSet(); }

  HashSet(int initialCapacity = HashMap::DEFAULT_CAPACITY, float initialLoadFactor = HashMap::DEFAULT_LOAD_FACTOR);
  virtual ~HashSet();
  foreign virtual bool add(IN(RObject) o);
  foreign virtual void clear() {  _map->clear(); }
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc);
  foreign virtual bool contains(IN(RObject) o)
  {
    return _map->containsKey(o);
  }
  foreign virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  foreign virtual RIterator iterator()
  {
    return _map->keySet()->iterator();
  }
  foreign virtual bool remove(IN(RObject) o)
  {
    return (_map->remove(o) != Nil);
  }
  foreign virtual int size() 
  {
    return _map->size();
  }
private:
  void _add(IN(RObject) o)
  {
    _map->put(o, (RObject)Boolean::getTRUE());
  }
};

} // util
} // acdk

#endif //acdk_util_HashSet_h

