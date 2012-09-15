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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/HashSet.cpp,v 1.10 2005/03/08 12:45:45 kommer Exp $



#include <acdk.h>

#include "HashSet.h"


namespace acdk {
namespace util {

using namespace acdk::lang;

HashSet::HashSet(int initialCapacity, float initialLoadFactor)
{
  _map = new HashMap(initialCapacity, initialLoadFactor);
}

//virtual 
HashSet::~HashSet()
{
}

//virtual
bool 
HashSet::add(IN(RObject) o)
{
  if (_map->containsKey(o) == true) 
    return false;
  _add(o);
  return true;
}

//virtual 
RObject 
HashSet::clone(sys::Allocator* alc)
{
  RIterator it = iterator();
  RHashSet hashset = new (alc) HashSet(_map->capacity(), _map->loadFactor());
  while (it->hasNext())
    hashset->_add(it->next());
  return (RObject)hashset;
}


} // Util
} // acdk
