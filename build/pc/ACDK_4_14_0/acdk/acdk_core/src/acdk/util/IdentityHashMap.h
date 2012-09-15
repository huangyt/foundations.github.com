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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/IdentityHashMap.h,v 1.4 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_IdentityHashMap_h
#define acdk_util_IdentityHashMap_h

#include <acdk.h>
#include "HashMap.h"

namespace acdk {
namespace util {


inline int _identityHashFunc(IN(RObject) obj)
{
  return (int)(void*)obj.impl();
}

ACDK_DECL_CLASS(IdentityHashMap);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC IdentityHashMap
: extends HashMap
{
  ACDK_WITH_METAINFO(IdentityHashMap)
public:
  static RObject create_instance() { return new IdentityHashMap(); }

  IdentityHashMap(int initialCapacity = HashMap::DEFAULT_CAPACITY, float initialLoadFactor = HashMap::DEFAULT_LOAD_FACTOR)
    : HashMap(initialCapacity, initialLoadFactor)
  {
    set_hashFunc(_identityHashFunc);
  }
  
  
};

} // util
} // acdk

#endif //acdk_util_IdentityHashMap_h

