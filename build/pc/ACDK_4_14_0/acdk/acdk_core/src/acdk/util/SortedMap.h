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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SortedMap.h,v 1.12 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_SortedMap_h
#define acdk_util_SortedMap_h

#include "Set.h"
#include "Map.h"
#include "Comparator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(SortedMap);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC SortedMap
: implements Map
{
  ACDK_WITH_METAINFO(SortedMap)
public:
  // Map
  overwrite void clear() = 0;
  overwrite bool containsKey(IN(RObject) key) = 0;
  overwrite bool containsValue(IN(RObject) value) = 0;
  overwrite RSet entrySet() = 0;
  overwrite bool equals(IN(RObject) o) = 0;
  overwrite RObject get(IN(RObject) key) = 0;
  overwrite int hashCode() = 0;
  overwrite bool isEmpty() = 0;
  overwrite RSet keySet() = 0;
  overwrite RObject put(IN(RObject) key, IN(RObject) value) = 0;
  overwrite void putAll(IN(RMap) m) = 0;
  overwrite RObject remove(IN(RObject) o) = 0;
  overwrite int size() = 0;
  overwrite RCollection values() = 0;

  // SortedMap
  virtual RComparator comparator() = 0;
  virtual RObject firstKey() = 0;
  virtual RSortedMap headMap(IN(RObject) key) = 0;
  virtual RObject lastKey() = 0;
  virtual RSortedMap subMap(IN(RObject) fromKey, IN(RObject) toKey) = 0;
  virtual RSortedMap tailMap(IN(RObject) fromKey) = 0;
};


} // util
} // acdk

#endif //acdk_util_SortedMap_h

