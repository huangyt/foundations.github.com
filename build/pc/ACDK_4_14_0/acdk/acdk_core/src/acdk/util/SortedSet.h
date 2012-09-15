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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SortedSet.h,v 1.10 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_SortedSet_h
#define acdk_util_SortedSet_h

#include "Set.h"
#include "Comparator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(SortedSet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
ACDK_INTERFACE class ACDK_CORE_PUBLIC SortedSet 
: implements Set
{
  ACDK_WITH_METAINFO(SortedSet)
public:
  // Set
  overwrite bool add(IN(RObject) o) = 0;
  overwrite bool addAll(IN(RCollection) c) = 0;
  overwrite void clear() = 0;
  overwrite bool contains(IN(RObject) o) = 0;
  overwrite bool containsAll(IN(RCollection) c) = 0;
  overwrite bool equals(IN(RObject) o) = 0;
  overwrite int hashCode() = 0;
  overwrite bool isEmpty() = 0;
  overwrite RIterator iterator() = 0;
  overwrite bool remove(IN(RObject) o) = 0;
  overwrite bool removeAll(IN(RCollection) c) = 0;
  overwrite bool retainAll(IN(RCollection) c) = 0;
  overwrite int size() = 0;
  overwrite RObjectArray toArray() = 0;
  overwrite RObjectArray toArray(IN(RObjectArray) array) = 0;

  // SortedSet
  virtual RComparator comparator() = 0;
  virtual RObject first() = 0;
  virtual RSortedSet headSet(IN(RObject) toElement) = 0;
  virtual RObject last() = 0;
  virtual RSortedSet subSet(IN(RObject) fromElement, IN(RObject) toElement) = 0;
  virtual RSortedSet tailSet(IN(RObject) fromElement) = 0;
};


} // util
} // acdk

#endif //acdk_util_SortedSet_h

