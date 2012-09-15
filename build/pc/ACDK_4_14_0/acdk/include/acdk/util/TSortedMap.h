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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TSortedMap.h,v 1.10 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TSortedMap_h
#define acdk_util_TSortedMap_h

#include "TSet.h"
#include "TMap.h"
#include "TComparator.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

#define ACDK_DECL_SORTEDMAP(Type1, RType1, Type2, RType2) \
  ACDK_DECL_MAP(Type1, RType1, Type2, RType2); \
  typedef ::acdk::util::TSortedMap<RType1, RType2> Type1##To##Type2##SortedMap; \
  typedef Type1##To##Type2##SortedMap::RefType R##Type1##To##Type2##SortedMap

/**
  A typeded sorted map. 
  The keys in the map are sorted 

  @see @ref tmap
  @see TTreeMap
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/SortedMap.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class K, class V>
class TSortedMap
: implements TMap<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TSortedMap<RKeyType, RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;

  

  typedef ThisType SortedMapType;
  typedef RThisType RSortedMapType;

  typedef TComparator<RKeyType> ComparatorType;
  typedef typename ComparatorType::RefType RComparatorType;

  virtual RComparatorType comparator() = 0;
  virtual RKeyType firstKey() = 0;
  virtual RThisType headMap(IN(RKeyType) key) = 0;
  virtual RKeyType lastKey() = 0;
  virtual RThisType subMap(IN(RKeyType) fromKey, IN(RKeyType) toKey) = 0;
  virtual RThisType tailMap(IN(RKeyType) fromKey) = 0;
};


} // util
} // acdk

#endif //acdk_util_TSortedMap_h

