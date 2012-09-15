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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Map.h,v 1.15 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Map_h
#define acdk_util_Map_h

#include "Collection.h"
#include "Set.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Map);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC Map
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Map)
public:
  

  virtual void clear() = 0;
  virtual bool containsKey(IN(RObject) key) = 0;
  virtual bool containsValue(IN(RObject) value) = 0;
  virtual RSet entrySet() = 0;
  virtual bool equals(IN(RObject) o) = 0;
  virtual RObject get(IN(RObject) key) = 0;
  virtual int hashCode() = 0;
  virtual bool isEmpty() = 0;
  virtual RSet keySet() = 0;
  virtual RObject put(IN(RObject) key, IN(RObject) value) = 0;
  virtual void putAll(IN(RMap) m) = 0;
  virtual RObject remove(IN(RObject) o) = 0;
  virtual int size() = 0;
  virtual RCollection values() = 0;

};


ACDK_DECL_INTERFACE(MapEntry);



class ACDK_CORE_PUBLIC MapEntry
      ACDK_INTERFACEBASE      
{
  ACDK_WITH_METAINFO(MapEntry)
public:
  virtual bool equals(IN(RObject) o) = 0;
  virtual RObject getKey() = 0;
  virtual RObject getValue() = 0;
  virtual int hashCode() = 0;
  virtual RObject setValue(IN(RObject) value) = 0;
};


} // util
} // acdk

#endif //acdk_util_Map_h

