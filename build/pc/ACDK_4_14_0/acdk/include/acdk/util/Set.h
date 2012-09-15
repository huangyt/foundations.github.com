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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Set.h,v 1.12 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Set_h
#define acdk_util_Set_h

#include "Collection.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Set);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC Set
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Set)
public:
  virtual bool add(IN(RObject) o) = 0;
  virtual bool addAll(IN(RCollection) c) = 0;
  virtual void clear() = 0;
  virtual bool contains(IN(RObject) o) = 0;
  virtual bool containsAll(IN(RCollection) c) = 0;
  virtual bool equals(IN(RObject) o) = 0;
  virtual int hashCode() = 0;
  virtual bool isEmpty() = 0;
  virtual RIterator iterator() = 0;
  virtual bool remove(IN(RObject) o) = 0;
  virtual bool removeAll(IN(RCollection) c) = 0;
  virtual bool retainAll(IN(RCollection) c) = 0;
  virtual int size() = 0;
  virtual RObjectArray toArray() = 0;
  virtual RObjectArray toArray(IN(RObjectArray) array) = 0;
};


} // util
} // acdk

#endif //acdk_util_Set_h

