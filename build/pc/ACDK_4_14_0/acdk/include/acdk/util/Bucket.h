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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Bucket.h,v 1.15 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_Bucket_h
#define acdk_util_Bucket_h

#include "Map.h"
#include "BasicMapEntry.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(BucketNode);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC BucketNode
: extends BasicMapEntry
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(BucketNode)
private:
  RBucketNode _next;
public:
  /** used for serialisation */
  static RObject create_instance() { return new BucketNode(Nil, Nil); }

  BucketNode(IN(RObject) key, IN(RObject) value)
  : BasicMapEntry(key, value)
  {
  }
  foreign virtual RBucketNode next() { return _next; }
  friend class Bucket;
};


ACDK_DECL_CLASS(Bucket);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC Bucket
: extends acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Bucket)
private:
  RBucketNode _first;
public:
  Bucket()
  : Object()
  {
  }
  RMapEntry add(IN(RBucketNode) newNode);
  RObject removeByKey(IN(RObject) key);
  RObject getValueByKey(IN(RObject) key)
  {
    RBucketNode entry = getEntryByKey(key);
    if (entry == Nil)
      return Nil;
    return entry->getValue();
  }
  RBucketNode getEntryByKey(IN(RObject) key);
  bool containsValue(IN(RObject) value);
  RBucketNode first() { return _first; }
  friend class HashMap;
};


} // util
} // acdk

#endif // #define acdk_util_Bucket_h


