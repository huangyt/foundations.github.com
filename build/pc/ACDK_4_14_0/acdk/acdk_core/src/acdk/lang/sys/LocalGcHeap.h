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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/LocalGcHeap.h,v 1.5 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_LocalGcHeap_h
#define acdk_lang_sys_LocalGcHeap_h

#include <acdk.h>

#include <map>

namespace acdk {
namespace lang {
namespace sys {

/**
  internal API

*/
class ACDK_CORE_PUBLIC LocalGcHeap
: public acdk::lang::Object //HeapFrame 
, implements ::acdk::lang::ref::NotifyObjectEventListener 
{
public:
  virtual Allocator* allocator();
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
  virtual bool gc(bool recursiv = true);

  struct SharedOwnedValue
  {
    RObject* referedBy;
    SharedOwnedValue(RObject* refBy)
      : referedBy(refBy)
    {}
    SharedOwnedValue() : referedBy(0) {}
    
  };
  typedef std::map<Object*, SharedOwnedValue> ObjectMap;
  ObjectMap _objects;
  LocalGcHeap(){}
  void addObject(Object* o, RObject* referedFrom = 0);
  /** remove this object from this heap */
  void eraseFromSet(Object* o);
  /** check if this object can be gced */
  static bool gcObject(Object* obj, RObject* referedFrom = 0);
  foreign virtual void notifyBeforeConstruction(Object* obj) {}
  foreign virtual void notifyWhileDestruction(Object* obj) {}
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) { return false; }
  foreign virtual bool notifyBeforeDestruction(Object* obj) { eraseFromSet(obj); return true; }
};


} // namespace sys
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_LocalGc_h

