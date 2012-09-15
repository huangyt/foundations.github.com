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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/SharedOwning.h,v 1.10 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_SharedOwning_h
#define acdk_lang_ref_SharedOwning_h

#include <acdk.h>
#include "NotifyObjectEvent.h"

namespace acdk {
namespace lang {
namespace ref {

foreign
struct ACDK_CORE_PUBLIC SharedOwned
{
  Object* _obj;
  RObject* _refedBy;
  bool _isMaster;
  SharedOwned() : _obj(0), _refedBy(0), _isMaster(0) {}
  SharedOwned(Object* obj, RObject* refedBy, bool isMaster) : _obj(obj), _refedBy(refedBy), _isMaster(isMaster) {}
};

/**
  Implements mechanism for small garbage collection
  for local shared owning
*/
foreign
class ACDK_CORE_PUBLIC SharedOwning
: extends acdk::lang::Object
, implements ::acdk::lang::ref::NotifyObjectEventListener 
{
public:
  typedef acdk::lang::sys::core_vector<SharedOwned> SharedOwnedVec;
  SharedOwnedVec _sharedElements;
  SharedOwning()
  {
  }
  virtual ~SharedOwning();
  void registerSharedObject(Object* obj, RObject* refedBy, bool isMaster);
  void unregisterSharedObject(Object* obj, RObject* refedBy, bool onDelete);
  void registerSharedObjectRefs(RObject* masterRef, RObject* slaveRef);
  void unregisterSharedObjectRefs(RObject* masterRef, RObject* slaveRef, bool onDelete = false);

  void unregisterAll();
protected:
  foreign virtual void notifyBeforeConstruction(Object* obj) {}
  foreign virtual void notifyWhileDestruction(Object* obj) {}
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) { return false; }
  foreign virtual bool notifyBeforeDestruction(Object* obj) { return canRelease(obj); }
  
  void setMaster(SharedOwned& shown, bool isMaster);
  bool isNowMaster(SharedOwned& toDel, SharedOwned& el, SharedOwned& oldMaster);
  static SharedOwned& _getEmtpySharedOwned();
  SharedOwned& getMaster();
  SharedOwned& getElement(Object* obj);
  bool canRelease(Object* obj);
  void removeElement(Object* obj);
  foreign void dump(const char* msg = "");
};


} // ref
} // lang
} // acdk

#endif //acdk_lang_ref_SharedOwning_h

