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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/Reference.h,v 1.14 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_Reference_h
#define acdk_lang_ref_Reference_h

#include <acdk.h>
#include "NotifyObjectEvent.h"
#include "ReferenceQueue.h"

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

ACDK_DECL_CLASS(Reference);
ACDK_DECL_CLASS(ReferenceQueue);


/** 
  The Reference class is similar to the corrsponding Java class.

  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/02/05 10:44:59 $
  @see NotifyRepository
*/

class ACDK_CORE_PUBLIC Reference
: extends ::acdk::lang::Object,
  implements NotifyObjectEventListener 
{
  ACDK_WITH_METAINFO(Reference)
protected:
  foreign friend class ReferenceQueue;
  foreign Object* _ref;
  bool _releaseReference;
  RReferenceQueue _queue;
  bool _isOnQueue;
  bool _isDequeued;
  /** for linked list in Queeue */
  RReference _next;
  bool _inDestructor;
public:
  /**
    Standard constructor
    @param ref the reference to hold. 
                do not pass a Object* to this constructor if queue !+ Nil, because 
                this may causes problems with the GC with a dangling pointer on MT & SMP environments
    @param queue which collects released references
  */
  Reference(IN(RObject) ref, IN(RReferenceQueue) queue = Nil);

  ~Reference();

  virtual RObject get();
  foreign Object* objectPtr() { return _ref; }
  virtual void clear();
  virtual bool isEnqueued();
  bool isDequeued() { return _isDequeued; }
  virtual bool enqueue();

  // interfaces from::acdk::lang::sys::NotifyObjectEventListener 
  foreign virtual void notifyBeforeConstruction(Object* obj);
  foreign virtual bool notifyBeforeDestruction(Object* obj);
  foreign virtual void notifyWhileDestruction(Object* obj);
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) { return false; }
};

} // ref
} // lang
} // acdk

#endif //acdk_lang_ref_Reference_h

