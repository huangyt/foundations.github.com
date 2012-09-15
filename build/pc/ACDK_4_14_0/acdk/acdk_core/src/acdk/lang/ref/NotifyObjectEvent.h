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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/NotifyObjectEvent.h,v 1.16 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_NotifyObjectEvent_h
#define acdk_lang_ref_NotifyObjectEvent_h





namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;
//class acdk::lang::sys::HeapFrame;


ACDK_DECL_INTERFACE(NotifyObjectEventListener);

/** 
  Abstract class for ObjectEvents
  API: Internal
  @author Roger Rene Kommer
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:44:59 $
  @see NotifyRepository
*/
class ACDK_CORE_PUBLIC NotifyObjectEventListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(NotifyObjectEventListener)
public:

  foreign virtual void notifyBeforeConstruction(Object* obj) = 0;
  /** called if RefCount == 0. 
    @return false if object should not be destroyd
    */
  foreign virtual bool notifyBeforeDestruction(Object* obj) = 0;
  /** 
    called in destructor of given object.
    Note virtual function will not work 
   */
  foreign virtual void notifyWhileDestruction(Object* obj) = 0;
  /**
    used as callback function to list all heaps of all threads.
    @param tid ThreadID of the Thread, which owns the Heap
    @param theheap
  */
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) = 0;
  /** 
    used as callback function, used by::acdk::lang::sys::HeapFrame::listObjects()
    @param thehead the current heap
    @param obj the Object in the heap
    @return to abort listing return false
  */
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) = 0;
};

/** 
  
  Internal API
  @author Roger Rene Kommer
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:44:59 $
  @see NotifyRepository
*/

class ACDK_CORE_PUBLIC NotifyObjectEvent
{
public:
  /**
    add element to listining at object events
  */
  static void add(Object* obj, NotifyObjectEventListener* el);
  
  /** 
    remove listener on all objects
  */
  static void removeListener(NotifyObjectEventListener* el);
  
  /** 
      removes object and all connected listener (without notification) 
  */
  static void removeObject(Object* obj);

  
  /** 
    removes map entry of object to listener.
    Listener may still listen on other listener.
    Objects itself are not removed.
  */
  static void remove(Object* obj, NotifyObjectEventListener* el);
  

  /** notify all listener that object will be destroy.
      if ohne listener returns false, Object will not 
      be destroyed an no further listener will be notified
  */
  static bool notifyBeforeDestruction(Object* obj);

  /** 
    notifies all listener, calles remove internal remove(Object* obj) 
  */
  static void notifyWhileDestruction(Object* obj);
};



class ACDK_CORE_PUBLIC AbstractHeapListener  
: extends ::acdk::lang::Object
, implements ::acdk::lang::ref::NotifyObjectEventListener
{
public:
  virtual void notifyBeforeConstruction(Object* obj) { }
  virtual bool notifyBeforeDestruction(Object* obj) { return true; }
  virtual void notifyWhileDestruction(Object* obj) { }
  virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap)  { return false; }
  virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) 
  { 
    return false;
  }
};


} // namespace ref
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_ref_NotifyObjectEvent_h

