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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BoehmGCAllocator.h,v 1.13 2005/03/07 13:47:28 kommer Exp $

#ifndef acdk_lang_sys_BoehmGCAllocator_h
#define acdk_lang_sys_BoehmGCAllocator_h

#include "core_threadsys.h"

//#ifdef ACDK_HAS_BOEHMGC


#include "HeapFrame.h"

#include "core_mutex.h"
#include "core_fastmutex.h"
#include "ObjectLockPool.h"
#include "SysRefHolder.h"
#include "BoehmGC.h"

namespace acdk {
namespace lang {
namespace sys {

#if defined(ACDK_DEBUG)
#define GC_DEBUG
#endif

ACDK_DECL_SYS_CLASS(BoehmGCAllocator);
class ACDK_CORE_PUBLIC  BoehmGCAllocator
: public AbstractAllocator
{
public:
  BoehmGCAllocator()
    : AbstractAllocator(NoRefCountAllocatorType | AutoGcAllocatorType, core_string("BoehmGCAllocator", false))
  {
  }
  static void finalizer_callback(void *slot, void* user);
  /* ###old implementation
  void* allocateObject(size_t size);
  void deallocateObject(void* ptr);
  */
  virtual void* allocate(size_t size, AllocatedType at = RawMem);
  virtual void deallocate(void* ptr, AllocatedType at = RawMem);
  void* gcalloc(size_t size, AllocatedType at) ;
  // Allocator
  virtual void* raw_allocate(size_t size, AllocatedType at);
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType at);
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
};


class ACDK_CORE_PUBLIC BoehmGCHeapFrame
: public HeapFrame 
{
  RBoehmGCAllocator _allocator;
  //## not implemented yetGC_warn_proc _oldWarn;
public:
  //static void gc_debug_messages(char *msg, GC_word arg);

  BoehmGCHeapFrame(RHeapFrame top = Nil, int flags = HeapHasGC | HeapIsConsGC, const char* name = "") 
  : HeapFrame (top, flags, name),
    _allocator(new BoehmGCAllocator())
  {
    //## not implemented yet_oldWarn = GC_set_warn_proc(gc_debug_messages);
  }
  virtual ~BoehmGCHeapFrame() 
  { 
    /* ## not implemented yetif (_oldWarn != 0)
      GC_set_warn_proc(_oldWarn);
    */
  }
  virtual void add(const Object* obj, size_t size) { }

  /** will called after finalize, before 'delete obj' */
  virtual bool onDestroy(Object* obj) { return false; }
  /** will called in ~ObjectBase(); */
  virtual bool removeObject(const Object* obj, bool recursiv = true) { return true; }
  virtual bool hasObject(const Object* obj, bool recursiv = true) const
  {
    return false;
  }
  virtual void printObjects(sys::core_output& os, bool recursiv = true) const { }
  virtual int objectCount(bool recursiv = true) const { return 0; }
  virtual size_t totalAllocated(bool recursiv = true) const { return 0; }
  virtual bool checkCyclicReferences(bool recursiv = true) const { return false; }
  virtual bool gc(bool recursiv = true);
  virtual bool gc(const Object* obj, bool recursiv = true) 
  {
    return true;
  }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags) 
  {
  }
  virtual Allocator* allocator() { return _allocator.getImpl(); }

};

} // namespace sys
} // namespace lang 
} // namespace acdk
 
//#endif //ACDK_HAS_BOEHMGC

#endif //acdk_lang_Allocator_h

