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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/Allocator.h,v 1.21 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_sys_Allocator_h
#define acdk_lang_sys_Allocator_h

#include <acdk.h>
#include "core_string.h"
#include "core_fastmutex.h"
#include "SysRefHolder.h"

namespace acdk {
  namespace lang {
  namespace ref {
  class NotifyObjectEventListener;
  } // namespace ref
  } // namespace lang
} // namespace acdk





namespace acdk {
namespace lang {
namespace sys {


foreign struct AllocTypeInfo
{
  int count;
  int size;
};

/**
  information about the given allocator
*/
foreign
enum AllocatorTypeFlags
{
  StandardAllocatorType   = 0x0000,
  /**
    if this is true, allocated
    Object should threaded as stack object.
    References to stack object aren't synchronized
  */
  StackObjectAllocatorType      = 0x0001,
  
  /**
    Allocator support listing objects
  */
  SupportListObjectAllocatorType = 0x0004,
  /**
    Allocator itself doesn't need synchronization
  */  
  NoSyncAllocatorType           = 0x0008,
  /**
    Allocator doesn't rely on RefHolders reference counting
  */
  NoRefCountAllocatorType       = 0x0010,
  /**
    Allocator support / needs manual garbage collecting
    calling gc()
  */
  ManualGcAllocatorType   =       0x0020,
  /**
    Automatic gc 
  */
  AutoGcAllocatorType           = 0x0040
};

foreign class ACDK_CORE_PUBLIC AllocatorInfo
{
public:
  core_string name;
  AllocTypeInfo memTypes[MaxAllocatedType + 1];
  
  AllocatorInfo(const core_string& n)
  :  name(n)
  {
    memset(memTypes, 0, sizeof(memTypes));
  }
};

ACDK_DECL_SYS_CLASS(Allocator);


/**
  uses by Allocator
*/
foreign struct MemChunkHeader
{
  Allocator* allocator;
  size_t chunkSize;
};

/** 
  API: ACDK<br>
  Heaps that manages memory
  An Allocator has a little overhead:
  <pre>
  AllocatorHeader:
    0 - 4 (Allocator*)
    5 - 8 size_t size of allocated chunk including AllocatorHeader
  Object/Raw
    9 - n the allocated memory 
  </pre>
  @author Roger Rene Kommer
  @version $Revision: 1.21 $
  @date $Date: 2005/02/05 10:44:59 $
*/
class ACDK_CORE_PUBLIC Allocator
: public SysObject
{
public: 
 
  /**
    bit combination of AllocatorTypeFlags
  */
  int flags;

  Allocator(int allocatorFlags = StandardAllocatorType) : flags(allocatorFlags) {}
  
  /** type of the allocated object
    
    @see acdk::lang::TracedRawAllocatorInfo
  */
  
  virtual ~Allocator() { }
  /**
    combination of AllocatorTypeFlags
    @see AllocatorTypeFlags
  */
  inline int getFlags() { return flags; }
  /** 
    in case of GC or other scanning activities the Allocator should be locked.
    allocate() and deallocate should simply block 
  */
  virtual void lock() = 0;
  /** 
    in case of GC or other scanning activities the Allocator should be locked.
    allocate() and deallocate should simply block 
  */
  virtual void unlock() = 0;
  /**
    Allocates Memory.
    It is important, that the Allocator itself will be found in MemChunkHeader directly before this memory location)
    @param size size of Object (not including MemChunkHeader)
    @return the raw preinitialized Object not including MemChunkHeader
  */
  virtual void* allocate(size_t size, AllocatedType at = RawMem) = 0;
  /**
    Frees Memory
    @param ptr points to user object (not including MemChunkHeader)
  */
  virtual void deallocate(void* ptr, AllocatedType at = RawMem) = 0;
  
  /**
    @param flags combination of ListObjectsFlags
  */
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags) = 0;
  /**
    @return the filled AllocatorInfo
   */
  virtual const AllocatorInfo& getAllocatorInfo() = 0;
  /**
    try to garbage collect items
    @param threadStorage if true only object allocated in the current thread should be 
                          gced
    @param force if true also release internal buffer
    @return true if any storage was freed
  */
  virtual bool doGc(bool threadStorage = true, bool force = false) = 0;
  
  /**
    use to initialize Object flags
    probably has no effects
    @todo check if this function is superflous
  */
  void initObjectPtr(void* ptr, AllocatedType type);
  /**
    underlying allocation mechanism via operator ::new
    may call doGc and or throw OutOfMemoryError 
  */
  void* os_alloc(size_t size);
  /**
    dealocate memory via operator ::delete
  */
  void os_dealloc(size_t size, void* ptr);

  /** @internal */
  inline static MemChunkHeader* getHeaderFromObject(void* obj) { return (MemChunkHeader*)(((char*)obj) - ALIGNEDSIZEOF(MemChunkHeader)); }
  /** @internal */
  inline static MemChunkHeader* getHeaderFromChunk(void* obj) { return (MemChunkHeader*)obj; }
  /** @internal */
  inline static size_t getHeaderSize() { return ALIGNEDSIZEOF(MemChunkHeader); }
  /** @internal */
  inline static void* getObjectFromChunk(void* ptr) { return ((char*)ptr) + getHeaderSize(); }
  /** @internal */
  inline static void* getChunkFromObject(void* ptr) { return ((char*)ptr) - getHeaderSize(); }
};


ACDK_DECL_SYS_CLASS(AbstractAllocator);

/**
  The AbstractAllocator allocates for 
  Object types a little bit memory, where
  the beginning of the allocated block a pointer to
  the Allocator can be found.
  The returned Object pointer is at offset ALIGNEDSIZEOF(MemChunkHeader).

  All operations are protected by a simple/fast mutex
*/
class ACDK_CORE_PUBLIC AbstractAllocator
:  public Allocator
{
protected:
  core_fastmutex _lock;
public:
  AllocatorInfo _allocInfo;
  AbstractAllocator(int allocatorFlags, const core_string& name);
  virtual ~AbstractAllocator();
  virtual void lock() { if ((NoSyncAllocatorType & flags) == false) _lock.lock(); }
  virtual void unlock() { if ((NoSyncAllocatorType & flags) == false) _lock.unlock(); }
  
  /**
    @param size is including MemChunkHeader
  */
  virtual void* raw_allocate(size_t size, AllocatedType type) = 0;
  /**
    @param size is including MemChunkHeader
    @param ptr is including MemChunkHeader
  */
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type) = 0;
  
  virtual void* allocate(size_t size, AllocatedType at = RawMem);
  virtual void deallocate(void* ptr, AllocatedType at = RawMem);
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags) = 0;

  virtual const AllocatorInfo& getAllocatorInfo() { return _allocInfo; }
  virtual bool doGc(bool threadStorage = true, bool force = false);

  const char* name() const { return _allocInfo.name.c_str(); }

};

ACDK_DECL_SYS_CLASS(RawAllocator);

/**
  This is a thin wrapper to raw allocation
  used by RC_Heap.
*/
class ACDK_CORE_PUBLIC RawAllocator
:  public AbstractAllocator
{
public:
  RawAllocator()
  : AbstractAllocator(StandardAllocatorType, core_string("RawAllocator", false))
  {
  }
  ~RawAllocator() { }
  
  virtual void* raw_allocate(size_t size, AllocatedType type) { return os_alloc(size); }
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type) { os_dealloc(size, ptr); }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
  {
  }
};


} // namespace sys
} // namespace lang 
} // namespace acdk 


/** this is the standard allocator, which directs calls to new/delete */
ACDK_CORE_PUBLIC acdk::lang::sys::Allocator* stdalloc();

inline void* acdk_allocate(size_t size) 
{   
  acdk::lang::sys::Allocator* allocptr = ::acdk::lang::sys::ObjectHeap::allocator();
  if (allocptr == 0) // should never be!!!
    *reinterpret_cast<char*>(allocptr) = 0;
  return  allocptr->allocate(size); 
}

inline void acdk_deallocate(void* ptr) 
{ 
  ::acdk::lang::sys::Allocator::getHeaderFromObject(ptr)->allocator->deallocate(ptr);  
}

inline void* acdk_allocate(size_t size, ::acdk::lang::sys::Allocator* allocator) 
{ 
  return allocator->allocate(size); 
}
inline void acdk_deallocate(void* ptr, ::acdk::lang::sys::Allocator* allocator)
{ 
  allocator->deallocate(ptr); 
}



#endif //acdk_lang_Allocator_h

