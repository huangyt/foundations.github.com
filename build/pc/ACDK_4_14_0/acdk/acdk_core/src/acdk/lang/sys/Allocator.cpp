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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/Allocator.cpp,v 1.30 2005/03/07 13:53:35 kommer Exp $


#include <acdk.h>

#include "Allocator.h"
#include "core_system.h"
#include "core_value_scope.h"
#include "core_sys_static_mutex.h"
#include <acdk/lang/Thread.h>
#include <acdk/lang/OutOfMemoryError.h>

#include <new>


//#define ALLOCATOR_DEBUG
#include <map>
#ifdef ALLOCATOR_DEBUG
#include <set>

typedef std::set<void*> ObjectSet;
ObjectSet allocated_objects;



#define DOUT(msg) \
 do { sys::coreout << msg; } while (false)


#define UNREGISTEROBJECT(obj) unregisterObject(obj)




void panic()
{
  char* ptr = 0;
  *ptr = 0;
}

void unregisterObject(void* obj)
{
  if (::acdk::lang::sys::core_system::inMain() == true) { 
    ObjectSet::iterator it = allocated_objects.find(obj); 
    if (it == allocated_objects.end()) { 
      sys::coreout << "allocator: ptr " << obj << " unknown!" << sys::eofl; 
      panic();
     } else { 
      allocated_objects.erase(it); 
    } 
  }
}


#define REGISTEROBJECT(obj) registerObject(obj)

void registerObject(void* obj)
{ 
  if (::acdk::lang::sys::core_system::inMain() == true) { 
    ObjectSet::iterator it = allocated_objects.find(obj); 
    if (it != allocated_objects.end()) { 
      sys::coreout << "allocator: ptr " << obj << " already registered" << sys::eofl; 
      panic();
    } else { 
      allocated_objects.insert(obj); 
    } 
  } 
} 


#else //ALLOCATOR_DEBUG

#define DOUT(msg) do { } while (false)
#define REGISTEROBJECT(obj) do { } while (false)
#define UNREGISTEROBJECT(obj) do { } while (false)

#endif //ALLOCATOR_DEBUG



namespace acdk {
namespace lang {
namespace sys {

#if defined(__BORLANDC__)
int __bccGetSP()
{
  int var = 0;
  return (int)&var;
  /*
  unsigned int _esp_val;
  __asm mov _esp_val, esp;
  return _esp_val;
  */
}

#endif //defined(__BORLANDC__)

using namespace acdk::lang;

void 
Allocator::initObjectPtr(void* ptr, AllocatedType type)
{
  if (type != ObjectMem)
    return;
  ::acdk::lang::ObjectBase* obase = (::acdk::lang::ObjectBase*)ptr;
  obase->_inititializeObjectAttrFlags(::acdk::lang::ObjectBase::IsUserHeapRef);
}
  

void throwOutOfMem(int triedToAllocateSize)
{
  
  //ObjectHeap::pushFrame(ObjectHeap::RC_Heap);
  RStringBuffer sb = new (stdalloc()) StringBuffer(245);
  sb->append("OutOfMemory. MaxGlobalSpace: ");
  sb->append(ObjectHeap::getMaxMemoryUsage());
  sb->append("; MaxtThreadSpace: ");
  sb->append(ObjectHeap::getMaxMemoryUsage());
  sb->append("; CurGlobalUsage: ");
  sb->append(ObjectHeap::curMemUsage());
  sb->append("; CurThreadUsage: ");
  sb->append(ObjectHeap::curThreadMemUsage());

  sb->append("; Tried to allocate: ");
  sb->append(triedToAllocateSize);
  ROutOfMemoryError oufm = new (stdalloc()) OutOfMemoryError(sb->toString());
  
  /*int *iptr = 0;
  *iptr = 0;*/
  throw oufm;
}
  
jlong object_count = 0;

typedef std::map<size_t, int> ObjectSizeMap;
typedef std::map<void*, size_t> ObjectPtrSizeMap;

ObjectPtrSizeMap&
getObjectPtrSizeMap()
{
  static ObjectPtrSizeMap objectPtrSizeMap;
  return objectPtrSizeMap;
}

ObjectSizeMap&
getObjectSizeMap()
{
  static ObjectSizeMap allocate_objects_count;
  return allocate_objects_count;
}

void add_object_allocatated(void* ptr, size_t size)
{
  //sys::coreout << "os_alloc: " << ptr << ": " << size << sys::eofl;
  return;
  ObjectSizeMap& allocate_objects_count = getObjectSizeMap();
  ObjectPtrSizeMap& osm = getObjectPtrSizeMap();
  if (osm.find(ptr) != osm.end())
  {
    sys::coreout << "ALLOC ERR: reallocated but no freed" << sys::eofl;
  }
  osm[ptr] = size;

  if (allocate_objects_count.find(size) == allocate_objects_count.end())
    allocate_objects_count[size] = 1;
  else
    allocate_objects_count[size] = allocate_objects_count[size] + 1;
}

void remove_object_allocatated(void* ptr, size_t size)
{
  //sys::coreout << "os_dealloc: " << ptr << ": " << size << sys::eofl;
  return;
  ObjectSizeMap& allocate_objects_count = getObjectSizeMap();
  ObjectPtrSizeMap& osm = getObjectPtrSizeMap();
  ObjectPtrSizeMap::iterator it = osm.find(ptr);
  if (it == osm.end())
  {
    sys::coreout << "ALLOC ERR: removed but no found: " << ptr << ": " << (int)size << sys::eofl;
  } 
  else
    osm.erase(it);

  if (allocate_objects_count.find(size) == allocate_objects_count.end() ||
      allocate_objects_count[size] == 0)
  {
    /*ObjectSizeMap::iterator it = allocate_objects_count.begin();
    ObjectSizeMap::iterator end = allocate_objects_count.end();
    for (; it != end; ++it)
    {
      if (it->second != 0)
        sys::coreout << it->first << ": " << it->second << "; ";
    }
    sys::coreout << sys::eofl;*/
    sys::coreout << "ALLOC ERR: Object in size not found: " << ptr << ": " << (int)size << sys::eofl;
  }
  else
  {
    allocate_objects_count[size] = allocate_objects_count[size] - 1;
  }

}

void* 
Allocator::os_alloc(size_t size)
{
  if (size > 4000)
  {
    ++object_count;
    --object_count;
  }
  ++object_count;
  
tryAgain:
  //getAllocatorInfo()
  jlong& maxGlob = ObjectHeap::maxMemoryUsage();
  jlong& istGlob = ObjectHeap::curMemUsage();
  jlong& maxLoc = ObjectHeap::threadMaxMemoryUsage();
  jlong& istLoc = ObjectHeap::curThreadMemUsage();
  bool overflow = false;
  bool globalOverflow = false;
  
  if (maxGlob != -1  && maxGlob < (istGlob + size))
  {
    globalOverflow = true;
    overflow = true;
  }
  else if (maxLoc != -1 && maxLoc < (istLoc + size))
  {
    overflow = true;
  }
  static bool recursion_guard = false;

  bool force = true;
  if (recursion_guard == false && overflow == true)
  {
    core_value_scope<bool> _recGuard(recursion_guard, true);
    if (doGc(globalOverflow == false, force) == false)
    {
      if (maxGlob < (istGlob + size) || maxLoc < (istLoc + size))
        throwOutOfMem(size);
    }
    else 
      goto tryAgain;
  }
  void* ret = 0;
  try {
    ret = ::operator new(size);
  } catch(const std::bad_alloc&) {
  }
  if (ret == 0)
  {
    if (doGc(false, force) == true)
      goto tryAgain;
    throwOutOfMem(size);
  }
  istGlob += size;
  istLoc += size;
  add_object_allocatated(ret, size);
  return ret;
}

void 
Allocator::os_dealloc(size_t size, void* ptr)
{
  --object_count;
  remove_object_allocatated(ptr, size);
  ObjectHeap::curMemUsage() -= size;
  ObjectHeap::curThreadMemUsage() -= size;
  
  ::operator delete(ptr);
}

sys::core_output& 
operator<<(sys::core_output& os, const AbstractAllocator& alloc)
{
  os << (void*)&alloc << ", " << alloc.name();
  return os;
}

AbstractAllocator::AbstractAllocator(int allocatorFlags, const core_string& name)
: Allocator(allocatorFlags)
,  _lock()
,  _allocInfo(name)
  
{
  DOUT("AbstractAllocator(" << *this << ")" << sys::eofl);
}

AbstractAllocator::~AbstractAllocator()
{
  DOUT("~AbstractAllocator(" << *this << ")" << sys::eofl);
}

#define ALLOCATORLOCK() SysStaticMutexLockGuard<AbstractAllocator> lockthis(Thread::isSingleThreaded() == false, *this)

//virtual 
void* 
AbstractAllocator::allocate(size_t size, AllocatedType type)
{
  //core_lock_guard<AbstractAllocator> lock(*this);
  ALLOCATORLOCK();
  addRef();
  ++_allocInfo.memTypes[type].count;
  char* chunk = (char*)raw_allocate(size + getHeaderSize(), RawMem); 
  MemChunkHeader* header = getHeaderFromChunk(chunk);
  header->allocator = this;
  header->chunkSize = size + getHeaderSize();
  
  void* obj = getObjectFromChunk(chunk);
  initObjectPtr(obj, type);
  
  REGISTEROBJECT(obj);
  DOUT("AbstractAllocator::allocate(" << *this << "): " << (void*)obj << sys::eofl);
  return obj;
}
  

//virtual 
void 
AbstractAllocator::deallocate(void* ptr, AllocatedType type)
{
  
  //core_lock_guard<AbstractAllocator> lock(*this);
  ALLOCATORLOCK();

  DOUT("AbstractAllocator::deallocate(" << *this << "): " << ptr << sys::eofl);
  UNREGISTEROBJECT(ptr);
  MemChunkHeader* ch = getHeaderFromObject(ptr);
  raw_deallocate(ch->chunkSize, getChunkFromObject(ptr), RawMem);
  --_allocInfo.memTypes[type].count;
  releaseRef();
}

//virtual 
bool 
AbstractAllocator::doGc(bool threadStorage, bool force)
{
  if (threadStorage == true && force == false)
    return false;
  return ObjectHeap::gc(true);
}


RawAllocator* _rawAllocator()
{
  static RawAllocator* __rawAllocator = 0;
  if (__rawAllocator == 0) 
  {
    __rawAllocator = new RawAllocator();
    __rawAllocator->addRef();
  }
  return __rawAllocator;
}

} // namespace sys
} // namespace lang 
} // namespace acdk 

ACDK_CORE_PUBLIC 
acdk::lang::sys::Allocator* stdalloc()
{
  return acdk::lang::sys::_rawAllocator(); // #### use really this allocator?
}

