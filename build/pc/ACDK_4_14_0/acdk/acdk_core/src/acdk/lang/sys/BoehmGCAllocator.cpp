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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BoehmGCAllocator.cpp,v 1.13 2005/03/07 13:47:28 kommer Exp $

#include <acdk.h>
#include "BoehmGCAllocator.h"


//#ifdef ACDK_HAS_BOEHMGC

#define DOUT(msg) do { sys::coreout << msg << sys::eofl; } while (false)
//#define DOUT(msg) do { } while(false)

namespace acdk {
namespace lang {
namespace sys {

namespace {



} // anon namespace


//static 
void 
BoehmGCAllocator::finalizer_callback(void *slot, void* user)
{
  Object* obase = (Object*)getObjectFromChunk(slot);
  //DOUT("dispose ref: [" << obase->getClass()->toString()->c_str() << "] [" << obase->toString()->c_str() << " " << user);
  obase->disposeRef();
}

void* 
BoehmGCAllocator::gcalloc(size_t size, AllocatedType at) 
{
  if (at == DumpBufferMem)
    return BoehmGC::malloc_atomic(size);
  else if (at == NoGcMem)
    return BoehmGC::malloc_uncollectable(size);
  return BoehmGC::malloc(size);
}


//virtual 
void* 
BoehmGCAllocator::raw_allocate(size_t size, AllocatedType at) 
{ 
  //initGc();
  void* ret = gcalloc(size, at);
  if (ret == 0)
  {
    DOUT("gcalloc returned 0: start collecting");
    BoehmGC::collect();
    ret = gcalloc(size, at);
  }
  return ret;
}

//virtual 
void 
BoehmGCAllocator::raw_deallocate(size_t size, void* ptr, AllocatedType) 
{ 
  BoehmGC::free(ptr); 
}

//virtual 
void* 
BoehmGCAllocator::allocate(size_t size, AllocatedType at)
{
  //initGc();
  char* chunk = (char*)raw_allocate(size + getHeaderSize(), RawMem); 
  MemChunkHeader* header = getHeaderFromChunk(chunk);
  header->allocator = this;
  header->chunkSize = size + getHeaderSize();
  
  void* obj = getObjectFromChunk(chunk);
  if (at == ObjectMem)
  {
    ObjectBase* obase = (ObjectBase*)obj;
    obase->_inititializeObjectAttrFlags(ObjectBase::IsUserHeapRef);
    BoehmGC::register_finalizer_ignore_self(chunk, finalizer_callback, 0, 0, 0);
  }
  return obj;
}

//virtual 
void 
BoehmGCAllocator::deallocate(void* ptr, AllocatedType at)
{
  BoehmGC::register_finalizer_ignore_self(BoehmGC::base(ptr), 0, 0, 0, 0);
  AbstractAllocator::deallocate(ptr, at);
}


void 
BoehmGCAllocator::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
}

//virtual 
bool 
BoehmGCHeapFrame::gc(bool recursiv) 
{
  //GC_dump_regions();
  if (recursiv == false)
  {
    //GC_dump_regions();
    BoehmGC::collect();
    
    return true;
  }
  /* ### not yet implemented 
    while (GC_collect_a_little()) 
    ;
  */
  for (int i = 0; i < 16; i++) 
  {
    BoehmGC::collect();
  }
  return true;
}

/*
//static 
void 
BoehmGCHeapFrame::gc_debug_messages(char *msg, GC_word arg)
{
  sys::coreout << "BoehmGC Warn: " <<  msg << " " << arg << sys::eofl;
}
*/

} // namespace sys
} // namespace lang 
} // namespace acdk
 
//#endif //ACDK_HAS_BOEHMGC



