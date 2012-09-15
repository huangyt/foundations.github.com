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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/TracedRawAllocator.cpp,v 1.18 2005/03/07 14:05:49 kommer Exp $

#include <acdk.h>

#include "TracedRawAllocator.h"
#include <acdk/lang/ref/NotifyObjectEvent.h>


#include "Allocator.h"

#include <map>
#include <set>

namespace acdk {
namespace lang {
namespace sys {


using namespace acdk::lang;

ACDK_DECL_SYS_CLASS(TracedRawAllocator);


//virtual 
void* 
TracedRawAllocator::allocate(size_t size, AllocatedType at)
{
  void* vret = AbstractAllocator::allocate(size, at);
  _heap.insert(std::pair<void*, TracedRawAllocatorInfo>(vret, 
                TracedRawAllocatorInfo(at, size)));

  ++_allocInfo.memTypes[at].count;
  return vret;
}

//virtual 
void 
TracedRawAllocator::deallocate(void* ptr, AllocatedType at)
{
  ObjectsSet::iterator it = _heap.find(ptr);
  if (it == _heap.end()) 
  {
    sys::coreout << "TracedRawAllocator::deallocate(" 
              << (void*)this << ", " << ptr 
              << "): Object cannot be found" << sys::eofl;
  } else {
      if (((AllocatedType)(*it).second.flags) != at)
        sys::coreout << "TracedRawAllocator::deallocate(" 
              << (void*)this << ", " << ptr 
              << "): ptr is wrong memory type: " << (int)(*it).second.flags << sys::eofl;
    _heap.erase(it);
  }
  --_allocInfo.memTypes[at].count;
  AbstractAllocator::deallocate(ptr, at);
}

//virtual 
void 
TracedRawAllocator::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  TracedRawAllocatorIterator trai(this);
  genericListObject(0, &trai, listener, flags);
  /*
  ObjectsSet::iterator it = _heap.begin();
  ObjectsSet::iterator end = _heap.end();
  while (it != end) 
  {
    if (listener->listedAllocated(0, (*it).first, AllocatedType((*it).second.flags), (*it).second.size) == false)
        return;
     
    ++it;
  }
  */
}

} // namespace sys 
} // namespace lang 
} // namespace acdk 



