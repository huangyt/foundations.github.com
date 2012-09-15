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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BitmapPagedAllocator.cpp,v 1.8 2005/02/05 10:45:00 kommer Exp $


#include <acdk.h>

#include "BitmapPagedAllocator.h"

namespace acdk {
namespace lang {
namespace sys {

//virtual 
BitmapPagedAllocator::~BitmapPagedAllocator()
{
}

/*
//virtual 
void* 
BitmapPagedAllocator::allocateObject(size_t size)
{
  return allocate(size);
  //return 0;
}


//virtual 
void 
BitmapPagedAllocator::deallocateObject(void* ptr)
{
  deallocate(ptr);
}

*/
//virtual 
void 
BitmapPagedAllocator::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
}



BitmapPagedHeap::BitmapPagedHeap(int pagesize/* = 1024*/, RHeapFrame top/* = 0*/, int flags, const char* name/* = ""*/)
: HeapFrame(top, flags | HeapTraceObjects | HeapHasRC | HeapHasGC, name)
, _allocator(new BitmapPagedAllocator())
{
}
//virtual 
bool 
BitmapPagedHeap::onDestroy(Object* obj)
{
  return true;
}
 
//virtual 
bool 
BitmapPagedHeap::gc(bool recursiv/* = true*/)
{
  return false;
}

//virtual 
void 
BitmapPagedHeap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
}

} // namespace sys
} // namespace lang 
} // namespace acdk 



