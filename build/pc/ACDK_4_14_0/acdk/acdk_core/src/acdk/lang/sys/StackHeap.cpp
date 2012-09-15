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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/StackHeap.cpp,v 1.7 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Error.h>
#include  "StackHeap.h"
#include <acdk/lang/ref/NotifyObjectEvent.h>
#include "../Error.h"
#include <acdk/lang/sys/core_hashmap.h>
#include "BitmapPagedAllocator.h"
#include <map>



//#define DEBUG_PA

#ifdef DEBUG_PA
#define DOUT(msgstr) sys::coreout << msgstr
#else //DEBUG_PA
#define DOUT(msgstr) do { } while(false)
#endif //DEBUG_PA



namespace acdk {
namespace lang {
namespace sys {


StackHeap::StackHeap(int pagesize, RHeapFrame top/* = 0*/, int flags, const char* name/* = ""*/) 
: HeapFrame(top, flags |  HeapHasRC | HeapHasGC | HeapTraceObjects, name)
{
}

StackHeap::~StackHeap()
{
}

bool 
StackHeap::dispose() 
{ 
  //delete this;
  return true; 
}

//virtual 
bool 
StackHeap::gc(bool recursiv/* = true */)
{
  
  return true;
}


//virtual 
bool 
StackHeap::onDestroy(Object* obj)
{
  return true;
}




//virtual 
void 
StackHeap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  
}



} // namespace sys
} // namespace lang 
} // namespace acdk 


