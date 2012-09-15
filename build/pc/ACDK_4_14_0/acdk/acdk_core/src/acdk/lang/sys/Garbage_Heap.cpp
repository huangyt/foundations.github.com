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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/Garbage_Heap.cpp,v 1.7 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include "sys.h"
#include "Garbage_Heap.h"

namespace acdk {
namespace lang {
namespace sys {

//virtual 
Garbage_Heap::~Garbage_Heap()
{
  // nothing todo
}

/*
//virtual 
void 
Garbage_Heap::add(const Object* obj, size_t size)
{
  _allocated += size;
  ++_objectCount;
  //obj->setWeakRef();
  // remove it from heap:
  (void)ObjectHeap::onHeapAllocated(obj);
}

//virtual 
bool 
Garbage_Heap::removeObject(const Object* obj, bool recursiv)
{
  //--_objectCount;
  return true;
}

//virtual 
bool 
Garbage_Heap::hasObject(const Object* obj, bool recursiv) const
{
  return false;
}

//virtual 
void 
Garbage_Heap::printObjects(sys::core_output& os, bool recursiv) const
{
  
}

//virtual 
int 
Garbage_Heap::objectCount(bool recursiv) const
{
  return _objectCount;
}

//virtual 
size_t 
Garbage_Heap::totalAllocated(bool recursiv) const
{
  return _allocated;
}

//virtual 
bool 
Garbage_Heap::checkCyclicReferences(bool recursiv) const
{
  return false;
}

*/
//virtual 
bool 
Garbage_Heap::gc(bool recursiv)
{
  return false;
}

/*
//virtual 
bool 
Garbage_Heap::gc(const Object* obj, bool recursiv)
{
  return false;
}
*/

//virtual 
void 
Garbage_Heap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  return;
  
}


} // sys
} // lang
} // acdk


