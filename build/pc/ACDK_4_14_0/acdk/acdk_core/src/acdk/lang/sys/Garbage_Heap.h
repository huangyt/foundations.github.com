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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/Garbage_Heap.h,v 1.9 2005/03/07 13:47:28 kommer Exp $

#ifndef acdk_lang_sys_Garbage_Heap_h
#define acdk_lang_sys_Garbage_Heap_h

#include "HeapFrame.h"

namespace acdk {
namespace lang {
namespace sys {

using namespace acdk::lang;

/** 
This heap does only Reference Counting without Garbage Collecting.

*/
class ACDK_CORE_PUBLIC Garbage_Heap 
: public HeapFrame 
{
protected:
  size_t _allocated;
  int _objectCount;
public:
  Garbage_Heap(RHeapFrame top = Nil, int flags = HeapIsThread, const char* name = "") 
  : HeapFrame(top, flags, name),
    _allocated(0),
    _objectCount(0)
  {
  }
  virtual ~Garbage_Heap();
  //virtual void add(const Object* obj, size_t size);
  //virtual bool removeObject(const Object* obj, bool recursiv = true);
  //virtual bool hasObject(const Object* obj, bool recursiv = true) const;
  //virtual void printObjects(sys::core_output& os, bool recursiv = true) const;
  //virtual int objectCount(bool recursiv = true) const;
  //virtual size_t totalAllocated(bool recursiv = true) const;
  //virtual bool checkCyclicReferences(bool recursiv = true) const;
  virtual bool gc(bool recursiv = true);
  virtual Allocator* allocator() { return stdalloc(); }
  //virtual bool gc(const Object* obj, bool recursiv = true);
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
};

} // Sys
} // Lang
} // acdk

#endif //acdk_lang_sys_Garbage_Heap_h

