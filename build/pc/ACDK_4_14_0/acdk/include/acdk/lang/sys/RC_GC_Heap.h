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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RC_GC_Heap.h,v 1.10 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_RC_GC_Heap_h
#define acdk_lang_sys_RC_GC_Heap_h


#ifdef _MSC_VER
#  pragma warning(disable: 4512 4786)
#endif

#include "HeapFrame.h"
#include "TracedRawAllocator.h"

#include <map>
#include <set>

namespace acdk {
namespace lang {
namespace sys {


class TracedRawAllocatorIterator;
foreign
class ACDK_CORE_PUBLIC RC_GC_Heap
:  public HeapFrame 
{
protected:
  typedef size_t ObjectInfo;
  //typedef std::map<const Object*, ObjectInfo>  ObjectStack;
  //ObjectStack _stack;  
  typedef TracedRawAllocator::ObjectsSet ObjectStack;
  RTracedRawAllocator _allocator;
public:
  

  RC_GC_Heap(RHeapFrame top = 0,int flags = HeapIsThread, const char* name = "") 
  : HeapFrame(top, flags | HeapHasRC | HeapTraceObjects | HeapHasGC, name),
    _allocator(new TracedRawAllocator())
  {
  }

 
  virtual Allocator* allocator() { return _allocator.impl(); }

  virtual bool gc(bool recursiv = true);
  
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);

  //virtual bool gc(const Object* obj, bool recursiv = true);
  /** decrement all references and remove them from collection, if refernce is marked as static */
  //void removeAllStaticReferences();
  
protected:
  ObjectStack& stack() { return _allocator->_heap; }
  bool _hasObject(const Object* obj) const;
  //bool _setInFieldToNil(const Object* obj, bool recursiv);
  friend class TracedRawAllocatorIterator;
};

} // sys
} // lang
} // acdk

#endif //acdk_lang_sys_RC_GC_Heap_h

