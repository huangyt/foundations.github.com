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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/PagedHeap.h,v 1.11 2005/03/07 13:47:28 kommer Exp $

#ifndef acdk_lang_sys_PagedHeap_h
#define acdk_lang_sys_PagedHeap_h

#include "PagedAllocator.h"
#include "HeapFrame.h"

namespace acdk {
namespace lang {
namespace sys {



class ACDK_CORE_PUBLIC PagedHeap
:  public HeapFrame 
{
protected:
  RPagedAllocator _allocator;  
public:
  
  PagedHeap(int pagesize = 4000, RHeapFrame top = 0, int flags = HeapIsThread, const char* name = "");
  /*
  virtual void add(const Object* obj, size_t size);
  
  virtual bool removeObject(const Object* obj, bool recursiv = true);
  virtual bool hasObject(const Object* obj, bool recursiv = true) const;
  virtual void printObjects(sys::core_output& os, bool recursiv = true) const;
  virtual int objectCount(bool recursiv = true) const;
  virtual size_t totalAllocated(bool recursiv = true) const;
  virtual bool checkCyclicReferences(bool recursiv = true) const;
  virtual bool gc(bool recursiv = true);
  virtual bool gc(const Object* obj, bool recursiv = true);
  
  */
  
  virtual bool onDestroy(Object* obj); 
  virtual bool gc(bool recursiv = true);
  virtual Allocator* allocator() { return _allocator.getImpl(); }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
};


} // namespace sys
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_PagedHeap_h

