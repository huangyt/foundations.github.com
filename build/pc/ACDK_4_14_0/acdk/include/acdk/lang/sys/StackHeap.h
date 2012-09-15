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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/StackHeap.h,v 1.6 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_StackHeap_h
#define acdk_lang_sys_StackHeap_h

#include "StackAllocator.h"

namespace acdk {
namespace lang {
namespace sys {



class ACDK_CORE_PUBLIC StackHeap
:  public HeapFrame 
{
  StackAllocScope stk;
public:
  StackHeap(int pagesize = 1024 * 1024, RHeapFrame top = 0, int flags = HeapIsThread, const char* name = "");
  ~StackHeap();
  virtual bool dispose();
  virtual bool onDestroy(Object* obj); 
  virtual bool gc(bool recursiv = true);
  virtual Allocator* allocator() { return stk; }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
};


} // namespace sys
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_StackHeap_h

