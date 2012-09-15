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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/StackAllocator.h,v 1.9 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_lang_sys_StackAllocator_h
#define acdk_lang_sys_StackAllocator_h

#if !defined(DOXYGENONLY)

#include <acdk.h>
#include "Allocator.h"


namespace acdk {
namespace lang {
namespace sys {



class StackAllocatorImpl;

::acdk::lang::sys::Allocator* 
#if !defined(_MSC_VER)
ACDK_CORE_PUBLIC 
#endif
getStackAllocator();

} // namespace sys
} // namespace lang
} // namespace acdk


foreign
enum StandardAllocPageSizeEnum
{
  StandardAllocPageSize = 1024 * 1024 // 1 Megabyte
};

foreign 
class ACDK_CORE_PUBLIC StackAllocScope
{
  
  ::acdk::lang::sys::StackAllocatorImpl* _allocator;
  void* _savedTop;
public:
  StackAllocScope(int reseverve = StandardAllocPageSize);
  ~StackAllocScope();
  operator ::acdk::lang::sys::Allocator*();
};
#endif //!defined(DOXYGENONLY)

#endif //acdk_lang_sys_StackAllocator_h

