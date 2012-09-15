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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_semaphore.h,v 1.8 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_semaphore_h
#define acdk_lang_sys_core_semaphore_h


#include "core_threadsys.h"
#include "core_fastmutex.h"
#include "core_condition.h"
#if defined(ACDK_OS_DARWIN)
#include <mach/task.h>
#include <mach/semaphore.h>
#endif


namespace acdk {
namespace lang {
namespace sys {

class ACDK_CORE_PUBLIC core_semaphore
{
#if defined (POSIX_THREADS)
# if defined(ACDK_OS_DARWIN)
  semaphore_t _sema;
#else
  sem_t  _sema;
#endif
#elif defined (WIN32_THREADS)
  HANDLE _sema;
#endif
public:
  explicit core_semaphore(int initcount = 0, int maxcount = -1);
  ~core_semaphore();
  /**
     increment the semaphore counter
  */
  bool up(int count = 1, int* prevcount = 0);
  /**
     decrement the semaphore counter
     blocks until semaphore counter is 0
  */
  bool down(int milliseconds = -1);
};



} // namespace sys 
} //namespace lang 
} // namespace acdk 


#endif //acdk_lang_sys_core_semaphore_h

