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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_semaphore.cpp,v 1.16 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include "core_semaphore.h"
#include "core_tick.h"
#include "../SystemError.h"
#include "../Thread.h"
#include <errno.h>
#if defined(ACDK_OS_DARWIN)
//# include <mach/shed.h>
# include <mach/mach_init.h>
#endif
namespace acdk {
namespace lang {
namespace sys {


//#define DOUT(msg) sys::coreout << "TID: " << ThreadID::getCurrentThreadID().getId()  << ": " << msg << sys::eofl
#define DOUT(msg)



//  explicit
core_semaphore::core_semaphore(int initcount/* = 0*/, int maxcount/* = -1*/)
{
  DOUT("core_semaphore::core_semaphore(" << (void*)this << ", int initcount = " << initcount << ", int maxcount = " << maxcount << ")");
#if defined(WIN32_THREADS)
  if (maxcount == -1)
    maxcount = 1000000;
  _sema = CreateSemaphore(0, initcount, maxcount, 0);
  if (_sema == NULL)
    THROW1(SystemError, "CreateSemaphore failed");
#endif
#if defined (POSIX_THREADS)
# if defined(ACDK_OS_DARWIN)
  semaphore_create(current_task(), &_sema, SYNC_POLICY_FIFO, initcount);
#else
   ACDK_SYS_CALL3(sem_init, &_sema, 0, initcount, == 0);
#endif
#endif  
}

core_semaphore::~core_semaphore()
{
  DOUT("core_semaphore::~core_semaphore(" << (void*)this << ")");
#if defined(WIN32_THREADS)
  CloseHandle(_sema);
#endif
#if defined (POSIX_THREADS)
# if defined (ACDK_OS_DARWIN)
  semaphore_destroy(current_task(), _sema);
#else
  ACDK_SYS_CALL1(sem_destroy, &_sema, == 0);
#endif
#endif  
}

bool
core_semaphore::up(int count/* = 1*/, int* prevcount/* = 0*/)
{
  DOUT("core_semaphore::up(" << (void*)this << "count=" << count << ")");
#if defined(WIN32_THREADS)
  long p;
  bool ret = ReleaseSemaphore(_sema, count, &p) == TRUE;
  if (ret == false || GetLastError() == ERROR_TOO_MANY_POSTS)
  {
    return false;
  }
  if (prevcount != 0)
    *prevcount = p;
  return ret;
#endif
#if defined (POSIX_THREADS)
#if defined(ACDK_OS_DARWIN)

#else
  int prevval;
  ACDK_SYS_CALL2(sem_getvalue, &_sema, &prevval, == 0);
  for (int i = 0; i < count; ++i)
  {
    ACDK_SYS_CALL1(sem_post, &_sema, == 0);
    //int ret = sem_post(&_sema);
    //if (ret != 0)
    //  sys::coreout << "sem_post() return " << ret << sys::eofl;
  }
  if (prevcount != 0)
    *prevcount = prevval;
  return true;
#endif
#endif  
}


bool 
core_semaphore::down(int millisecs/* = -1*/)
{
  DOUT("core_semaphore::up(" << (void*)this << "ms=" << millisecs << ")");
#if defined(WIN32_THREADS)
  if (millisecs == -1)
    millisecs = INFINITE;
  DWORD res = WaitForSingleObject(_sema, millisecs);
  if (res == WAIT_FAILED) 
  { 
    THROW1(SystemError, "WaitForSingleObject failed");
  }
  return res == WAIT_OBJECT_0;
#endif
#if defined (POSIX_THREADS)
#if defined(ACDK_OS_DARWIN)

#else
#ifndef EAGAIN
# define EAGAIN 11
#endif
  if (millisecs == -1) {
    ACDK_SYS_CALL1(sem_wait, &_sema, == 0);
    return true;
  } else {
    tick_t start = core_tick::now();
    while (true) 
    {
      int ret = sem_trywait(&_sema);
      if (ret == 0)
	      return true;
      if (ret != EAGAIN)
	      THROW1(SystemError, "sem_trywait failed");
      ::acdk::lang::Thread::sleep(millisecs / 10);
      tick_t now = core_tick::now();
      if (core_tick::TicksPerMillisecond == 0) {
	if ((now - start) / core_tick::TicksPerSecond >= millisecs / 1000)
	  return false;
      } else {
	if ((now - start) / core_tick::TicksPerMillisecond >= millisecs)
	  return false;
      }
    }
  }
#endif
#endif  
}

} // namespace sys 
} //namespace lang 
} // namespace acdk 



