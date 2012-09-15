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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_condition.cpp,v 1.23 2005/04/19 08:35:22 kommer Exp $


#include <acdk.h>

#include "core_condition.h"

#include "../SystemError.h"
#include "core_tick.h"

#if defined(POSIX_THREADS)
#include <unistd.h>
#include <sys/time.h>
#include <errno.h>
#endif

#include "../InterruptedException.h"

namespace acdk {
namespace lang {
namespace sys {

#if defined (POSIX_THREADS)
#include "core_ptherror.h"
#endif //defined (POSIX_THREADS)

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
#define DOUT(msg) do { sys::coreout << msg << sys::eofl; } while(false)
#else
#define DOUT(msg) do { } while(false)
#endif

core_condition::core_condition(core_mutex& mutex)
: _mutex(mutex)
#if defined (POSIX_THREADS)
{
  pthread_cond_init(&_condition, 0);
#elif defined (WIN32_THREADS)
  ,_waitersCount(0),
  _waiters_cs(),
  _waitersSemaphore(0),
  _waitersEvent(0),
  _broadcasted(0)
{
  _waitersSemaphore = CreateSemaphore(0, 0, 0x7fffffff, 0);// == INVALID_HANDLE_VALUE
  _waitersEvent = CreateEvent(0, false, false, 0); // == 0
  InitializeCriticalSection(&_waiters_cs);
#endif

}

core_condition::~core_condition()
{
#if defined (POSIX_THREADS)
  acdk_pthread_cond_destroy(&_condition);
#elif defined (WIN32_THREADS)
  DeleteCriticalSection(&_waiters_cs);
  CloseHandle(_waitersSemaphore);
  CloseHandle(_waitersEvent);
#endif
}
  
void 
core_condition::notify()
{
#if defined (POSIX_THREADS)
  acdk_pthread_cond_signal(&_condition);
#elif defined (WIN32_THREADS)
  if (_waitersCount > 0 ) 
  {
    ReleaseSemaphore(_waitersSemaphore, 1, 0); // == TRUE
  }
  else
  {
    DOUT("notify without waiters");
  }
#endif
}

void 
core_condition::notifyAll()
{
#if defined (POSIX_THREADS)
  acdk_pthread_cond_broadcast(&_condition);
#elif defined (WIN32_THREADS)
  if (_waitersCount > 0) 
  {
    _broadcasted = true;
    ReleaseSemaphore(_waitersSemaphore, _waitersCount, 0); //   == TRUE
    DWORD result = WaitForSingleObject(_waitersEvent, INFINITE); //!= 0xFFFFFFFF
    _broadcasted = false;
    if (result != WAIT_OBJECT_0) 
      THROW1(SystemError, "Notifyall failed");
  }
  else
  {
    //DOUT("notifyAll without waiters");
  }
#endif
}

void 
core_condition::wait()
{
#if defined (POSIX_THREADS)
  wait(-1);
#elif defined (WIN32_THREADS)
  wait(-1);
#endif
}

#if defined(ACDK_OS_UNIX)
void
fillAbsTime(struct timespec& abstime, int millis, int nanos = 0)
{
  if (millis < 0)
  {
    abstime.tv_sec = 0;
    abstime.tv_nsec = 0;
    return;
  }
  timeval tv;
  gettimeofday(&tv, 0);
  //123456789 - 10^9
  const long one_billion = 1000000000;
  abstime.tv_sec = tv.tv_sec + (millis / 1000);
  abstime.tv_nsec = (tv.tv_usec * 1000) + ((millis % 1000) * 1000000) + nanos;
  if (abstime.tv_nsec > one_billion) {
    ++abstime.tv_sec;
    abstime.tv_nsec -= one_billion;
  }
  
}
#endif

bool 
core_condition::wait(int milliseconds) // ### todo support also mnsecs
{
#if defined (POSIX_THREADS)
#if defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)

  if (milliseconds == -1) {
    int erg = acdk_pthread_cond_wait(&_condition, &_mutex._mutex); 
    if (erg == EINTR) {
      THROW0(InterruptedException);
    } 
    return true;
  }
  struct timespec abstime;
  fillAbsTime(abstime, milliseconds);
  int erg = acdk_pthread_cond_timedwait(&_condition, &_mutex._mutex, &abstime);
  if (erg == EAGAIN || erg == ETIMEDOUT) {
    return false;
  }
  return true;
#else //defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  int count = 0;
  bool reterg = true;
  int perg = 0;
  {
    core_lock_guard<core_fastmutex> lock(_mutex._internalMutex);

    count = _mutex._lockCount;
    _mutex._lockCount = 0;
    _mutex._threadOwner = pthread_self();

  }
  struct timespec abstime;
  fillAbsTime(milliseconds);
  while (true) {
    try {
      if (milliseconds < 0) {
        int erg = acdk_pthread_cond_wait(&_condition, &_mutex._mutex.mutex()); 
        if (erg == EINTR) {
          {
            core_unlock_guard<core_fastmutex> im(_mutex._internalMutex);
          }
          THROW0(InterruptedException);
        } 
      } else {
        int erg = acdk_pthread_cond_timedwait(&_condition, &_mutex._mutex.mutex(), &abstime);
        if (erg == EAGAIN || erg == ETIMEDOUT) {
          reterg = false;
          break;
        } 
        
      }
    } catch (.../*interupted */) {
      acdk_pthread_mutex_unlock(&_mutex.mutex());
      _mutex._lock(count);
      throw;
    }
    break;
  }
  {
    acdk_pthread_mutex_unlock(&_mutex.mutex());  
    _mutex._lock(count);
  }
  return reterg;
#endif //defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
#elif defined (WIN32_THREADS)
  _waitersCount++;  
  if (milliseconds == -1)
    milliseconds = INFINITE;
  _mutex.unlock();
  DWORD result = WaitForSingleObject( _waitersSemaphore, milliseconds );
  if (result == WAIT_TIMEOUT)
  {
    _mutex.lock();
    _waitersCount--;
    return false;
  } 
  else if (result != WAIT_OBJECT_0)
  {
    _mutex.lock();
    _waitersCount--;
    THROW1(SystemError, "Error in wait()");
  }
    
  EnterCriticalSection(&_waiters_cs);
  _waitersCount--;
  bool last_waiter = false;
  if (_broadcasted == true && _waitersCount == 0)
    last_waiter = true;
  LeaveCriticalSection(&_waiters_cs);

  if (last_waiter == true) 
  {
    result = SetEvent(_waitersEvent);
    if (result == 0) 
    {
      _mutex.lock();
      THROW1(SystemError, "SetEvent() failed"); 
    }
  }
  result = WaitForSingleObject(_mutex.handle(), INFINITE);
  if (result == WAIT_FAILED) {
    THROW1(SystemError, "Inconstent mutex state");
    return false;
  } else if (result == WAIT_ABANDONED ) {
    return false;
  } else if (result != WAIT_OBJECT_0) {
    THROW1(SystemError, "Inconstent mutex state");
    return false;
  } 
  return true;
#else
#error unsupported threads
#endif
}

} // namespace sys 
} //namespace lang 
} // namespace acdk 


