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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_mutex.cpp,v 1.14 2005/03/07 14:02:13 kommer Exp $

#include <acdk.h>
#include "core_mutex.h"
#include "core_fastmutex.h"
#include "core_guard.h"
#include "core_system.h"
#include <errno.h>

#include "../SystemError.h"

#if defined(ACDK_HAS_PTHREAD_MUTEX_SETKIND)
extern "C" int pthread_mutexattr_settype(pthread_mutexattr_t *__attr,
                                         int __kind);
#endif //ACDK_HAS_PTHREAD_MUTEX_SETKIND

namespace acdk {
namespace lang {
namespace sys {

#if defined (POSIX_THREADS)
#include "core_ptherror.h"
#endif //defined (POSIX_THREADS)

core_mutex::core_mutex()
#if defined (POSIX_THREADS)
: _lockCount(0)
#if !defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
,  _threadOwner(0)
#endif //!ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
{
#elif defined (WIN32_THREADS)
{
  _mutex = ::CreateMutex(0, false, 0);
#endif
#if defined (POSIX_THREADS) && (defined(ACDK_HAS_PTHREAD_MUTEX_ATTR_SETTYPE) || defined(ACDK_HAS_PTHREAD_MUTEX_SETKIND))
  pthread_mutexattr_t pattr;
  int erg = pthread_mutexattr_init(&pattr);
  //### error handling

#if defined(ACDK_HAS_PTHREAD_MUTEX_ATTR_SETTYPE)
  //erg = pthread_mutexattr_settype(&pattr, PTHREAD_MUTEX_RECURSIVE);
#if !defined(ACDK_OS_DARWIN)
  erg = pthread_mutexattr_settype(&pattr, PTHREAD_MUTEX_ERRORCHECK);
#endif
#else
  //erg = pthread_mutexattr_setkind_np(&pattr, PTHREAD_MUTEX_ERRORCHECK_NP);
  erg = pthread_mutexattr_settype(&pattr, PTHREAD_MUTEX_RECURSIVE_NP);
  //### error handling
#endif
#if defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  erg = pthread_mutex_init(&_mutex, &pattr);
#endif
  //### error handling
#elif defined(ACDK_HAS_PTHREAD_MUTEX_SETKIND)
  pthread_mutexattr_setkind_np

#endif
}

core_mutex::~core_mutex()
{
#if defined (POSIX_THREADS) && defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  int erg = pthread_mutex_destroy(&_mutex);
  //### error handling
#elif defined (WIN32_THREADS)
  CloseHandle(_mutex);
#endif
}


void 
core_mutex::_lock(int count)
{
#if defined (POSIX_THREADS)
  if (core_system::inMain() == false)
    return;
#ifdef ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
  while (count--)
  {
    int erg = pthread_mutex_lock(&_mutex);
    //###assert
  }
#else //ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
  pthread_t tid = pthread_self();
  bool obtained = false;
  while (obtained == false) {
    core_lock_guard<core_fastmutex> lock(_internalMutex);
    if (_lockCount == 0) {
      _lockCount += count;
      _threadOwner = tid;
      obtained = true;
      _mutex.lock();
    } else if (_threadOwner == tid) {
      _lockCount += count;
      obtained = true;
    }
    if (obtained == false) {
      core_unlock_guard<core_fastmutex> unlockg(_internalMutex);
      core_lock_guard<core_fastmutex> lockg(_mutex);
    }
  }
#endif //ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
#elif defined (WIN32_THREADS)
  if (WaitForSingleObject(_mutex, INFINITE) == WAIT_FAILED)
    sys::coreout << "WaitForSingleObject, _mutex, INFINITE) == WAIT_FAILED" << sys::eofl;
#endif
}


void 
core_mutex::lock()
{
  _lock(1);
}
 
void 
core_mutex::unlock()
{
#if defined (POSIX_THREADS)
  if (core_system::inMain() == false)
    return;
#ifdef ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
    int erg = pthread_mutex_unlock(&_mutex);
  //### error handling
#else //ACDK_HAS_PTHREAD_RECURSIVE_MUTEX

  core_lock_guard<core_fastmutex> lock(_internalMutex);
  pthread_t tid = pthread_self();
  if (_lockCount <= 0 ) {
    THROW1(SystemError, RString("core_mutex::unlock(): underflow, count: ") + String::valueOf((int)_lockCount));
  }
  if (_threadOwner != tid) {
    THROW1(SystemError, "core_mutex::unlock(), not owner");
  }
  if (--_lockCount == 0) {
    _threadOwner = (pthread_t) 0;
    _mutex.unlock();
  }
#endif
#elif defined (WIN32_THREADS)
  ReleaseMutex(_mutex);
#endif
}
 
bool 
core_mutex::try_lock()
{
#if defined (POSIX_THREADS)
  if (core_system::inMain() == false)
    return true;
#ifdef ACDK_HAS_PTHREAD_RECURSIVE_MUTEX
    int erg = pthread_mutex_trylock(&_mutex);
    if (erg == EBUSY)
      return false;
    //### rorro
    return true;
#else //ACDK_HAS_PTHREAD_RECURSIVE_MUTEX

  bool obtained = false;
  pthread_t tid = pthread_self();
  core_lock_guard<core_fastmutex> lock(_internalMutex);
  if (_lockCount == 0) {
    _lockCount++;
    _threadOwner = tid;
    obtained = true;
    _mutex.lock();
  } else if (_threadOwner == tid) {
      _lockCount++;
      obtained = true;
  }
  return obtained;
#endif // ACDK_HAS_PTHREAD_RECURSIVE_MUTEX

#elif defined (WIN32_THREADS)
  DWORD result = WaitForSingleObject(_mutex, 0);
  if (result == WAIT_FAILED) {
    //THROW1(SystemError, "WaitForSingleObject");
    return false;
  }
  return result != WAIT_TIMEOUT;
#endif
}

static void foo()
{
#if _MSC_VER == 1300

   TLockGuard<core_mutex> tlock;
   core_mutex mutex;
   TLockGuard<core_mutex> tlock2(mutex);
#endif
}


} // namespace sys 
} // namespace lang 
} // namespace acdk 


