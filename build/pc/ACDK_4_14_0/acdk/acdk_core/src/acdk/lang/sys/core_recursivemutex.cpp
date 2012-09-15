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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_recursivemutex.cpp,v 1.10 2005/03/07 14:02:13 kommer Exp $


#if 0 // not yet working

#include <acdk.h>
 
#include "core_recursivemutex.h"
#include <errno.h>
#include "../SystemError.h"

namespace acdk {
namespace lang {
namespace sys {

#if defined (POSIX_THREADS)
#include "core_ptherror.h"
#endif //defined (POSIX_THREADS)

core_recursivemutex::core_recursivemutex()
#if defined (POSIX_THREADS)
  : _isValid(false)
  , _owner()
  , _mutex()
  , _lockCount(0)
  //, _unlockedCondition(_mutex)
{

#elif defined (WIN32_THREADS)
{
  _mutex = ::CreateMutex(0, false, 0);
#endif
}

core_recursivemutex::~core_recursivemutex()
{
#if defined (POSIX_THREADS)
  // nothing 
#elif defined (WIN32_THREADS)
  CloseHandle(_mutex);
#endif
}


void 
core_recursivemutex::_lock(int count)
{
#if defined (POSIX_THREADS)
  //core_lock_guard<fast_mutex> lock(_mutex);
  _mutex.lock();
  core_thread_id tid = core_thread_id::get_current();

  if (_isValid == true && tid == _owner) {
    ++_lockCount;
    return;
  }
  
  // other
  while (_isValid == true) {
    _unlockedCondition.wait();
  }
  _owner = tid;
  _isValid = true;
  _lockCount = 1;
  _mutex.unlock();

#elif defined (WIN32_THREADS)
  if (WaitForSingleObject(_mutex, INFINITE) == WAIT_FAILED)
    sys::coreout << "WaitForSingleObject, _mutex, INFINITE) == WAIT_FAILED" << sys::eofl;
#endif
}


void 
core_recursivemutex::lock()
{
  _lock(1);
}
 
void 
core_recursivemutex::unlock()
{
#if defined (POSIX_THREADS)
  core_lock_guard<core_fastmutex> lock(_mutex);
  core_thread_id tid = core_thread_id::get_current();
  if (_lockCount <= 0 ) {
    THROW1(SystemError, RString("core_recursivemutex::unlock(): underflow, count: ") + String::valueOf((int)_lockCount));
  }
  if (_owner != tid) {
    THROW1(SystemError, "core_recursivemutex::unlock(), not owner");
  }
  if (--_lockCount == 0) {
    _owner = (pthread_t) 0;
    _mutex.unlock();
  }
#elif defined (WIN32_THREADS)
  ReleaseMutex(_mutex);
#endif
}
 
bool 
core_recursivemutex::try_lock()
{
#if defined (POSIX_THREADS)
  bool obtained = false;
  core_thread_id tid = core_thread_id::get_current();
  core_lock_guard<core_fastmutex> lock(_mutex);
  if (_lockCount == 0) {
    _lockCount++;
    _owner = tid;
    obtained = true;
    _mutex.lock();
  } else if (_owner == tid) {
      _lockCount++;
      obtained = true;
  }
  return obtained;
#elif defined (WIN32_THREADS)
  DWORD result = WaitForSingleObject(_mutex, 0);
  if (result == WAIT_FAILED) {
    //THROW1(SystemError, "WaitForSingleObject");
    return false;
  }
  return result != WAIT_TIMEOUT;
#endif
}



} // namespace sys 
} // namespace lang 
} // namespace acdk 



#endif //0
