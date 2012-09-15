
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_fastmutex.cpp,v 1.9 2005/02/21 09:40:36 kommer Exp $


#include <acdk.h>
#include "core_fastmutex.h"

#if defined (POSIX_THREADS)
#include "core_ptherror.h"
#endif //defined (POSIX_THREADS)
#include "core_atomicop.h"

namespace acdk {
namespace lang {
namespace sys {

//static
bool
core_atomicop::singleProcessor = false;

core_fastmutex::core_fastmutex()
{
#if defined (POSIX_THREADS)
#if defined(ACDK_DEBUG)
  _threadOwner = 0;
#endif //defined(ACDK_DEBUG)
  acdk_pthread_mutex_init(&_mutex, 0);
#elif defined (WIN32_THREADS)
  InitializeCriticalSection(&_section);
#endif
  
}

core_fastmutex::~core_fastmutex()
{
  
#if defined (POSIX_THREADS)
  acdk_pthread_mutex_destroy(&_mutex);
#elif defined (WIN32_THREADS)
  DeleteCriticalSection(&_section);
#endif
}
  
void 
core_fastmutex::lock()
{
#if defined (POSIX_THREADS)
#if defined(ACDK_DEBUG)
  if (_threadOwner != 0) {
    if (_threadOwner == pthread_self()) {
      char* ptr = 0; // envorse debugger because recursiv lock
      *ptr = 0;
    }
  }     
  acdk_pthread_mutex_lock(&_mutex);
  _threadOwner = pthread_self();
#else
    acdk_pthread_mutex_lock(&_mutex);
#endif //defined(ACDK_DEBUG)

#elif defined (WIN32_THREADS)
  EnterCriticalSection(&_section);
#endif
}
 
void 
core_fastmutex::unlock()
{
#if defined (POSIX_THREADS)
  #if defined(ACDK_DEBUG)
  if (_threadOwner != pthread_self()) {
      char* ptr = 0; // envorse debugger
      *ptr = 0;
  }
  _threadOwner = 0;
#endif //defined(ACDK_DEBUG)
  acdk_pthread_mutex_unlock(&_mutex); 
#elif defined (WIN32_THREADS)
  LeaveCriticalSection(&_section);
#endif
}

bool 
core_fastmutex::try_lock()
{
#if defined (POSIX_THREADS)
  int erg = pthread_mutex_trylock(&_mutex);
  if (erg == EBUSY)
    return false;
  if (erg != 0)
    ; //### error
  return true;
#elif defined (WIN32_THREADS)
  return TryEnterCriticalSection(&_section) == TRUE;
#endif
}



} // namespace sys 
} // namespace lang 
} // namespace acdk 



