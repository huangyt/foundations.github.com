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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_mutex.h,v 1.11 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_mutex_h
#define acdk_lang_sys_core_mutex_h

#include "../../Config.h"
#include "core_threadsys.h"
#include "core_fastmutex.h"

namespace acdk {
namespace lang {
namespace sys {


/**
  This is the internal implementation of a recursiv mutex.
  Note that is is designed to work with core_condition
  to enable Java style object synchronisation.
  Use fast_mutex/static_mutex if want to protect
  static not recursiv code.
*/


class ACDK_CORE_PUBLIC core_mutex
{
  int _lockCount;
#if defined (POSIX_THREADS)
#if defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  pthread_mutex_t _mutex;
#else //defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  core_fastmutex _mutex;
  pthread_t _threadOwner;
  core_fastmutex _internalMutex;
#endif //defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  
public:
#if !defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
  pthread_t owner() { return _threadOwner; }
  pthread_mutex_t& mutex() { return _mutex.mutex(); }
  int lockCount() { return _lockCount; }
#endif //!defined(ACDK_HAS_PTHREAD_RECURSIVE_MUTEX)
#elif defined (WIN32_THREADS)
   HANDLE _mutex;
public:
  HANDLE mutex() { return _mutex; }
  HANDLE handle() { return _mutex; }
#endif
public:
  core_mutex();
  ~core_mutex();
  
  void lock();
  void unlock();
  bool try_lock();
  int get_lock_count() const { return _lockCount; }
private:
  void _lock(int count); 
  friend class core_condition;
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_mutex_h

