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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_ptherror.h,v 1.7 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_lang_sys_core_ptherror_h
#define acdk_lang_sys_core_ptherror_h

#ifndef DOXYGENONLY

#ifdef POSIX_THREADS

#include <sys/errno.h>

class SyncException
{
 public:
  int _err;
  const char* _msg;
  SyncException(const char* msg, int err) 
  : _err(err),
    _msg(msg)
    
    {
    }

};

inline void pmsg(const char* msg, pthread_mutex_t *mutex)
{
  // sys::coreout << msg << ": mutex=" << (void*)mutex << "; tid: " << (int) pthread_self() << sys::eofl;
}

inline void pmsg(const char* msg, pthread_cond_t* cond)
{
  //sys::coreout << msg << ": cond=" << (void*)cond << "; tid: " << (int) pthread_self() << sys::eofl;
}
inline void pmsg(const char* msg, pthread_cond_t* cond, pthread_mutex_t *mutex)
{
  //sys::coreout << msg << ": cond=" << (void*)cond << ": mutex=" << (void*)mutex << "; tid: " << (int) pthread_self() << sys::eofl;
}


inline void acdk_pthread_mutex_lock(pthread_mutex_t *mutex)
{
  pmsg("lock", mutex);
  int erg = pthread_mutex_lock(mutex);
  if (erg != 0)
    throw SyncException("pthread_mutex_lock() failed", erg);
  pmsg("locked", mutex);
}

inline int acdk_pthread_mutex_trylock(pthread_mutex_t *mutex)
{
  pmsg("trylock", mutex);
  int erg = pthread_mutex_trylock(mutex);
  if (erg != 0) {
    if (erg == EBUSY)
      return erg;
    throw SyncException("pthread_mutex_trylock() failed", erg);
  }
  pmsg("trylocked", mutex);
  return erg;
}

inline void acdk_pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  pmsg("unlock", mutex);
  int erg = pthread_mutex_unlock(mutex);
  if (erg != 0)
    throw SyncException("pthread_mutex_unlock() failed", erg);
  pmsg("unlocked", mutex);
}

inline void acdk_pthread_mutex_destroy(pthread_mutex_t *mutex)
{
  pmsg("destroy", mutex);
  int erg = pthread_mutex_destroy(mutex);
  if (erg != 0)
    throw SyncException("pthread_mutex_destroy() failed", erg);
}

inline void acdk_pthread_mutex_init(pthread_mutex_t* mutex, const pthread_mutexattr_t* mutexattr)
{
  pmsg("init", mutex);
  int erg = pthread_mutex_init(mutex, mutexattr);
  if (erg != 0)
    throw SyncException("pthread_mutex_init() failed", erg);
}

inline void acdk_pthread_cond_init(pthread_cond_t* cond, pthread_condattr_t* cond_attr)
{
  pmsg("init", cond);
  int erg = pthread_cond_init(cond, cond_attr);
  if (erg != 0)
    throw SyncException("pthread_cond_init() failed", erg);
}

inline void acdk_pthread_cond_signal(pthread_cond_t *cond)
{
  pmsg("signal", cond);
  int erg = pthread_cond_signal(cond);
  if (erg != 0)
    throw SyncException("pthread_cond_signal() failed", erg);
}

inline void acdk_pthread_cond_broadcast(pthread_cond_t *cond)
{
  pmsg("broadcast", cond);
  int erg = pthread_cond_broadcast(cond);
  if (erg != 0)
    throw SyncException("pthread_cond_broadcast() failed", erg);
}

inline int acdk_pthread_cond_wait(pthread_cond_t* cond, pthread_mutex_t *mutex)
{
  pmsg("condwait", cond, mutex);
  int erg = pthread_cond_wait(cond, mutex);
  if (erg != 0)
    throw SyncException("pthread_cond_wait() failed", erg);
  return erg;
}

inline int acdk_pthread_cond_timedwait(pthread_cond_t* cond, pthread_mutex_t* mutex, const struct timespec* abstime)
{
  pmsg("timedwait", cond, mutex);
  int erg = pthread_cond_timedwait(cond, mutex, abstime);
  return erg;
}

inline void acdk_pthread_cond_destroy(pthread_cond_t *cond)
{
  pmsg("destroy", cond);
  int erg = pthread_cond_destroy(cond);
  if (erg != 0)
    throw SyncException("pthread_cond_destroy() failed", erg);
}

#endif // POSIX_THREADS

#endif // DOXYGENONLY
#endif //acdk_lang_sys_core_ptherror_h

