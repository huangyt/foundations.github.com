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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_fastmutex.h,v 1.11 2005/02/07 11:22:35 kommer Exp $

#ifndef acdk_lang_sys_core_fastmutex_h
#define acdk_lang_sys_core_fastmutex_h

#include "../../Config.h"
#include "../../AcdkCoreConfig.h"
#include "core_threadsys.h"

namespace acdk {
namespace lang {
namespace sys {

class ACDK_CORE_PUBLIC core_abstractmutex
{
public:
  core_abstractmutex() { }
  virtual ~core_abstractmutex() { }
  virtual void lock() = 0;
  virtual void unlock() = 0;

};

/**
  This mutex is a simple fast mutex to protect static code
*/
class ACDK_CORE_PUBLIC core_fastmutex
{
#if defined (POSIX_THREADS)
public:
  pthread_mutex_t _mutex;
#ifdef ACDK_DEBUG
  pthread_t _threadOwner;
  
#endif //ACDK_DEBUG
public:
  pthread_mutex_t& mutex() { return _mutex; }
#elif defined (WIN32_THREADS)
   CRITICAL_SECTION _section;
#endif
public:
  core_fastmutex();
  ~core_fastmutex();
  void lock();
  void unlock();
  bool try_lock();
  
};

typedef core_fastmutex static_mutex;

} // namespace sys 
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_fastmutex_h


