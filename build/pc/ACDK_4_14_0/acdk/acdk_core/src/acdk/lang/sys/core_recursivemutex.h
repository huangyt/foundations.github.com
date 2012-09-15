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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_recursivemutex.h,v 1.8 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_recursivemutex_h
#define acdk_lang_sys_core_recursivemutex_h


#include "../../Config.h"
#include "core_threadsys.h"
#include "core_fastmutex.h"
#include "core_condition.h"
#include "core_guard.h"
#include "core_thread_id.h"

namespace acdk {
namespace lang {
namespace sys {


/**

*/


class ACDK_CORE_PUBLIC core_recursivemutex
{
#if defined (POSIX_THREADS)
  bool _isValid;
  core_thread_id _owner;
  core_fastmutex _mutex;
  int _lockCount; 
  core_condition _unlockedCondition;

#elif defined (WIN32_THREADS)
   HANDLE _mutex;
public:
  HANDLE mutex() const { return _mutex; }
  HANDLE handle() { return _mutex; }
#endif
public:

  typedef core_lock_guard<core_recursivemutex> lockguard;

  core_recursivemutex();
  ~core_recursivemutex();
  
  void lock();
  void unlock();
  bool try_lock();
private:
  void _lock(int count); 
  friend class core_condition;
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_recursivemutex_h

