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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_sys_static_mutex.h,v 1.1 2005/02/21 09:37:04 kommer Exp $

#ifndef acdk_lang_sys_core_sys_static_mutex_h
#define acdk_lang_sys_core_sys_static_mutex_h

#include "../../Config.h"
#include "core_threadsys.h"
#include "core_fastmutex.h"

namespace acdk {
namespace lang {
namespace sys {

template <class M>
class SysStaticMutexLockGuard
{
  bool _wasLocked;
  M& _mutex;
public:
  SysStaticMutexLockGuard(bool isMultiThread, M& mutex)
    : _wasLocked(false)
    , _mutex(mutex)
  {
    if (isMultiThread == true)
    {
      _wasLocked = true;
      _mutex.lock();
    }
  }
  ~SysStaticMutexLockGuard()
  {
    if (_wasLocked == true)
      _mutex.unlock();
  }
};

template <class M>
class SysStaticMutexUnLockGuard
{
  bool _wasLocked;
  M& _mutex;
public:
  SysStaticMutexUnLockGuard(bool isMultiThread, M& mutex)
    : _wasLocked(false)
    , _mutex(mutex)
  {
    if (isMultiThread == true)
    {
      _wasLocked = true;
      _mutex.unlock();
    }
  }
  ~SysStaticMutexUnLockGuard()
  {
    if (_wasLocked == true)
      _mutex.lock();
  }
};


} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_sys_static_mutex_h

