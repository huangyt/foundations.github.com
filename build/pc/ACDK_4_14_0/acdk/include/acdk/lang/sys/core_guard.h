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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_guard.h,v 1.8 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_guard_h
#define acdk_lang_sys_core_guard_h

#include "../../Config.h"
#include "core_system.h"

namespace acdk {
namespace lang {
namespace sys {

/**
   provides a lock guard used to control
   lock()/unlock on the stack in exception
   save way
*/
template <class LockClass>
class core_lock_guard
{
   LockClass& _lock;
public:
   core_lock_guard(LockClass& locker)
   : _lock(locker)
   {
      _lock.lock();
   }
   ~core_lock_guard()
   {
      _lock.unlock();
   }
};

/**
   provides a lock guard used to control
   unlock()/lock on the stack in exception
   save way
*/
template <class LockClass>
class core_unlock_guard
{
   LockClass& _lock;
public:
   core_unlock_guard(LockClass& locker)
   : _lock(locker)
   {
      _lock.unlock();
   }
   ~core_unlock_guard()
   {
      _lock.lock();
   }
};

/**
  Use this to lock a static defined mutex 
  This implementation looks, if the lock 
  would be tried outside of main() 
  (than it would fail on some plattforms)
*/
template <class LockClass>
class core_static_lock_guard 
{
  LockClass& _lock;
  bool _locked;
public:
   core_static_lock_guard(LockClass& locker)
   : _lock(locker)
     , _locked(false)
   {
     if (core_system::inMain() == true) {
       _lock.lock();
       _locked = true;
     }
   }
   ~core_static_lock_guard()
   {
      if (_locked == true)
        _lock.unlock();
   }
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_guard_h

