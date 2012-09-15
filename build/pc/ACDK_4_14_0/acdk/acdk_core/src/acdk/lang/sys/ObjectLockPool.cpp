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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/ObjectLockPool.cpp,v 1.11 2005/04/26 22:05:05 kommer Exp $




#include <acdk.h>

#include "sys.h"

#include "ObjectLockPool.h"

#include "core_vector.h"

#include "core_fastmutex.h"
#include "core_mutex.h"
#include "core_condition.h"
#include "../Thread.h"
#include "../System.h"

#ifdef ACDK_MT

//#define LOCAL_DEBUG

#ifdef LOCAL_DEBUG
#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr)
#endif


namespace acdk {
namespace lang {

//using namespace sys;

class ObjectLockPool
{
  sys::core_vector<ObjectLock*> _vec;
  int _firstFreePos;
  bool _destroyed;
public:
  ObjectLockPool()
  : _vec(0)
  , _firstFreePos(0)
  , _destroyed(false)
  {

  }
  ~ObjectLockPool()
  {
    for (int i = 0; i < _vec.size(); ++i)
    {
      ObjectLock* olock = _vec[i];
      if (olock != 0 && olock->_obj == 0)
        delete olock;
    }
    _destroyed = true;
  }
  ObjectLock* aquire(const Object* obj)
  {
    if (_firstFreePos >= _vec.size())
    {
      int idx = _vec.size();
      _vec.push_back(new ObjectLock(idx));
      ObjectLock* ret = _vec[idx];
      ret->_obj = obj;
      ret->ensureLock();
      _firstFreePos = _vec.size();
      return ret;
    }
    ObjectLock* ret = _vec[_firstFreePos];
    ret->_obj = obj;
    ret->ensureLock();

    int i;
    for (i = _firstFreePos + 1; i < _vec.size() && _vec[i]->_obj != 0; ++i)
    {
    }
    _firstFreePos = i;
    return ret;
  }
  void release(ObjectLock* ol)
  {
    ol->_obj = 0;
    // destructor already called for this static object
    if (_destroyed == true)
    {
      delete ol;
      return;
    }
    if (_firstFreePos > ol->_index)
      _firstFreePos = ol->_index;
  }
};


ObjectLockPool& _getObjectLockPool()
{
  static ObjectLockPool sobjectLockPool;
  return sobjectLockPool;
}


//static
void
ObjectLock::releaseLock(acdk::lang::ObjectLock* lock)
{
  //delete lock;
  lock->reset();
  _getObjectLockPool().release(lock);
}

//static
ObjectLock*
ObjectLock::aquireLock(const acdk::lang::Object* obj)
{
  //return new ObjectLock();
  return _getObjectLockPool().aquire(obj);
}

ObjectLock::ObjectLock()
: _index(-1)
, _mutex(0)
, _monitor(0)
, _obj(0)
{
}

ObjectLock::ObjectLock(int idx)
: _index(idx)
, _mutex(0)
, _monitor(0)
, _obj(0)
{
}

ObjectLock::~ObjectLock()
{
 if (_monitor != 0) {
    delete _monitor;
    _monitor = 0;
  }
 if (_mutex != 0) {
   delete _mutex;
  _mutex = 0;
 }
}

void
ObjectLock::lock()
{
  ensureLock();
  _mutex->lock();
}

void
ObjectLock::unlock()
{
  ensureLock();
  _mutex->unlock();
}

acdk::lang::sys::static_mutex _aquire_lock;

void
ObjectLock::ensureLock()
{
  if (_mutex == 0) {
    TLockGuard< acdk::lang::sys::static_mutex>  lockthis(_aquire_lock);
    if (_mutex == 0)
      _mutex = new ObjectMutex();
  }
}

void
ObjectLock::ensureCondition()
{
  ensureLock();
  if (_monitor == 0)
  {
    TLockGuard< acdk::lang::sys::static_mutex>  lockthis(_aquire_lock);
    if (_monitor == 0)
    {
      _monitor = new ObjectMonitor(*_mutex);
      DOUT("ObjectLock: allocated condition: " << (int)(void*)_monitor);
    }
    else
    {
      DOUT("ObjectLock: had condition2: " << (int)(void*)_monitor);
    }
  }
  else
  {
    DOUT("ObjectLock: had condition: " << (int)(void*)_monitor);
  }

}

void
ObjectLock::reset()
{
  TLockGuard< acdk::lang::sys::static_mutex>  lockthis(_aquire_lock);
  if (_monitor != 0)
  {
    ObjectMonitor* smon = _monitor;
    DOUT("ObjectLock: reset condition: " << (int)(void*)_monitor);
    _monitor = 0;
    delete smon;
  }
  /* crashes
  if (_mutex != 0)
    _mutex->reset();
  */
}

void
ObjectLock::wait(int timeoutms, int timeoutus)
{
  ensureCondition();
  ScopedWaitingMonitor sm(_monitor);

  if (timeoutms == 0 && timeoutus == 0)
    _monitor->wait();
  else
    _monitor->wait(timeoutms/*, timeoutus*/);
  Thread::checkForPendingException();
}

void
ObjectLock::notify()
{
  ensureCondition();
  TUnLockGuard <ObjectMutex> unlock(*_mutex);
  _monitor->notify();
}

void
ObjectLock::notifyAll()
{
  ensureCondition();
  _monitor->notifyAll();
}


} // lang
} // acdk

#endif //ACDK_MT
