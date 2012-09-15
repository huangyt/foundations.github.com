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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/ObjectLockPool.h,v 1.12 2005/04/14 10:41:55 kommer Exp $

#ifndef acdk_lang_sys_ObjectLockPool_h
#define acdk_lang_sys_ObjectLockPool_h

#ifdef ACDK_MT
#include <acdk/lang/ThreadImpl.h>
#include "core_fastmutex.h"
#include "core_mutex.h"
//#include "core_recursivemutex.h"
#include "core_condition.h"



namespace acdk {
namespace lang {
  // VC is confused about this:
// namespace sys {


template <class T>
class TLockGuard 
{
protected:
  mutable T* _lock;
public:
  TLockGuard<T>() : _lock(0) { }
  TLockGuard(const T& lock) : _lock(const_cast<T*>(&lock)) { _lock->lock(); }
  
  void setLock(const T& lock) { _lock = const_cast<T*>(&lock); _lock->lock(); }
  ~TLockGuard() { _lock->unlock(); }
};

template <class T>
class TUnLockGuard 
{
  mutable T* _lock;
public:
  TUnLockGuard<T>() : _lock(0) { }
  TUnLockGuard(const T& lock) : _lock(const_cast<T*>(&lock)) { _lock->unlock(); }
  void setLock(const T& lock) { _lock = const_cast<T*>(&lock); _lock->unlock(); }
  ~TUnLockGuard() { _lock->lock(); }
};


/** use this for debugging only */
template <class T> 
class TracedTLockGuard
{
protected:
  mutable T* _lock;
  const char* _file;
  int _line;
 public:
  TracedTLockGuard(T& lock, const char* file, int line)
    : _lock(&lock),
      _file(file),
      _line(line)
  {
    sys::coreout << "lock[" << (void*)_lock << "] at: " << _file << ":" << _line << sys::eofl;
    _lock->lock();
  }
  ~TracedTLockGuard()
  {
    
    sys::coreout << "unlock[" << (void*)_lock << "] at: " << _file << ":" << _line << sys::eofl;
    _lock->unlock();
  }
};




typedef sys::core_mutex ObjectMutex;

/**
  interface for waiting an interuptable monitors
*/
foreign
class ACDK_CORE_PUBLIC MonitorInterface
{
public:
  virtual ~MonitorInterface() {}
  virtual void signal() = 0;
  virtual void signalAll() = 0;
};




class ACDK_CORE_PUBLIC ObjectMonitor
: implements MonitorInterface
{
  sys::core_condition _core_condition;
public:
  ObjectMonitor(sys::core_mutex& mutex)
    : _core_condition(mutex)
  {
  }
  virtual ~ObjectMonitor() {}
  virtual void signal() { _core_condition.notify(); }
  virtual void signalAll() { _core_condition.notifyAll(); }
  void notify() { _core_condition.notify(); }
  void notifyAll() { _core_condition.notifyAll(); }
  void wait() { _core_condition.wait(); }
  bool wait(int milliseconds) { return _core_condition.wait(milliseconds); }
 
};

class ACDK_CORE_PUBLIC ObjectLock 
{
  
  /**
    index of instance in in the ObjectLockPool.
    Just to improve perfomance in, when ObjectLock will be released
  */
  int _index;
  /**
    This is the reentrant mutex, which will be used
    to implement the synchronized (synchronized, lock(), unlock()) methods.
    The ObjectLockPool will allocated it, if an
    Object acquire an lock. If the object will be destroyed
    and the ObjectLock will be released into the ObjectLockPool
    it will not be deleted, but reused if the next Object acquire
    an ObjectLock.
  */
  ObjectMutex* _mutex;
  /**
    The condition is unlying implementation of
    the event signaling mechanism, (wait(..), notify[All]()).
    If the Object releases the ObjectLock into the ObjectLockPool
    the _condition will be deleted.
  */
  ObjectMonitor* _monitor;
  /**
    An Pointer to the Object, which actually owns the ObjectLock
  */
  const acdk::lang::Object* _obj;
public:
  ObjectLock();
  ObjectLock(int idx);
  ~ObjectLock();
  void lock();
  void unlock();
  void ensureLock();
  void ensureCondition();
  void wait(int timeoutms, int timeoutus);
  void notify();
  void notifyAll();
  int getLockCount() const { return _mutex == 0 ? 0 : _mutex->get_lock_count(); }

  /** because ObjectLock's are mananged in cache, 
      with this method a given ObjectLock can be reinitialized.
  */
  void reset();
  friend class ObjectLockPool;
  static void releaseLock(acdk::lang::ObjectLock* lock);
  static ObjectLock* aquireLock(const acdk::lang::Object* obj);

};





} // lang
} // acdk


#endif //ACDK_MT

#endif //acdk_lang_sys_ObjectLockPool_h

