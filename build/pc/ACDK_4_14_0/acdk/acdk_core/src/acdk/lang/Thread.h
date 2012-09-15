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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Thread.h,v 1.28 2005/04/22 11:13:56 kommer Exp $

#ifndef acdk_lang_Thread_h
#define acdk_lang_Thread_h

#include <acdk.h>

#include "Runnable.h"

#include "ThreadImpl.h"
#include "ThreadGroup.h"

#include "ThreadLocalImpl.h"
#include "sys/core_semaphore.h"
#include "sys/core_fastmutex.h"

namespace acdk {
namespace lang {

//============================= class Thread ===========================//

ACDK_DECL_CLASS(Thread);
ACDK_DECL_CLASS(ThreadGroup);


/** The Type or Creator of a Thread */
enum ThreadType
{
    /** Thread was created by User */
  UserThreadType,
    /** Thread was created by System / Unknown */
  SystemThreadType,
    /** Thread is Main-Thread */
  MainThreadType
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, ThreadType);

/**
  This is base implementation to start and manage Thread. 
  Use not the default constructor Thread().
  If you use thread functionality of ACDK it is important
  to use the System::main funktionality for proper initialization.

  See also acdk::lang::Runnable here in Package #acdk::lang.
  
  @author Roger Rene Kommer
  @version $Revision: 1.28 $
  @date $Date: 2005/04/22 11:13:56 $
*/  
class ACDK_CORE_PUBLIC Thread 
: extends ::acdk::lang::Object,
  implements ::acdk::lang::Runnable
{
  ACDK_WITH_METAINFO(Thread)  
public:
  enum State 
  {
    Created,
    Running,
    Suspended,
    Terminated,
    Detached
  };
  
private:
  RRunnable _runnable;
  RString _name;
  Thread::State _state;
  bool _isDaemon;
  /// thread should be interrupted
  bool _interrupted;
  /// this lock is currently waiting
  MonitorInterface* _currentMonitor;

  ThreadType _type;
  RThreadGroup _group;
  static sys::core_atomicop _threadCount;
  RThrowable _exceptionToThrow;
  /// for protecting setting/reading internal state
  typedef sys::core_fastmutex InternalMutexType;
  InternalMutexType _internalMutex;
  
#ifdef POSIX_THREADS
  /** 
      used to wait in start until
      Thread finisished initializing
  */
  RObject _startSync;
#endif //POSIX_THREADS
public:
  /** Construct an user Thread */
  Thread(IN(RRunnable) target = Nil, IN(RString) name = Nil);
  
  /** Construct an user Thread */
  Thread(IN(RThreadGroup) group, IN(RRunnable) target = Nil, IN(RString) name = Nil);
  
  /** Construct an system or main Thread */
  Thread(ThreadType type, IN(RString) name = Nil);

  foreign virtual ~Thread();
  
  /// reimplemented from String
  virtual RString toString();
  
  virtual void join(int millis = -1, int nanos = 0) THROWS1(RIllegalArgumentException);
  /**
    Interrupt this thread
  */
  virtual void interrupt();
  /**
    return true if this thread has interrupted signal set
  */
  virtual bool isInterrupted();
  
  /**
    return true if currentThread() is interrupted
  */
  static bool interrupted();
  /**
    return true if thread is running
  */
  bool isAlive();
  /**
    start the thread
  */
  virtual void start();
  /**
    overload this method to implement
    working code for this thread
  */
  virtual void run();
  /**
    return the priority of this thread
  */
  virtual int getPriority();
  /**
    sets the priority of this thread
    0 (hight) - 5 (low)
  */
  virtual void setPriority(int newPriority);
  /**
    force switch to another thread
  */
  static void yield();
  /**
    Sleep the current threads
    @param millis milli seconds
    @param nanaos nano seconds
  */
  static void sleep(int millis, int nanaos = 0);
  /**
    return the current thread
  */
  static RThread currentThread();
  /**
    return the threadgroup associated with this thread
  */
  RThreadGroup getThreadGroup();
  /**
    delegate to ThreadGroup::activeCount if exists
    otherwise return 1
  */
  static int activeCount();
  /**
    non static version of activeCount()
  */
  int getActiveCount();
  /**
    return stack frames in this thread.
    This method only works with currentThread.

    @deprectated 
  */
  virtual int countStackFrames();
  /**
    @deprecated should not be used
  */
  foreign static unsigned int getStackBase() { return sys::ObjectHeap::stackBase(); }
  
  /**
    Kills this thread, without releasing any resource
  */
  virtual void destroy();
  /**
    calls stop(new ThreadDeath()) 
    @see void stop(IN(RThrowable) obj);
  */
  virtual void stop();
  /**
    The given exception will be thrown in the thread either it
    is in a waiting situation (inside wait, sleep, join, etc.)
    or the thread is calling the Thread::checkForPendingException().
    If the given exception is not of type ThreadDeath or InterruptedException
    the stop() method will try to load the extended metainfo for this 
    exception type to throw the instance.
  */
  virtual void stop(IN(RThrowable) exception);
  /**
    Suspends the thread.
    If calling inside this thread the thread stops until
    the thread will be resumed outside this thread.
    Calling suspend() may result in a deadlock situtation if the thread
    currently holds a lock on thread-shared ressources
  */
  virtual void suspend();
  /**
    weak up thread previously suspended with suspend()
  */
  virtual void resume();
  /**
    List all threads of current threadgroup
  */
  static int enumerate(IN(RThreadArray) tarray);
  /**
    calls acdk::lang::System::printStackTrace()
  */
  static void dumpStack();
  /**
    set this thread to a deamon thread
  */
  virtual void setDaemon(bool on) { _isDaemon = on; }
  virtual bool isDaemon() { return _isDaemon; }

  virtual void setName(RString str);
  /**
    return the known threads, whereas main thread not included
  */
  inline static int getThreadCount()
  {
    return _threadCount.read();
  }
  /**
    internal helper
  */
  void doSleep(int millis);

  /** a new system thread was dedected */
  static void newSystemThread(ThreadType type = SystemThreadType);
  /** remove this system thread */
  static void removeSystemThread();
  /** checks if all thread are registered, removes zombies */
  static void checkSystemThreads();
  
  inline static bool isSingleThreaded() { return _threadCount.read() == 0; }

  /// System should be friend of it, but include-chain disallows it.
  foreign void _saveStackBase(unsigned int sp) 
  { 
    sys::ObjectHeap::saveStackBase(sp); 
  } 
  /**
    after basic operations (like sleep) it calls checkForThreadException()
    and if a throwable was set with setThreadExceptionToThrow this exception
    will be raised
  */
  inline void setThreadExceptionToThrow(IN(RThrowable) ex) { _exceptionToThrow = ex; }
  inline RThrowable getThreadExceptionToThrow() { return _exceptionToThrow; }
  /**
    throws a previously set exception
    @throw IllegalThreadStateException if called outside this thread
  */
  inline void checkForThreadException()
  {
    if (_exceptionToThrow == Nil)
      return;
    _invokeThreadException();
  }
  /// calls currentThread()->checkForThreadException()
  static void checkForPendingException()
  {
    currentThread()->checkForThreadException();
  }
  /**
    internal helper
  */
  bool _readResetIsInterrupted();
  /** set current waiting monitor */
  foreign static void setCurrentMonitor(MonitorInterface* monitor)
  {
    Thread::currentThread()->setCurrentWaitingMonitor(monitor);
  }
  foreign inline void setCurrentWaitingMonitor(MonitorInterface* monitor)
  {
    sys::core_lock_guard<InternalMutexType> _lock(_internalMutex);
    _currentMonitor = monitor;
  }
  foreign inline Thread::State _getState() const { return _state; }
  foreign inline void _setState(Thread::State state)  { _state = state; }
protected:
  void _invokeThreadException();
  void _run();
  foreign ThreadID _threadID;
public:
  foreign ThreadID threadID() { return _threadID; }
  foreign const ThreadID threadID() const { return _threadID; }
private:
#ifdef POSIX_THREADS
  foreign static void* _posixThreadFunc(void* arg);
  
public:
  //foreign RObject _joinWaitMonitor;
#endif //POSIX_THREADS
  
#ifdef WIN32_THREADS
#ifdef __BORLANDC__
#define _stdcall __stdcall
#endif  
  foreign static /*DWORD WINAPI*/ unsigned int _stdcall _Win32ThreadFunc(void* arg);
  foreign HANDLE _threadHandle;
public:
  foreign HANDLE threadHandle() { return _threadHandle; }
#endif //WIN32_THREADS

  bool _isThisThreadCurrent() { return _threadID.isThisThreadCurrent(); }
  foreign static ThreadID currentThreadId();
protected:
  void _checkAccess();

};

/**
  class to register/deregister waiting monitor
*/
foreign
class ScopedWaitingMonitor
{
  MonitorInterface* _monitor;
public:
  ScopedWaitingMonitor(MonitorInterface* monitor)
    : _monitor(monitor)
  {
    Thread::setCurrentMonitor(_monitor);
  }
  ~ScopedWaitingMonitor()
  {
    Thread::setCurrentMonitor(0);
  }
};



} // lang
} // acdk

#endif //acdk_lang_Thread_h

