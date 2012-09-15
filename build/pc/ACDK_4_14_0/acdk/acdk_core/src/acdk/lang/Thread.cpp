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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Thread.cpp,v 1.40 2005/04/30 16:38:06 kommer Exp $


#include <acdk.h>

#ifdef ACDK_OS_UNIX
#include <unistd.h>
#include <signal.h>

#endif // ACDK_OS_UNIX

#ifdef ACDK_OS_WIN32
#include <process.h>
#endif

#include <acdk/util/Vector.h>
#include <acdk/io/PrintWriter.h>

#include "Thread.h"
#include "System.h"
#include "ThreadLocal.h"

#include "Exception.h"
#include "IllegalArgumentException.h"
#include "IllegalThreadStateException.h"
#include "UnsupportedOperationException.h"
#include "ThreadDeath.h"


namespace acdk {
namespace lang {

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
# define DOUT(msg) do { StringBuffer sb; sb << msg; System::out->println(sb.toString()); } while(false)
#else
#define DOUT(msg) do { } while(false)
#endif

static RThreadLocal __currentThread;
sys::core_atomicop Thread::_threadCount;

static RThread __getCurrentThread(bool autocreate = true)
{
  if (__currentThread == Nil) {
    if (autocreate == false)
      return Nil;
    __currentThread = new ThreadLocal();
    System::registerStaticReference(__currentThread);
  }
  RThread t = RThread(__currentThread->get());
  if (t == Nil) {
    if (autocreate == false)
      return Nil;
    t = new Thread(SystemThreadType);
    __currentThread->set((RObject)t);
  }
  return t;
}

static void __setCurrentThread(IN(RThread) t)
{
  if (__currentThread == Nil) {
    __currentThread = new ThreadLocal();
    System::registerStaticReference(__currentThread);
  }
  __currentThread->set((RObject)t);
}

Thread::Thread(IN(RRunnable) target, IN(RString) name)
: _runnable(target)
, _name(name)
, _state(Created)
, _isDaemon(false)
, _interrupted(false)
, _currentMonitor(0)
,  _type(UserThreadType)
, _group(ThreadGroup::getRoot())
#ifdef POSIX_THREADS
  //, _joinWaitMonitor(new Object())
, _startSync(new Object())
#endif
#ifdef WIN32_THREADS
  , _threadHandle(NULL)
#endif
{
  ACDK_SAFE_CONSTRUCTOR();
  if (_group != Nil)
    _group->_addThread(this);
}

Thread::Thread(IN(RThreadGroup) group, IN(RRunnable) target, IN(RString) name)
:  _runnable(target)
, _name(name)
, _state(Created)
, _isDaemon(false)
, _interrupted(false)
, _currentMonitor(0)
, _type(UserThreadType)
, _group(group)
#ifdef POSIX_THREADS
, _startSync(new Object())
#endif
{
  ACDK_SAFE_CONSTRUCTOR();
  if (_group != Nil)
    _group->_addThread(this);
}


Thread::Thread(ThreadType type, IN(RString) name)
: _runnable()
, _state(Created)
, _isDaemon(false)
, _interrupted(false)
, _currentMonitor(0)
, _type(type)
, _group(ThreadGroup::getRoot())
#ifdef POSIX_THREADS
, _startSync(new Object())
#endif
#ifdef WIN32_THREADS
  , _threadHandle(NULL)
#endif
{
  ACDK_SAFE_CONSTRUCTOR();
  if (_group != Nil)
    _group->_addThread(this);
  if (type == SystemThreadType || type == MainThreadType)
  {
    _state = Running;
    _isDaemon = true;
    _threadID = currentThreadId();
    __setCurrentThread(this);
  }
}



Thread::~Thread()
{
  ACDK_SAFE_DESTRUCTOR();
  if (_group != Nil) {
    _group->_removeThread(this);
  }
  if (_state == Created)
    return;
  if (_state == Running && _type == UserThreadType)
  {
    THROW0(IllegalThreadStateException);
  }
#ifdef WIN32_THREADS
  CloseHandle(_threadHandle);
#endif

#ifdef POSIX_THREADS
  if (_state == Terminated && _isThisThreadCurrent() == false) {
    void* retptr;
    //may fail ACDK_SYS_CALL2(pthread_join, _threadID.threadID(), &retptr, == 0);
    pthread_join(_threadID.threadID(), &retptr);
    _state = Detached;
  }
#endif
}

RString
Thread::toString()
{
  if (_name != Nil)
    return _name;
    return RString("Thread [") + String::valueOf((int)threadID().threadID()) + "]";
}

//static
RThread
Thread::currentThread()
{
  RThread o = __getCurrentThread();
  if (o == Nil) {
    THROW1(UnsupportedOperationException, "Thread::currentThread()");
    return Nil;
  }
  return o;
}


void
Thread::_checkAccess()
{
  if (_isThisThreadCurrent() == true)
    THROW0(IllegalThreadStateException);
}

void
Thread::join(int millis, int nanos) THROWS1(RIllegalArgumentException)
{

  SYNCTHIS();
  _checkAccess();
  if (millis != - 1 && millis < 0)
    THROW1(IllegalArgumentException, "timeout value is negative");
  if (nanos < 0 || nanos > 0xf423f)
    THROW1(IllegalArgumentException, "nanosecond timeout value out of range");
  if (_state != Running && _state != Created)
    return;
  wait(millis, nanos);
  if (_state == Terminated && _isThisThreadCurrent() == false)
  {
#ifdef POSIX_THREADS
    void* retptr;
    // call pthread_join to free internal pthread ressources
    pthread_join(_threadID.threadID(), &retptr);
#endif //POSIX_THREADS
    _state = Detached;
  }
  /*
#ifdef WIN32_THREADS
if (millis == -1)
    millis = INFINITE;
  DWORD derg = WaitForSingleObjectEx(_threadHandle, (DWORD)millis,  FALSE);
  switch (derg) {
    case WAIT_OBJECT_0: break;
    case WAIT_ABANDONED: break;
    case WAIT_IO_COMPLETION: break;
    case WAIT_TIMEOUT: break;
    case 0xFFFFFFFF:
      THROW1(Exception, "WaitForSingleObjectEx() failed");
      // error here
      break;
    default:
      break;
  }
#endif //#ifdef WIN32_THREADS
#ifdef POSIX_THREADS
  void* retptr;
  if (millis == -1) {
    ACDK_SYS_CALL2(pthread_join, _threadID.threadID(), &retptr, == 0);
  } else {
    _joinWaitMonitor->wait(millis, nanos);
    if (_state == Running)
      return;
    if (_state == Terminated) {
      SYNCTHIS();
      ACDK_SYS_CALL2(pthread_join, _threadID.threadID(), &retptr, == 0);
      _state = Detached;
    }
  }
#endif //POSIX_THREADS
  */
}

//virtual
void
Thread::run()
{
  acdk::lang::System::err->println("*** Thread::run() called!. Should be overwritten!");
}

void
Thread::_run()
{
  _threadCount.increment();
  /*
#if !defined(ACDK_DEBUG) && !defined(_MSC_VER)
  try {
#endif //!defined(ACDK_DEBUG) && !defined(_MSC_VER)
  */
  try {
    __setCurrentThread(this);
    if (_runnable != Nil)
      _runnable->run();
    else
      run();
  } catch (RThrowable ex) {
    if (_group != Nil)
       _group->uncaughtException(this, ex);
  }
    /*
#if !defined(ACDK_DEBUG) && !defined(_MSC_VER)
  } catch (RThrowable ex) {
    ex->printStackTrace(System::err);
    System::err->println(RString("Uncaught Exception in thread: [") + (int)threadID().threadID() + "]: " + ex->getMessage());
    if (_group != Nil)
      _group->uncaughtException(this, ex);
  } catch (...) {
    System::err->println(RString("Uncaught foreign Exception in thread: [") + (int)threadID().threadID());
  }
#endif //!defined(ACDK_DEBUG) && !defined(_MSC_VER)
    */
    try {
      if (_group != Nil)
      {
	      _group->_removeThread(this); // this to avoid  cyclic references
	      _group = Nil;
      }
    } catch( ...) {
    }
    __setCurrentThread(Nil);
    ThreadLocal::threadEnd();
    _threadCount.decrement();
}

#if !defined(DOXYGENONLY)
/**
  @internal
  to avoid problems with virtual derived pointer casts from void*, wrapp it in a simple
 structs
*/
struct TempThreadHolder {
  TempThreadHolder(RThread th)
    : _thread(th)
  {
  }
  ~TempThreadHolder()
  {
    _thread = Nil;
  }
  RThread _thread;
};

#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
LONG ExceptionFilter(LPEXCEPTION_POINTERS pep);
#endif

static void finishThread(TempThreadHolder*& th)
{
  if (th == 0)
    return;
  RThread thread = th->_thread;
  SYNCOBJECT(thread);
  th->_thread->_setState(Thread::Terminated);
  th->_thread->notifyAll();
  delete th;
  sys::ObjectHeap::removeThreadHeap();
  th = 0;
}
#endif // !defined(DOXYGENONLY)

#ifdef WIN32_THREADS //-------------------------------------



//static
//DWORD WINAPI
unsigned int _stdcall
Thread::_Win32ThreadFunc(void* arg)
{
  TempThreadHolder *th = (TempThreadHolder *)arg;
#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  __try {
    __try {
#endif

  th->_thread->_state = Running;
  th->_thread->_saveStackBase(sys::core_system::_getSP());
  //not needed on Windows: th->_thread->_startSema.up();
  th->_thread->_run();
#if !defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  finishThread(th);
#endif //!defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
#if defined(ACDK_USE_MSC_STRUCTURED_C_HANDLING)
  }
  __except(ExceptionFilter(GetExceptionInformation()))
  {

  }
  }
  __finally
  {
#endif //ACDK_USE_MSC_STRUCTURED_C_HANDLING
    finishThread(th);
#if defined(_MSC_VER)
    _endthreadex(0);
  }
#endif
  return 0;
}
#endif //WIN32_THREADS //-------------------------------------

#ifdef POSIX_THREADS //-------------------------------------
//static
void*
Thread::_posixThreadFunc(void* arg)
{
  TempThreadHolder *th = (TempThreadHolder *)arg;
  th->_thread->_state = Running;
  th->_thread->_saveStackBase(sys::core_system::_getSP());
  //sys::coreout << "_posixThreadFunc" << sys::eofl;
  {
    RObject lo = th->_thread->_startSync;
    SYNCOBJECT(lo);
    th->_thread->_startSync->notify();
  }
  th->_thread->_run();
  finishThread(th);
  return 0;
}
#endif //POSIX_THREADS //-------------------------------------


void
Thread::start()
{
  SYNCTHIS();
  if (_state != Created)
    return;
  TempThreadHolder *th = new TempThreadHolder(this);

#ifdef WIN32_THREADS //-------------------------------------
#if defined(_MSC_VER)
  // MSC
  _threadHandle = (HANDLE)_beginthreadex(0, 0, Thread::_Win32ThreadFunc, th, CREATE_SUSPENDED, (unsigned int*)&_threadID.id); // because of c-library
#else
  _threadHandle = CreateThread(NULL, 0, (DWORD (__stdcall*)(void *))Thread::_Win32ThreadFunc, th, CREATE_SUSPENDED, &_threadID.id);
#endif
  if (_threadHandle == NULL)
    THROW1(Exception, "CreateThread() failed");
  ResumeThread(_threadHandle);
#endif //#ifdef WIN32_THREADS //-------------------------------------


#ifdef POSIX_THREADS //-------------------------------------
  int ret = 0;
  //_startStatus.increment();
  //_startStatus.increment();
  ret = pthread_create(&(_threadID.id), NULL, Thread::_posixThreadFunc, th);
  if (ret != 0)
  {
    sys::coreout << "pthread_create ret:" << ret << sys::eofl;
    ACDK_SYS_CALL4(ret = pthread_create, &(_threadID.id), NULL, Thread::_posixThreadFunc, th, == 0);
  }
  {
    SYNCOBJECT(_startSync);
    _startSync->wait(1000);
  }
  /*
  if (_startStatus.decr_test_zero() == false)

    _startSema.down();
  */
#endif //POSIX_THREADS //-------------------------------------

}

#ifdef POSIX_THREADS //-------------------------------------

struct timespec
milliAmicro2timespec(int millisec, int usecond)
{
  struct timespec abstime;
  abstime.tv_sec = millisec / 1000;
  long restmillisec = millisec % 1000;
  abstime.tv_nsec = restmillisec * 1000 + usecond / 1000;
  return abstime;
}

#if  !defined(ACDK_HAVE_NANOSLEEP)

int
nanosleep(struct timespec* tv, int)
{
  unsigned nsecs = tv->tv_sec * 1000000;
  nsecs += tv->tv_nsec / 1000;
  usleep(nsecs);
  return 0;
}
#endif // ACDK_HAVE_NANOSLEEP
#endif //POSIX_THREADS //-------------------------------------

void
Thread::doSleep(int millis)
{
  SYNCHRONIZETHIS();
  wait(millis);
}

//static
void
Thread::sleep(int millis, int nanos)
{
  if (millis > 1)
  {
    currentThread()->doSleep(millis);
  }
  else
  {


#ifdef WIN32_THREADS //-------------------------------------
  ::Sleep (millis);
#endif //WIN32_THREADS //-------------------------------------
#ifdef POSIX_THREADS //-------------------------------------
  struct timespec delay;
  long seconds = (millis / 1000);
  delay.tv_sec = seconds;
  long restmillis = millis % 1000;
  delay.tv_nsec = (restmillis) * 1000 + nanos;
  ::nanosleep(&delay, 0);
#endif //POSIX_THREADS //-------------------------------------
  }
}

//static
void
Thread::yield()
{
#ifdef WIN32_THREADS //-------------------------------------
  ::Sleep (0);
#endif //WIN32_THREADS //-------------------------------------
#ifdef POSIX_THREADS //-------------------------------------
  sched_yield();
#endif //POSIX_THREADS //-------------------------------------
}

//static
ThreadID
Thread::currentThreadId()
{
#ifdef WIN32_THREADS //-------------------------------------
  return GetCurrentThreadId();
#endif //WIN32_THREADS //-------------------------------------
#ifdef POSIX_THREADS //-------------------------------------
  return pthread_self();
#endif //POSIX_THREADS //-------------------------------------
}

int threadPriorityTable[] =
{
#ifdef WIN32_THREADS
  THREAD_PRIORITY_HIGHEST,
  THREAD_PRIORITY_ABOVE_NORMAL,
  THREAD_PRIORITY_NORMAL,
  THREAD_PRIORITY_BELOW_NORMAL,
  THREAD_PRIORITY_LOWEST
#endif //WIN32_THREADS

#ifdef POSIX_THREADS
  1,
  2,
  3,
  4,
  5
#endif //POSIX_THREADS
};



//virtual
int
Thread::getPriority()
{
#ifdef WIN32_THREADS
  DWORD erg = GetThreadPriority(threadHandle());
  for (int i = 0; i < sizeof(threadPriorityTable) / sizeof(threadPriorityTable[0]); i++)
    if (threadPriorityTable[i] == erg)
      return i;
  return 2;
#endif

#ifdef POSIX_THREADS
  sched_param param;
  int policy;
  ACDK_SYS_CALL3(pthread_getschedparam, threadID().threadID(), &policy, &param, == 0);
  int prio = param.sched_priority;
  for (int i = 0; i < int(sizeof(threadPriorityTable) / sizeof(threadPriorityTable[0])); i++)
    if (threadPriorityTable[i] == prio)
      return i;
  return 2;
#endif
}

//virtual
void
Thread::setPriority(int newPriority)
{
#ifdef WIN32_THREADS
  SetThreadPriority(threadHandle(), threadPriorityTable[newPriority]);
#endif

#ifdef POSIX_THREADS
  sched_param param;
  int policy = 0;
  ACDK_SYS_CALL3(pthread_getschedparam, threadID().threadID(), &policy, &param, == 0);
  param.sched_priority = threadPriorityTable[newPriority];
  ACDK_SYS_CALL3(pthread_setschedparam, threadID().threadID(), policy, &param, == 0);
#endif
}


RThreadGroup
Thread::getThreadGroup()
{
  return _group;
}

int
Thread::getActiveCount()
{
  if (_group == Nil)
    return 1;
  return _group->activeCount();
}
//static
int
Thread::activeCount()
{
  return currentThread()->getActiveCount();
}

//static
void
Thread::dumpStack()
{
  System::printStackTrace();
}

void
Thread::setName(RString str)
{
  _name = str;
}

//virtual
void
Thread::interrupt()
{
  sys::core_lock_guard<InternalMutexType> _lock(_internalMutex);

  if (_interrupted == false)
  {
    _interrupted = true;
    _exceptionToThrow = new InterruptedException();
    if (_currentMonitor != 0)
      _currentMonitor->signal();
  }
}

bool
Thread::isInterrupted()
{
  sys::core_lock_guard<InternalMutexType> _lock(_internalMutex);
  return _interrupted;
}

//static
bool
Thread::interrupted()
{
  return currentThread()->_readResetIsInterrupted();
}

bool
Thread::_readResetIsInterrupted()
{
  sys::core_lock_guard<InternalMutexType> _lock(_internalMutex);
  bool ret = _interrupted;
  _interrupted = false;
  return ret;
}

bool
Thread::isAlive()
{
  return _state == Running;
}



//static
void
Thread::newSystemThread(ThreadType type)
{
  new Thread(type);
}

//static
void
Thread::removeSystemThread()
{
  RThread t = __getCurrentThread(false /*autocreate*/);
  if (t == Nil)
    return;
  __setCurrentThread(Nil);

  t->getThreadGroup()->_removeThread(t);
}

//static
void
Thread::checkSystemThreads()
{
  __getCurrentThread();
}

//virtual
int
Thread::countStackFrames()
{
  if (currentThread().impl() != this)
    return -1;
  ::acdk::lang::sys::BackTrace _stackFrame;
  return _stackFrame.getSize();
}
//virtual
void
Thread::destroy()
{
  DOUT("Thread::destroy()");
  resume();
  if (_state != Running)
    return;

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  TerminateThread(_threadHandle, -1);
#else
  pthread_kill(threadID().threadID(), SIGKILL);
#endif
  _state = Terminated;
}

//virtual
void
Thread::stop()
{
  stop(new ThreadDeath());
}

//virtual
void
Thread::stop(IN(RThrowable) obj)
{
  sys::core_lock_guard<InternalMutexType> _lock(_internalMutex);
  _exceptionToThrow = obj;
  if (_currentMonitor != 0)
    _currentMonitor->signal();
}

//virtual
void
Thread::suspend()
{
  if (_state != Running)
    return;

#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  SuspendThread(_threadHandle);
#elif defined(ACDK_HAS_PTHREAD_SUSPEND_RESUME)
  pthread_suspend(threadID().threadID());
#else
  pthread_kill(threadID().threadID(), SIGSTOP);
#endif
  _state = Suspended;
}

void
Thread::resume()
{
  if (_state != Suspended)
    return;
#if defined(ACDK_OS_WIN32) && !defined(ACDK_OS_CYGWIN32)
  ResumeThread(_threadHandle);
#elif defined(ACDK_HAS_PTHREAD_SUSPEND_RESUME)
  pthread_continue(threadID().threadID());
#else
  pthread_kill(threadID().threadID(), SIGCONT);
#endif
}

//static
int
Thread::enumerate(IN(RThreadArray) tarray)
{
  RThreadGroup group = currentThread()->getThreadGroup();
  if (group != Nil)
    return group->enumerate(tarray);
  return 0;
}

void
Thread::_invokeThreadException()
{
  SYNCTHIS();
  if (_exceptionToThrow == Nil)
    return;
  if (_threadID.isThisThreadCurrent() == false)
    THROW0(IllegalThreadStateException);

  RThrowable ex = _exceptionToThrow;
  _exceptionToThrow = Nil;
  if (ex->getClass() == InterruptedException::GetClass())
    throw RInterruptedException(ex);
  else if (ex->getClass() == ThreadDeath::GetClass())
    throw RThreadDeath(ex);

  THROW_INSTANCE(ex);
}

static void __instantiateThreadTempate()
{
  RThreadArray t;
  t = new ThreadArray(0);
  RThreadGroupArray tg;
  tg = new ThreadGroupArray(0);
}


} // lang
} // acdk


