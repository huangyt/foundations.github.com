// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_make/src/acdk/make/ThreadPool.h,v 1.7 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_make_ThreadPool_h
#define acdk_make_ThreadPool_h

#include <acdk.h>
#include "Task.h"
#include <acdk/lang/Thread.h>
#include <acdk/util/Vector.h>



namespace acdk {
namespace make {

enum JobResult
{
  JobUnknown = -1,
  JobOk = 0,
  JobFailed = 1,
  JobAbortAll = 2
};
ACDK_DEF_LIB_ENUM(ACDK_ACDK_MAKE_PUBLIC, JobResult);
  
ACDK_DECL_INTERFACE(Job);

class ACDK_ACDK_MAKE_PUBLIC Job
: implements ::acdk::lang::Runnable
{
public:
  virtual JobResult getResult() = 0;
};

ACDK_DECL_CLASS(ThreadPool);
ACDK_DECL_CLASS(PooledThread);


/** 
  A pool of Threads to handle short requests very quickly
  without the overhead to create a thread for each request
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/08 10:53:20 $
  @see PoolThread
*/
class ACDK_ACDK_MAKE_PUBLIC ThreadPool
: extends ::acdk::lang::Thread
{
  ACDK_WITH_METAINFO(ThreadPool)
protected:
  /** contains PooledThread */ 
  RPooledThreadArray _threads;
  /** contains Jobs */
  ::acdk::util::RVector _queue;
  int _minCount;
  int _maxCount;
  int _shutdown;
  int _failedCount;
  ::acdk::lang::sys::core_semaphore _freeThread;
public:
  /**
    @arg obj ObjectCreator, which must create an instance of PooledThread.
  */
  ThreadPool(int minCount, int maxCount);
  bool shutdown() 
  { 
    // No SYNCTHIS
    return _shutdown; 
  }
  void enqeue(IN(RJob) runnable);
  RJob dequeue();
  void doShutdown();
  void setJobResult(JobResult result);
  int failedCount() { return _failedCount; }
  void startJob(IN(RJob) runnable);
  virtual void run();
};

using ::acdk::lang::Runnable;
/** 
  A Thread, which will be managed by a ThreadPool
  API: ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/08 10:53:20 $
  @see ThreadPool
*/
class ACDK_ACDK_MAKE_PUBLIC PooledThread
: extends ::acdk::lang::Thread
{
  ACDK_WITH_METAINFO(PooledThread)
protected:
  bool _active;
  bool _quit;
  RJob _job;
  RThreadPool _threadPool;
  ::acdk::lang::sys::core_semaphore _boring;
public:
  PooledThread(IN(RThreadPool) threadPool = Nil);
  void setThreadPool(IN(RThreadPool) threadPool) 
  { 
    _threadPool = threadPool; 
  }
  bool isActive();
  void setQuit();
  void setActive(bool active);
  void setJob(IN(RJob) job)
  {
    _job = job;
  }
  
  virtual void run();
  
};

} // namespace make 
} // namespace acdk

#endif //acdk_make_ThreadPool_h
