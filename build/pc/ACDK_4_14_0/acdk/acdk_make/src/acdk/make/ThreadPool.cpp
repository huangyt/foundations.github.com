// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_make/src/acdk/make/ThreadPool.cpp,v 1.7 2003/06/19 14:37:20 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include "ThreadPool.h"

namespace acdk {
namespace make {

using namespace acdk::lang;



bool 
PooledThread::isActive() 
{ 
  return _active; 
}

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)

#else //defined(LOCAL_DEBUG)

#define DOUT(strexpr) do { } while(false)
#endif //defined(LOCAL_DEBUG)


PooledThread::PooledThread(IN(RThreadPool) threadPool)
: ::acdk::lang::Thread()
, _active(false)
, _quit(false)
, _threadPool(threadPool)
{
  
}

void 
PooledThread::setActive(bool active)
{
  //SYNCTHIS();
  if (active == false)
  {
    _active = active;
    _boring.down();
  } else {
    _boring.up();
    _active = active;
  }
  //_active = active;
}

void 
PooledThread::setQuit()
{
  _boring.up();
  _quit = true;
}




//virtual 
void 
PooledThread::run()
{
  _job->run();
  _threadPool->setJobResult(_job->getResult());
  setActive(false);
/*

  DOUT("PT: running: " << Integer::toString(Thread::currentThreadId().getId()));
  //SYNCTHIS();

  setActive(false);
  while (_threadPool == Nil)
    Thread::sleep(200);

  DOUT("PT: waiting: " << Integer::toString(Thread::currentThreadId().getId()));
  
  while (_threadPool->shutdown() == false ) 
  {
    DOUT("PT: Start Job resuming: "  << Integer::toString(Thread::currentThreadId().getId()));
    if (_job == Nil)
    {
      DOUT("PT: ISNIL!: " << Integer::toString(Thread::currentThreadId().getId()));
    } else
      _job->run();
    DOUT("PT: Job finished with: " << Integer::toString(_job->getResult()));
    _threadPool->setJobResult(_job->getResult());
    DOUT("PT: waiting: " << Integer::toString(Thread::currentThreadId().getId()));
    setActive(false);

    if (_quit == true)
      break;
  }
  DOUT("PT: quit: " << Integer::toString(Thread::currentThreadId().getId()));
  */
}



ThreadPool::ThreadPool(int minCount, int maxCount)
: Thread()
, _queue(new acdk::util::Vector())
, _minCount(minCount)
, _maxCount(maxCount)
, _shutdown(false)
, _failedCount(0)
{
  ACDK_SAFE_CONSTRUCTOR();
  _threads = new PooledThreadArray(_maxCount);
  /*
  for (int i = 0; i < _minCount; i++) 
  {
    RPooledThread pt = new PooledThread(this);//(RPooledThread)_obc();
    pt->start();
    _threads[i] = pt;
  }*/
}

void 
ThreadPool::enqeue(IN(RJob) runnable)
{
  _queue->add((RObject)runnable);
}

RJob 
ThreadPool::dequeue()
{
  return (RJob)_queue->remove(0);
}

void 
ThreadPool::run()
{
  DOUT(RString("TP::run with jobjs: ") << _queue->size());
  while (_queue->size() > 0 && _shutdown == false)
  {
    RJob job = dequeue();
    if (_maxCount == 1)
    {
      job->run();
      setJobResult(job->getResult());
    } else
      startJob(job);
  }
  if (_maxCount != 1)
    doShutdown();
}

void 
ThreadPool::startJob(IN(RJob) job)
{
  do {
    for (int i = 0; i < _threads->length(); i++) 
    {
      RPooledThread thread = _threads[i];
      if (thread == Nil || thread->isActive() == false) 
      {
        thread = new PooledThread();
        _threads[i] = thread;
        thread->setJob(job);
        thread->setThreadPool(this);
        thread->setActive(true);
        thread->start();
        return;
      }
    }
    DOUT("TP: Waiting for free Thread");
    //wait(200);
    sleep(400);

  } while (_shutdown == false);

  /*
  do {
    for (int i = 0; i < _threads->length(); i++) 
    {
      RPooledThread thread = _threads[i];
      if (thread == Nil || thread->isActive() == false) 
      {
        if (thread == Nil) 
        {
          thread = new PooledThread();
          thread->start();
          _threads[i] = thread;
        }
        //while (thread->isActive() == true)
        //  sleep(200);
        DOUT("TP: Starting Thread Index: " << Integer::toString(i) << " " << _threads[i]->threadID().getId());
        thread->setJob(job);
        thread->setThreadPool(this);
        thread->setActive(true);
        
        //_threads[i]->notify();
        return;
      }
    }
    {
      SYNCTHIS();
  
    // no free thread found, wait for a thread finishing
    DOUT("TP: Waiting for free Thread");
    //wait(200);
    sleep(400);
    }
  } while (_shutdown == false);
  */
}

void 
ThreadPool::setJobResult(JobResult result)
{
  //SYNCTHIS();
  if (result == JobFailed || result == JobAbortAll)
    ++_failedCount;
  if (result == JobAbortAll)
    _shutdown = true;
}

void 
ThreadPool::doShutdown()
{
  while (_shutdown == false && _queue->size() > 0)
  {
    sleep(200);
  }
  //_shutdown = true;
  DOUT("Shutdown all Threads starting");
  for (int i = 0; i < _threads->length(); i++) 
  {
    if (_threads[i] != Nil) 
    {
      _threads[i]->setQuit();
      
      DOUT(RString("TP start join: ") << _threads[i]->threadID().getId());
      _threads[i]->join();
      DOUT(RString("TP end join: ") << _threads[i]->threadID().getId());
      _threads[i] = Nil;
    }
  }
  DOUT("TP: Shutdown all Threads finished");
}

} // namespace make 
} // namespace acdk


