// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Thread_Test2.cpp,v 1.8 2005/04/19 09:22:05 kommer Exp $
//
// $Log: acdk_lang_Thread_Test2.cpp,v $
// Revision 1.8  2005/04/19 09:22:05  kommer
// panta rei
//
// Revision 1.7  2005/03/21 10:59:12  kommer
// panta rei
//
// Revision 1.6  2003/12/29 13:14:46  kommer
// panta rei
//
// Revision 1.5  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.4  2001/12/28 17:36:18  kommer
// panta rei
//
// Revision 1.3  2001/12/20 21:30:49  kommer
// panta rei
//
// Revision 1.2  2001/12/02 13:16:49  kommer
// renamed testunit to aunit
//
// Revision 1.1  2001/06/26 12:17:48  kommer
// initial revision
//


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/ThreadGroup.h>
#include <acdk/lang/ThreadLocal.h>
#include <acdk/lang/InterruptedException.h>
#include <acdk/lang/Integer.h>

#include <acdk/tools/aunit/TestRunner.h>




namespace tests {
namespace acdk {
namespace lang {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;
  
BEGIN_DECLARE_TEST( Thread_Test2 )
  DECLARE_TEST( ProducerConsumer )
  DECLARE_TEST( interruptSleep )
  DECLARE_TEST( interruptWait )
  DECLARE_TEST( interruptJoin )
END_DECLARE_TEST( Thread_Test2  )

BEGIN_DEFINE_TEST( Thread_Test2 )

  ADD_TEST( Thread_Test2, ProducerConsumer ) 
  ADD_TEST( Thread_Test2, interruptSleep ) 
  ADD_TEST( Thread_Test2, interruptWait ) 
  
  ADD_TEST( Thread_Test2, interruptJoin ) 
  
END_DEFINE_TEST( Thread_Test2 )



#define TESTLOOPS 50
#define MAX_QUEUE 5

ACDK_DECL_CLASS(Buffer);
class Buffer
: extends ::acdk::lang::Object
{
  RintArray _buffer;
public:
  Buffer()
    : _buffer(new intArray(0))
  {
  }
  void put(int i)
  {
    SYNCTHIS();
    while (_buffer->length() >= MAX_QUEUE)
    {
      //System::out->println("put, wait start");
      wait();
      //System::out->println("put, wait end");
    }
    _buffer->append(i);
    notify();
  }
  int get()
  {
    SYNCTHIS();
    while (_buffer->length() == 0)
    {
      //System::out->println("get, wait start");
      wait();
      //System::out->println("get, wait end");
    }
    int ret = _buffer[0];
    _buffer->remove(0);
    notify();
    return ret;
  }
};

ACDK_DECL_CLASS(PThread);
int finishedTest = false;
class PThread
  : public ::acdk::lang::Thread
{
public:
  RObject _consumer;
  RBuffer _buffer;
  PThread(IN(RBuffer) buffer)
    : Thread()
    , _buffer(buffer)
  {
  }
  virtual void run()
  {
    for (int i = 0; i < TESTLOOPS; i++)
    {
      sleep((int)(Math::random()*70));
      _buffer->put(i);
    }
  }
};

ACDK_DECL_CLASS(CThread);

class CThread
  : public ::acdk::lang::Thread
{
public:
  RObject _producer;
  RBuffer _buffer;
  CThread(IN(RBuffer) buffer)
    : Thread()
    , _buffer(buffer)
  {
  }
  virtual void run()
  {
    for (int i = 0; i < TESTLOOPS; i++)
    {
      sleep((int)(Math::random()*100));
      int i2 = _buffer->get();
    }
  }
};



void Thread_Test2::ProducerConsumer()
{
  RBuffer buffer = new Buffer();
  RPThread pt = new PThread(buffer);
  RCThread ct = new CThread(buffer);
  pt->_consumer = &ct;
  ct->_producer = &pt;
  ct->start();
  pt->start();
  pt->join();
  ct->join();
}

ACDK_DECL_CLASS(DClass);

class DClass
: extends acdk::lang::Thread
{
public:
  bool interruptCalled;
  DClass()
    : interruptCalled(false)
  {
  }
  virtual void run()
  {
    try {
      while (true)
      {
        Thread::sleep(5000);
      }
    } catch (RInterruptedException ex) {
      interruptCalled = true;
      //System::out->println("ex: " + ex->getMessage());
    }
  }
};

void
Thread_Test2::interruptSleep()
{
  RDClass t = new DClass();
  t->start();
  Thread::sleep(500);
  t->interrupt();
  t->join();
  testAssert(t->interruptCalled == true);
}


ACDK_DECL_CLASS(EClass);

class EClass
: extends acdk::lang::Thread
{
public:
  bool interruptCalled;
  EClass()
    : interruptCalled(false)
  {
  }
  virtual void run()
  {
    try {
      SYNCHRONIZETHIS();
      wait(10000);

    } catch (RInterruptedException ex) {
      interruptCalled = true;
      //System::out->println("ex: " + ex->getMessage());
    }
  }
};

void
Thread_Test2::interruptWait()
{
  RDClass t = new DClass();
  t->start();
  Thread::sleep(500);
  t->interrupt();
  t->join();
  testAssert(t->interruptCalled == true);
}

ACDK_DECL_CLASS(FClass);

class FClass
: extends acdk::lang::Thread
{
public:
  RThread _caller;
  FClass(IN(RThread) caller)
    : _caller(caller)
  {
  }
  virtual void run()
  {
    Thread::sleep(500);
    _caller->interrupt();
  }
};

void
Thread_Test2::interruptJoin()
{
  try {
    RFClass t = new FClass(Thread::currentThread());
    t->start();
    t->join();
    testAssertComment(false, "expected RInterruptedException");
  } catch (RInterruptedException ex) {
    testAssertComment(true, "caught expected RInterruptedException");
  }
}


} //namespace lang 
} //namespace acdk 
} // namespace tests 
