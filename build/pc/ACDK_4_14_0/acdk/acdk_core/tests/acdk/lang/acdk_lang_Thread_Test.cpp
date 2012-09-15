// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Thread_Test.cpp,v 1.32 2005/04/19 09:22:04 kommer Exp $
//
// $Log: acdk_lang_Thread_Test.cpp,v $
// Revision 1.32  2005/04/19 09:22:04  kommer
// panta rei
//
// Revision 1.31  2005/03/31 21:15:10  kommer
// minor
//
// Revision 1.30  2005/03/21 12:25:00  kommer
// panta rei
//
// Revision 1.29  2005/03/07 17:52:09  kommer
// panta rei
//
// Revision 1.28  2004/11/26 10:33:36  kommer
// stress test with atomic inc
//
// Revision 1.27  2004/11/22 17:45:35  kommer
// panta rei
//
// Revision 1.26  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.25  2001/12/29 13:49:53  kommer
// dos2unix
//
// Revision 1.24  2001/12/27 11:45:44  kommer
// panta rei
//
// Revision 1.23  2001/12/20 21:30:49  kommer
// panta rei
//
// Revision 1.22  2001/12/19 22:08:52  kommer
// panta rei
//
// Revision 1.21  2001/12/14 12:04:20  kommer
// dos2unix
//
// Revision 1.20  2001/12/14 09:30:25  kommer
// panta rei
//
// Revision 1.19  2001/12/02 13:16:49  kommer
// renamed testunit to aunit
//
// Revision 1.18  2001/08/12 15:25:00  kommer
// dos2unix
//
// Revision 1.17  2001/08/06 09:45:35  kommer
// faster String implementation
//
// Revision 1.16  2001/08/04 17:40:37  kommer
// *** empty log message ***
//
// Revision 1.15  2001/08/03 15:53:40  kommer
// added test case
//
// Revision 1.14  2001/07/20 11:14:33  kommer
// fixes
//
// Revision 1.13  2001/07/06 18:21:42  kommer
// panta rei
//
// Revision 1.12  2001/06/26 16:24:31  kommer
// varios condition/mutex fixes
//
// Revision 1.11  2001/06/26 00:20:09  kommer
// *** empty log message ***
//
// Revision 1.10  2001/06/25 23:35:20  kommer
// panta rei
//
// Revision 1.9  2001/06/25 22:35:08  kommer
// condition tests
//
// Revision 1.8  2001/06/25 21:13:58  kommer
// panta rei
//
// Revision 1.7  2001/06/17 10:40:51  kommer
// panta rei
//
// Revision 1.6  2001/06/09 01:19:44  kommer
// panta rei
//
// Revision 1.5  2001/06/04 17:53:20  kommer
// dos2unix
//
// Revision 1.4  2001/06/04 13:41:26  kommer
// panta rei
//
// Revision 1.3  2001/03/03 10:51:44  kommer
// panta rei
//
// Revision 1.2  2000/12/11 22:08:22  kommer
// fixed unreasonable loop counts in test
//
// Revision 1.1.1.1  2000/12/11 18:05:22  kommer
// ACDK Free edition
//
// Revision 1.3  2000/12/08 21:10:22  roger
// panta rei
//
// Revision 1.2  2000/12/08 15:52:26  roger
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.2  2000/08/10 08:55:38  roger
// panta rei
//
// Revision 1.1  2000/06/09 10:27:52  roger
// initial revision
//
// Revision 1.17  2000/04/13 17:45:43  roger
// panta rei
//
// Revision 1.16  2000/04/03 21:32:53  roger
// panta rei
//
// Revision 1.15  2000/04/03 13:39:20  roger
// panta rei
//
// Revision 1.14  2000/03/23 21:52:39  roger
// fixes for Threading on linux
//
// Revision 1.13  2000/03/22 20:51:17  roger
// panta rei
//
// Revision 1.12  2000/02/10 15:50:28  roger
// ACDK_DECL_CLASS 2 ACDK_DECL_THROWABLE
//
// Revision 1.11  2000/02/08 16:29:54  roger
// RefHolder and Arrays changed
//
// Revision 1.10  1999/11/30 15:01:50  roger
// panta rei
//
// Revision 1.9  1999/10/24 11:59:03  roger
// changed to consistent naming model
//
// Revision 1.8  1999/10/22 21:15:34  roger
// fixed File::pathSeperator|File::seperator mismatch
//
// Revision 1.7  1999/10/22 20:13:04  roger
// panta rei
//
// Revision 1.6  1999/10/21 17:56:06  roger
// Copyright notice updated
//
// Revision 1.5  1999/10/04 08:10:27  roger
// renamed M\MLib => ACDK
//
// Revision 1.4  1999/09/28 11:00:09  roger
// little casting problem fixed
//
// Revision 1.3  1999/09/02 15:07:34  roger
// new reference-mechanism
//
// Revision 1.2  1999/08/18 08:58:30  kai
// Umstrukturiert: Besitzt nun Methode Main.
//
// Revision 1.1  1999/08/13 09:57:41  kai
// Moved from src directory.
//
// Revision 1.2  1999/08/12 13:52:06  kai
// Dos2Unix Konvertierung.
//
// Revision 1.1.1.1  1999/08/05 11:14:56  roger
// Inital unstable snapshot
//



#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/ThreadGroup.h>
#include <acdk/lang/ThreadLocal.h>
#include <acdk/lang/InterruptedException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/sys/core_tick.h>

#include <acdk/tools/aunit/TestRunner.h>




namespace tests {
namespace acdk {
namespace lang {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;
  
BEGIN_DECLARE_TEST( Thread_Test )
  DECLARE_TEST( core )
  DECLARE_TEST( standard )
  DECLARE_TEST( noActionThreads )
  DECLARE_TEST( threadLocalStorage )
  DECLARE_TEST( runnableRecursive )
  DECLARE_TEST( ProducerConsumer)
  DECLARE_TEST( refCounting  )
  DECLARE_TEST( threadStress  )
END_DECLARE_TEST( Thread_Test  )

BEGIN_DEFINE_TEST( Thread_Test )

  ADD_TEST( Thread_Test, core ) 
  ADD_TEST( Thread_Test, standard ) 
  ADD_TEST( Thread_Test, threadLocalStorage ) 
  //ADD_TEST( Thread_Test, noActionThreads )
  ADD_TEST( Thread_Test, runnableRecursive ) 
   
  ADD_TEST( Thread_Test, ProducerConsumer ) 
  
  ADD_TEST( Thread_Test, refCounting ) 
  ADD_TEST( Thread_Test, threadStress ) 
  
END_DEFINE_TEST( Thread_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);


class AThreadTest 
: public Thread
{
public:
  AThreadTest() : Thread() { }
  static int _waittime;
  virtual void run()
  {
    //System::out->println("AThreadTest start");
    _waittime += 10;
    sleep(_waittime);
    { 
      acdk::lang::Integer i(42);
    }
    //System::out->println("AThreadTest end");
  }
};


class NothingThread
: extends Thread
{
public:
  void run()
  {
    // nothing
  }
};

int AThreadTest::_waittime = 0;

class BThreadTest
: public Object,
  public Runnable
{
  RObject _mutex;
  RString _name;
public:
  BThreadTest(RObject mutex, RString name)
  : Object(),
    _mutex(mutex),
    _name(name)
  {
  }
  virtual void run()
  {
    System::out->println("create ThreadLocal");
    RThreadLocal tls = new ThreadLocal(new String("Hallo"));
    System::out->println("ThreadLocal created");
    for (int i = 0; i < 5; i++) 
    {
      SYNCHRONIZEOBJECT(_mutex);
      //System::out->println("Thread sleeping now 1 sec: " + _name);
      Thread::sleep(30);
      //System::out->println("Thread awaked now: " + _name);
    }
    //System::out->println("TLS: " + RString(tls->get()));
  }
};

class SyncedObject 
: public Object
{
public:
  void doIt2(RString str)
  {
    SYNCHRONIZETHIS();
    
  }
  void doIt(RString str)
  {
    SYNCHRONIZETHIS();
    //sys::coreout << "Hello, here I' am exclusive: " << str->c_str() << sys::eofl;
    Thread::sleep(50);
    doIt2(str);
    //sys::coreout << "Hello, goodby: " << str->c_str() << sys::eofl;
  }
};
typedef RefHolder<SyncedObject> RSyncedObject;

class SyncedObjectAccess
: public Object,
  public Runnable
{
  RSyncedObject _obj;
  RString _str;
public:
  SyncedObjectAccess(RSyncedObject obj, RString str)
  : Object(),
    _obj(obj),
    _str(str)
  {
  }
  virtual void run() 
  {
    for (int i = 0; i < 5; i++)
      _obj->doIt(_str);
  }
};

class Consumer;
typedef RefHolder<Consumer> RConsumer;
class Producer;
typedef RefHolder<Producer> RProducer;
class Depot;
typedef RefHolder<Depot> RDepot;


class Depot
: public Object
{
  int buffer;
  bool empty;
public:
  Depot() : buffer(0), empty(true) { }
  void put(int data) 
  {
    SYNCHRONIZETHIS();
    while (empty == false) {
      try {  
	      wait();  
      }  catch(RInterruptedException ) {
	    //System::out->println("RInterruptedException: " + toString());
      }
    }
    //System::out->println(RString("put: ") + data);
    buffer = data;
    empty = false;
    notifyAll();
  }

  int get() 
  {
    SYNCHRONIZETHIS();
    while (empty == true) {
      try {
	      wait();
      } catch(RInterruptedException e) {
      }
    }
    //System::out->println(RString("get: ") + buffer);
    
    empty = true;
    int ret = buffer;
    notifyAll();
    return ret;
  }
};

class Consumer 
: public Thread 
{
  RDepot _depot;
public:
  Consumer(RDepot depot) 
    : Thread(),
      _depot(depot)
  {
  }
  virtual void run() 
  {
    for (int i = 0; i < 1000; i++) 
    {
      int g = _depot->get();
      if (g != i)
        System::out->println(RString("Consumer got ") + g + " instead of " + i);
      /*try {
        int tsleep = (int)(Math::random() * 500);
        //System::out->println("sleeping: " + String::valueOf(tsleep));
        //Thread::sleep(tsleep);
      } catch (RInterruptedException)  {
        //System::out->println("RInterruptedException: " + toString());
      }*/
    }
  }
};

class Producer 
: public Thread 
{
  RDepot _depot;
public:
  Producer(RDepot depot) 
    : Thread(),
      _depot(depot)
  {
  }
  virtual void run() 
  {
     for (int i = 0; i < 1000; i++) {
      _depot->put(i);
      /*try {
        int tsleep = (int)(Math::random() * 500);
        //System::out->println(RString("sleeping: ") + String::valueOf(tsleep));
        //Thread::sleep(tsleep);
        //sleep(200);
      } catch (RInterruptedException ex)  {
        //System::out->println("RInterruptedException: " + ex->toString());
      }*/
    }
  }
};

struct Sm
{
  Sm()
    : cm()
    , cc(cm)
    , pm()
    , pc(pm)
  {
  }
  sys::core_mutex cm;
  sys::core_condition cc;
  sys::core_mutex pm;
  sys::core_condition pc;
};

int buffer = 0;
bool empty = true;

void core_put(Sm& sm, int data) 
{
  {

    TLockGuard<sys::core_mutex>  lockthisp(sm.pm);
    if (empty == false) {
      try {  
	      sm.pc.wait();  
      } catch(RInterruptedException ex) {
	      //System::out->println("RInterruptedException: " + ex->toString());
      }
    }
    //System::out->println(RString("put: ") + data);
    buffer = data;
    }
    {
    //TLockGuard<sys::core_mutex>  lockthis(sm.cm);
    empty = false;
    sm.cc.notify();
    }
}

int core_get(Sm& sm) 
{
  {
    TLockGuard<sys::core_mutex>  lockthisc(sm.cm);

    if (empty == true) {
      try {
	      sm.cc.wait();
      } catch(RInterruptedException e) {
      }
    }
    //System::out->println(RString("get: ") + buffer);

    }
    {
    TLockGuard<sys::core_mutex>  lockthis(sm.pm);
    empty = true;
    sm.pc.notify();
    }
  return buffer;
}

class CoreConsumer
: public Thread 
{
public:
  Sm& _sm;
  CoreConsumer(Sm& sm)
    : Thread()
    , _sm(sm)
  {
  }
  virtual void run() 
  {
    for (int i = 0; i < 50; i++) {
      core_get(_sm);
      try {
        int tsleep = (int)(Math::random() * 500);
        if (tsleep == 0) tsleep = 1;
        //System::out->println("sleeping: " + String::valueOf(tsleep));
        //Thread::sleep(tsleep);
      } catch (RInterruptedException)  {
        System::out->println("RInterruptedException: " + toString());
      }
    }
  }
};

class CoreProducer
: public Thread 
{
public:
  Sm& _sm;
  CoreProducer(Sm& sm)
    : Thread()
    , _sm(sm)
  {
  }
  virtual void run()
  {
    for (int i = 0; i < 50; i++) {
      core_put(_sm, i);
      try {
        int tsleep = (int)(Math::random() * 500);
        if (tsleep == 0) tsleep = 1;
        //System::out->println(RString("sleeping: ") + String::valueOf(tsleep));
        //Thread::sleep(tsleep);
        //sleep(200);
      } catch (RInterruptedException ex)  {
        System::out->println("RInterruptedException: " + ex->toString());
      }
    }
  }
};

ACDK_DECL_CLASS(DummyObject);

class DummyObject
: public Object
{
public:
  DummyObject()
  {
  }
  virtual ~DummyObject()
  {
    //sys::coreout << "~DummyObject()" << sys::eofl;
  }
};

ACDK_DECL_CLASS(Test5Thread);
class Test5Thread 
: public Thread
{
  RObject obj;
  int count;
  
public:
  Test5Thread(RObject o, int c)
    : Thread(),
      obj(o),
      count(c)
  {
  }
  virtual void run() 
  {
    {
      SYNCTHIS();
      
    }
    for (int i = 0; i < count; i++) {
      RObject o1 = obj;
      {
        RObject o2 = obj;
        {
          RObject o3 = o2;
        }
      }
    }
    
  }
};

void Thread_Test::core()
{
  /* does not work with win32
  Sm sm;
  RThread producer = new CoreProducer(sm);
  RThread consumer = new CoreConsumer(sm);
  producer->start();
  //Thread::sleep(100);
  consumer->start();
  //Thread::sleep(1000);
  producer->join();
  consumer->join();
  */
}

void Thread_Test::standard()
{
  for (int i = 0; i < 10; i++) 
  {
    RThread trh = new AThreadTest();
    trh->start();
    //sys::coreout << "acdk_lang_Thread_Test1(): start trh->join();" << sys::eofl;
    trh->join();
    //sys::coreout << "acdk_lang_Thread_Test1(): end trh->join();" << sys::eofl;
  }
}
void Thread_Test::noActionThreads()
{
  for (int j = 0; j < 3; ++j) {
  RThreadArray ta = new ThreadArray(10);
  int i = 0;
  for (i = 0; i < ta->length(); i++) {
    ta[i] = new NothingThread();
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->start();
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->join();
  }
  }
}

void Thread_Test::threadLocalStorage()
{
  RObject  mutex = new Object;
  RThread thr1  = new Thread(RRunnable(new BThreadTest(mutex, "thr1")));
  RThread thr2  = new Thread(RRunnable(new BThreadTest(mutex, "thr2")));
  thr1->start();
  thr2->start();
  thr1->join();
  thr2->join();
}

void Thread_Test::runnableRecursive()
{
  /*
  sys::coreout << "\tTest if synchronization to given object works recursivally\n" 
    << "\tOutput should be\n"
    << "Hello, here I' am: Thread1\nHello, goodby: Thread1\nHello, here I' am: Thread2\nHello, goodby: Thread2\n\n"
    << "Start Test now:" << sys::eofl;
  */
  RSyncedObject so = new SyncedObject();
  RThread thr1 = new Thread(RRunnable(new SyncedObjectAccess(so, "Thread1")));
  RThread thr2 = new Thread(RRunnable(new SyncedObjectAccess(so, "Thread2")));
  thr1->start();
  thr2->start();
  thr1->join();
  thr2->join();
  
}

void Thread_Test::ProducerConsumer()
{
  RDepot depot = new Depot();
  RProducer producer = new Producer(depot);
  RConsumer consumer = new Consumer(depot);
  producer->start();
  consumer->start();
  producer->join();
  consumer->join();
}

void Thread_Test::refCounting()
{
  /*System::out->println(
    "This Test checks conflicts on multiple access to Refcounting."
    " If it fail, a segmentation fault should occour."
    " It takes a few seconds");*/
  RObject o = new DummyObject();
  //o->setInterThreadReference(true);
  RTest5ThreadArray ta = new Test5ThreadArray(20);
  int i = 0;
  for (i = 0; i < ta->length(); i++) {
    ta[i] = new Test5Thread(o, 20000);
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->start();
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->join();
  }
  
}

sys::core_atomicop atomic_value;

class DoNothingRunnable
: extends acdk::lang::Thread
{
public:
  virtual void run()
  {
    atomic_value.increment();
  }
};

void
Thread_Test::threadStress()
{
  sys::tick_t start = sys::core_tick::now();
  
  int threadCount = 100;
  RThreadArray ta = new ThreadArray(threadCount);
  int i = 0;
  for (i = 0; i < ta->length(); i++) {
    ta[i] = new DoNothingRunnable();
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->start();
  }
  for (i = 0; i < ta->length(); i++) {
    ta[i]->join();
  }
  int ms = sys::core_tick::millisecsSince(start);
  System::out->println(SBSTR("Started and finished " << threadCount << " threads in " << ms << " ms incs=(" << atomic_value.read() << ")"));
}


} // namespace tests 
} //namespace acdk 
} //namespace lang 






