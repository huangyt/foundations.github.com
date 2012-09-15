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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/ref/acdk_lang_ref_Reference_Test.cpp,v 1.13 2005/03/07 17:52:10 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/System.h>
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( Reference_Test )
  DECLARE_TEST( noReferenceQueue )
  DECLARE_TEST( withReferenceQueue )
  DECLARE_TEST( threadedReferences )
END_DECLARE_TEST( Reference_Test  )

BEGIN_DEFINE_TEST( Reference_Test )
  ADD_TEST( Reference_Test, noReferenceQueue ) 
  ADD_TEST( Reference_Test, withReferenceQueue ) 
  ADD_TEST( Reference_Test, threadedReferences ) 
  
END_DEFINE_TEST( Reference_Test )

using namespace ::acdk::lang::ref;
using namespace ::acdk::lang::sys;


/** just to test instances */
class ReferenceTestClass
: extends ::acdk::lang::Object
{
public:
  static int instanceCount;
  static core_fastmutex _counterMutex;

  ReferenceTestClass()
    : Object()
  {
    core_lock_guard<core_fastmutex> lock(_counterMutex);
    ++instanceCount;
    //::acdk::lang::System::out->print("+"); System::out->flush();    
  }
  ~ReferenceTestClass()
  {
    core_lock_guard<core_fastmutex> lock(_counterMutex);
    --instanceCount;
    //::acdk::lang::System::out->print("-"); System::out->flush();    
  }
};

int ReferenceTestClass::instanceCount = 0;
core_fastmutex ReferenceTestClass::_counterMutex;

void
Reference_Test::noReferenceQueue()
{
  { // test case 1 with no Reference Queue
    RWeakReference wr;
    {
      RObject str = new ReferenceTestClass();
      wr = new WeakReference(&str);
      testAssert(wr->get()->equals(&str) == true);
    }
    testAssert(wr->get() == Nil);
  }
  testAssert(ReferenceTestClass::instanceCount == 0);
}

void
Reference_Test::withReferenceQueue()
{
  RReferenceQueue refqueue = new ReferenceQueue();
  ReferenceTestClass::instanceCount = 0;
  { // test case 1 with no Reference Queue

    RWeakReference wr;
    {
      RObject str = new ReferenceTestClass();
      wr = new WeakReference(&str, refqueue);
      testAssert(wr->get()->equals(&str) == true);
    }
    testAssert(wr->get() == Nil);
    testAssert(refqueue->poll() == wr);
    testAssert(wr->get() == Nil);
  }
  int rc = ReferenceTestClass::instanceCount;
  testAssert(ReferenceTestClass::instanceCount == 0);
}

RReferenceQueue gRefQueue;
class UseReferenceThread
: public ::acdk::lang::Thread
{
public:
  void run()
  {
    //Thread::sleep(200);
    for (int i = 0; i < 2000; ++i)
    {
      RReference wr;
      {
        RObject obj = new ReferenceTestClass();
        wr = new WeakReference(obj, gRefQueue);
        
        obj = Nil;
        if (wr->isEnqueued() == false)
        {
          SYNCOBJECT(gRefQueue);
          sys::coreout << "Reference_Test::threadedReferences Oops " << wr->refCount() << " " << (void*)wr.iptr() << " " << (void*)wr->objectPtr() << sys::eofl;
        }
      }
      if (wr->isEnqueued() == false)
      {
        SYNCOBJECT(gRefQueue);
        sys::coreout << "Reference_Test::threadedReferences Oops " << wr->refCount() << " " << (void*)wr.iptr() << " " << (void*)wr->objectPtr() << sys::eofl;
      }
      {
        RObject obj = wr->get();
      }
    }
  }
};

void
Reference_Test::threadedReferences()
{
  gRefQueue = new ReferenceQueue();

  int threadcount = 2;
  RThreadArray ta = new ThreadArray(threadcount);
  int i = 0;
  for (i = 0; i < threadcount; ++i)
  {
    ta[i] = new UseReferenceThread();
  }
  for (i = 0; i < threadcount; ++i)
  {
    ta[i]->start();
  }
  Thread::sleep(300);
  RReference ref;
  while ((ref = gRefQueue->poll()) != Nil)
  {
      //sys::coreout << "poll: " << (void*)&ref << " " << ref->isEnqueued() << " " << ref->refCount() << sys::eofl;
    //::acdk::lang::System::out->print(RString(". (") + ); System::out->flush();
    //Thread::sleep(1);
    ref = Nil;
  }
  for (i = 0; i < threadcount; ++i)
  {
    ta[i]->join();
  }
  while ((ref = gRefQueue->poll()) != Nil)
  {
    //::acdk::lang::System::out->print("."); System::out->flush();
    //Thread::sleep(1);
    ref = Nil;
  }
  gRefQueue = Nil;
  testAssert(ReferenceTestClass::instanceCount == 0);
  //sys::coreout << "instanceCount: " << ReferenceTestClass::instanceCount << sys::eofl;
}


} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




