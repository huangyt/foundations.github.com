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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/ref/acdk_lang_ref_NotifyObjectEvent_Test.cpp,v 1.7 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/System.h>
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( NotifyObjectEvent_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( notDeleteInConstructor )
  DECLARE_TEST( threaded )
END_DECLARE_TEST( NotifyObjectEvent_Test  )

BEGIN_DEFINE_TEST( NotifyObjectEvent_Test )
  ADD_TEST( NotifyObjectEvent_Test, standard ) 
  ADD_TEST( NotifyObjectEvent_Test, notDeleteInConstructor ) 
  
  ADD_TEST( NotifyObjectEvent_Test, threaded ) 
  
END_DEFINE_TEST( NotifyObjectEvent_Test )

using namespace ::acdk::lang::ref;
using namespace ::acdk::lang::sys;



ACDK_DECL_CLASS(NotifyObjectEventTestClass);

class NotifyObjectEventTestClass
: extends ::acdk::lang::Object
, implements NotifyObjectEventListener
{
public:
  static core_fastmutex _counterMutex;
/** just to test instances */
  RObject subObject;
  static int instanceCount;
  bool doDelete;
  NotifyObjectEventTestClass(RObject sub = Nil)
    : Object()
    , subObject(sub)
    , doDelete(true)
  {
    core_lock_guard<core_fastmutex> lock(_counterMutex);
    ++instanceCount;
    NotifyObjectEvent::add(this, this);
    //::acdk::lang::System::out->print("+"); System::out->flush();    
  }
  ~NotifyObjectEventTestClass()
  {
    NotifyObjectEvent::remove(this, this);
    core_lock_guard<core_fastmutex> lock(_counterMutex);
    --instanceCount;
    //::acdk::lang::System::out->print("-"); System::out->flush();    
  }

  // interfaces from::acdk::lang::sys::NotifyObjectEventListener 
  foreign virtual void notifyBeforeConstruction(Object* obj) { }
  foreign virtual bool notifyBeforeDestruction(Object* obj) 
  {
    return doDelete;
  }
  foreign virtual void notifyWhileDestruction(Object* obj) { }
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) { return false; }
};
core_fastmutex NotifyObjectEventTestClass::_counterMutex;
int NotifyObjectEventTestClass::instanceCount = 0;


void
NotifyObjectEvent_Test::standard()
{
  for (int i = 0; i < 100; i++)
  {
    RNotifyObjectEventTestClass obj = new NotifyObjectEventTestClass();
    for (int j = 0; j < 20; ++j)
    {
      obj = new NotifyObjectEventTestClass(&obj);
    }
  }
}


void
NotifyObjectEvent_Test::notDeleteInConstructor()
{
  for (int i = 0; i < 100; i++)
  {
    NotifyObjectEventTestClass* optr;
    {
      RNotifyObjectEventTestClass o = new NotifyObjectEventTestClass();
      o->doDelete = false;
      optr = &o;
    }
    optr->doDelete = true;
    {
      RNotifyObjectEventTestClass o = optr;
    }
  }
}

class NotifyObjectEventThread
: public ::acdk::lang::Thread
{
public:
  void run()
  {
  }
};

void
NotifyObjectEvent_Test::threaded()
{
  int threadcount = 1;
  RThreadArray ta = new ThreadArray(threadcount);
  int i = 0;
  for (i = 0; i < threadcount; ++i)
  {
    ta[i] = new NotifyObjectEventThread();
  }
  for (i = 0; i < threadcount; ++i)
  {
    ta[i]->start();
  }

  for (i = 0; i < threadcount; ++i)
  {
    ta[i]->join();
  }
  testAssert(NotifyObjectEventTestClass::instanceCount == 0);
}


} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




