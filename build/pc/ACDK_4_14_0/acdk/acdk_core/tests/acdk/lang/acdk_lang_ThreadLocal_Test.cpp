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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_ThreadLocal_Test.cpp,v 1.19 2005/04/19 09:22:04 kommer Exp $



#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/ThreadImpl.h>
#include <acdk/lang/ThreadGroup.h>
#include <acdk/lang/ThreadLocal.h>
#include <acdk/lang/InterruptedException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/sys/core_specific.h>

#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace lang {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;



  
BEGIN_DECLARE_TEST( ThreadLocal_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( ThreadLocal_Test  )

BEGIN_DEFINE_TEST( ThreadLocal_Test )
  ADD_TEST( ThreadLocal_Test, standard ) 
END_DEFINE_TEST( ThreadLocal_Test )


core_specific _theValue;



class PollOnThreadLocal
: extends acdk::lang::Thread
{
public:
  ~PollOnThreadLocal()
  {
  }
  void setCreateTls()
  {
    core_specific aValue;
    aValue.set((void*)1);
    void* p = aValue.get();
    if ((int)p != 1)
      sys::coreout << "core_specific does not work in thread!" << sys::eofl;
    //testAssert((int)p == 1);
  }
  virtual void run()
  {
    setCreateTls();
    //Thread::sleep(300);
    _theValue.set((void*)0);
    for (int i = 1; i < 10000; i++) {
      void* val = _theValue.get();
      if ((int)val != i - 1)
        sys::coreout << "core_specific does not work in thread!" << sys::eofl;
      //testAssert((int)val == i - 1);
      _theValue.set((void*)i); 
    }
    
  } 
};


void
ThreadLocal_Test::standard()
{
  /** this code seems to work, but in ObjectLockPool 
      it corrupts internal vector of ObejectLock's
  */
  {
    core_specific aValue;
    aValue.set((void*)42);
    void* p = aValue.get();
    testAssert((int)p == 42);
  }
  
  int threadCount = 10;
  RThreadArray ta = new ThreadArray(threadCount);
  int i;
  for (i = 0; i < threadCount; i++) {
    ta[i] = new PollOnThreadLocal();
  }
  for (i = 0; i < threadCount; i++) {
    ta[i]->start();
  }
  for (i = 0; i < threadCount; i++) {
    ta[i]->join();
  }
  testAssert(true);
}


} // namespace tests
} //namespace acdk 
} //namespace lang 


