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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_StackAlloc_Test.cpp,v 1.21 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/util/Hashtable.h>
#include <acdk/lang/sys/StackAllocator.h>
#include <acdk/lang/sys/StackHeap.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/lang/sys/core_tick.h>



namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;
using namespace ::acdk::util;

  
BEGIN_DECLARE_TEST( StackAlloc_Test )
  DECLARE_TEST( rawAllocator )
  DECLARE_TEST( nullAllocator )
  DECLARE_TEST( standard )
  DECLARE_TEST( perfTest )
  DECLARE_TEST( heap )
  DECLARE_TEST( reporting )
  DECLARE_TEST( threadedTest )
  DECLARE_TEST( copyConstructor )
  DECLARE_TEST( core_system_isPtrInStack )
END_DECLARE_TEST( StackAlloc_Test  )

BEGIN_DEFINE_TEST( StackAlloc_Test )
  ADD_TEST( StackAlloc_Test, rawAllocator ) 
  ADD_TEST( StackAlloc_Test, nullAllocator ) 
  
  ADD_TEST( StackAlloc_Test, standard ) 
  ADD_TEST( StackAlloc_Test, perfTest ) 
  ADD_TEST( StackAlloc_Test, heap ) 
  ADD_TEST( StackAlloc_Test, reporting ) 
  
  ADD_TEST( StackAlloc_Test, threadedTest ) 
  ADD_TEST( StackAlloc_Test, copyConstructor ) 
  ADD_TEST( StackAlloc_Test, core_system_isPtrInStack ) 
  
END_DEFINE_TEST( StackAlloc_Test )

void
StackAlloc_Test::rawAllocator()
{
  StackAllocScope stk;
  //RawAllocator allocator;
  //Allocator* allocator = stdalloc();
  //StackAllocator allocator;
  {
    RObject obj = new (stk) Object();
  }
}

void
StackAlloc_Test::nullAllocator()
{
  {
    RObject obj = new ((Allocator*)0)Object();
  }
}

ACDK_DECL_CLASS(MyTestObject);

class MyTestObject
: extends ::acdk::lang::Object
{
  RObject obj;
public:
  MyTestObject() 
  {
    obj = new (allocator()) String(42);
  }
  ~MyTestObject()
  {
  }
};

void 
StackAlloc_Test::standard()
{
  StackAllocScope stk;
  RMyTestObject mto = new (stk) MyTestObject();
  /*
  {
    RObject obj = new (stk) Object();
    {
      RObject obj = new (stk) Object();
    }
  }

  {
    RString str = new (stk) String("ACDK");
    testAssert(str->equals("ACDK") == true);
  }*/
}

void allocRecursiv(int counter, ::acdk::lang::sys::Allocator* allocator)
{
  StackAllocScope stk;
  RString str1 = new (allocator) String("ACDK");
  if (counter <= 0)
    return;
  allocRecursiv(--counter, allocator);
}



void allocTest(::acdk::lang::sys::Allocator* allocator)
{
  RObject myobj = new (allocator) MyTestObject();
  RString str1 = new (allocator) String("ACDK");
  RObject obj1 = new (allocator) Object();
  int loops = 1000;
  int num = 100;
  {
    RString str1 = new (allocator) String("ACDK");
    for (int i = 0; i < loops; ++i) 
    {
      //RString str2 = new (allocator) String("ACDK");
      //RStringArray sa = new (allocator) StringArray(num);
      for (int j = num - 1; j >= 0; --j) 
      {
        StackAllocScope stk;
        RString str = new (allocator) String("ACDK");
        ::acdk::util::RHashtable dic = new (allocator) ::acdk::util::Hashtable();
        for (int k = 0; k < 3; ++k)
        {
          dic->put(new (allocator) Integer(k), new (allocator) Long(k));
        }

      }
    }
  }
  allocRecursiv(100, allocator);
}



void allocTest2()
{
  RObject myobj = new  MyTestObject();
  RString str1 = new  String("ACDK");
  RObject obj1 = new Object();
  int loops = 500;
  int num = 100;
  {
    
    RString str1 = new String("ACDK");
    for (int i = 0; i < loops; ++i) 
    {
      //StackAllocScope stk;
      //RString str2 = new (allocator) String("ACDK");
      //RStringArray sa = new (allocator) StringArray(num);
      for (int j = num - 1; j >= 0; --j) 
      {
        RString str = new String("ACDK");
        ::acdk::util::RHashtable dic = new  ::acdk::util::Hashtable();
        for (int k = 0; k < 3; ++k)
        {
          dic->put(new Integer(k), new Long(k));
        }

      }
    }
  }
}


void StackAlloc_Test::perfTest()
{
  StackAllocScope stk;
  tick_t start = ::acdk::lang::sys::core_tick::now();
  allocTest(ObjectHeap::allocator());
  tick_t end1 = ::acdk::lang::sys::core_tick::now();
  allocTest(stk);
  tick_t end2 = ::acdk::lang::sys::core_tick::now();
  System::out->println(RString("Allocator 0: ") 
      + int(end1 - start) 
      + RString("; Allocator stackalloc: ") + int(end2 - end1));
  
}


void
StackAlloc_Test::heap()
{
  System::out->println("StackHeap currently not working");
  return;
  /*
  ::acdk::lang::sys::StackHeap stkheap;
  ObjectHeap::pushFrame(&stkheap);
  tick_t start1 = ::acdk::lang::sys::core_tick::now();
  allocTest2();
  tick_t end1 = ::acdk::lang::sys::core_tick::now();
  ObjectHeap::popFrame();
  tick_t start2 = ::acdk::lang::sys::core_tick::now();
  allocTest2();
  tick_t end2 = ::acdk::lang::sys::core_tick::now();
   System::out->println(RString("Stack Frame Allocator: ") 
      + int(end1 - start1) 
      + RString("; Standard Allocator Frame: ") + int(end2 - start2));
    */  
}


void
StackAlloc_Test::reporting()
{
  System::out->println("StackAlloc_Test::reporting not implemented yet");
}


void StackAlloc_Test::threadedTest()
{
  System::out->println("StackAlloc_Test::threadedTest not implemented yet");

}

void
StackAlloc_Test::copyConstructor()
{
  Object obj1; // on stack
  RObject obj2 = new Object(obj1);
  RObject obj3 = new Object(obj1);
}

void
StackAlloc_Test::core_system_isPtrInStack()
{
  int bla = 0;
  testAssert(core_system::isPtrInStack((void*)&bla) == true);

  char* tb = new char[1];
  testAssert(core_system::isPtrInStack((void*)tb) == false);
  delete tb;
}

} // namespace sys
} // namespace lang
} // namespace acdk
} // namespace tests
