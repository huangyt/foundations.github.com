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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/sys/acdk_lang_sys_core_specific_test.cpp,v 1.13 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/lang/sys/core_specific.h>


namespace tests {
namespace acdk {
namespace lang {
namespace sys {

using namespace ::acdk::lang;
using namespace ::acdk::lang::sys;
using namespace ::acdk::util;

  
BEGIN_DECLARE_TEST( core_specific_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( globalThread )
END_DECLARE_TEST( core_specific_Test  )

BEGIN_DEFINE_TEST( core_specific_Test )
  ADD_TEST( core_specific_Test, standard ) 
  ADD_TEST( core_specific_Test, globalThread )
END_DEFINE_TEST( core_specific_Test )



struct MyClass
{
  int _value;
  MyClass(int val = 0)
    : _value(val)
  {
  }
  MyClass(const MyClass& other)
  : _value(other._value)
  {
  }
  MyClass& operator=(const MyClass& other)
  {
    _value = other.value();
    return *this;
  }
  int value() const { return _value; }
  bool operator!=(const MyClass& other) const
  {
    return _value != other.value();
  }
  static int instances;
};

  specific<MyClass> global_specific;
  
ACDK_DECL_CLASS(MyThread);



class MyThread
: public ::acdk::lang::Thread
{
public:
  int _value;
  bool _testfailed;
  MyThread(int val = 0)
    : Thread()
    , _value(val)
    , _testfailed(false)
  {
  }
  void run()
  {
    MyClass mycls(_value);
    global_specific.get() = mycls;
    if (global_specific.get() != MyClass(_value))
      _testfailed = true;
  }
};

int MyClass::instances = 0;

void core_specific_Test::standard()
{
  {  
    specific<MyClass> spec;
    MyClass mcls(42);
    spec.get() = mcls;
    testAssert(spec.get().value() == 42);
  }
  testAssert(MyClass::instances == 0);
}

void core_specific_Test::globalThread()
{

  RMyThread mt = new MyThread(12);
  mt->start();
  mt->join();
  testAssert(mt->_testfailed == false);
  testAssert(MyClass::instances == 0);
}


} // namespace sys
} // namespace lang
} // namespace acdk
} // namespace tests

