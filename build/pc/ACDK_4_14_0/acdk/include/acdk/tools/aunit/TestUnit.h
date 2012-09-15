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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestUnit.h,v 1.26 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_TestUnit_h
#define acdk_tools_TestUnit_h

#include "Config.h"
#include <acdk.h>

#include <acdk/lang/sys/core_fastmutex.h>
#include <acdk/lang/sys/core_guard.h>
#include "TestException.h"
#include "Test.h"
#include "TestFailure.h"
#include "TestResult.h"

namespace acdk {
/**
  a collection of tools for ACDK
*/
namespace tools {
/**
  unit-test framework
  @ingroup acdkaunit
*/
namespace aunit {



ACDK_DECL_CLASS(TestCase);
/**
  A single test case
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestCase
: extends ::acdk::lang::Object
, implements Test
{
  ACDK_WITH_METAINFO(TestCase)
private:
  RString _name;
  /**
    will only used in running test
  */
  RTestResult _activeResult;
public:
  /**
    This can be set to true if no side effect
    by logging successfull testAsserts
  */
  static bool NoSuccessTestLogging;
  /**
    if this value is set all test, which needs interaction
    should be skipped
  */
  static bool TestInBatchMode;
  TestCase(IN(RString) name)
  : _name(name)
  {
  }
  virtual ~TestCase()
  {
  }
  virtual void run(IN(RTestResult) result);

  virtual void runTest() = 0;
  virtual int testCount() { return 1; }
  RString name() { return _name; }
  RString getName() 
  {  
     return RString(typeid(*this).name()) + "." + name(); 
  }
  RString toString() { return getName(); }
  /** before starting a test setUp() will be called */
  virtual void setUp() { }
  /** after executing a test tearDown() will be called */
  virtual void tearDown() { }
  /**
    check test condition.
  */
  void assertImplementation(bool condition, IN(RString) str,  int linenum,  IN(RString) fileName);
  /**
    native C++ implementation for checking test condition.
    Mainly used via testAssert or testAssertComment macros
  */
  foreign void assertImplementation( bool condition, const char* str
                            ,  int linenum
                            ,  const char* fileName);
  
  
  foreign static TestCase* activeTest;
};

#ifdef testAssert
#  undef testAssert
#endif
/**
  Test a test condition.
  Will throw a TestException if condition is false
  @param condition any C++ expression containing only ASCII letters
  @ingroup acdkaunit
*/
#define testAssert(condition) \
::acdk::tools::aunit::TestCase::activeTest->assertImplementation ((condition),(#condition), __LINE__, __FILE__)
/**
  Test a test condition.
  Will throw a TestException if condition is false
  @param condition any C++ expression containing only ASCII letters, optional encoded in ASCIIUtf
  @ingroup acdkaunit
*/
#define testAssertUc(condition) \
::acdk::tools::aunit::TestCase::activeTest->assertImplementation ((condition),RString((_US(#condition))), __LINE__, __FILE__)

/**
  Test a test condition.
  Will throw a TestException if condition is false
  @param condition any C++ expression
  @param comment String containing a comment to this test
  @ingroup acdkaunit
*/
#define testAssertComment(condition, comment) \
::acdk::tools::aunit::TestCase::activeTest->assertImplementation ((condition),(comment), __LINE__, __FILE__)

ACDK_DECL_CLASS(TestSuite);

/**
  A TestSuite is a collection of TestCases
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestSuite
: extends TestCase
, implements Test

{
  ACDK_WITH_METAINFO(TestSuite)
private:
  RTestArray _tests;
public:
  
  TestSuite(IN(RString) str = "")
  : TestCase(str)
  , _tests(new TestArray(0))
  {
  }
  virtual void run(IN(RTestResult) result);
  virtual void runTest()
  {
     THROW1(Throwable, "Cannto run TestSuite::runTest(), use run(result) instead");
  }
  int testCount()
  {
    int erg = 0;
    for (int i = 0; i < _tests.length(); i++) 
      erg += _tests[i]->testCount();
    return erg;
  }
  void addTest(IN(RTest) test)
  {
     _tests->append(test);
  }
  virtual ::acdk::tools::aunit::RTest suite()
  {
     return this;
  }
  RString getName() { return name(); }
  RString toString() { return name(); }
  RTestArray tests() { return _tests; }
};

/**
  adapter for native C++ test methods implemenations
*/
template <class TestClass> 
class TestCaller 
: extends TestCase
{ 
public:
  typedef void (TestClass::*TestMethod)();
private:
  RefHolder<TestClass> _testClass;
  TestMethod _testMethod;
public:
  TestCaller(const char *name, TestMethod testmethod)
   : TestCase (name)
   , _testClass(new TestClass(name))
   , _testMethod(testmethod)
  {
  }
  void runTest() 
  {
     TestClass* tc = &_testClass;
    (tc->*_testMethod)(); 
  }
  void setUp() { _testClass->setUp (); }
  void tearDown() { _testClass->tearDown (); }
  RString toString() { return name(); }
  RString getName() { return name(); }
};


}
}
}


#endif //acdk_tools_TestUnit_h
