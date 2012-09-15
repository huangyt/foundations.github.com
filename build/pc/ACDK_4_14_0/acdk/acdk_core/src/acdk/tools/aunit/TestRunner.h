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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestRunner.h,v 1.19 2005/04/09 19:26:56 kommer Exp $


#ifndef acdk_tools_testunit_TestRunner_h
#define acdk_tools_testunit_TestRunner_h

#include "TestUnit.h"

namespace acdk {
namespace tools {
namespace aunit {

/**
  runs a collection of tests
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestRunner
: extends ::acdk::lang::Object
{
   
public:
  //bool _verbose;
  //bool _print_summary;
  bool _abortonfail;
  //int _countTest;
  //int _countRunTest;
  //int _countFailure;
  //int _countError;
  //bool _htmlReport;
  static RTestArray _getTests();
  TestRunner()
  : Object()
  //, _verbose(true)
  //, _print_summary(true)
  , _abortonfail(false)
  //, _countTest(0)
  //, _countRunTest(0)
  //, _countFailure(0)
  //, _countError(0)
  //, _htmlReport(false)
  {
  }
  bool executeTest(IN(RTest) test, IN(RTestResult) result);
  bool executeTest(IN(RString) strtest, IN(RTestResult) result);
  int executeTests(IN(RStringArray) args);
  bool executeTests(IN(RTestResult) result);
  void listTests();
  void listTests(IN(RTest) test);
  static int testmain(RStringArray args);

};

class ACDK_TOOLS_AUNIT_PUBLIC TestRunnerStaticAdder
{
public:
   TestRunnerStaticAdder(IN(RTest) test)
   {
     TestRunner::_getTests()->append(test);
   }
};

/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define BEGIN_DECLARE_TEST( ClassName ) \
class ClassName \
   : public ::acdk::tools::aunit::TestSuite \
{   \
public: \
  virtual ::acdk::tools::aunit::RTest suite(); \
  ClassName(IN(RString) name) \
  : ::acdk::tools::aunit::TestSuite(name) \
  {\
  }

/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define DECLARE_TEST( methodName ) \
   void methodName();

/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define END_DECLARE_TEST(ClassName) \
};
 
/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define AUTOEXEC_TEST_SUITE( className ) \
static ::acdk::tools::aunit::TestRunnerStaticAdder t( new className(#className) );

/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define BEGIN_DEFINE_TEST(ClassName) \
   AUTOEXEC_TEST_SUITE(ClassName) \
   ::acdk::tools::aunit::RTest ClassName::suite()\
      { \
         ::acdk::tools::aunit::RTestSuite testSuite = new ::acdk::tools::aunit::TestSuite(name());

/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define END_DEFINE_TEST(ClassName) \
      return (::acdk::tools::aunit::RTest)testSuite; \
   }


/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define ADD_TEST( ClassName, methodName) \
   testSuite->addTest ( \
      new ::acdk::tools::aunit::TestCaller < ClassName >( \
          #ClassName "." #methodName ,  \
         (::acdk::tools::aunit::TestCaller< ClassName > ::TestMethod)&ClassName::methodName ));




/**
  used to define C++ unit test.
  @ingroup acdkaunit
  @see gw_ref[acdk_tools_aunit_man]
*/
#define ACDK_TEST_MAIN \
int main(int argc, char* argv[], char** envptr) \
{ \
   return acdk::lang::System::main(::acdk::tools::aunit::TestRunner::testmain, argc, argv, envptr); \
}


} // namespace aunit
} // namespace tools
} // namespace acdk




#endif //acdk_tools_testunit_TestRunner_h

