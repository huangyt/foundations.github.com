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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestResult.h,v 1.11 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestResult_h
#define acdk_tools_testunit_TestResult_h


#include <acdk.h>
#include "Config.h"

#include "Test.h"
#include "TestFailure.h"
#include "TestListener.h"
#include "TestCollectorListener.h"
#include <acdk/io/StringWriter.h>

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(TestResult);

/**
  represents a result of a test
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestResult
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TestResult)
private:
  RTestListenerArray _listener;
  bool _success;
  bool _stop;
  RTestCollectorListener _testCollector;
public:
  TestResult(IN(acdk::io::RStringWriter) out = Nil, 
             IN(acdk::io::RStringWriter) err = Nil);
  
  RTestResultEntryArray getResults() { return _testCollector->getResults(); }

  virtual bool startTest(IN(RTest) test);
  virtual void endTest(IN(RTest) test);
  
  virtual void addError(IN(RTest) test, IN(RThrowable) e);
  virtual void addFailure(IN(RTest) test, IN(RTestException) e);
  virtual void addSuccess(IN(RTest) test, IN(RTestExpression) expr);
  virtual int testErrors()
  {
    SYNCTHIS();
    RTestResultEntryArray tra = _testCollector->getResults();
    if (tra == Nil)
      return 0;
    int count = 0;
    for (int i = 0; i < tra->length(); ++i)
      count += tra[i]->errors->length();
    return count;
  }
  virtual int testFailures()
  {
    SYNCTHIS();
    RTestResultEntryArray tra = _testCollector->getResults();
    if (tra == Nil)
      return 0;
    int count = 0;
    for (int i = 0; i < tra->length(); ++i)
      count += tra[i]->failures->length();
    return count;
  }
  virtual bool wasSuccessful()
  {
     SYNCTHIS();
     return (testFailures() + testFailures()) == 0;
  }
  virtual bool shouldStop()
  {
     SYNCTHIS();
     return _stop;
  }
  virtual void stop()
  {
     SYNCTHIS();
     _stop = true;
  }
  void addTestListener(INP(RTestListener) listener);
  void removeTestListener(INP(RTestListener) listener);

};
 
} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestResult_h
