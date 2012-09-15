// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany->
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE->	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details->

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www->acdk->de
// - http://www->artefaktur->com
// - http://acdk->sourceforge->net
// for more information->
// 
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/tools/aunit/guitestrunner/GuiTestListener.h,v 1.4 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_tools_aunit_guitestrunner_GuiTestListener_h
#define acdk_tools_aunit_guitestrunner_GuiTestListener_h

#include <acdk.h>
#include <acdk/tools/aunit/ProcessTestSuite.h>

namespace acdk {
namespace tools {
namespace aunit {
namespace guitestrunner {

using namespace acdk::tools::aunit;

ACDK_DECL_CLASS(GuiTestListener);

class GuiTestListener
: extends acdk::lang::Object
, implements acdk::tools::aunit::TestListener
{
public:
  RString _lastTest;
  RString _lastState;
  RString _lastErrMsg;
  //acdk::wx::REvtHandler _pingTo;
  int runnedTest;
  bool shouldStop;
  GuiTestListener(/*IN(acdk::wx::REvtHandler) pingTo*/)
  {
    runnedTest = 0;
    shouldStop = false;
    _lastState = "";
    _lastTest = "";
    _lastErrMsg = "";
    //_pingTo = pingTo;
  }
  bool startTest(IN(RTest) test)
  {
    {
      SYNCHRONIZETHIS();
      if (shouldStop == true)
        return false;
    }
    _lastState = "start";
    _lastTest = test->getName();
    //RNotifyEvent notify = new NotifyEvent(EvtNull, Event_TestListenerPing);
    //_pingTo->addPendingEvent(notify);
    //System::out->println("start test: " + _lastTest);
    return true;
  }
  void endTest(IN(RTest) test)
  {
    runnedTest = runnedTest + 1;
    _lastState = "end";
    _lastTest = test->getName();
    //System::out->println("end test: " + _lastTest);
  }
  void addError(IN(RTest) test, IN(RThrowable) ex)
  {
    _lastState = "error";
    _lastTest = test->getName();
    _lastErrMsg = ex->getMessage();
  }
  void addFailure(IN(RTest) test, IN(RTestException) ex)
  {
    _lastState = "fail";
    _lastTest = test->getName();
    _lastErrMsg = ex->getMessage();
  }
  void addSuccess(IN(RTest) test, IN(RTestExpression) testExpr) 
  {
    _lastState = "success";
    _lastTest = test->getName();
    //_lastErrMsg = ex.getMessage();
  }
  RString getLastMessage()
  {
    SYNCHRONIZETHIS();
    RString ret = _lastState + ": " + _lastTest;
    if (_lastErrMsg == Nil)
      return ret;
    if (_lastErrMsg->equals("") == false)
      ret = ret + ": " + _lastErrMsg;
    return ret;
  }
  void doStop()
  {
    SYNCHRONIZETHIS();
    shouldStop = true;
  }
};

ACDK_DECL_CLASS(TestRunThread);

class TestRunThread
: extends acdk::lang::Thread
{
public:
  RTest test;
  RTestResult _result;
  TestRunThread(IN(RTest) t, IN(RTestResult) result)
  {
    test = t; 
    _result = result; 
  }
  
  void run()
  {
    //result = new TestResult();
    test->run(_result); 
  }
};


} // namespace guitestrunner 
} // namespace aunit
} // namespace tools 
} // namespace acdk 


#endif //acdk_tools_aunit_guitestrunner_GuiTestListener_h
