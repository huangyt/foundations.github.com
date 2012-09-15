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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestDebuggerListener.h,v 1.6 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestDebuggerListener_h
#define acdk_tools_testunit_TestDebuggerListener_h



#include "TestListener.h"
#include "TestReport.h"

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(TestDebuggerListener);

/**
  Listen to test and prints to console
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestDebuggerListener
: extends acdk::lang::Object
, implements TestListener
{
  ACDK_WITH_METAINFO(TestDebuggerListener)
private:
  int _options;
  acdk::io::RPrintWriter _out;
public:
  /**
    @param combination of TestReportOptions 
  */
  TestDebuggerListener(int options = ReportAllSummary | ReportTestDetails | ReportTestSuccess);
  virtual bool startTest(INP(RTest) test);
  virtual void endTest(INP(RTest) test);
  virtual void addError(INP(RTest) test, INP(RThrowable) ex);
  virtual void addFailure(INP(RTest) test, INP(RTestException) ex);
  virtual void addSuccess(INP(RTest) test, INP(RTestExpression) testExpr);
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestDebuggerListener_h
