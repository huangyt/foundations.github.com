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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestDebuggerListener.cpp,v 1.9 2005/03/08 14:56:33 kommer Exp $


#include "TestDebuggerListener.h"
#include <acdk/lang/System.h>
#include <acdk/io/OutputDebugStringWriter.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace tools {
namespace aunit {

TestDebuggerListener::TestDebuggerListener(int options)
: _options(options)
{
#if defined(_MSC_VER)
  _out = new acdk::io::PrintWriter((acdk::io::RWriter)new acdk::io::OutputDebugStringWriter());
#else
    _out = acdk::lang::System::out;
#endif
}

bool 
TestDebuggerListener::startTest(INP(RTest) test)
{
  if ((_options & ReportTestDetails) == 0)
    return true;
  _out->println("[AUNIT:STRT] Start Test: " + test->getName());
  return true;
}

void 
TestDebuggerListener::endTest(INP(RTest) test)
{
  if ((_options & ReportTestDetails) == 0)
    return;
  _out->println("[AUNIT:ENDT] End Test: " + test->getName());
}

void 
TestDebuggerListener::addError(INP(RTest) test, INP(RThrowable) ex)
{
  StringBuffer sb;
  _out->println(SBSTR("[AUNIT:ERRR] Error in Test: " << test->getName() << ": " << ex->getMessage()));
  _out->println("Called in:");
  RStackFrameArray array = ex->getStackFrames();
  for (int i  = 0; i < array->length(); ++i)
  {
    _out->println(acdk::util::logging::Logger::formatCompilerSourceMsg(array[i]->getFunctionSignature(), array[i]->getFileName(), array[i]->getFileLineNo()));
  }
}

void 
TestDebuggerListener::addFailure(INP(RTest) test, INP(RTestException) ex)
{
  _out->println(SBSTR("[AUNIT:FAIL] Failure in Test: " << test->getName() << "in:\n" << ex->getMessage()));
}


void 
TestDebuggerListener::addSuccess(INP(RTest) test, INP(RTestExpression) testExpr)
{
  if ((_options & ReportTestSuccess) == 0)
    return;

  _out->println(SBSTR("[AUNIT:SUCC]: " << test->getName() << ":\n" << testExpr->toString()));
}


} //namespace aunit
} // namespace tools
} // namespace acdk 

