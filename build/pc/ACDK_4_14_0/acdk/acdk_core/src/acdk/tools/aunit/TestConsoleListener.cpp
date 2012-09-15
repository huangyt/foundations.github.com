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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestConsoleListener.cpp,v 1.6 2005/02/05 10:45:03 kommer Exp $


#include "TestConsoleListener.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace tools {
namespace aunit {

TestConsoleListener::TestConsoleListener(INP(acdk::io::RPrintWriter) out, int options)
: _options(options)
, _out(out)
{
  if (_out == Nil)
    _out = acdk::lang::System::out;
}

bool 
TestConsoleListener::startTest(INP(RTest) test)
{
  if ((_options & ReportTestDetails) == 0)
    return true;
  _out->println(SBSTR("[AUNIT:STRT][" << test->getName() << "] Start Test"));
  return true;
}

void 
TestConsoleListener::endTest(INP(RTest) test)
{
  if ((_options & ReportTestDetails) == 0)
    return;
  _out->println(SBSTR("[AUNIT:ENDT][" << test->getName() << "] End Test"));
}

void 
TestConsoleListener::addError(INP(RTest) test, INP(RThrowable) ex)
{
  _out->println(SBSTR("[AUNIT:ERRR][" << test->getName()  << "] Error in Test: " << test->getName() << ": " << ex->getMessage()));
}

void 
TestConsoleListener::addFailure(INP(RTest) test, INP(RTestException) ex)
{
  _out->println(SBSTR("[AUNIT:FAIL][" << test->getName() << "] Failure in Test: " << ex->getMessage()));
}


void 
TestConsoleListener::addSuccess(INP(RTest) test, INP(RTestExpression) testExpr)
{
  if ((_options & ReportTestSuccess) == 0)
    return;

  _out->println(SBSTR("[AUNIT:SUCC][" << test->getName() << "] " << testExpr->expression));
}


} //namespace aunit
} // namespace tools
} // namespace acdk 

