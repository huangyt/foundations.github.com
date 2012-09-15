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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestUnit.cpp,v 1.15 2005/03/29 19:06:41 kommer Exp $


#include "TestUnit.h"
#include <acdk/lang/System.h>

namespace acdk {
namespace tools {
namespace aunit {

//static 
bool 
TestCase::NoSuccessTestLogging = false;
bool
TestCase::TestInBatchMode = true;

void 
TestCase::assertImplementation(  bool condition, const char* str
                 ,  int linenum
                 ,  const char* fileName)
{
  if (condition == false) 
  {
    THROW3(TestException, str, fileName, linenum);
  }
  else if (TestCase::NoSuccessTestLogging == false)
    _activeResult->addSuccess(this, new TestExpression(str, fileName, linenum));
}

void 
TestCase::assertImplementation(  bool condition, IN(RString) str
                 ,  int linenum
                 ,  IN(RString) fileName)
{
  if (condition == false) 
  {
    THROW3(TestException, str, fileName, linenum);
  }
  else if (TestCase::NoSuccessTestLogging == false)
    _activeResult->addSuccess(this, new TestExpression(str, fileName, linenum));
}


//static 
TestCase* TestCase::activeTest = 0;

//virtual 
void 
TestCase::run(IN(RTestResult) result)
{
  _activeResult = result;
  
  TestCase::activeTest = this;
  result->startTest(this);

  setUp();
  try {
    runTest();
  } 
  catch (RTestException ex) 
  {
    result->addFailure(this, ex);

  } 
  catch (RThrowable ex) 
  {
    result->addError(this, ex);
  } 
  catch (const std::exception& e) 
  {
	  result->addError(this, new TestException(e.what()));
  } 
  /* must not handle otherwise NullPointer doesn't work
  catch (...) 
  {
    result->addError(this, new TestException("NullPointer or unknown exception"));
  }*/
  tearDown();
  result->endTest(this);
  _activeResult = Nil;
  TestCase::activeTest = 0;
}

//virtual 
void TestSuite::run(IN(RTestResult) result)
{
  for (int i = 0; i < _tests.length(); i++) 
  {
    if (result->shouldStop() == true)
      break;
    _tests[i]->run(result);
  }
}

} // namespace aunit
} // namespace tools
} // namespace acdk




