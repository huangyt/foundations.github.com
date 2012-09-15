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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestResult.cpp,v 1.6 2005/02/05 10:45:03 kommer Exp $



#include "TestResult.h"



namespace acdk {
namespace tools {
namespace aunit {

TestResult::TestResult(IN(acdk::io::RStringWriter) out, IN(acdk::io::RStringWriter) err)
: _listener(new TestListenerArray(0))
, _success(true)
, _stop(false)
, _testCollector(new TestCollectorListener(out, err))
{
  _listener->append(&_testCollector);
}

void 
TestResult::addError(IN(RTest) test, IN(RThrowable) e)
{
  SYNCTHIS();
  _success = false;
  for (int i = 0; i < _listener->length(); ++ i)
    _listener[i]->addError(test, e);
}

void 
TestResult::addFailure(IN(RTest) test, IN(RTestException) e)
{
  SYNCTHIS();
  _success = false;
  for (int i = 0; i < _listener->length(); ++ i)
    _listener[i]->addFailure(test, e);
}


bool
TestResult::startTest(IN(RTest) test)
{
  SYNCTHIS();
  for (int i = 0; i < _listener->length(); ++ i)
    if (_listener[i]->startTest(test) == false)
      return false;
  return true;
}

void 
TestResult::endTest(IN(RTest) test)
{
  SYNCTHIS();
  for (int i = 0; i < _listener->length(); ++ i)
    _listener[i]->endTest(test);

}
  
//virtual 
void 
TestResult::addSuccess(IN(RTest) test, IN(RTestExpression) expr)
{
  SYNCTHIS();
  for (int i = 0; i < _listener->length(); ++ i)
    _listener[i]->addSuccess(test, expr);
}

void 
TestResult::addTestListener(INP(RTestListener) listener)
{
  _listener->append(listener);
}

void 
TestResult::removeTestListener(INP(RTestListener) listener)
{
  for (int i = 0; i < _listener->length(); ++ i)
  {
    if (_listener[i] == listener)
    {
      _listener->remove(i);
      break;
    }
  }
}
 
} //namespace aunit
} // namespace tools
} // namespace acdk 


