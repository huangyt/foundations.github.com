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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestCollectorListener.h,v 1.8 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestCollectorListener_h
#define acdk_tools_testunit_TestCollectorListener_h


#include <acdk.h>
#include <acdk/io/StringWriter.h>
#include "Config.h"

#include "TestException.h"
#include "TestReport.h"
#include "TestListener.h"

namespace acdk {
namespace tools {
namespace aunit {






ACDK_DECL_CLASS(TestCollectorListener);

/**
  Used internally to collect the results of a test
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestCollectorListener
: extends acdk::lang::Object
, implements TestListener
{
  ACDK_WITH_METAINFO(TestCollectorListener)
protected:
  RTestResultEntryArray _results;
  acdk::io::RStringWriter _out;
  acdk::io::RStringWriter _err;
public:
  TestCollectorListener(IN(acdk::io::RStringWriter) out, IN(acdk::io::RStringWriter) err)
  : _results(new TestResultEntryArray(0))
  , _out(out)
  , _err(err)
  {
    if (_out == Nil)
      _out = new acdk::io::StringWriter();
     if (_err == Nil)
      _err = new acdk::io::StringWriter();
  }
  virtual bool startTest(INP(RTest) test) 
  {
    _results->append(new TestResultEntry(test));
    return true;
  }
  virtual void endTest(INP(RTest) test) 
  {
    _results[_results->length() - 1]->output = _out->getString();
    _out->setString("");
    _results[_results->length() - 1]->errput = _err->getString();
    _err->setString("");
  }
  virtual void addError(INP(RTest) test, INP(RThrowable) ex) 
  {
    _results[_results->length() - 1]->errors->append(ex);
  }
  virtual void addFailure(INP(RTest) test, INP(RTestException) ex) 
  {
    _results[_results->length() - 1]->failures->append(ex);
  }
  virtual void addSuccess(INP(RTest) test, INP(RTestExpression) testExpr) 
  {
    _results[_results->length() - 1]->success->append(testExpr);
  }
  RTestResultEntryArray getResults() { return _results; }
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestCollectorListener_h
