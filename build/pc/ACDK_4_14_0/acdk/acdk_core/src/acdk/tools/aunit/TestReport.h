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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestReport.h,v 1.11 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestReport_h
#define acdk_tools_testunit_TestReport_h


#include <acdk.h>
#include "Config.h"
#include "Test.h"
#include "TestException.h"

namespace acdk {
namespace tools {
namespace aunit {


/**
  Options flags to generate a test report
*/
enum TestReportOptions
{
  /**
    Report over all summary
  */
  ReportAllSummary     = 0x00001,
  /**
    Report Summary for each TestCase
  */
  ReportTestSummary    = 0x00002,
  /**
    Report details of each test
    including failures, etc.
  */
  ReportTestDetails     = 0x00004,
  /**
    Report details including success tests
  */
  ReportTestSuccess     = 0x00008,
  ReportStatistics      = 0x00010,
  ReportStartEndTest    = 0x00020,
};
ACDK_DEF_LIB_ENUM(ACDK_TOOLS_AUNIT_PUBLIC, TestReportOptions);

ACDK_DECL_CLASS(TestResultEntry);

/**
  holds results of one Test
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestResultEntry
: extends acdk::lang::Object
, implements Comparable
{
  ACDK_WITH_METAINFO(TestResultEntry)
public:
  RTest test;
  RThrowableArray errors;
  RTestExceptionArray failures;
  RTestExpressionArray success;
  RString output;
  RString errput;
  TestResultEntry(INP(RTest) t)
  : test(t)
  , errors(new ThrowableArray(0))
  , failures(new TestExceptionArray(0))
  , success(new TestExpressionArray(0))
  {}
  foreign RString toString() { return test == Nil ? RString("<unset test>") : test->getName(); }
  foreign int hashCode() { return (test == Nil) ? 0 : test->getName()->hashCode(); }
  foreign int compareTo(IN(RTestResultEntry) other)  { return toString()->compareTo(other->toString()); }
  foreign int compareTo(IN(RObject) other) { return compareTo(RTestResultEntry(other)); }

};



ACDK_DECL_INTERFACE(TestReport);

/**
  Writes an Report after running the tests
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestReport
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(TestReport)
public:
  virtual void print(INP(RTestResultEntryArray) tests) = 0;
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestReport_h
