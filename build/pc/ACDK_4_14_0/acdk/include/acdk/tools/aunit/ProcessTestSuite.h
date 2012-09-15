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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/ProcessTestSuite.h,v 1.10 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_ProcessTestSuite_h
#define acdk_tools_testunit_ProcessTestSuite_h


#include <acdk.h>
#include <acdk/io/CharWriter.h>
#include <acdk/lang/System.h>
#include "TestUnit.h"


namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(ProcessTestSuite);
/**
  experimental TestSuite to test an external process
*/
class ACDK_TOOLS_AUNIT_PUBLIC ProcessTestSuite
: extends TestSuite
{
  ACDK_WITH_METAINFO(ProcessTestSuite)
public:
  RString _executable;
  RString _testName;
  bool _testFilled;
  acdk::io::RCharWriter _cout;
  acdk::io::RCharWriter _cerr;
  acdk::util::RProperties _env;
public:
  ProcessTestSuite(INP(RString) executable, IN(RString) name = Nil, IN(acdk::util::RProperties) env = Nil)
  : TestSuite(executable)
  , _executable(executable)
  , _testName(name)
  , _testFilled(false)
  , _cout(acdk::lang::System::out)
  , _cerr(acdk::lang::System::err)
  , _env(env)
  {
  }
  void setOutWriter(IN(acdk::io::RCharWriter) c);
  void setErrWriter(IN(acdk::io::RCharWriter) c);
  RString toString();
  RString getName() { return toString(); }
  void run(IN(RTestResult) result);
  RTestArray tests();
  void runTest();
protected:
  bool _fillTestNames();
};
  
ACDK_DECL_CLASS(ProcessTestCase);
/**
  experimental TestSuite to test an external process
*/
class ACDK_TOOLS_AUNIT_PUBLIC ProcessTestCase
: extends TestCase
{
  ACDK_WITH_METAINFO(ProcessTestCase)
public:
  RString _executable;
  RString _testName;
  acdk::io::RCharWriter _cout;
  acdk::io::RCharWriter _cerr;
  acdk::util::RProperties _env;
public:
  ProcessTestCase(INP(RString) executable, IN(RString) name = Nil, IN(acdk::util::RProperties) env = Nil)
  : TestCase(executable)
  , _executable(executable)
  , _testName(name)
  , _cout(acdk::lang::System::out)
  , _cerr(acdk::lang::System::err)
  , _env(env)
  {
  }
  void setOutWriter(IN(acdk::io::RCharWriter) c) { _cout = c; }
  void setErrWriter(IN(acdk::io::RCharWriter) c) { _cerr = c; }
  RString toString();
  RString getName() { return toString(); }
  void run(IN(RTestResult) result);
  void runTest();
};

} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_ProcessTestSuite_h
