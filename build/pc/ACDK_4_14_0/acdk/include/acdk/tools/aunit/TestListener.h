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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestListener.h,v 1.6 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestListener_h
#define acdk_tools_testunit_TestListener_h


#include <acdk.h>
#include "Config.h"

#include "TestException.h"
#include "Test.h"

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_INTERFACE(TestListener);

/**
  Listen to a executing test 
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(TestListener)
public:
  /**
    return true if test should be started
  */
  virtual bool startTest(INP(RTest) test) { return true; }
  virtual void endTest(INP(RTest) test) = 0;
  virtual void addError(INP(RTest) test, INP(RThrowable) ex) = 0;
  virtual void addFailure(INP(RTest) test, INP(RTestException) ex) = 0;
  virtual void addSuccess(INP(RTest) test, INP(RTestExpression) testExpr) = 0;
};



} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestListener_h
