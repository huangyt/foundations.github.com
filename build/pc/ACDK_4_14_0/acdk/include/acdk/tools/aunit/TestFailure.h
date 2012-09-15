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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/TestFailure.h,v 1.9 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_tools_testunit_TestFailure_h
#define acdk_tools_testunit_TestFailure_h


#include <acdk.h>
#include "Config.h"

#include "Test.h"

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(TestFailure);

/**
  represents a failure
  @see TestResult
  @ingroup acdkaunit
*/
class ACDK_TOOLS_AUNIT_PUBLIC TestFailure
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TestFailure)
protected:
  RTest _failedTest;
  RThrowable _thrownException;
public:
  TestFailure(IN(RTest) failed, IN(RThrowable) thrown)
  : _failedTest(failed)
  , _thrownException(thrown)
  {
  }
  RTest failedTest() { return _failedTest; }
  RThrowable thrownException() { return _thrownException; }
  RString toString()
  {
     return _failedTest->getName() + ": " + _thrownException->getMessage();
  }
};
} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_TestFailure_h
