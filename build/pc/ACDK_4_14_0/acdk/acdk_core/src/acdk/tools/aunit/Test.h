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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/aunit/Test.h,v 1.5 2005/02/07 11:14:08 kommer Exp $

#ifndef acdk_tools_testunit_Test_h
#define acdk_tools_testunit_Test_h


#include <acdk.h>
#include "Config.h"

namespace acdk {
namespace tools {
namespace aunit {

ACDK_DECL_CLASS(TestResult);

ACDK_DECL_INTERFACE(Test);
/**
  Base interface for all tests
*/
class ACDK_TOOLS_AUNIT_PUBLIC Test
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Test)
public:
  virtual void run(IN(RTestResult) result) = 0;
  virtual void runTest() = 0;
  virtual int testCount() = 0;
  virtual RString getName() = 0;
};

} //namespace aunit
} // namespace tools
} // namespace acdk 

#endif //acdk_tools_testunit_Test_h
