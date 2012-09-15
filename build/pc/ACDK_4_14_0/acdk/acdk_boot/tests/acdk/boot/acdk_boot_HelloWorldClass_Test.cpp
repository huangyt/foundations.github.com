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
// $Header: /cvsroot/acdk/acdk/acdk_boot/tests/acdk/boot/acdk_boot_HelloWorldClass_Test.cpp,v 1.6 2005/02/05 10:44:52 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/boot/HelloWorldClass.h>


namespace tests {
namespace acdk {
namespace util {

// Declare test cases
BEGIN_DECLARE_TEST( HelloWorldClass_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( viaInterface )
END_DECLARE_TEST( HelloWorldClass_Test  )


// define tests.
// tests will executed in the same order
// like defined with ADD_TEST
BEGIN_DEFINE_TEST( HelloWorldClass_Test )
  ADD_TEST( HelloWorldClass_Test, standard ) 
  ADD_TEST( HelloWorldClass_Test, viaInterface ) 
  
END_DEFINE_TEST( HelloWorldClass_Test )



void HelloWorldClass_Test::standard()
{
  ::acdk::boot::RHelloWorldClass hwc = 
      new ::acdk::boot::HelloWorldClass("Hello");

  // check test with testAssert
  testAssert(hwc->getGreetings()->equals("Hello") == true);
}


void HelloWorldClass_Test::viaInterface()
{
  ::acdk::boot::RHelloWorldInterface hwi = 
      new ::acdk::boot::HelloWorldClass("Hello");

  // check test with testAssert
  testAssert(hwi->getGreetings()->equals("Hello") == true);
}


} // namespace util
} // namespace acdk
} // namespace tests

