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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_AalObject_Test.cpp,v 1.5 2005/02/05 10:44:51 kommer Exp $


#include <acdk.h>
#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/aal/AalCompiler.h>
#include <acdk/io/MemReader.h>
#include <acdk/lang/System.h>
#include <acdk/aci/OpCode.h>

namespace tests {
namespace acdk {
namespace aal {


using namespace ::acdk::aci;
using namespace ::acdk::aal;



// Declare test cases
BEGIN_DECLARE_TEST( AalObject_Test )
  DECLARE_TEST( staticMember )
  DECLARE_TEST( dynMember )
END_DECLARE_TEST( AalObject_Test  )

BEGIN_DEFINE_TEST( AalObject_Test )
  ADD_TEST( AalObject_Test, staticMember ) 
  ADD_TEST( AalObject_Test, dynMember ) 

END_DEFINE_TEST( AalObject_Test )


void
AalObject_Test::staticMember()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "class BaseClass { public static int ivar = 42; }\n"
    "__assert(BaseClass.ivar == 42, \"initialization of static members failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "using acdk.lang;\n"
    "class BaseClass { public static int ivar = 42; static int getIvar() { return BaseClass.ivar; return ivar; } }\n"
    "__assert(BaseClass.getIvar() == 42, \"initialization of static members failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "class BaseClass { public static int ivar = 0; static BaseClass() { ivar = 42; } }\n"
    "__assert(BaseClass.ivar == 42, \"initialization of static members failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

}
void
AalObject_Test::dynMember()
{
  AalInterpreter aint;

  RString text;
  /*
  text =
    "using acdk.lang;\n"
    "class BaseClass { public int ivar; BaseClass() : ivar(42) {} int getivar() { return ivar; } }\n"
    "__assert((new BaseClass()).getivar() == 42, \"initialization of dynamic members failed\");\n"
    "__assert((new BaseClass()).ivar == 42, \"initialization of dynamic members failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  */
  text =
    "using acdk.lang;\n"
    "class BaseClass { public BaseClass() {} String getString() { return \"A\"; } }\n"
    "__assert((new BaseClass()).getString().length() == 1, \"initialization of dynamic members failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

} // namespace aal
} // namespace acdk
} // namespace tests

