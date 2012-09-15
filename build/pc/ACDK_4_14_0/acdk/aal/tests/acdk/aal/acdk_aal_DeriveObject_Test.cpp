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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_DeriveObject_Test.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( DeriveObject_Test )
  DECLARE_TEST( deriveAalObject )
  DECLARE_TEST( deriveAcdkObject )
  
END_DECLARE_TEST( DeriveObject_Test  )

BEGIN_DEFINE_TEST( DeriveObject_Test )
  ADD_TEST( DeriveObject_Test, deriveAalObject ) 
  ADD_TEST( DeriveObject_Test, deriveAcdkObject ) 
END_DEFINE_TEST( DeriveObject_Test )


void
DeriveObject_Test::deriveAalObject()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "class BaseClass { public int _ivar; public BaseClass(int i) : _ivar(i) {} }\n"

    "class ExtClass extends BaseClass { public ExtClass(int i) : BaseClass(i) {} } \n"
    "ExtClass extcls = new ExtClass(42);\n"
    "__assert(extcls._ivar == 42, \"Base Aal class Contructor initialization failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

   text =
    "using acdk.lang;\n"
    "class BaseClass { public int foo() { return 42; } }\n"

    "class ExtClass extends BaseClass { public int foo() { return ((BaseClass)this).foo(); } } \n"
    "ExtClass extcls = new ExtClass();\n"
    "__assert(extcls.foo() == 42, \"Base Aal super method invocation failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "class BaseClass { public int foo() { return 42; } }\n"

    "class ExtClass extends BaseClass {\n"
    "  public int bar() { return ((BaseClass)this).foo(); }\n"
    "  public int foo() { return BaseClass.foo(); }\n"
    // does not work: "  public int baz() { return super.foo(); }\n"
    "} \n"
    "ExtClass extcls = new ExtClass();\n"
    "__assert(extcls.foo() == 42, \"Base Aal super method invocation failed\");\n"
    //"__assert(extcls.baz() == 42, \"Base Aal super method invocation failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "class ExtInteger extends Integer {\n"
    "  public ExtInteger(int i) : acdk.lang.Integer(i) {}\n"
    "  public ExtInteger() : Integer(42) {}\n"
    "  String toString() { return Integer.toString(); } }\n"

    "ExtInteger extcls1 = new ExtInteger(42);\n"
    "__assert(extcls1.toString().equals(\"42\") == true, \"Base Aal super method invocation failed\");\n"
    "ExtInteger extcls2 = new ExtInteger();\n"
    "__assert(extcls2.toString().equals(\"42\") == true, \"Base Aal super method invocation failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void 
DeriveObject_Test::deriveAcdkObject()
{
  //setupLogger();
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "class ExtInteger extends acdk.lang.Integer { public ExtInteger(int i) : acdk.lang.Integer(i) {} } \n"
    "ExtInteger eint = new ExtInteger(42);\n"
    "String s = eint.toString();\n"
    "__assert(s.equals(\"42\") == true, \"calling super AcdkObject method failed\");\n"
    "s = ExtInteger.toString(42);\n"
    "__assert(s.equals(\"42\") == true, \"calling super AcdkObject static method failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "using acdk.lang;\n"
    "class ExtInteger extends Integer { public ExtInteger(int i) : Integer(i) {} } \n"
    "ExtInteger eint = new ExtInteger(42);\n"
    ;
}



} // namespace aal
} // namespace acdk
} // namespace tests

