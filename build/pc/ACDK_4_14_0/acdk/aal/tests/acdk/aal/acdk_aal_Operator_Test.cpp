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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Operator_Test.cpp,v 1.7 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( Operator_Test )
  DECLARE_TEST( plusOperator )
  DECLARE_TEST( callOperator )
  DECLARE_TEST( arrayOperator )
END_DECLARE_TEST( Operator_Test  )

BEGIN_DEFINE_TEST( Operator_Test )
  ADD_TEST( Operator_Test, plusOperator ) 
  ADD_TEST( Operator_Test, callOperator ) 
  ADD_TEST( Operator_Test, arrayOperator ) 
 
END_DEFINE_TEST( Operator_Test )


void setupLogger(); 
void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");


void 
Operator_Test::plusOperator()
{

  AalInterpreter aint;
  RString text;
  text =
    "using acdk.lang;\n"
    "String s = \"A\";\n"
    "String e = s + \"B\";\n"
    "__assert(e.equals(\"AB\"), \"Plus operator on strings failed\");\n"
    "__assert(\"A\" + \"B\" === \"AB\", \"Plus operator on strings failed\");\n"
      ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
      "class AClass { public AClass() {} public int foo(int i) { return i; } public int operator+(int i) { return i + 2; } }\n"
    "AClass acls = new AClass();\n"
    //"acls.foo(40);\n"
    "__assert(acls + 40 == 42, \"Plus operator failed\");\n"
    //"acls.foo(acls + 40);\n"
    //"acls + acls.foo(40);\n"
    //"acls.foo(acls.foo(40));\n"
      ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  
}

void 
Operator_Test::callOperator()
{
  //setupLogger();
  RString text;
  text =
    "class AClass { public AClass() {} public int foo(int i) { return i + 2; } public int operator()(int i) { return i + 2; } }\n"
    "AClass acls = new AClass();\n"
    "acls.foo(42) == 44;\n"
    "acls.operator()(42) == 44;\n"
    "acls(42) == 44;\n"
    //"__assert(acls.operator()(42) == 44, \"explicit obj.operator()(..) failed\");\n"
    ;
  //parseTreeInterpret(text);
  
  text =
    "using acdk.lang;\n"
    "class AClass { public AClass() {} public String foo() { return \"Hello\"; } public String operator()() { return \"Hello\"; } }\n"
    "AClass acls = new AClass();\n"
    "acls.foo().length() == 5;\n"
    "acls().length() == 5;\n"
    ;
  //parseTreeInterpret(text);
  text =
    "class AClass { public AClass() {} public static int operator()(int i) { return i + 2; } }\n"
    "AClass.operator()(42);\n"
    "AClass(42);\n"
    //"AClass acls = new AClass();\n"
    //"acls.operator()(42);\n"
    //"acls(42);\n"
    //"__assert(acls(42) == 44, \"implicite operator() on object failed\");\n"
    ;
  parseTreeInterpret(text);
}

void
Operator_Test::arrayOperator()
{
  //setupLogger();
  RString text;
  text =
    "using acdk.lang;\n"
    "String[] sa = new String[](2);\n"
    "sa[1] = \"Hello\";\n"
    "System.out.println(sa[1]);\n"
    "__assert(sa.length() == 2, \"standard arrays\");\n"
    ;
  //parseTreeInterpret(text);

  text =
    "using acdk.lang;\n"
    "String[] sa = new String[](2);\n"
    "sa[1] = \"Hello\";\n"
    "System.out.println(sa[1]);\n"
    "sa[1].length() == 5;\n"
    //"__assert(sa.length() == 2, \"standard arrays\");\n"
    ;
  parseTreeInterpret(text);
}

} // namespace aal
} // namespace acdk
} // namespace tests

