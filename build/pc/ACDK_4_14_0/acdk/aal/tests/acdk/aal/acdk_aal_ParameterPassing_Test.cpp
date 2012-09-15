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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_ParameterPassing_Test.cpp,v 1.5 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( ParameterPassing_Test )
  DECLARE_TEST( polymorph )
  DECLARE_TEST( namedParameter )
  DECLARE_TEST( restParameter )
END_DECLARE_TEST( ParameterPassing_Test  )

BEGIN_DEFINE_TEST( ParameterPassing_Test )
  ADD_TEST( ParameterPassing_Test, polymorph ) 
  ADD_TEST( ParameterPassing_Test, namedParameter ) 
  ADD_TEST( ParameterPassing_Test, restParameter ) 
  
END_DEFINE_TEST( ParameterPassing_Test )


void setupLogger();
void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");

void 
ParameterPassing_Test::polymorph()
{
  //setupLogger();
  RString text;
  text =
    "using acdk.lang;\n"
    "Integer integer = new Integer(\"42\");\n"
    "String s = Integer.toString(42, 10);\n"
    "integer.toString();\n"
    "Integer.toString(42);\n"
    ;
  //parseTreeInterpret(text);

  text =
    "using acdk.tools.aunit;\n"
    "using acdk.lang;\n"
    "DmiTestClass obj = new DmiTestClass();\n"
    "String s;\n"
    "s = obj.polymorphFunc(new Integer(42));\n"
    "Number number = new Integer(41);\n"
    "s = obj.polymorphFunc(number);\n"
    "number = new Short(2);\n"
    "s = obj.polymorphFunc(number);\n"
    "s = obj.polymorphFunc(\"asdf\");\n"
    ;
  parseTreeInterpret(text);
  
}

void
ParameterPassing_Test::namedParameter()
{
  RString text;
  text =
    "using acdk.lang;\n"
    "String s = Integer.toString(radix: 10, value: 42);\n"
    ;
  parseTreeInterpret(text);
}

void
ParameterPassing_Test::restParameter()
{
  RString text;
  text =
    "class AClass { public AClass() {} public void foo(int i, acdk.lang.dmi.DmiObjectArray rest) { } }\n"
    "AClass a = new AClass();\n"
    "a.foo(2, 40, 41, 42);\n"
    ;
  parseTreeInterpret(text);
}

} // namespace aal
} // namespace acdk
} // namespace tests

