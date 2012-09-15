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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_WeakInvoke_Test.cpp,v 1.8 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( WeakInvoke_Test )
  DECLARE_TEST( callAalObject )
  DECLARE_TEST( callComObject )
  
END_DECLARE_TEST( WeakInvoke_Test  )

BEGIN_DEFINE_TEST( WeakInvoke_Test )
  ADD_TEST( WeakInvoke_Test, callAalObject ) 
  ADD_TEST( WeakInvoke_Test, callComObject ) 
END_DEFINE_TEST( WeakInvoke_Test )


void
WeakInvoke_Test::callAalObject()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "class AClass {\n"
    "  public int foo(Object obj) { return 1; }\n"
    "  public int foo(String obj) { return 2; }\n"
    "}"
    "AClass cls = new AClass();\n"
    "String s = new String("");\n"
    "Object o = s;\n"
    "__assert(cls.foo(o) == 1, \"Static overload binding does not work\");\n"
    "__assert(cls.foo(s) == 2, \"Static overload binding does not work\");\n"
    "__assert(cls->foo(o) == 2, \"dynamic overload binding does not work\");\n"
    "__assert(cls->foo(s) == 2, \"dynamic overload binding does not work\");\n"
    ;
  //aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "class AClass {\n"
    "  public int foo(String obj) { return 2; }\n"
    "}"
    "AClass cls = new AClass();\n"
    "try {\n"
    "  cls->foo(42);\n"
    "} catch (ParamsMismatchException ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "}\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  
}

void 
WeakInvoke_Test::callComObject()
{
  //setupLogger();
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "using acdkx.com;\n"
    "ComObject o = new ComObject(\"Word.Application\");\n"
    "o.Visible = true;\n"
    "int exitcode = 0;\n"
    "acdk.lang.Thread.sleep(3);\n"
    "o.Quit(exitcode);\n"

    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  
}



} // namespace aal
} // namespace acdk
} // namespace tests

