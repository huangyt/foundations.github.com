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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_RelExpr_Test.cpp,v 1.7 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( RelExpr_Test )
  DECLARE_TEST( basicRels )
  DECLARE_TEST( operatorRels )
  DECLARE_TEST( equalsExpr )
END_DECLARE_TEST( RelExpr_Test  )

BEGIN_DEFINE_TEST( RelExpr_Test )
  ADD_TEST( RelExpr_Test, basicRels ) 
  ADD_TEST( RelExpr_Test, operatorRels ) 
  ADD_TEST( RelExpr_Test, equalsExpr ) 
 
END_DEFINE_TEST( RelExpr_Test )


void setupLogger();


void 
RelExpr_Test::basicRels()
{
  //setupLogger();
  RString text;
  AalInterpreter aint;
  text =
    "__assert(41 < 42, \"compare int failed\");\n"
    "__assert(42 > 41, \"compare int failed\");\n"
    "__assert(42 >= 41, \"compare int failed\");\n"
    "__assert(42 >= 42, \"compare int failed\");\n"
    "__assert(41 <= 42, \"compare int failed\");\n"
    "__assert(42 <= 42, \"compare int failed\");\n"
    "__assert(42 == 42, \"compare int failed\");\n"
    "__assert(42 != 41, \"compare int failed\");\n"
    ;
  //aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "__assert(42 == 42, \"compare int failed\");\n"
    "__assert((42 == 41) == false, \"compare int failed\");\n"
    ;
  //aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "Integer integer1 = new Integer(42);\n"
    "Integer integer2 = integer1;\n"
    "__assert(integer1 == integer2, \"compare object instances failed\");\n"
    "integer2 = new Integer(42);\n"
    "__assert(integer1 != integer2, \"compare object instances failed\");\n"
    "__assert(integer1.equals(integer2) == true, \"compare object via equals failed\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
RelExpr_Test::operatorRels()
{
  //setupLogger();
  AalInterpreter aint;
  RString text;
  text =
    "class AClass { public AClass() {}\n"
    "  public bool operator<(int other) { return 42 < other; }\n"
    "  public bool operator<=(int other) { return 42 <= other; }\n"
    "  public bool operator>(int other) { return 42 > other; }\n"
    "  public bool operator>=(int other) { return 42 >= other; }\n"
    "  public bool operator==(int other) { return 42 == other; }\n"
    "  public bool operator!=(int other) { return 42 != other; }\n"
    "}\n"
    "AClass a = new AClass();\n"

    "__assert(a < 43, \"compare int failed\");\n"
    "__assert(a > 41, \"compare int failed\");\n"
    "__assert(a >= 41, \"compare int failed\");\n"
    "__assert(a >= 42, \"compare int failed\");\n"
    "__assert(a <= 43, \"compare int failed\");\n"
    "__assert(a <= 42, \"compare int failed\");\n"
    //"__assert(a == 42, \"compare int failed\");\n"
    //"__assert(42 != 41, \"compare int failed\");\n"*/
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
RelExpr_Test::equalsExpr()
{
  AalInterpreter aint;
  //setupLogger();
  RString text;
  text =
    "__assert(1 !== 2, \"equals operator fails\");\n"
    "acdk.lang.String s = \"a\";\n"
    "s.equals(\"a\");\n"
    "__assert(s === \"a\", \"equals operator fails\");\n"
    "__assert(s !== \"b\", \"equals operator fails\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

} // namespace aal
} // namespace acdk
} // namespace tests

