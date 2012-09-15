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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_AritmExpr_Test.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( AritmExpr_Test )
  DECLARE_TEST( incDec )
  DECLARE_TEST( multiply )
END_DECLARE_TEST( AritmExpr_Test  )

BEGIN_DEFINE_TEST( AritmExpr_Test )
  ADD_TEST( AritmExpr_Test, incDec ) 
  ADD_TEST( AritmExpr_Test, multiply ) 
END_DEFINE_TEST( AritmExpr_Test )


void setupLogger();
void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");

void 
AritmExpr_Test::incDec()
{
  //setupLogger();
  AalInterpreter aint;
  RString text;
  text =
    "int i = 0;\n"
    "int j = ++i;\n"
    "--i;\n"
    "__assert(j == 1 && i == 0, \"Prefix ++ not working\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "int i = 0;\n"
    "int j = i++;\n"
    "--i;\n"
    "__assert(j == 0 && i == 0, \"Postfix ++ not working\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
AritmExpr_Test::multiply()
{
  AalInterpreter aint;
  RString text;
  text =
    "__assert((2 * 3) == 6 , \"multiply not working\");\n"
    "__assert((6 / 2) == 3 , \"divide not working\");\n"
    "__assert((7 % 2) == 1, \"modulo not working\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

}

} // namespace aal
} // namespace acdk
} // namespace tests

