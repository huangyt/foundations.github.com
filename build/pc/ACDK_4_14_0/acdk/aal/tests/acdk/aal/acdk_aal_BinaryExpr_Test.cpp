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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_BinaryExpr_Test.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( BinaryExpr_Test )
  DECLARE_TEST( basicRels )
  
END_DECLARE_TEST( BinaryExpr_Test  )

BEGIN_DEFINE_TEST( BinaryExpr_Test )
  ADD_TEST( BinaryExpr_Test, basicRels ) 
  
 
END_DEFINE_TEST( BinaryExpr_Test )


void setupLogger();

void 
BinaryExpr_Test::basicRels()
{
  byte erg = byte(1) << 2;
  int i = 42;
  int j = ~i;
  i = 4 >> 2;
  byte b1 = 4;
  byte b2 = 2;
  byte e = b1 >> b2;
  i = -4;
  unsigned int ui = i;
  int ue = ui >> 2;
  //setupLogger();
  AalInterpreter aint;
  RString text;
  text =
    "__assert(1 & 3, \"binary and does not work\");\n"
    "__assert((1 | 2) == 3, \"binary or does not work\");\n"
    "__assert((3 ^ 2) == 1, \"binary xor does not work\");\n"
    "__assert((1 << 2) == 4, \"binary left shift does not work\");\n"
    "__assert((4 >> 2) == 1, \"binary right shift does not work\");\n"
    "__assert((-4 >> 2) == -1, \"binary right shift on neg values does not work\");\n"
    "int i = -4;\n"
    "__assert((i >>> 2) == 1073741823, \"binary right unsigned shift does not work\");\n"
    "i = 42;\n"
    "__assert(~i == -43, \"binary not does not work\");\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  
}

} // namespace aal
} // namespace acdk
} // namespace tests

