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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_DefunStatements_Test.cpp,v 1.5 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( DefunStatements_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( DefunStatements_Test  )

BEGIN_DEFINE_TEST( DefunStatements_Test )
  ADD_TEST( DefunStatements_Test, standard ) 
  
 
END_DEFINE_TEST( DefunStatements_Test )


void setupLogger();

void 
DefunStatements_Test::standard()
{
  RString text;
   
  AalInterpreter aint;
  //setupLogger();
  text = 
    "defun Incrementator static int (int value);\n"
    "class AClass { public int adder(int value) { return value + 1; } }\n"
    "AClass acls = new AClass();\n"
    "Incrementator inc = acls.adder;\n"
    "inc(41);\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}


} // namespace aal
} // namespace acdk
} // namespace tests

