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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Closure_Test.cpp,v 1.7 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( Closure_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Closure_Test  )

BEGIN_DEFINE_TEST( Closure_Test )
  ADD_TEST( Closure_Test, standard ) 
 
END_DEFINE_TEST( Closure_Test )


void setupLogger();
void parseTreeInterpret(IN(RString) text, IN(RString) initalEl = "CodeText");

void 
Closure_Test::standard()
{
  
  //setupLogger();
  RString text;
  text =
    "interface IFace { public int foo(); }\n"
    "int i = 42;\n"
    "class AClass implements IFace { int i; public AClass(int i_) : i(i_) {} public int foo() { return i; } }\n"
    "IFace acls = new AClass(i);\n"
    "IFace iface = new (i) implements IFace { public int foo() { return i + 1; } };\n"
    "iface.foo();\n"
    ;
  parseTreeInterpret(text);
}

} // namespace aal
} // namespace acdk
} // namespace tests

