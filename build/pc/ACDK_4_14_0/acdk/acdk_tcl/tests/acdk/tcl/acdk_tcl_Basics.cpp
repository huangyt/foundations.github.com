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
// $Header: /cvsroot/acdk/acdk/acdk_tcl/tests/acdk/tcl/acdk_tcl_Basics.cpp,v 1.9 2005/02/05 10:45:32 kommer Exp $

#include <acdk/io/RessourceFileSystem.h>
#include <acdk/tcl/TclInterpreter.h>
#include <acdk/lang/System.h>
#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace tcl {

BEGIN_DECLARE_TEST( Basics_Test )
  DECLARE_TEST( helloTcl )
  DECLARE_TEST( helloACDK )
  DECLARE_TEST( intAndFloats )
END_DECLARE_TEST( Basics_Test  )

BEGIN_DEFINE_TEST( Basics_Test )
  ADD_TEST( Basics_Test, helloTcl ) 
  ADD_TEST( Basics_Test, helloACDK ) 
  ADD_TEST( Basics_Test, intAndFloats ) 
END_DEFINE_TEST( Basics_Test )


using namespace ::acdk::tcl;

void
Basics_Test::helloTcl()
{
  const char* ressource_array1 = 
"puts \"Hello Tcl\"\n"
;
  RString code = new String(ressource_array1);

  System::out->println("Interpreting Code: [");
  System::out->println(code + "]");
  RTclInterpreter ti = new TclInterpreter();
  ti->eval(code);
}

void
Basics_Test::helloACDK()
{
const char* ressource_array2 = 
"set sb [acdk_new \"acdk/lang/StringBuffer\" \"Hallo \"]\n"
"acdk_invoke $sb append \"Tcl from ACDK\"\n"
"puts [acdk_invoke $sb toString]\n"
;
  RString code = new String(ressource_array2);
  
  System::out->println("Interpreting Code: [");
  System::out->println(code + "]");
  RTclInterpreter ti = new TclInterpreter();
  ti->eval(code);
}
void 
Basics_Test::intAndFloats()
{
  const char* ressource_array = 
"set integer [acdk_invoke_static \"acdk/lang/Integer\" toHexString 1234]\n"
"puts $integer\n"
;
  RString code = new String(ressource_array);
  
  System::out->println("Interpreting Code: [");
  System::out->println(code + "]");
  RTclInterpreter ti = new TclInterpreter();
  ti->eval(code);
}


} // namespace tcl
} // namespace acdk
} // namespace tests

