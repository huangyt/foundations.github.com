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
// $Header: /cvsroot/acdk/acdk/acdk_tcl/tests/acdk/tcl/acdk_tcl_CoObject.cpp,v 1.9 2005/02/05 10:45:32 kommer Exp $

#include <acdk.h>
#include <acdk/tcl/TclInterpreter.h>
#include <acdk/lang/System.h>

#include <acdk/tools/aunit/TestRunner.h>
#ifdef ACDK_OS_WIN32

namespace tests {
namespace acdk {
namespace tcl {

BEGIN_DECLARE_TEST( ComObject_Test )
  DECLARE_TEST( helloWord )
END_DECLARE_TEST( ComObject_Test  )

BEGIN_DEFINE_TEST( ComObject_Test )
  ADD_TEST( ComObject_Test, helloWord ) 
END_DEFINE_TEST( ComObject_Test )


using namespace ::acdk::tcl;

void
ComObject_Test::helloWord()
{
  const char* ressource_array1 = 
"proc require_class { clsname } {\n"
"  set cl [acdk_new \"acdk/lang/ClassLoader\"]\n"
"  acdk_invoke $cl findClass $clsname\n"
"}\n"
"\n"

"require_class \"acdkx/com/ComObject\"\n"
"puts \"class loaded\"\n"
"set word [acdk_invoke_static \"acdkx/com/ComObject\" \"New\" \"Word.Application\"]\n"
"acdk_poke $word \"Visible\" 1\n"
"set doc [acdk_invoke [acdk_peek $word \"Documents\"] add]\n"
"set sel [acdk_peek [acdk_peek $word \"ActiveWindow\"] \"Selection\"]\n"
"acdk_invoke $sel TypeText \"This is \"\n"
"acdk_poke [acdk_peek $sel Font] Bold 1\n"
"acdk_invoke $sel TypeText \"ACDK\"\n"
"acdk_poke [acdk_peek $sel Font] Bold 0\n"
"acdk_invoke $sel TypeText \" instrumenting Word through acdk_tcl\"\n"
"acdk_invoke_static \"acdk/lang/Thread\" sleep 3000\n"
"acdk_invoke $word Quit 0\n"
;
  
  RString code = new String(ressource_array1);
  
  System::out->println("Interpreting Code: [");
  System::out->println(code + "]");
  RTclInterpreter ti = new TclInterpreter();
  ti->eval(code);
}


} // namespace tcl
} // namespace acdk
} // namespace tests

#endif //ACDK_OS_WIN32

