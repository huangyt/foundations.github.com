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
// $Header: /cvsroot/acdk/acdk/acdk_perl/tests/acdk/perl/acdk_perl_CoObject.cpp,v 1.10 2005/02/05 10:45:30 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/RessourceFileSystem.h>
#include <acdk/perl/PerlInterpreter.h>

#include <acdk/tools/aunit/TestRunner.h>
#ifdef ACDK_OS_WIN32

namespace tests {
namespace acdk {
namespace perl {

BEGIN_DECLARE_TEST( ComObject_Test )
  DECLARE_TEST( helloWord )
END_DECLARE_TEST( ComObject_Test  )

BEGIN_DEFINE_TEST( ComObject_Test )
  ADD_TEST( ComObject_Test, helloWord ) 
END_DEFINE_TEST( ComObject_Test )


using namespace ::acdk::perl;

void
ComObject_Test::helloWord()
{
  const char* ressource_array1 = 
"sub require_class($)\n"
"{\n"
"  my ($cls) = @_;\n"
"  my $cl = pacdk::new(\"acdk/lang/ClassLoader\");\n"
"  $cl->findClass($cls);\n"
"}\n"
"\n"

"require_class('acdkx/com/ComObject');\n"
"use pacdk;\n"
"$word = pacdk::invoke_static('acdkx/com/ComObject', 'New', 'Word.Application');\n"
"$word->poke('Visible', 1);\n"
"$doc = $word->peek('Documents')->add();\n"
"$sel = $word->peek('ActiveWindow')->peek('Selection');\n"
"$sel->TypeText(\"This is \");\n"
"$sel->peek('Font')->poke('Bold', 1);\n"
"$sel->TypeText(\"ACDK\");\n"
"$sel->peek('Font')->poke('Bold', 0);\n"
"$sel->TypeText(\" instrumenting Word through acdk_perl\");\n"
"pacdk::invoke_static('acdk/lang/Thread', 'sleep', 3000);\n"
"$word->Quit(0);\n"
;

  RString code = new String(ressource_array1, ConstSST | CCAscii);
  RAPerlInterpreter pi = new APerlInterpreter();
  pi->parse(code);

  ::acdk::lang::System::out->println("Executing code:\n" + code);
  pi->run();

}


} // namespace perl
} // namespace acdk
} // namespace tests

#endif //ACDK_OS_WIN32

