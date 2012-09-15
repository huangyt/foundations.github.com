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
// $Header: /cvsroot/acdk/acdk/acdk_perl/tests/acdk/perl/acdk_perl_Basics.cpp,v 1.11 2005/02/05 10:45:30 kommer Exp $


#include <acdk/io/RessourceFileSystem.h>
#include <acdk/perl/PerlInterpreter.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/System.h>

namespace tests {
namespace acdk {
namespace perl {

BEGIN_DECLARE_TEST( Basics_Test )
  DECLARE_TEST( helloPerl )
  DECLARE_TEST( helloACDK )
  DECLARE_TEST( helloACDK2 )
  DECLARE_TEST( helloACDK3 )
END_DECLARE_TEST( Basics_Test  )

BEGIN_DEFINE_TEST( Basics_Test )
  ADD_TEST( Basics_Test, helloPerl ) 
  ADD_TEST( Basics_Test, helloACDK ) 
  ADD_TEST( Basics_Test, helloACDK2 ) 
  ADD_TEST( Basics_Test, helloACDK3 ) 
END_DEFINE_TEST( Basics_Test )


using namespace ::acdk::perl;

void
Basics_Test::helloPerl()
{
  const char* ressource_array1 = 
"print(\"Hello Perl\");\n"
;

  RString code = new String(ressource_array1, ConstSST | CCAscii);
  RAPerlInterpreter pi = new APerlInterpreter();
  pi->parse(code);
  pi->run();

}

void
Basics_Test::helloACDK()
{
const char* ressource_array2 = 
"use pacdk;\n"
"$sb = pacdk::new('acdk/lang/StringBuffer', 'Hi ');\n"
"$sb->invoke('append', 'ACDK World');\n"
"print('Hello via Perl: ' . $sb->invoke('toString') . \"\\n\");\n"
"pacdk::peek_static('acdk/lang/System', 'out')->invoke('println', 'Hello via ACDK: ' . $sb->invoke('toString'));\n"
"exit 0;\n"
;
  RString code = new String(ressource_array2, ConstSST | CCAscii);
  RAPerlInterpreter pi = new APerlInterpreter();
  pi->parse(code);
  pi->run();

}

void
Basics_Test::helloACDK2()
{
  const char* ressource_array3 = 
"use pacdk;\n"
"$sb = pacdk::new(\"acdk/lang/StringBuffer\", \"Hi \");\n"
"$sb->append(\"ACDK World\");\n"
"print('Hello via Perl: ' . $sb->toString() . \"\\n\");\n"
"pacdk::peek_static('acdk/lang/System', 'out')->println('Hello via ACDK: ' . $sb->toString());\n"
"exit 0;\n"
;
  RString code = new String(ressource_array3, ConstSST | CCAscii);
  RAPerlInterpreter pi = new APerlInterpreter();
  pi->parse(code);
  System::out->println("On some perl installation $sb->append() will not work.\n"
                       "Reason: AUTOLOAD doesn't with XS on this versions.");
  pi->run();

}


void
Basics_Test::helloACDK3()
{
  
  RAPerlInterpreter pi = new APerlInterpreter();
  const char* tcode = 
    "use pacdk;\n"
    "my $sb = acdk::new(\"acdk/lang/StringBuffer\", \"Hi\");\n"
    "print \"constructed sb\\n\";\n"
    "$sb->append(\" Perl\");\n"
    "print($sb->toString() . \"\\n\");\n"
    "exit 0;\n"
    
    ;
  pi->parse(tcode);
  pi->run();
}

} // namespace perl
} // namespace acdk
} // namespace tests

