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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/tests/acdk/lisp/acdk_lisp_Dmi.cpp,v 1.8 2005/02/05 10:45:12 kommer Exp $


#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lisp/LispInterpreter.h>
#include <acdk/lisp/LispObject.h>

#include <acdk/tools/aunit/SayHelloInterface.h>

namespace tests {
namespace acdk {
namespace lisp {
  
BEGIN_DECLARE_TEST( Dmi_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( implementInterface )
END_DECLARE_TEST( Dmi_Test  )

BEGIN_DEFINE_TEST( Dmi_Test )
  ADD_TEST( Dmi_Test, standard ) 
  ADD_TEST( Dmi_Test, implementInterface ) 
  
END_DEFINE_TEST( Dmi_Test )

using namespace ::acdk::lisp;
USING_CLASS(::acdk::tools::aunit::, SayHelloInterface);

void 
Dmi_Test::standard()
{

  LispInterpreter linterpreter;
  linterpreter.init();

  const char* testcode =
    testcode =
    "(defclass EMyClass () ("
    "  (mBar :initarg :bar)"
    "  (foo :initform (lambda (self firstint secondint) "
    "(+ firstint secondint (self 'mBar))))"
    "))"
    "(make-instance 'EMyClass :bar 3)"
    ;
  
  RObject obj = linterpreter.eval(testcode);
  testcode = 
    "(setf t (make-instance 'EMyClass :bar 3))"
    "(t 'foo 2 5)"
    ;
  linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
  int erg = obj->invoke("foo", 2, 5);
  testAssert(erg == 10);
}



void Dmi_Test::implementInterface()
{
  System::out->println("**  Dmi_Test for Lisp currently not working");
  /* ### FIXME
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode =
    testcode =
    "(defclass SayHelloImpl () ("
    "  (sayHello :initform (lambda (self to) "
                      " (setf out (peek-static 'acdk.lang.System 'out)) "
                      " (setf msg \"Hello from ACDK Lisp to \")"
                      " (setf msg (msg 'concat to))"
                      " (invoke out 'println msg)"
                      " msg"
                      ")"
        ")"
    "))"
    "(make-instance 'SayHelloImpl)"
    ;
  
  RObject obj = linterpreter.eval(testcode);
  RLispObject lobj = new LispObject("SayHelloImpl");
  RSayHelloInterface hi = (RSayHelloInterface) SayHelloInterface::getDmiProxy(&lobj);
  RString str = hi->sayHello("C++");
  testAssert(str->equals("Hello from ACDK Lisp to C++") == true);
  */
}


} // namespace lisp
} // namespace acdk
} // namespace tests



