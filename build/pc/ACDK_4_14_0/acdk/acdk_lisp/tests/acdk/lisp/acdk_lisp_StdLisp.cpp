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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/tests/acdk/lisp/acdk_lisp_StdLisp.cpp,v 1.8 2005/02/05 10:45:12 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lisp/LispInterpreter.h>

namespace tests {
namespace acdk {
namespace lisp {
  
BEGIN_DECLARE_TEST( StdLisp_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( misc )
  DECLARE_TEST( lamda )
  DECLARE_TEST( quote )
  DECLARE_TEST( comma )
  DECLARE_TEST( defmacro )
  DECLARE_TEST( parseCode )
  DECLARE_TEST( defclass )
END_DECLARE_TEST( StdLisp_Test  )

BEGIN_DEFINE_TEST( StdLisp_Test )
  ADD_TEST( StdLisp_Test, standard ) 
  ADD_TEST( StdLisp_Test, misc ) 
  
  ADD_TEST( StdLisp_Test, lamda ) 
  ADD_TEST( StdLisp_Test, quote ) 
  ADD_TEST( StdLisp_Test, comma ) 
  ADD_TEST( StdLisp_Test, defmacro ) 
  
  ADD_TEST( StdLisp_Test, parseCode ) 
  ADD_TEST( StdLisp_Test, defclass  ) 
END_DEFINE_TEST( StdLisp_Test )

using namespace ::acdk::lisp;


void 
StdLisp_Test::standard()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  try {
    const char* testcode =
    "(include \"selftest.lsp\")"
    ;

    RObject obj = linterpreter.eval(testcode);
    RString str = (RString)obj;
  } catch (RThrowable ex) {
    System::out->println("Test failed with Exception: " + ex->getMessage());
    testAssert(false);
  }
}

void
StdLisp_Test::misc()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  try {
    const char* testcode =
      "(setf a (new '[acdk.lang.String 1))\n"
      "(setf i (invoke a 'length))\n"
      ;
    RObject obj = linterpreter.eval(testcode);
  } catch (RThrowable ex) {
    System::out->println("Test failed with Exception: " + ex->getMessage());
    testAssert(false);
  }
}

void 
StdLisp_Test::lamda()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode = 
    "(setf x (lambda (a b) (+ a b)))"
    "(x 3 4)"
    ;
  RObject obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
}

void
StdLisp_Test::quote()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode = 
    "(setf x '(a (1 c)))"
    ;
  RObject obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");
  testcode = 
    "'a"
  ;
  obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");

}


void 
StdLisp_Test::comma()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode = 
    "(setf a 1)"
    "(setf b '(2 3))"
    ;
  linterpreter.eval(testcode);
  testcode = 
    "`(a is ,a and b is ,b)"
    ;
  RObject obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");

  testcode = 
    "(setf a 1)"
    "(setf b '(2 3))"
    "`(a is ,a and b is ,@b inserted)"
    ;
  obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");
}

void
StdLisp_Test::defmacro()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode = 
    "(trace 1)"
    "(defmacro doaddition (&rest doaddition_args)"
    "  `(+ ,@doaddition_args)"
    ")"
    ;
  RObject obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");
  testcode = 
    "(setf a 3)"
    "(doaddition 1 a 4)"
    ;
  obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + RLispVar(obj)->toCode() + "]");
}



void 
StdLisp_Test::parseCode ()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  const char* testcode = 
    "(- 4 1)"
    "(+ 1 2)"
    ;
  RObject obj = linterpreter.eval(testcode);
  System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
}

void StdLisp_Test::defclass()
{
  LispInterpreter linterpreter;
  linterpreter.init();
  {
    const char* testcode = 
    "(setf x 3)"
    "(defclass AMyClass () ((slot-a :initform x) slot-b))";
    RObject obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
  }
  {
    const char* testcode = 
    "(defclass BMyClass () ("
    "(slot-a :initarg :a :initform \"Hallo\")"
    "(slot-b :initform \"ACDK\" :allocation :class)))";

    RObject obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed class definition [") + testcode  + "] to [" + obj->toString() + "]");
    
    testcode = 
    "(defclass CMyClass (BMyClass) ("
      "(slot-c :initarg :c)))";
    obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
    testcode = 
    "(setf obj (make-instance 'CMyClass))";
    obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
    testcode = 
    "(setf obj (make-instance 'CMyClass :a \"Hello\" :c \" Lisp\"))";
    obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");

    testcode = 
    "(defun methodA (self arg1)"
       "(self 'slot-b (self 'slot-a))"
       "(self 'slot-b)"
     ")"
     ;
      obj = linterpreter.eval(testcode);
      System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
    testcode = 
     "(defclass DMyClass (CMyClass)"
      "((methodA :initform methodA) (methodB :initform methodA))"
     ")"
     ;
    obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
    
    testcode =
    "(setf obj (make-instance 'DMyClass :a \"Hello\" :c \" Lisp\"))"
    "(obj 'methodB \" from Test\")"
    ;
    obj = linterpreter.eval(testcode);
    System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");

    testcode =
     "(defclass EMyClass () ("
     "  (mBar :initarg :bar)"
     "  (foo :initform (lambda (self firstint secondint) "
                          "(+ firstint secondint (self 'mBar))))"
      "))"
      ;
      obj = linterpreter.eval(testcode);
      System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");
      testcode =
      "((make-instance 'EMyClass :bar 3) 'foo 2 5)"
      ;
      
      obj = linterpreter.eval(testcode);
      System::out->println(RString("Parsed code [") + testcode  + "] to [" + obj->toString() + "]");

  }
}


} // namespace lisp
} // namespace acdk
} // namespace tests



