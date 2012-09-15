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
// $Header: /cvsroot/acdk/acdk/acdk_python/tests/acdk/python/acdk_python_Interpreter_Test.cpp,v 1.11 2005/03/10 15:10:54 kommer Exp $


#include <acdk.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/python/PythonInterpreter.h>

namespace tests {
namespace acdk {
namespace python {
  
BEGIN_DECLARE_TEST( Interpreter_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( acdkObject )
  DECLARE_TEST( opaqueAcdkObject )
  DECLARE_TEST( invoke_static )
  DECLARE_TEST( peek )
  DECLARE_TEST( peek_static )
  DECLARE_TEST( poke )
  DECLARE_TEST( poke_static )
  
  DECLARE_TEST( handledError )
  DECLARE_TEST( unhandledError )
  DECLARE_TEST( unhandledInternalError )
  
END_DECLARE_TEST( Interpreter_Test  )


BEGIN_DEFINE_TEST( Interpreter_Test )
  ADD_TEST( Interpreter_Test, standard ) 
  ADD_TEST( Interpreter_Test, acdkObject );
  ADD_TEST( Interpreter_Test, opaqueAcdkObject );
  ADD_TEST( Interpreter_Test, invoke_static );
  ADD_TEST( Interpreter_Test, peek );
  ADD_TEST( Interpreter_Test, peek_static );
  ADD_TEST( Interpreter_Test, poke );
  ADD_TEST( Interpreter_Test, poke_static );
  
  ADD_TEST( Interpreter_Test, handledError );
  ADD_TEST( Interpreter_Test, unhandledError );
  ADD_TEST( Interpreter_Test, unhandledInternalError   );
  


END_DEFINE_TEST( Interpreter_Test )

using namespace ::acdk::lang;

USING_CLASS(::acdk::python::, PythonInterpreter);



void 
Interpreter_Test::standard()
{
  PythonInterpreter pi;
  RString helloword_py = "print \"Hello World\"\n";
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  testAssert(pi.getLastReturnCode() == 0);
}


void
Interpreter_Test::acdkObject()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "sb = acdk.Object(\"acdk/lang/StringBuffer\", \"Hallo \")\n"
    "sb.append(\"World\")\n"
    "sb.append(\"!\")\n"
    "print sb.toString()\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  testAssert(pi.getLastReturnCode() == 0);
  return;
}

void
Interpreter_Test::opaqueAcdkObject()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "sb = acdk.Object(\"acdk/lang/StringBuffer\", \"Hallo \")\n"
    "isequal = sb.equals(sb)\n"
    "print isequal\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  testAssert(pi.getLastReturnCode() == 0);
  return;
}


void
Interpreter_Test::peek()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "tc = acdk.Object(\"acdk/tools/aunit/DmiTestClass\")\n"
    "print tc.peek(\"pubString\")"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  testAssert(pi.getLastReturnCode() == 0);
  return;
}


void
Interpreter_Test::peek_static()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "out = acdk.peek_static(\"acdk/lang/System\", \"out\")\n"
    "out.println(\"Hallo World\")\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() == 0);
  return;
}

void
Interpreter_Test::poke()  
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "tc = acdk.Object(\"acdk/tools/aunit/DmiTestClass\")\n"
    "tc.poke(\"pubString\", \"Hello\")\n"
    "print tc.peek(\"pubString\")\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() == 0);
}

void
Interpreter_Test::poke_static()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "acdk.poke_static(\"acdk/tools/aunit/DmiTestClass\", \"pubStaticInt\", 42)\n"
    "print acdk.peek_static(\"acdk/tools/aunit/DmiTestClass\", \"pubStaticInt\")\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() == 0);
}
  


void
Interpreter_Test::invoke_static()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "out = acdk.invoke_static(\"acdk/lang/Integer\", \"toString\", 42)\n"
    "if out != \"42\": raise RuntimeError, \"Test failed\""
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() == 0);
  return;
}

void
Interpreter_Test::handledError()
{
  PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "try:\n"
    "\tout = acdk.peek_static(\"acdk/lang/System\", \"there_is_no_such_field\")\n"
    "\traise RuntimeError, \"Should never reached here\"\n"
    "except RuntimeError, e:\n"
    "\tprint e\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() == 0);
}


void
Interpreter_Test::unhandledError()
{
   PythonInterpreter pi;
  RString helloword_py = 
    "raise RuntimeError, \"forced Exception\"\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() != 0);
}

void
Interpreter_Test::unhandledInternalError()
{
   PythonInterpreter pi;
  RString helloword_py = 
    "import acdk\n"
    "out = acdk.peek_static(\"acdk/lang/System\", \"there_is_no_such_field\")\n"
    ;
  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  
  testAssert(pi.getLastReturnCode() != 0);
}



} // namespace python
} //namespace acdk 
} //namespace tests



