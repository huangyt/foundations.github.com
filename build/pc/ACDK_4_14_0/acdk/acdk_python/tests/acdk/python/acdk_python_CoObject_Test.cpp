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
// $Header: /cvsroot/acdk/acdk/acdk_python/tests/acdk/python/acdk_python_CoObject_Test.cpp,v 1.9 2005/03/10 15:10:54 kommer Exp $


#include <acdk.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/python/PythonInterpreter.h>
#ifdef ACDK_OS_WIN32

namespace tests {
namespace acdk {
namespace python {
  
BEGIN_DECLARE_TEST( ComObject_Test )
  DECLARE_TEST( helloWord )
END_DECLARE_TEST( ComObject_Test  )


BEGIN_DEFINE_TEST( ComObject_Test )
  ADD_TEST( ComObject_Test, helloWord ) 
  
END_DEFINE_TEST( ComObject_Test )

using namespace ::acdk::lang;

USING_CLASS(::acdk::python::, PythonInterpreter);



void 
ComObject_Test::helloWord()
{
  PythonInterpreter pi;
  RString helloword_py = 
"import acdk\n"
"classloader = acdk.Object(\"acdk/lang/ClassLoader\")\n"
"classloader.findClass(\"acdkx/com/ComObject\")\n"    
"word = acdk.invoke_static(\"acdkx/com/ComObject\", \"New\", \"Word.Application\")\n"
"word.poke(\"Visible\", 1)\n"
"doc = word.peek(\"Documents\").Add()\n"
"sel = word.peek(\"ActiveWindow\").peek(\"Selection\")\n"
"sel.TypeText(\"This is \")\n"
"sel.peek(\"Font\").poke(\"Bold\", 1)\n"
"sel.TypeText(\"ACDK\")\n"
"sel.peek(\"Font\").poke(\"Bold\", 0)\n"
"sel.TypeText(\" instrumenting Word through acdk_python\")\n"
"acdk.invoke_static(\"acdk/lang/Thread\", \"sleep\", 3000)\n"
"word.Quit(0)\n"
;



  System::out->print("Executing: [\n" + helloword_py + "]\n> ");
  System::out->flush();
  pi.eval(helloword_py);
  testAssert(pi.getLastReturnCode() == 0);
}


} // namespace python
} //namespace acdk 
} //namespace tests 

#endif //ACDK_OS_WIN32

