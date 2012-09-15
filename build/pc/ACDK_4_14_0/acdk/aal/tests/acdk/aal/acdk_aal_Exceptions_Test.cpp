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
// $Header: /cvsroot/acdk/acdk/aal/tests/acdk/aal/acdk_aal_Exceptions_Test.cpp,v 1.6 2005/02/05 10:44:51 kommer Exp $


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
BEGIN_DECLARE_TEST( Exceptions_Test )
  DECLARE_TEST( basicThrow )
  DECLARE_TEST( catchAalException )
  DECLARE_TEST( throwAckdCatchAalException )
  DECLARE_TEST( aalExceptions )
END_DECLARE_TEST( Exceptions_Test  )

BEGIN_DEFINE_TEST( Exceptions_Test )
  ADD_TEST( Exceptions_Test, basicThrow ) 
  ADD_TEST( Exceptions_Test, catchAalException ) 
  ADD_TEST( Exceptions_Test, throwAckdCatchAalException ) 
  ADD_TEST( Exceptions_Test, aalExceptions ) 
  
END_DEFINE_TEST( Exceptions_Test )


void
Exceptions_Test::basicThrow()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "throw new Exception(\"asdf\");\n"
    "\n"
    ;
  try {
    aint.parseTreeInterpret(text);
  } catch (RException ex) {
  }
  aint.resetEnv();
  
}

void
Exceptions_Test::catchAalException()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "try {\n"
    "  throw new Exception(\"asdf\");\n"
    "} catch (Exception ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "void throwEx() { throw new Exception(\"asdf\"); }\n"
    "try {\n"
    "  throwEx();\n"
    "} catch (Exception ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "using acdk.lang;\n"
    "void throwEx() { throw new Exception(\"asdf\"); }\n"
    "try {\n"
    "  throwEx();\n"
    "} catch (Exception ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "} finally {\n"
    "  System.out.println(\"Finally called\");\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "using acdk.lang;\n"
    "void throwEx() { try { throw new Exception(\"asdf\"); } finally { System.out.println(\"Finally called in throwEx\"); } }\n"
    "try {\n"
    "  throwEx();\n"
    "} catch (Exception ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "} finally {\n"
    "  System.out.println(\"Finally called\");\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
  text =
    "using acdk.lang;\n"
    "void throwEx() { try { throw new acdk.io.IOException(\"asdf\"); } catch (ClassNotFoundException ex) {  } }\n"
    "try {\n"
    "  throwEx();\n"
    "} catch (acdk.io.IOException ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "} finally {\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();

  text =
    "using acdk.lang;\n"
    "try {\n"
    "  try {\n"
    "    throw new Throwable(\"asdf\");\n"
    "  } catch (Exception ex) {\n"
    //"    System.out.println(ex.getMessage());\n"
    "  }\n"
    "} catch (Throwable ex) {\n"
    "  System.out.println(\"trowable called\");\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Exceptions_Test::throwAckdCatchAalException()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "try {\n"
    "  Object o;\n"
    "  o.toString();\n" // throws NullPointerException
    "} catch (NullPointerException ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}

void
Exceptions_Test::aalExceptions()
{
  AalInterpreter aint;

  RString text;
  text =
    "using acdk.lang;\n"
    "class MyException extends acdk.lang.Exception { public MyException(String msg) : Exception(msg) {} }\n"

    "try {\n"
    "  throw new MyException(\"Test\");\n"
    "} catch (MyException ex) {\n"
    "  System.out.println(ex.getMessage());\n"
    "}\n"
    "\n"
    ;
  aint.parseTreeInterpret(text);
  aint.resetEnv();
}


} // namespace aal
} // namespace acdk
} // namespace tests

