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
// $Header: /cvsroot/acdk/acdk/acdkx_com/tests/acdkx/com/acdkx_com_DmiObject_Test.cpp,v 1.10 2005/04/13 17:09:46 kommer Exp $

#include <acdk.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>

#include <acdkx/com/CreateTypeLib.h>
#include <acdkx/com/CoException.h>
#include <acdkx/com/ComObject.h>
#include <acdkx/com/ActiveScript.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Integer.h>

namespace tests {
namespace acdkx {
namespace com {
  
BEGIN_DECLARE_TEST( DmiObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( dmiTestClass )
  DECLARE_TEST( activeScript )
  
END_DECLARE_TEST( DmiObject_Test  )

BEGIN_DEFINE_TEST( DmiObject_Test )
  ADD_TEST( DmiObject_Test, standard ) 
  ADD_TEST( DmiObject_Test, dmiTestClass ) 
  ADD_TEST( DmiObject_Test, activeScript ) 
  
END_DEFINE_TEST( DmiObject_Test )

using namespace ::acdkx::com;
using namespace ::acdk::lang::dmi;

void
DmiObject_Test::standard()
{
  
  try {
    RComObject obj = new ComObject("Acdk.Object");
    obj->unwrapAcdkObject(false);
    RComObject sb = (RComObject)obj->invoke("New", (const char*)"acdk/lang/StringBuffer", (const char*)"Hello ");
    sb->invoke("append", (const char*)"ACDKX COM");
    RString str = (RString)sb->invoke("toString");
    testAssert(str->equals("Hello ACDKX COM") == true);

  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
  }
}

void
DmiObject_Test::dmiTestClass()
{
  try {
    RComObject obj = new ComObject("Acdk.Object");
    obj->unwrapAcdkObject(false);
    
    RComObject dtc = (RComObject)obj->invoke("New", (const char*)"acdk/tools/aunit/DmiTestClass", (const char*)"Hello ");
    RString str = (RString)(RObject)dtc->peek("pubString");
    testAssert(str->equals("Hello "));
    dtc->poke("pubString", (const char*)"ACDK");
    str = (RString)(RObject)dtc->peek("pubString");
    testAssert(str->equals("ACDK"));
    obj->invoke("invoke_static", (const char*)"acdk/tools/aunit/DmiTestClass", (const char*)"setGetPubStaticInt", 61, (const char*)"Hello ACDK");
    int ierg = obj->invoke("invoke_static", (const char*)"acdk/tools/aunit/DmiTestClass", (const char*)"getPubStaticInt");
    testAssert(ierg == 61);
    
    dtc->invoke("inOutMethod", 2, (const char*)"ACDK", outOf(ierg), outOf(str));
    testAssert(ierg == 3);
    testAssert(str->equals("ACDK returned") == true);
    RInteger integer;
    dtc->invoke("outMethod", 42, outOf(integer));
    testAssert(integer->intValue() == 42);

    // named args doens't work yet
    //bool erg = dtc->invoke("namedArgsMethod", NamedArg("sarg", &RString("sarg")) << NamedArg("iarg", 42) << NamedArg("sbarg", new StringBuffer("")));

  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
  }
}

void
DmiObject_Test::activeScript()
{
  try {
  //ActiveScript script("JavaScript");
  ActiveScript script("VBScript");
  RString code = 
    "set acdk = CreateObject(\"acdk.Object\")\n"  
    "set sb = acdk.New(\"acdk/lang/StringBuffer\", \"Hello \")\n"
    "sb.append \"from VB\"\n"
    //"MsgBox sb.toString()\n"
    "set dtc = acdk.New(\"acdk/tools/aunit/DmiTestClass\", \"Hello \")\n"
    "str = dtc.pubString\n"
    "if str <> \"Hello \" then\n"
    " MsgBox \"peek failed\"\n"
    "End If\n"
    "dtc.pubString = \"ACDK\"\n"
    "if dtc.pubString <> \"ACDK\" then\n"
    " MsgBox \"poke failed\"\n"
    "End If\n"
    "acdk.invoke_static \"acdk/tools/aunit/DmiTestClass\", \"setGetPubStaticInt\", 61, \"Hello ACDK\"\n"
    "ierg = acdk.invoke_static(\"acdk/tools/aunit/DmiTestClass\", \"getPubStaticInt\")\n"
    "if ierg <> 61 then\n"
    " MsgBox \"invoke_static failed\"\n"
    "End If\n"
    "ierg = 1\n"
    "str = \"ACDK\"\n"
    //not supported "dtc.inOutMethod 1, \"ACDK\", ierg, str\n"

    //"MsgBox \"All VB <-> ACDK DMI tests OK!\"\n"
  ;
  System::out->println("Evaluate Code:\n" + code);
  script.parseEval(code);
  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
  }
} 

} // com
} // acdkx
} // tests
