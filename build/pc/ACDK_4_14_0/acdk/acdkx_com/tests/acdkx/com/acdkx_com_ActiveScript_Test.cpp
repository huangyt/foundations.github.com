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
// $Header: /cvsroot/acdk/acdk/acdkx_com/tests/acdkx/com/acdkx_com_ActiveScript_Test.cpp,v 1.6 2005/02/05 10:45:38 kommer Exp $


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
  
BEGIN_DECLARE_TEST( ActiveScript_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( globalVars )
END_DECLARE_TEST( ActiveScript_Test  )

BEGIN_DEFINE_TEST( ActiveScript_Test )
  ADD_TEST( ActiveScript_Test, standard ) 
  ADD_TEST( ActiveScript_Test, globalVars ) 
END_DEFINE_TEST( ActiveScript_Test )

using namespace ::acdkx::com;
using namespace ::acdk::lang::dmi;

void
ActiveScript_Test::standard()
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
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

    ///"MsgBox \"All VB <-> ACDK DMI tests OK!\"\n"
  ;
  System::out->println("Evaluate Code:\n" + code);
  script.parseEval(code);
  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
    testAssertComment(false, "test failed with execption: " + ex->getMessage());
  }
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
}

void
ActiveScript_Test::globalVars()
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
  try {
    ActiveScript script("VBScript");
    RStringBuffer sb = new StringBuffer("Hello ");
    script.setVar("astr", &(RString)"ACDK");
    RStringBuffer erg = new StringBuffer("");
    script.setVar("erg", &erg);
    script.setVar("sb", &sb);
    RString code =
      "sb.append(astr.toString())\n"
      //"MsgBox sb.toString()\n"
      "erg = sb\n"
      ;
    script.parseEval(code);
    testAssert(sb->toString()->equals("Hello ACDK") == true);

    //testAssert(erg->toString()->equals("Hello ACDK") == true);
  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
    testAssertComment(false, "test failed with execption: " + ex->getMessage());
  }
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
}

} // com
} // acdkx
} // tests
