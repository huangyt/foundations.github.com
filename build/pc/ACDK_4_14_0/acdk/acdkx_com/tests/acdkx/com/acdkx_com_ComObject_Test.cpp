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
// $Header: /cvsroot/acdk/acdk/acdkx_com/tests/acdkx/com/acdkx_com_ComObject_Test.cpp,v 1.16 2005/04/13 17:09:45 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>

#include <acdkx/com/CreateTypeLib.h>
#include <acdkx/com/CoException.h>
#include <acdkx/com/ComObject.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdkx {
namespace com {
  
BEGIN_DECLARE_TEST( ComObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( self )
END_DECLARE_TEST( ComObject_Test  )

BEGIN_DEFINE_TEST( ComObject_Test )
  ADD_TEST( ComObject_Test, standard ) 
  ADD_TEST( ComObject_Test, self ) 
END_DEFINE_TEST( ComObject_Test )

using namespace ::acdkx::com;
using namespace ::acdk::lang::dmi;

void
ComObject_Test::standard()
{
  CoInitialize(NULL);
  try {
    RComObject obj = new ComObject("Word.Application");

    obj->poke("Visible", true);
    RComObject docs = (RComObject)obj->peek("Documents");
    RComObject doc = (RComObject)docs->invoke("Add");
    RComObject sel = (RComObject)obj->peek("ActiveWindow")->peek("Selection");
    RString s("This is ACDK");
    sel->invoke("TypeText", new String("This is "));
    sel->peek("Font")->poke("Bold", true);
    sel->invoke("TypeText", (const char*)"ACDK ");
    sel->peek("Font")->poke("Bold", false);
    sel->invoke("TypeText", (const char*)"instrumenting Word!");
    
    Thread::sleep(3000);
    obj->invoke("Quit", 0);
  } catch (RCoException ex) {
    System::out->println(ex->getMessage());
  } catch (RException ex) {
     System::out->println(ex->getMessage());
  }
}

void
ComObject_Test::self()
{
  CoInitialize(NULL);
  try {
    RComObject obj = new ComObject("Acdk.Object");
    obj->unwrapAcdkObject(false);
    RComObject sb = (RComObject)obj->invoke("New", (const char*)"acdk/lang/StringBuffer", (const char*)"Hello ");
    sb->invoke("append", (const char*)"ACDKX COM");
    RString str = (RString)sb->invoke("toString");
    testAssert(str->equals("Hello ACDKX COM") == true);

  } catch (RThrowable ex) {
    ::acdk::lang::System::out->println(ex->getMessage());
    testAssertComment(false, "ComObject_Test::self throws Ex: " + ex->getMessage());
  }
}

} // com
} // acdkx
} // tests
