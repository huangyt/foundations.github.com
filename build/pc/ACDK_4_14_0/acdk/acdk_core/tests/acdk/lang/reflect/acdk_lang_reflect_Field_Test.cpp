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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/reflect/acdk_lang_reflect_Field_Test.cpp,v 1.8 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/reflect/Method.h>
#include <acdk/lang/NoSuchFieldException.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/tools/aunit/DmiTestClass.h>

namespace tests {
namespace acdk {
namespace lang {
namespace reflect {

BEGIN_DECLARE_TEST( Field_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( FieldInfos )
  DECLARE_TEST( exceptions )
END_DECLARE_TEST( Field_Test  )

BEGIN_DEFINE_TEST( Field_Test )
  ADD_TEST( Field_Test, standard ) 
  ADD_TEST( Field_Test, FieldInfos ) 
  ADD_TEST( Field_Test, exceptions ) 
END_DEFINE_TEST( Field_Test )

using namespace ::acdk::lang::reflect;

USING_CLASS(::acdk::tools::aunit::, DmiTestClass);

void
Field_Test::standard()
{
  {
    RObject obj = new DmiTestClass();
    RField f = DmiTestClass::GetClass()->getDeclaredField("pubInt");
    f->setInt(obj, 42);
    testAssert(f->getInt(obj) == 42);
  }
  {
    RObject obj = new DmiTestClass();
    RField f = DmiTestClass::GetClass()->getDeclaredField("pubInteger");
    f->set(obj, new Integer(42));
    testAssert(f->get(obj)->equals(new Integer(42)));
  }
  {
    RObject obj = Nil;
    RField f = DmiTestClass::GetClass()->getDeclaredField("pubStaticInt");
    f->setInt(obj, 42);
    testAssert(f->getInt(obj) == 42);
  }
  {
    RObject obj = Nil;
    RField f = DmiTestClass::GetClass()->getDeclaredField("pubStaticInteger");
    f->set(obj, new Integer(42));
    testAssert(f->get(obj)->equals(new Integer(42)));
  }
  
}

void 
Field_Test::FieldInfos()
{
    
}

void
Field_Test::exceptions()
{
  try {
    RObject obj = new DmiTestClass();
    RField f = DmiTestClass::GetClass()->getDeclaredField("NotExistantField");
    testAssert(0 == "Expect NoSuchFieldException");
  } catch (RNoSuchFieldException ex) {
  } catch (RThrowable ex) {
    System::out->println(ex->getName() + ": " + ex->getMessage());
    testAssert(0 == "Expect NoSuchFieldException");
  }

}


} // namespace reflect
} // namespace lang 
} //namespace acdk 
} //namespace tests 




