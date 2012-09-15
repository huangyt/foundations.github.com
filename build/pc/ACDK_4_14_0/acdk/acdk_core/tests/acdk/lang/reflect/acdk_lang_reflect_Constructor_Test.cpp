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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/reflect/acdk_lang_reflect_Constructor_Test.cpp,v 1.6 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/reflect/Method.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
#include <acdk/util/ResourceBundle.h>
#include <acdk/lang/Process.h>

namespace tests {
namespace acdk {
namespace lang {
namespace reflect {

BEGIN_DECLARE_TEST( Constructor_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( methodInfos )
  DECLARE_TEST( exceptions )
END_DECLARE_TEST( Constructor_Test  )

BEGIN_DEFINE_TEST( Constructor_Test )
  ADD_TEST( Constructor_Test, standard ) 
  ADD_TEST( Constructor_Test, methodInfos ) 
  ADD_TEST( Constructor_Test, exceptions ) 
  
END_DEFINE_TEST( Constructor_Test )

using namespace ::acdk::lang::reflect;

void
Constructor_Test::standard()
{
  // invoke non static method
  {
  StringBuffer sb("AC");
  RClassArray params = new ClassArray(1);
  params[0] = String::GetClass();
  RConstructor constructor = sb.getClass()->getDeclaredConstructor(params);
  testAssert(constructor->isAccessible() == true);
  RObjectArray args = new ObjectArray(1);
  args[0] = new String("ACDK");
  RObject obj = constructor->newInstance(args);
  testAssert(obj->toString()->equals("ACDK") == true);
  }
}

void 
Constructor_Test::methodInfos()
{
  RConstructorArray constructors = StringBuffer::GetClass()->getDeclaredConstructors();
  testAssert(constructors->length() > 0);
  for (int i = 0; i < constructors->length(); ++i)
  {
    RClassArray extypes = constructors[i]->getExceptionTypes();
    RClassArray paramtypes = constructors[i]->getParameterTypes();
  }
  
}
void 
Constructor_Test::exceptions()
{
  try {
    RClassArray params = new ClassArray(1);
    params[0] = Process::GetClass();
    RConstructor constructor = StringBuffer::GetClass()->getDeclaredConstructor(params);
    testAssert(0 == "expect exception");
  } catch (::acdk::lang::RNoSuchMethodException ex) {
    // OK
  } catch (::acdk::lang::RThrowable ex) {
    testAssertComment(false, "expect NoSuchMethodException, but caught throwable: " + ex->getMessage());
  }
  try {
    RClassArray params = new ClassArray(1);
    params[0] = String::GetClass();
    RConstructor constructor = Process::GetClass()->getDeclaredConstructor(params);
    testAssert(0 == "expect exception");
  } catch (::acdk::lang::RNoSuchMethodException ex) {
    // OK
  } catch (::acdk::lang::RThrowable ex) {
    testAssertComment(false, "Expect NoSuchMethodException, but caught throwable: " + ex->getMessage());
  }
    
}

} // namespace reflect
} // namespace lang 
} //namespace acdk 
} //namespace tests 




