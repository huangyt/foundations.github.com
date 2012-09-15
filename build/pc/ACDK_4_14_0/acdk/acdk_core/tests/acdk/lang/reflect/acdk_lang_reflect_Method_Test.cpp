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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/reflect/acdk_lang_reflect_Method_Test.cpp,v 1.9 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/reflect/Method.h>
#include <acdk/lang/System.h>
#include <acdk/util/Properties.h>
#include <acdk/util/ResourceBundle.h>

namespace tests {
namespace acdk {
namespace lang {
namespace reflect {

BEGIN_DECLARE_TEST( Method_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( methodInfos )
END_DECLARE_TEST( Method_Test  )

BEGIN_DEFINE_TEST( Method_Test )
  ADD_TEST( Method_Test, standard ) 
  ADD_TEST( Method_Test, methodInfos ) 
  
END_DEFINE_TEST( Method_Test )

using namespace ::acdk::lang::reflect;

void
Method_Test::standard()
{
  // invoke non static method
  {
  StringBuffer sb("AC");
  RClassArray params = new ClassArray(1);
  params[0] = String::GetClass();
  RMethod appendmethod = sb.getClass()->getMethod("append", params);
  testAssert(appendmethod->isAccessible() == true);
  RObjectArray args = new ObjectArray(1);
  args[0] = new String("DK");
  appendmethod->invoke(&sb, args);
  testAssert(sb.toString()->equals("ACDK") == true);
  }
  
  // invoke static method
  {
    RClassArray params = new ClassArray(0);
    RMethod meth = Class::forName("acdk/lang/System")->getMethod("getProperties", params);
    RObjectArray args = new ObjectArray(0);
    ::acdk::util::RProperties props = (::acdk::util::RProperties)meth->invoke(Nil, args);
  }

}

void 
Method_Test::methodInfos()
{
  {
  RClass cls = ::acdk::util::ResourceBundle::GetClass();
  RClassArray params = new ClassArray(1);
  params[0] = String::GetClass();
  RMethod meth = cls->getMethod("getStringArray", params);
  
  RClassArray argclasses = meth->getParameterTypes();
  testAssert(argclasses->length() == 1);
  testAssert(argclasses[0] == String::GetClass());

  RClassArray exclasses = meth->getExceptionTypes();

  const ::acdk::lang::dmi::ClazzInfo* ci =  ::acdk::util::RMissingResourceException::clazzInfo();
  testAssert(exclasses->length() >= 1);
  testAssert(exclasses[0] == ::acdk::util::MissingResourceException::GetClass());
  }
  {
    RClass cls = ::acdk::lang::Integer::GetClass();
    RMethodArray meths = cls->getMethods();
    RMethodArray declmeths = cls->getDeclaredMethods();
    testAssert(meths->length() > declmeths->length());
  }
}


} // namespace reflect
} // namespace lang 
} //namespace acdk 
} //namespace tests 




