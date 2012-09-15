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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/dmi/acdk_lang_dmi_MetaObject_Test.cpp,v 1.8 2005/02/05 10:45:09 kommer Exp $

#include <acdk.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/tools/aunit/DmiTestClass.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/DmiObject.h>

namespace tests {
namespace acdk {
namespace lang {
namespace dmi {

BEGIN_DECLARE_TEST( MetaObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( instanceAttribute )
  DECLARE_TEST( altName )
END_DECLARE_TEST( MetaObject_Test  )

BEGIN_DEFINE_TEST( MetaObject_Test )
  ADD_TEST( MetaObject_Test, standard ) 
  ADD_TEST( MetaObject_Test, instanceAttribute ) 
  
  ADD_TEST( MetaObject_Test, altName ) 
  
END_DEFINE_TEST( MetaObject_Test )

using namespace ::acdk::lang::dmi;

void
MetaObject_Test::standard()
{
  
  RObject obj = new ::acdk::tools::aunit::DmiTestClass();
  RClass cls = obj->getClass();
  RMetaAttributeArray maa = cls->getMetaAttributes();
  testAssert(maa->length() == 3);
  System::out->println("MetaAttribute of class " + cls->getName());
  for (int i = 0; i < maa->length(); ++i)
  {
    RMetaAttribute ma = maa[i];
    System::out->print(ma->name);
    if (ma->value != Nil)
    {
      System::out->println(": " + ma->value->toString());
    }
    else
    {
      System::out->println(": Nil");
    }
  }
}

void
MetaObject_Test::instanceAttribute()
{
  {
    RStringBuffer sb = new StringBuffer("");
    RClass cls = sb->getClass();
    cls->setMetaAttribute("acdk_lang_dmi_Test_classAttribute", RObject(new String("Hello Class")));
    RMetaAttribute ma = cls->getMetaAttribute("acdk_lang_dmi_Test_classAttribute");
    testAssert(ma != Nil);
    RString erg = (RString)ma->value;
    testAssert(erg->equals("Hello Class") == true);
    cls->setInstanceMetaAttribute(&sb, "acdk_lang_dmi_Test_instanceAttribute", RObject(new String("Hello Object")));
    
    ma = cls->getInstanceMetaAttribute(&sb, "acdk_lang_dmi_Test_instanceAttribute");
    testAssert(ma != Nil);
    erg = (RString)ma->value;
    testAssert(erg->equals("Hello Object") == true);
    RStringBuffer othersb = new StringBuffer("");
    ma = cls->getInstanceMetaAttribute(&othersb, "acdk_lang_dmi_Test_instanceAttribute");
    testAssert(ma == Nil);

  }
  {
    RStringBuffer sb = new StringBuffer("");
    RClass cls = sb->getClass();
    
    RMetaAttribute ma = cls->getMetaAttribute("acdk_lang_dmi_Test_classAttribute");
    testAssert(ma != Nil);
    RString erg = (RString)ma->value;
    testAssert(erg->equals("Hello Class") == true);
    cls->deleteMetaAttribute("acdk_lang_dmi_Test_classAttribute");
    ma = cls->getMetaAttribute("acdk_lang_dmi_Test_classAttribute");
    testAssert(ma == Nil);
    
  }

}


void
MetaObject_Test::altName()
{
  RObject obj = new ::acdk::tools::aunit::DmiTestClass();
  int methoderg  = obj->invoke("methodWithDefaultArgs");
  testAssert(methoderg == 3);
  methoderg = obj->invoke("methodWithDefaultArgs", 4);
  testAssert(methoderg == 4);
  methoderg  = obj->invoke("methodWithoutArgs");
  testAssert(methoderg == 3);
  methoderg  = obj->invoke("methodWithOneArgs", 5);
  testAssert(methoderg == 5);
}

} // namespace lang 
} // namespace dmi 
} //namespace acdk 
} //namespace tests 




