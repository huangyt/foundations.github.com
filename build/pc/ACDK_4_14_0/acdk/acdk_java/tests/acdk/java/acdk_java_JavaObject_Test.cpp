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
// $Header: /cvsroot/acdk/acdk/acdk_java/tests/acdk/java/acdk_java_JavaObject_Test.cpp,v 1.14 2005/04/13 15:37:25 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/java/JavaInterpreter.h>
#include <acdk/java/JavaObject.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace java {

using namespace ::acdk::lang;
  

BEGIN_DECLARE_TEST( JavaObject_Test )
  DECLARE_TEST( simple )  
  DECLARE_TEST( standard )
  DECLARE_TEST( exception )
  DECLARE_TEST( peek_poke )
END_DECLARE_TEST( JavaObject_Test  )

BEGIN_DEFINE_TEST( JavaObject_Test )
  ADD_TEST( JavaObject_Test, simple ) 
  ADD_TEST( JavaObject_Test, standard ) 
  ADD_TEST( JavaObject_Test, exception ) 
  ADD_TEST( JavaObject_Test, peek_poke ) 
  
END_DEFINE_TEST( JavaObject_Test )


void
JavaObject_Test::simple()
{
  //::acdk::java::RJavaInterpreter jint = new ::acdk::java::JavaInterpreter();
  //return;
  ::acdk::java::RJavaObject jo = new ::acdk::java::JavaObject("java/lang/StringBuffer", (const char*)"Hello");
  jo->invoke("append", (const char*)" Java from ACDK");
  RObject obj = (RObject)jo->invoke("toString");
  testAssert(obj != Nil);
  RString str = (RString)obj;
  testAssert(str->equals("Hello Java from ACDK") == true);
  RObject jout = (RObject)::acdk::java::JavaObject::peek_static("java/lang/System", "out");
  jout->invoke("println", obj);
}

void 
JavaObject_Test::standard()
{
  /*
    This shows, that it is even not neccessary to linke acdk_java to our 
    executable.
    Only drawback: you cannot call peek_static and invoke_static directly, 
    but via a invoke_static method.
  */
  {
      RObject jsb = (RObject)Object::New("acdk/java/JavaObject", (const char*)"java/lang/StringBuffer", (const char*)"Hello");
      testAssert(jsb != Nil);
      RString str1 = (RString)jsb->invoke("toString");
      /*RObject t = */jsb->invoke("append", (const char*)" Java from ACDK");
      RObject obj = (RObject)jsb->invoke("toString");
      //obj = jsb->invoke("toString");
      testAssert(obj != Nil);
      RString str = (RString)obj;
      testAssert(str->equals("Hello Java from ACDK") == true);
      RObject javaout = (RObject)Object::invoke_static("acdk/java/JavaObject", "peek_static", (const char*)"java/lang/System", (const char*)"out");
      javaout->invoke("println", obj);
    }
}

void 
JavaObject_Test::exception()
{
  bool caught = false;
  try {
    RObject jsb = (RObject)Object::New("acdk/java/JavaObject", (const char*)"java/lang/StringBuffer", (const char*)"Hello");
    jsb->invoke("insert", -3, (const char*)"Bla");
    testAssertComment(false, "Should not reach here cause Ex");
  } catch(RIndexOutOfBoundsException ex) {
    caught = true;
  } catch(RThrowable ex) {
    testAssertComment(false, "Caught wrong Ex");
  }
  testAssertComment(caught == true, "Exception was not thrown");
}

void
JavaObject_Test::peek_poke()
{
  ::acdk::java::RJavaObject jobj;
  try {
    jobj = new ::acdk::java::JavaObject("tests/acdk/java/JavaDmiServer_Test");
  } catch (RThrowable ex) {
    System::out->println("Cannot found Object: " + ex->getMessage());
    THROW1(Throwable, "Probably the $ACDKHOME/bin is not in our java classpath or the java classes are not compiled (execute 'make -f acdk_java.jmk')");
  }
  int ival = ::acdk::java::JavaObject::peek_static("tests/acdk/java/JavaDmiServer_Test", "publicStaticInt");
  testAssertComment(ival == 42, "JavaObject::peek_static");
  int newIval = 43;
  ::acdk::java::JavaObject::poke_static("tests/acdk/java/JavaDmiServer_Test", "publicStaticInt", newIval);
  ival = ::acdk::java::JavaObject::peek_static("tests/acdk/java/JavaDmiServer_Test", "publicStaticInt");
  testAssertComment(newIval == ival, "JavaObject::poke_static");
  ::acdk::java::JavaObject::poke_static("tests/acdk/java/JavaDmiServer_Test", "publicStaticInt", 42);


  ival = jobj->peek("publicInt");
  testAssertComment(ival == 42, "JavaObject::peek");
  jobj->poke("publicInt", newIval);
  ival = jobj->peek("publicInt");
  testAssertComment(newIval == ival, "JavaObject::poke");
  jobj->poke("publicInt", 42);

}


} // namespace util
} // namespace acdk
} // namespace tests

