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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdkx_orb_AcdkObject_Test.cpp,v 1.19 2005/04/21 08:28:52 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include "TestInterfaceImpl.h"
#include <acdkx/orb/CorObject.h>
#include <acdkx/orb/AcdkObject.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/LogManager.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/tools/aunit/SayHelloInterface.h>

namespace tests {
namespace acdkx {
namespace orb {

BEGIN_DECLARE_TEST( AcdkObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( exceptionHandling )
  DECLARE_TEST( sampleCode )
  DECLARE_TEST( dmiProxy )
END_DECLARE_TEST( AcdkObject_Test  )

BEGIN_DEFINE_TEST( AcdkObject_Test )
  ADD_TEST( AcdkObject_Test, standard )
  ADD_TEST( AcdkObject_Test, exceptionHandling )
  ADD_TEST( AcdkObject_Test, sampleCode )
  ADD_TEST( AcdkObject_Test, dmiProxy )
  
END_DEFINE_TEST( AcdkObject_Test )

using namespace ::acdk::lang;
using namespace ::acdkx::orb;
using namespace ::acdk::lang::dmi;

#define OUTL(statement) \
do { \
  std::cout << #statement << "> " << std::endl; \
  statement; \
  std::cout << #statement << "< " << std::endl; \
} while (false)




RString
StdDispatch_findMethodAltName(const ClazzInfo* ci, IN(RString) funcname, IN(RDmiObjectArray) args, int flags) // ### into StdDispatch
{
  ScriptVarArray cargs; // ArgumentExprType needs flags
  for (int i = 0; i < args.length(); ++i)
    cargs.push_back(*args[i]);
  AcdkDmiClient dc;
  const ClazzMethodInfo* mi = StdDispatch::_lookupMethod(ci, funcname, cargs, dc, flags, 0);//findMethod(ci, funcname, cargs, dc, Nil, flags, false);
  if (mi == 0)
    return "";
  return mi->altlabel;
}

RString
StdDispatch_findMethodAltName(IN(RString) classname, IN(RString) funcname, IN(RDmiObjectArray) args, int flags)
{
  return StdDispatch_findMethodAltName(Class::forName(classname)->objectClazzInfo(), funcname, args, flags);
}

RString
StdDispatch_findMethod(IN(RString) spec)
{
  /*
    "public static acdk.lang.StringBuffer acdk.lang.StringBuffer::StringBuffer(in acdk.lang.String)"
  */
  return Nil;
}


void
AcdkObject_Test::standard()
{
  
  acdk::util::logging::RLogger rlogger = acdk::util::logging::LogManager::getRootLogger();

    acdk::util::logging::LogManager::MinLevel = acdk::util::logging::LogManager::Threshold
      = acdk::util::logging::Debug;
    rlogger->addConsumer(new acdk::util::logging::ConsoleConsumer(new acdk::util::logging::SimpleFormatter()));
  
  System::out->println("Dynamic Method Invokation over IIOP doesn't currently not working");
  
  TestAcdkServerHolder _orbserver;

  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  AORB::getAORB().ignoreLocal = true;
  RDmiObjectArray inp;
  RDmiObjectArray outp;
  /*
  {
    inp = new DmiObjectArray(1);
    inp[0] = new DmiObject(inOf(new String("Hello ")));
    RString altname = StdDispatch_findMethodAltName("acdk/lang/StringBuffer", "StringBuffer", inp, MiMiConstructor | MiPublic);
    RCorObject ti = (RCorObject)acdkserver->dyn_new("acdk/lang/StringBuffer", altname, inp, outp);
    inp[0] = new DmiObject(inOf(new String("ACDK")));
    altname = StdDispatch_findMethodAltName("acdk/lang/StringBuffer", "append", inp, MiPublic);
    ti->dyn_invoke(altname, inp, outp);
    inp = new DmiObjectArray(0);
    RString erg = (RString)ti->dyn_invoke("toString", inp, outp)->getObjectVar();
    testAssert(erg->equals("Hello ACDK") == true);
  }*/
  {
    inp = new DmiObjectArray(2);
    inp[0] = new DmiObject(inOf(new String("ACDK")));
    inp[1] = new DmiObject(43);
    RString altname = StdDispatch_findMethodAltName("acdk/tools/aunit/DmiTestClass", "DmiTestClass", inp, MiMiConstructor | MiPublic);

    RCorObject ti = (RCorObject)acdkserver->dyn_new("acdk/tools/aunit/DmiTestClass", altname, inp, outp);
    RDmiObject tobj = ti->dyn_peek("pubInt");
    testAssert(ti->dyn_peek("pubInt")->getIntVar() == 43);
    ti->dyn_poke("pubInt", new DmiObject(61));
    testAssert(ti->dyn_peek("pubInt")->getIntVar() == 61);
    inp = new DmiObjectArray(9);
    inp[0] = new DmiObject(true);
    inp[1] = new DmiObject(char(12));
    inp[2] = new DmiObject(byte(210));
    inp[3] = new DmiObject(short(6001));
    inp[4] = new DmiObject(123456);
    inp[5] = new DmiObject(123456789);
    inp[6] = new DmiObject(float(3.12));
    inp[7] = new DmiObject(double(12345.6789));
    inp[8] = new DmiObject(inOf(new Integer(42)));
    tobj = ti->dyn_invoke("dynamicMethodf", inp, outp);
    testAssert(ti->dyn_invoke("dynamicMethodz", inp, outp)->getBoolVar() == true);
    testAssert(ti->dyn_invoke("dynamicMethodc", inp, outp)->getCharVar() == 12);
    testAssert(ti->dyn_invoke("dynamicMethodb", inp, outp)->getByteVar() == 210);
    testAssert(ti->dyn_invoke("dynamicMethods", inp, outp)->getShortVar() == 6001);
    testAssert(ti->dyn_invoke("dynamicMethodi", inp, outp)->getIntVar() == 123456);
    testAssert(ti->dyn_invoke("dynamicMethodl", inp, outp)->getLongVar() == 123456789);
    ti->dyn_invoke("dynamicMethodf", inp, outp)->getFloatVar(); // testAssert doesn't work because compiler == 3.12);
    testAssert(ti->dyn_invoke("dynamicMethodd", inp, outp)->getDoubleVar() == 12345.6789);
    RCorObject corobj = (RCorObject)ti->dyn_invoke("dynamicMethodO", inp, outp)->getObjectVar();
    //corobj->invoke("equals",
    ////testAssert(->equals(new Integer(42)) == true);
  }
  
}

void
AcdkObject_Test::exceptionHandling()
{
  TestAcdkServerHolder _orbserver;
  AORB::getAORB().ignoreLocal = true;

  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  
  RDmiObjectArray inp;
  RDmiObjectArray outp;
  {
    inp = new DmiObjectArray(2);
    inp[0] = new DmiObject(inOf(new String("ACDK")));
    inp[1] = new DmiObject(43);
    RString altname = StdDispatch_findMethodAltName("acdk/tools/aunit/DmiTestClass", "DmiTestClass", inp, MiMiConstructor | MiPublic);
    RCorObject ti = (RCorObject)acdkserver->dyn_new("acdk/tools/aunit/DmiTestClass", altname, inp, outp);
    try {
      inp = new DmiObjectArray(1);
      inp[0] = new DmiObject(1);
      ti->dyn_invoke("throwExceptionMethod", inp, outp);
    }
    catch(RThrowable ex)
    {
      System::out->println(ex->getMessage());
    }
  }

}


void
AcdkObject_Test::sampleCode()
{
  TestAcdkServerHolder _orbserver;
  AORB::getAORB().ignoreLocal = true;

  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);

  {
    // first the sample code in normal ACDK C++ using
    // the known classes directly

    RString initvalue = "Hello";

    ::acdk::lang::RStringBuffer sb = new ::acdk::lang::StringBuffer(initvalue);
    sb->append(" ACDK");

    ::acdk::io::RPrintWriter out = System::out;
    RString str = sb->toString();
    RString erg = "String should contain 'Hello ACDK': " + str;
    out->println(erg);
  }

  {
    // the same code using the Dynamic Method Invocation
    RString initvalue = "Hello";

    // create a class acdk::lang::StringBuffer and call the constructur
    // with one argument.
    // the new instance will be assigned to sb
    RDmiObjectArray inp = new DmiObjectArray(1);
    inp[0] = new DmiObject(inOf(new String("Hello ")));
    RDmiObjectArray outp;
    RString altname = StdDispatch_findMethodAltName("acdk/lang/StringBuffer", "StringBuffer", inp, MiMiConstructor | MiPublic);

    RCorObject sb = (RCorObject)acdkserver->dyn_new("acdk/lang/StringBuffer", altname, inp, outp);

    // calling a non static method 'append' with one argument
    inp[0] = new DmiObject(inOf(new String("ACDK")));
    altname = StdDispatch_findMethodAltName("acdk/lang/StringBuffer", "append", inp, MiPublic);
    sb->dyn_invoke("append", inp, outp);


    // from the class acdk::lang::System get the static member
    // 'out'
    RCorObject out = (RCorObject)acdkserver->dyn_peek_static("acdk/lang/System", "out")->getObjectVar();

    // call from sb the method 'toString'
    inp = new DmiObjectArray(0); // toString has not argument
    RString str = (RString)sb->dyn_invoke("toString", inp, outp)->getObjectVar();
    RString erg = "String should contain 'Hello ACDK': " + str;

    // call from out (which is a PrintWriter) the the method println
    inp = new DmiObjectArray(1);
    inp[0] = new DmiObject(inOf(erg));
    out->dyn_invoke("println", inp, outp);
  }
}

void
AcdkObject_Test::dmiProxy()
{
  TestAcdkServerHolder _orbserver;
  AORB::getAORB().ignoreLocal = true;

  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  RClass sbClass = StringBuffer::GetClass();
  
  ::acdk::lang::reflect::RMethod defaultConstructor = sbClass->getMethod("StringBuffer", new ClassArray(0)); // get the default constructor
  // corba doesn't overload methods, so you have to receive an uniquefied name
  RString altName = defaultConstructor->getAlternativeName();
  
  // create a StringBuffer object on the server side
  RDmiObjectArray outp;
  RCorObject sbCorObject = (RCorObject)acdkserver->dyn_new("acdk/lang/StringBuffer", altName, new DmiObjectArray(0), outp);
  
  // create a dynamic Proxy for the corba object

  RStringBuffer sb = (RStringBuffer)sbCorObject->createDmiProxy(sbClass);
  // !!! append is not a virtual method, so the server object will not be called
  sb->append("Hello"); 
  // !!! toString is a virtual method, so the server object will be called
  RString erg = sb->toString();


  RDmiObjectArray args = new DmiObjectArray(1);
  args[0] = new DmiObject(56);
  RCorObject testICorObject = (RCorObject)acdkserver->dyn_new("acdk/tools/aunit/SayHelloInterfaceImpl", "SayHelloInterfaceImpl", args, outp);
  ::acdk::tools::aunit::RSayHelloInterface ti = (::acdk::tools::aunit::RSayHelloInterface)testICorObject->createDmiProxy(::acdk::tools::aunit::SayHelloInterface::GetClass());
  // call remote object via DMI over IIOP
  erg = ti->sayHello("ACDK over DMI/IIOP");
  testAssert(erg->equals("Hello to ACDK over DMI/IIOP"));
}

} // orb
} // acdkx
} // tests
