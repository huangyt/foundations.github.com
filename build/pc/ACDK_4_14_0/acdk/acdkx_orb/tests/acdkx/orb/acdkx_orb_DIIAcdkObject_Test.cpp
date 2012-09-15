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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdkx_orb_DIIAcdkObject_Test.cpp,v 1.9 2005/04/21 08:28:52 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/lang/System.h>
#include "TestInterfaceImpl.h"
#include <acdkx/orb/CorObject.h>
#include <acdkx/orb/AcdkObject.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/LogManager.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>



namespace tests {
namespace acdkx {
namespace orb {
  
BEGIN_DECLARE_TEST( DIIAcdkObject_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( factory )
  DECLARE_TEST( dmi )
  DECLARE_TEST( exceptions )
END_DECLARE_TEST( DIIAcdkObject_Test  )

BEGIN_DEFINE_TEST( DIIAcdkObject_Test )
  ADD_TEST( DIIAcdkObject_Test, standard ) 
  ADD_TEST( DIIAcdkObject_Test, factory) 
  ADD_TEST( DIIAcdkObject_Test, dmi ) 
  ADD_TEST( DIIAcdkObject_Test, exceptions ) 
  
END_DEFINE_TEST( DIIAcdkObject_Test )

using namespace ::acdk::lang;
using namespace ::acdkx::orb;


RString
StdDispatch_findMethodAltName(IN(RString) classname, IN(RString) funcname, IN(RDmiObjectArray) args, int flags);
void
DIIAcdkObject_Test::standard()
{
  System::out->println("ORB DII is broken in this release, we may fix it if somebody request");
  return;
  /*
  acdk::util::logging::RLogger rlogger = acdk::util::logging::LogManager::getRootLogger();
    acdk::util::logging::LogManager::MinLevel = acdk::util::logging::LogManager::Threshold 
      = acdk::util::logging::Debug;
    rlogger->addConsumer(new acdk::util::logging::ConsoleConsumer(new acdk::util::logging::SimpleFormatter()));
  
  TestAcdkServerHolder _orbserver;
  
  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  AORB::getAORB().ignoreLocal = true;
  RDmiObjectArray inp;
  RDmiObjectArray outp;
  RCorObject ti;
  {
    inp = new DmiObjectArray(1);
    inp[0] = new DmiObject(inOf(""));
    //inp[0] = new DmiObject(new String("Hello "));
    RString altname = StdDispatch_findMethodAltName("acdk/cfgscript/Script", "Script", inp, ::acdk::lang::dmi::MiPublic);
    ti = (RCorObject)acdkserver->dyn_new("acdk/cfgscript/Script", altname, inp, outp);
  }
  ti->setRemoteClass(Class::forName("acdk/cfgscript/Script"));

  ti->invoke("eval", (const char*)"out.println(\"hello on server\");", New("acdk/cfgscript/Props"), 0x02);
  //ti->invoke("append", (const char*) "ACDK");
  //RString erg = (RString)ti->invoke("toString");
  //testAssert(erg->equals("Hello ACDK") == true);
  */
}


void
DIIAcdkObject_Test::factory()
{
  /*
  acdk::util::logging::RLogger rlogger = acdk::util::logging::LogManager::getRootLogger();
    acdk::util::logging::LogManager::MinLevel = acdk::util::logging::LogManager::Threshold 
      = acdk::util::logging::Debug;
    rlogger->addConsumer(new acdk::util::logging::ConsoleConsumer(new acdk::util::logging::SimpleFormatter()));
  
   */
  /* 
   TestAcdkServerHolder _orbserver;
  
  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  AORB::getAORB().ignoreLocal = true;
  RCorObject sbfactory = (RCorObject)acdkserver->get_cor_factory("acdk/lang/StringBuffer");
  sbfactory->setRemoteClass(StringBuffer::GetClass());
  RCorObject sb = (RCorObject)sbfactory->invoke("createCorStringBuffer", (const char*)"Hello "); //"createCor_3_StringBuffer"
  sb->setRemoteClass(StringBuffer::GetClass());
  sb->invoke("append", "ACDK");
  RString erg = (RString)sb->invoke("toString");
  testAssert(erg->equals("Hello ACDK") == true);
  */
  
}

void 
DIIAcdkObject_Test::dmi()
{
  /*
  TestAcdkServerHolder _orbserver;
  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  AORB::getAORB().ignoreLocal = true;

  RCorObject sysfactory = (RCorObject)acdkserver->get_cor_factory("acdk/lang/System");
  sysfactory->setRemoteClass(System::GetClass());
  RCorObject out = (RCorObject)sysfactory->peek("out");
  out->setRemoteClass(acdk::io::PrintWriter::GetClass());
  out->invoke("println9", "Hello ACDK");
  */

}


void 
DIIAcdkObject_Test::exceptions()
{
#if 0
  TestAcdkServerHolder _orbserver;
  RCorObject acdkserver = new CorObject(gTestInterfaceImplReference);
  AORB::getAORB().ignoreLocal = true;
  /*

  RCorObject tifactory = acdkserver->get_cor_factory("acdkx/orb/selftests/TestInterfaceImpl");
  tifactory->setRemoteClass("acdkx/orb/selftests/TestInterfaceImpl");
  RCorObject ti = (RCorObject)tifactory->invoke("createCorTestInterfaceImpl");
  ti->setRemoteClass("acdkx/orb/selftests/TestInterfaceImpl");
  try {
    ti->invoke("throwException", 1);
  } 
  catch (RTestException ex)
  {

  }
  catch (RThrowable ex)
  {

  }
  */
  RCorObject tifactory = (RCorObject)acdkserver->get_cor_factory("acdk/tools/aunit/DmiTestClass");
  tifactory->setRemoteClass("acdk/tools/aunit/DmiTestClass");
  RCorObject ti = (RCorObject)tifactory->invoke("createCorDmiTestClass1");
  ti->setRemoteClass("acdk/tools/aunit/DmiTestClass");
  try {
    ti->invoke("throwExceptionMethod", 1);
  } 
  catch (RNumberFormatException ex)
  {
  }
  catch (RTestException ex)
  {

  }
  catch (RThrowable ex)
  {

  }
#endif //0
}


} // orb
} // acdkx
} // tests
