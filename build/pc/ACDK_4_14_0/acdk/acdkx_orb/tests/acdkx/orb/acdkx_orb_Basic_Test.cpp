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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdkx_orb_Basic_Test.cpp,v 1.18 2005/03/07 18:59:47 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/lang/System.h>
#include "TestInterfaceImpl.h"
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/LogManager.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>



namespace tests {
namespace acdkx {
namespace orb {
  
BEGIN_DECLARE_TEST( Basic_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( Basic_Test  )

BEGIN_DEFINE_TEST( Basic_Test )
  ADD_TEST( Basic_Test, standard ) 
END_DEFINE_TEST( Basic_Test )

using namespace ::acdk::lang;
using namespace ::acdkx::orb;

RString gTestInterfaceImplReference = Nil;

#define OUTL(statement) \
do { \
  sys::coreout << #statement << "> " << sys::eofl; \
  statement; \
  sys::coreout << #statement << "< " << sys::eofl; \
} while (false)

class BasicTestClient
: extends acdk::lang::Thread
{
public:
  void runTests()
  {
      {
      RORB orb = ORB::init();
      RTestInterface ti = (RTestInterface)orb->string_to_object(gTestInterfaceImplReference);
      OUTL(ti->invalue(42));
      // no dmi by default for ACDK objects like Integer: testAssert(RInteger(ti->getManager())->intValue() == 42);
      int ival = 0;
      ti->outvalue(ival);
      OUTL(ti->outvalue(ival));
      testAssert(ival == 42);
      int ioval = 0;
      OUTL(ti->inoutvalue(ioval));
      testAssert(ioval == 42);
      OUTL(testAssert(ti->retvalfoo() == 0));
      RString str; 
      OUTL(ti->stringTest("Hello", str));
      testAssert(str->equals("Hello returned") == true);
      try {
        ti->throwException(1);
        testAssert(false);
      } catch (RTestException ex) {
        
      }
    }
  
  }
  virtual void run()
  {
    RORB orb = ORB::init();
    while (gTestInterfaceImplReference == Nil)
      Thread::sleep(500);
    
    //runTests();
    AORB::getAORB().ignoreLocal = true;
    runTests();
    orb->shutdown(false);
  }
};


void
Basic_Test::standard()
{
  /*
  acdk::util::logging::RLogger rlogger = acdk::util::logging::LogManager::getRootLogger();
    acdk::util::logging::LogManager::MinLevel = acdk::util::logging::LogManager::Threshold 
      = acdk::util::logging::Debug;
    rlogger->addConsumer(new acdk::util::logging::ConsoleConsumer(new acdk::util::logging::SimpleFormatter()));
  */
  AORB::reset();
  RORB orb = ORB::init();
  RThreadArray ta = new ThreadArray(2);
  ta[0] = new TestInterfaceImplServer();
  ta[1] = new BasicTestClient();
  int i;
  for (i = 0; i < ta->length(); ++i)
    ta[i]->start();
  for (i = 0; i < ta->length(); ++i)
    ta[i]->join();
  gTestInterfaceImplReference  = Nil;
}




ACDK_BCC_RTHROWABLE_DEFINITION_FQ(::org::omg::CORBA::portable::, ApplicationException)
ACDK_BCC_RTHROWABLE_DEFINITION_FQ(::org::omg::CORBA::portable::, RemarshalException)
ACDK_BCC_RTHROWABLE_DEFINITION_FQ(::acdkx::orb::selftests::, TestException)


} // orb
} // acdkx
} // tests
