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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/TestInterfaceImpl.h,v 1.10 2005/02/05 10:45:41 kommer Exp $

#ifndef tests_acdkx_orb_TestInterfaceImpl_h
#define tests_acdkx_orb_TestInterfaceImpl_h

#include <acdkx/orb/selftests/TestInterfaceImpl.h>
#include <acdk/lang/Throwable.h>
#include <acdkx/orb/AORB.h>
#include <acdkx/orb/AcdkObject.h>

namespace tests {
namespace acdkx {
namespace orb {
  

USING_CLASS(::acdkx::orb::selftests::, TestInterface);
USING_CLASS(::acdkx::orb::selftests::, TestInterfaceImpl);
USING_CLASS(::acdkx::orb::selftests::, TestException);


using ::acdkx::orb::ServerDelegate;


extern RString gTestInterfaceImplReference;

class TestInterfaceImplServer
: extends acdk::lang::Thread
{
public:
  virtual void run()
  {
    ::org::omg::CORBA::RORB orb = ::org::omg::CORBA::ORB::init();
    RTestInterfaceImpl ti = new TestInterfaceImpl();
    orb->impl_is_ready((::acdk::lang::RObject)ti);
    gTestInterfaceImplReference = orb->object_to_string((::org::omg::CORBA::RObject)ti);
    orb->run();
  }
};



class TestAcdkImplServer
: extends acdk::lang::Thread
{
public:
  virtual void run()
  {
    ::org::omg::CORBA::RORB orb = ::org::omg::CORBA::ORB::init();
    ::acdkx::orb::RAcdkObject ti = new ::acdkx::orb::AcdkObject(new acdk::lang::Object());
    orb->impl_is_ready((::acdk::lang::RObject)ti);
    gTestInterfaceImplReference = orb->object_to_string((::org::omg::CORBA::RObject)ti);
    orb->run();
  }
};


class TestAcdkServerHolder
{
public:
  RThread server;
  ::org::omg::CORBA::RORB orb;
  TestAcdkServerHolder()
  {
    ::acdkx::orb::AORB::reset();
    orb = ::org::omg::CORBA::ORB::init();
  
    server = new TestAcdkImplServer();
    server->start();
    while (gTestInterfaceImplReference == Nil)
      Thread::sleep(100);
  }
  ~TestAcdkServerHolder()
  {
    gTestInterfaceImplReference = Nil;
    orb->shutdown(false);
    server->join();
  }
};



} // orb
} // acdkx
} // tests

#endif //tests_acdkx_orb_TestInterfaceImpl_h





