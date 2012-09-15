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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdkx_orb_Server_Test.cpp,v 1.8 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileReader.h>

#include <acdkx/orb/orb.h>
#include <acdkx/orb/ServerDelegate.h>


#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>

#include <acdkx/orb/CDRObjectReader.h>
#include <acdkx/orb/CDRObjectWriter.h>
#include <acdkx/orb/selftests/TestInterface.h>

namespace tests {
namespace acdkx {
namespace arb {

USING_CLASS(::acdk::io::, MemWriter);
USING_CLASS(::acdk::io::, MemReader);
USING_CLASS(::acdkx::orb::, CDRObjectReader);
USING_CLASS(::acdkx::orb::, CDRObjectWriter);

USING_CLASS(::org::omg::CORBA::, ORB);

USING_CLASS(::acdkx::orb::selftests::, TestInterface);
USING_CLASS(::acdkx::orb::selftests::, TestException);

ACDK_DECL_CLASS(TestInterfaceImpl);

class TestInterfaceImpl
: extends ::acdkx::orb::ServerDelegate,
  implements ::acdkx::orb::selftests::TestInterface
{
public:
  TestInterfaceImpl()
  {
  }
  virtual void activate() throw (RTestException, RThrowable)
  {
    System::out->println("TestInterfaceImpl""activate()");
  }
  virtual void hold_requests(OUT(bool) wait_for_completion) throw (RTestException, RThrowable)
  {
    wait_for_completion = false;
  }
  virtual void discard_requests(OUT(bool) wait_for_completion) throw (RTestException, RThrowable)
  {
    THROW1(TestException, "Just to Test the exception");
    wait_for_completion = true;
    
  }

  virtual void deactivate(OUT(bool) etherealize_objects, OUT(bool) wait_for_completion) throw (RTestException, RThrowable)
  {
    etherealize_objects = true;
    wait_for_completion = false;
  }
  virtual RClass getClass() { return TestInterface::GetClass(); }

  virtual int retvalfoo() 
  { 
    System::out->println("retvalfoo() return 42");
    return 42; 
  }
  virtual void outvalue(OUT(int) inval)
  {
    System::out->println("outvalue(OUT(int) inval) return 42");
    inval = 42;
  }
  virtual void invalue(IN(int) inval)
  {
    System::out->println("invalue(OUT(int)" + Integer::toString(inval) + ")");
    //inval = inval + 42;
  }
  virtual void inoutvalue(INOUT(int) inval)
  {
    System::out->println("inoutvalue(INOUT(int)" + Integer::toString((int)inval) + ")");
    inval = (int)inval + 42;
  }
  virtual RString stringTest(IN(RString) ostr, OUT(RString) istr)
  {
    RString tstr = new String("Success: ");
    istr = tstr;
    RString retval = tstr + ostr;
    System::out->println("stringTest(" + ostr + ", " + istr->toString() + ") = " + retval);
    return retval;
  }
  virtual RObject getManager()
  {
    return this;
  }
  virtual oneway void doSomething()
  {
    System::out->println("doSomething start");
    Thread::sleep(1000);
    System::out->println("doSomething end");
  }
};

void foo(OUT(RObject) obj)
{
  obj = new String("Hallo");
}

ACDK_DECL_CLASS(HelloImpl);

class HelloImpl
: extends ::acdkx::orb::ServerDelegate,
  implements ::Hello
{
public:
  virtual RClass getClass() { return Hello::GetClass(); }
  virtual void hello()
  {
    System::out->println("Hello Server");
  }
};


ACDK_DECL_CLASS(TtyImpl);

class TtyImpl
: extends ::acdkx::orb::ServerDelegate,
  implements ::tty
{
public:
  virtual RClass getClass() { return tty::GetClass(); }

  virtual void print(IN(RString) msg)
  {
    System::out->println("msg:" + msg);
  }
};



class Server_Test
{
public:
  static ::acdkx::orb::RServerDelegate  testMico(RORB orb)
  {
    RTtyImpl obj = new TtyImpl();
    orb->impl_is_ready((RObject)obj);
    RString ostr = orb->object_to_string((::org::omg::CORBA::RObject)obj);
    {
      RString fname = "tty.ref";
      ::acdk::io::FileWriter f(fname);
      f.write(ostr->getBytes());
      System::out->println(ostr);
    }
    {
      Rtty ttyobj = (Rtty)orb->string_to_object(ostr);
    }
    return obj;
  }
  static ::acdkx::orb::RServerDelegate testOB(RORB orb)
  {
    RHelloImpl obj = new HelloImpl();
    obj->objectKey()->type_id = "Hello";
    orb->impl_is_ready((RObject)obj);
    RString ostr = orb->object_to_string((::org::omg::CORBA::RObject)obj);
    {
      RString fname = "D:\\programr\\lang\\corba\\lib\\c\\OB-3.2\\ob\\demo\\hello\\Hello.ref";
      ::acdk::io::FileWriter f(fname);
      f.write(ostr->getBytes());
      System::out->println(ostr);
    }
    return obj;
    //RTestInterfaceImpl poamanager = (RTestInterfaceImpl)orb->string_to_object(ostr);
    
    
  }
  
  static int doit(RStringArray args)
  {
    RORB orb = ORB::init();
    ::acdkx::orb::RServerDelegate obsobj = testOB(orb);
    ::acdkx::orb::RServerDelegate micosobj = testMico(orb);
    orb->run();
    return 0;
  }
};

} // namespace arb 
} // namespace acdkx 
} // namespace tests 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdkx::arb::Server_Test::doit, argc, argv, envptr);
}



