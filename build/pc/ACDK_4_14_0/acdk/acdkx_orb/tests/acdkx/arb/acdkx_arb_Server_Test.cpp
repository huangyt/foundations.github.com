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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/arb/acdkx_arb_Server_Test.cpp,v 1.5 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/FileReader.h>

#include <acdkx/arb/arb.h>
#include <acdkx/arb/AObjectImpl.h>
#include <org/omg/PortableServer/POA.h>
#include <org/omg/PortableServer/POAManager.h>

namespace tests {
namespace acdkx {
namespace arb {

USING_CLASS(::acdkx::arb::, ARB);
USING_CLASS(::acdkx::arb::, ObjectID);

USING_CLASS(::org::omg::PortableServer::, AdapterInactive);
USING_CLASS(::org::omg::PortableServer::, POAManager);

ACDK_DECL_CLASS(APOAManager);

class APOAManager
: //extends ::acdk::lang::Object,
  extends ::acdkx::arb::AObjectImpl,
  implements ::org::omg::PortableServer::POAManager
{
public:
  APOAManager()
  {
  }
  virtual void activate() throw (RAdapterInactive, RThrowable)
  {
    System::out->println("APOAManager""activate()");
  }
  virtual void hold_requests(OUT(bool) wait_for_completion) throw (RAdapterInactive, RThrowable)
  {
    wait_for_completion = false;
  }
  virtual void discard_requests(OUT(bool) wait_for_completion) throw (RAdapterInactive, RThrowable)
  {
    THROW1(AdapterInactive, "Just to Test the exception");
    wait_for_completion = true;
    
  }

  virtual void deactivate(OUT(bool) etherealize_objects, OUT(bool) wait_for_completion) throw (RAdapterInactive, RThrowable)
  {
    etherealize_objects = true;
    wait_for_completion = false;
  }
  virtual RClass getClass() { return ::org::omg::PortableServer::POAManager::GetClass(); }

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
    inval = (int)inval + 42;
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
    RString ret = tstr + ostr;
    System::out->println("stringTest(" + ostr + ", " + istr.value() + ") = " + ret);
    return ret;
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

class Server_Test
{
public:
  static int doit(RStringArray args)
  {
    /*
    RObject tobj;
    foo(tobj);
  */
    RARB arb = ARB::getARB();

    RAPOAManager obj = new APOAManager();
    RObjectID objid = arb->impl_is_ready(obj);
    {
      ::acdk::io::FileWriter f(RString("arb.ref"));
      f.write(objid->toString()->c_str());
      System::out->println(objid->toString());
    }

    //RObjectID objid = new ObjectID("XML", "local", "0", Integer::toString((int)obj.impl()));
    
    /*
    RPOAManager remoteProxy = POAManager::GetProxy(objid);
    remoteProxy->activate();
    
    RPOAManager localProxy = POAManager::GetProxy((RObject)obj);
    localProxy->activate();
    */
    arb->run();
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



