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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdkx_orb_Client_Test.cpp,v 1.6 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/FileReader.h>
#include <org/omg/CORBA/ORB.h>
#include <org/omg/PortableServer/POA.h>
#include <acdkx/orb/selftests/TestInterface.h>

namespace tests {
namespace acdkx {
namespace arb {

USING_CLASS(::org::omg::CORBA::, ORB);

USING_CLASS(::acdkx::orb::selftests::, TestInterface);
USING_CLASS(::acdkx::orb::selftests::, TestException);





class Client_Test
{
public:
  static void connectToMicoHello(RORB orb)
  {
    RString refid = "IOR:010000000c00000049444c3a7474793a312e30000200000000000000400000000101000024000000524f47534552562e6b6f6d6d65722e726f67736572762e6d6963726f6d6174612e646500471200000c000000424f41c0a80001000010cc030100000024000000010000000100000001000000140000000100000001000100000000000901010000000000";
    Rtty obj = (Rtty)orb->string_to_object(refid);
    obj->print("hallo");
  }
  static void connectToOBHello(RORB orb)
  {
    RString refid;
    {
      RString fname = "D:\\programr\\lang\\corba\\lib\\c\\OB-3.2\\ob\\demo\\hello\\Hello.ref";

      ::acdk::io::InputReader f(new ::acdk::io::FileReader(fname));
      refid = f.readString();
    }
    System::out->println("Calling refid: " + refid);
    RHello obj = (RHello)orb->string_to_object(refid);
    obj->hello();
  }
  static int doit(RStringArray args)
  {
    RORB orb = ORB::init();
    //connectToMicoHello(orb);
    //connectToOBHello(orb);
    return 0;


    System::out->println("PID: " + Integer::toString(Process::getProcessId()));

    RString refid;
    {
      ::acdk::io::InputReader f(new ::acdk::io::FileReader((RString)"orb.ref"));
      refid = f.readString();
    }
    
    
    

    System::out->println("Calling refid: " + refid);
    RTestInterface obj = (RTestInterface)orb->string_to_object(refid);
    
    if (true) {
    //obj->doSomething();
    //obj->doSomething();
    obj->activate();
    
    int i = obj->retvalfoo();

    System::out->println("obj->retvalfoo(): " + Integer::toString(obj->retvalfoo()));
    int outval;
    obj->outvalue(outval);
    System::out->println("obj->invalue(inval): " + Integer::toString(outval));
    obj->invalue(outval);
    outval = 12;
    obj->inoutvalue(outval);
    System::out->println("obj->invalue(12):" + Integer::toString(outval));
    obj->inoutvalue(outval);
    {
    RString ret;
    RString istr;
    RString ostr = "Test";
    ret = obj->stringTest(ostr, istr);
    System::out->println("stringTest(" + ostr + ", " + istr + ") = " + ret);
    }
    {
      RTestInterface tobj = (RTestInterface)obj->getManager();
      tobj->activate();
    }
    }
    
    
    try {
      bool dwait = false;
      obj->discard_requests(dwait);
    } catch (RTestException ex) {
      System::out->println("catched RTestException" + ex->getMessage());
    } catch (RThrowable ex) {
      System::out->println("catched RThrowable" + ex->getMessage());
    }
    return 0;
  }
};

} // namespace arb 
} // namespace acdkx 
} // namespace test 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdkx::arb::Client_Test::doit, argc, argv, envptr);
}



