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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/arb/acdkx_arb_Client_Test.cpp,v 1.5 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/FileReader.h>
#include <acdkx/arb/arb.h>
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


class Client_Test
{
public:
  static int doit(RStringArray args)
  {
    RARB arb = ARB::getARB();
    RString refid;
    {
      ::acdk::io::InputReader f(new ::acdk::io::FileReader((RString)"arb.ref"));
      refid = f.readString();
    }
    System::out->println("Calling refid: " + refid);
    RPOAManager obj = (RPOAManager)arb->string_to_object(refid);

    obj->doSomething();
    obj->doSomething();
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
      RPOAManager tobj = (RPOAManager)obj->getManager();
      tobj->activate();
    }
    try {
      bool dwait = false;
      obj->discard_requests(dwait);
    } catch (RAdapterInactive ex) {
      System::out->println("catched RAdapterInactive" + ex->getMessage());
    } catch (RThrowable ex) {
      System::out->println("catched RThrowable" + ex->getMessage());
    }
    return 0;
  }
};

} // namespace arb 
} // namespace acdkx 
} // namespace tests


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdkx::arb::Client_Test::doit, argc, argv, envptr);
}



