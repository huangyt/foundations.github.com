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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/arb/APOAManager.h,v 1.6 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <org/omg/PortableServer/POAManager.h>

namespace tests {
namespace acdkx {
namespace arb {



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
  virtual void activate() THROWS1(RAdapterInactive)
  {
    System::out->println("APOAManager""activate()");
  }
  virtual void hold_requests(IN(bool) wait_for_completion) THROWS1(RAdapterInactive)
  {
    wait_for_completion = false;
  }
  virtual void discard_requests(IN(bool) wait_for_completion) THROWS1(RAdapterInactive)
  {
    wait_for_completion = true;
  }

  virtual void deactivate(IN(bool) etherealize_objects, IN(bool) wait_for_completion) THROWS1(RAdapterInactive)
  {
    etherealize_objects = true;
    wait_for_completion = false;
  }
};

void foo(IN(RObject) obj)
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
    arb->impl_is_ready(obj);

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



