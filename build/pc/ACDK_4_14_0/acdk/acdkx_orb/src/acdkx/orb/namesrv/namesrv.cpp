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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/namesrv/namesrv.cpp,v 1.9 2005/02/05 10:45:40 kommer Exp $

#include <acdk.h>
#include <acdk/lang/CmdLineParser.h>
#include <acdk/lang/CmdLineParseException.h>
#include <org/omg/CORBA/ORB.h>
#include <acdkx/orb/AORB.h>
#include "CosNamingImpl.h"


namespace acdkx {
namespace orb {
namespace namesrv {

USING_CLASS(::org::omg::CORBA::, ORB);

using namespace ::org::omg::CosNaming;

class NameSrv
{
public :
  static int acdkmain(RStringArray args)
  {
    
    RORB orb = ORB::init(args);
    acdk::util::RProperties props = System::getProperties();
    
    //props->setProperty("acdkx-orb-port", Integer::toString(::acdkx::orb::StdOrbPort));
    acdk::lang::CmdLineParser cmdparser;
    cmdparser.addOption(new acdk::lang::CmdLineOption("-port", "acdkx-orb-port", true, "Port address where the deamon can be found"));
    try {
      cmdparser.parse(System::getProperties(), args);
    } catch (::acdk::lang::RCmdLineParseException cmdlineex) {
      System::out->println(cmdlineex->getMessage());
      return -1;
    }
    if (System::getProperty("ACDKHOME") == Nil) {
      //System::out->println("Error: Enviromnet ACDKHOME is not set");
      return -1;
    }

    RNamingContextImpl rootnc = new NamingContextImpl();

    orb->impl_is_ready(&rootnc);
    
    RString ostr = orb->object_to_string((::org::omg::CORBA::RObject)rootnc);

    {
      RString fname = ::acdk::io::File::concat(System::getProperty("ACDKHOME"), "cfg/acdk_orb_namesrv.ref");
      ::acdk::io::FileWriter f(fname);
      f.write(ostr->getBytes());
      System::out->println(ostr);
    }
    orb->run();
    return 0;
  }
};


} //namespace namesrv 
} //namespace orb 
} //namespace acdkx 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(acdkx::orb::namesrv::NameSrv::acdkmain, argc, argv, envptr);
}




