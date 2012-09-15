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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdk/AdressServer.cpp,v 1.9 2005/02/05 10:45:41 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/FileReader.h>
#include <org/omg/CORBA/ORB.h>
#include "AdressBookImpl.h"


namespace tests {
namespace acdkx {
namespace orb {

  USING_CLASS(::acdkx::orb::, ORB);

class AdressServer
{
public:
  static  int doit(RStringArray args)
  {
    RORB orb = ORB::init();
    RAdressBookImpl adressimpl = new AdressBookImpl();
    orb->impl_is_ready((::acdk::lang::RObject)adressimpl);

    RString ostr = orb->object_to_string((::org::omg::CORBA::RObject)adressimpl);
    {
      RString fname = "./AdressBook.ref";
      ::acdk::io::FileWriter f(fname);
      f.write(ostr->getBytes());
      System::out->println(ostr);
    }
    orb->run();
    return 0;
  }
};

} // namespace orb 
} // namespace acdkx 
} // namespace tests 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdkx::orb::AdressServer::doit, argc, argv, envptr);
}



