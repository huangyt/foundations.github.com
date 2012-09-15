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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/tests/acdkx/orb/acdk/AdressClient.cpp,v 1.12 2005/02/05 10:45:41 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/FileReader.h>
#include <acdk/net/SocketException.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/LogManager.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>


#include <org/omg/CORBA/ORB.h>
#include <org/omg/CORBA/Repository.h>
#include <org/omg/CosNaming/CosNaming.h>
#include <acdkx/orb/AORB.h>
#include <acdkx/orb/AcdkObject.h>

#include "IdlMappingTest.h"
#include "AdressBookImpl.h"


namespace tests {
namespace acdkx {
namespace orb {

USING_CLASS(::org::omg::CORBA::, ORB);






class AdressClient
{
public:
  static int doit(RStringArray args)
  {
     
  acdk::util::logging::RLogger rlogger = acdk::util::logging::LogManager::getRootLogger();
    acdk::util::logging::LogManager::MinLevel = acdk::util::logging::LogManager::Threshold 
      = acdk::util::logging::Debug;
    rlogger->addConsumer(new acdk::util::logging::ConsoleConsumer(new acdk::util::logging::SimpleFormatter()));
  

    try {
    RORB orb = ORB::init(args);
    //::acdkx::orb::RAcdkObject own = new ::acdkx::orb::AcdkObject(new AdressBookImpl());
    RAdressBookImpl own = new AdressBookImpl();
    orb->impl_is_ready((RObject)own);
    ::acdkx::orb::RAORB(orb)->start();
    /*
    
    {
      RString irref = "corbaloc::rogwork:8888/InterfaceRepository";
      ::org::omg::CORBA::RObject obj = orb->string_to_object(irref);
      ::org::omg::CORBA::IRObjectProxy iorproxy((::acdkx::orb::RCorObject)obj);
      //::org::omg::CORBA::DefinitionKind defkind = iorproxy.get_def_kind();
      //iorproxy.destroy();
      RString nsref = "corbaloc::rogwork:8889/NameService";
      ::org::omg::CORBA::RObject nso = orb->string_to_object(nsref);
      ::org::omg::CosNaming::RNamingContext nctx(nso);
      //::org::omg::CosNaming::RNamingContext nnctx = nctx->new_context();
      //nctx->bind("iorproxy", iorproxy);
      ::org::omg::CosNaming::RBindingArray bl;
      ::org::omg::CosNaming::RBindingIterator bi;
      nctx->list (20, bl, bi);
      
      ::org::omg::CosNaming::RNameComponentArray nca = new ::org::omg::CosNaming::NameComponentArray(1);
      //nca[0] = new ::org::omg::CosNaming::NameComponent("acdkx", "dir");
      nca[0] = new ::org::omg::CosNaming::NameComponent("irp", "ref");
      nctx->bind(nca, obj);
      nctx->list (20, bl, bi);
      StringBuffer sb;
      for (int i = 0; i < bl->length(); ++i)
      {
        sb << "\n";
        for (int j = 0; j < bl[i]->binding_name->length(); ++j)
        {
          sb << " / " << bl[i]->binding_name[j]->id << ":" <<  bl[i]->binding_name[j]->kind;
        }
      }
      System::out->println(sb.toString());
      //return 0;
    }
    */
    
    RString refid;
    {
       RString fname = "./AdressBook.ref";
      ::acdk::io::InputReader f(new ::acdk::io::FileReader(fname));
      refid = f.readString();
    }
    System::out->println("Calling refid: " + refid);
    RAdressBook obj = (RAdressBook)orb->string_to_object(refid);
    obj->ping();
    RStringArray ergs;
    intArray ia(3);
    ia[0] = 42;
    ia[1] = 43;
    ia[2] = 44;
    obj->testArray(SR(intArray, ia), ergs);
    for (int i = 0; i < ergs->length(); i++) {
      System::out->println(ergs[i]);
    }
    RAdressInfo ai = new AdressInfo();
    ai->name = "roger";
    ai->street = "mainstreet";
    ai->streetnumber = 41;
    RString name = "Kommer";
    System::out->println("Set Address: for " + name + ": " + ai->toString());
    obj->setAddressInfo(name, ai);
    RAdressInfo ai2 = obj->getAddressInfoA(name);
    System::out->println("Retrived Address: for " + name + ": " + ai2->toString());
    RAdressInfo ai3;
    obj->getAddressInfoB(name, ai3);
    System::out->println("Retrived Address: for " + name + ": " + ai3->toString());
    
    obj->setOtherAdressBook(&obj);
    RAdressBook own2;
     obj->getOtherAdressBook(own2);
    obj->setOtherAdressBook(&own);
    RAdressBook own3;
     obj->getOtherAdressBook(own3);
    return 0;
    } catch (::acdk::net::RSocketException sockex) {
      System::out->println(sockex->getMessage());
      return 1;
    }

  }
};

} // namespace orb 
} // namespace acdkx 
} // namespace tests 


int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdkx::orb::AdressClient::doit, argc, argv, envptr);
}



