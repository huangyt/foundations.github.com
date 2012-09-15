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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/tests/acdkx/rdmi/acdk_rdmi_Basic_Test.cpp,v 1.6 2005/04/30 20:17:39 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/File.h>
#include <acdk/lang/dmi/DmiObject.h>

#include <acdkx/rdmi/RemoteDmiServer.h>
#include <acdkx/rdmi/TcpServer.h>
#include <acdkx/rdmi/BinaryProtocol.h>

#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>

namespace tests {
namespace acdkx {
namespace rdmi {

  
BEGIN_DECLARE_TEST( Basic_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( exception )
END_DECLARE_TEST( Basic_Test  )

BEGIN_DEFINE_TEST( Basic_Test )
  ADD_TEST( Basic_Test, standard ) 
  ADD_TEST( Basic_Test, exception ) 
  
END_DEFINE_TEST( Basic_Test )

using namespace ::acdkx::rdmi;

namespace {


} // anon namespace



void 
Basic_Test::standard()
{
  ::acdk::util::logging::RLogger log = ::acdk::util::logging::LogManager::getCreateLogger("acdkx.rdmi");
  log->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::SimpleFormatter()));
  ::acdk::util::logging::LogManager::MinLevel = ::acdk::util::logging::LogManager::Threshold 
    = ::acdk::util::logging::Debug;
    
  RRemoteDmiServer server = new RemoteDmiServer(new TcpServer(1111), new BinaryProtocol());
  server->startInBackground();
  Thread::sleep(300);
  RRemoteDmiServer client = new RemoteDmiServer(new TcpServer(::acdk::net::InetAddress::getLocalHost(), 1111), new BinaryProtocol());
  /*
  {
  
    ::acdk::lang::dmi::ScriptVarArray args(1);
    args[0] = inOf(42);
    RNumber remoteO = (RNumber)client->createRemoteObject("acdk/lang/Integer", "acdk/lang/Number", args);
    int num = remoteO->intValue();
    remoteO = Nil;
  }
  //client->createRemoteObject("acdk/lang/Integer", "acdk/lang/Number", args);
  {
    RObject remSb = client->createRemoteObject("acdk/lang/StringBuffer", "acdk/lang/Object", inOf("Hello"));
    RString s = (RString)remSb->invoke("toString");
    try {
      remSb->invoke("charAt", -23);
    } catch (RIndexOutOfBoundsException ex) {
      // OK
    }

  }*/
  {
    ::acdk::lang::dmi::RDmiObjectArray args = new ::acdk::lang::dmi::DmiObjectArray(1);
    args[0] = new ::acdk::lang::dmi::DmiObject(inOf("Test"));
    RObject remSb = client->createRemote("acdk/lang/StringBuffer", args);
    client = Nil;
    //RCollection remSb = client->createRemoteObject("acdk/lang/StringBuffer", "acdk/lang/Object", inOf("Hello"));
  }
  server->shutdown();
  server->join();
  //Thread::sleep(500);
}


void
Basic_Test::exception()
{
  RRemoteDmiServer server = new RemoteDmiServer(new TcpServer(1112), new BinaryProtocol());
  server->startInBackground();
  Thread::sleep(300);
  RRemoteDmiServer client = new RemoteDmiServer(new TcpServer(::acdk::net::InetAddress::getLocalHost(), 1112), new BinaryProtocol());
  RObject o = client->createRemoteObject("acdk.lang.StringBuffer", "", inOf((RString)"hello"));
  try {
    o->invoke("substring", inOf(-42), inOf(234234));
  } catch (RIndexOutOfBoundsException ex) {
    ex->printStackTrace();
    System::out->println(ex->getMessage());
  }
  server->shutdown();
}



} // namespace rdmi
} //namespace acdkx
} //namespace tests 

