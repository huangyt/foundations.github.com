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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_InetAddress_Test.cpp,v 1.3 2005/05/09 13:47:58 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/net/Socket.h>
#include <acdk/net/SocketException.h>
#include <acdk/net/ServerSocket.h>
#include <acdk/util/Arrays.h>

namespace tests {
namespace acdk {
namespace net {

BEGIN_DECLARE_TEST( InetAddress_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( InetAddress_Test  )

BEGIN_DEFINE_TEST( InetAddress_Test )
  ADD_TEST( InetAddress_Test, standard )
END_DEFINE_TEST( InetAddress_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;

void
printInetAddresses(IN(RInetAddress) address)
{
  RString host = address->getHostName();
  if (host == Nil)
  {
    System::out->println("InetAddress: " + address->toString());
    return;
  }
  System::out->println("InetAddress: " + address->toString() + "; "+  address->getHostName());
  RInetAddressArray ia = InetAddress::getAllByName(address->getHostName());
  for (int i = 0; i < ia->length(); ++i)
  {
    System::out->println(" also associated with: " + ia[i]->toString() + ", " + ia[i]->getHostName());
  }
}

void
printInetAddresses(IN(RString) host)
{
  System::out->println("InetAddress::getByName(" + host + "): " + InetAddress::getByName(host)->toString());
  RInetAddressArray ia = InetAddress::getAllByName(host);
  for (int i = 0; i < ia->length(); ++i)
  {
    System::out->println(" also associated with: " + ia[i]->toString() + ", " + ia[i]->getHostName());
  }
}

void 
InetAddress_Test::standard()
{
  System::out->println("InetAddress::getDefaultHost():");
  printInetAddresses(InetAddress::getDefaultHostName());
  System::out->println("InetAddress::getLocalHostName():");
  printInetAddresses(InetAddress::getLocalHostName());

  System::out->println("InetAddress::getNullHost():");
  printInetAddresses(InetAddress::getNullHost());

  System::out->println("InetAddress::getWildcardHost():");
  printInetAddresses(InetAddress::getWildcardHost());
  
  System::out->println("InetAddress::getAnyAddress():");
  printInetAddresses(InetAddress::getAnyAddress());

  System::out->println("acdk.sf.net");
  printInetAddresses("acdk.sf.net");

  System::out->println("www.artefaktur.com");
  printInetAddresses("www.artefaktur.com");
   System::out->println("www.acdk.de");
  printInetAddresses("www.acdk.de");
}


} // namespace net
} // namespace acdk
} // namespace tests



