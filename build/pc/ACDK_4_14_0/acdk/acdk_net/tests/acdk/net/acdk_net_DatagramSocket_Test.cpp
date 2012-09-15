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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_DatagramSocket_Test.cpp,v 1.1 2005/03/26 15:02:02 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/Double.h>
#include <acdk/net/DatagramSocket.h>

namespace tests {
namespace acdk {
namespace net {
  

int getNextServerPort();
int getServerPort();


BEGIN_DECLARE_TEST( DatagramSocket_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( DatagramSocket_Test  )

BEGIN_DEFINE_TEST( DatagramSocket_Test )
  ADD_TEST( DatagramSocket_Test, standard ) 
END_DEFINE_TEST( DatagramSocket_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;

ACDK_DECL_CLASS(UDPServer);

class UDPServer
: extends ::acdk::lang::Thread
{
public:
  virtual void run()
  {
    RDatagramSocket _sock = new DatagramSocket(8888, InetAddress::getLocalHost());

    CoreByteBuffer buf(10, 10, 0);
    DatagramPacket dp(&buf);

    _sock->receive(&dp);
    System::out->println("server received: " + dp.getSocketAddress()->toString() + "; " + Buffers::toString(&buf));
    
    _sock->send(&dp);
    //Thread::sleep(3000);
    _sock->close();
  }

};




void DatagramSocket_Test::standard()
{
  RUDPServer server = new UDPServer();
  server->start();
  Thread::sleep(300);
  CoreByteBuffer buf(10, 10, 0);
  int i = 0;
  for (byte* it = buf.begin(); it != buf.end(); ++it, ++i)
    *it = i;
  
  DatagramPacket dp(&buf, InetAddress::getLocalHost(), 8888);

  RDatagramSocket sock = new DatagramSocket();
  sock->send(&dp);
  System::out->println("client send: " + Buffers::toString(&buf));

  sock->receive(&dp);
  System::out->println("client received: " + Buffers::toString(&buf));
  server->join();
}


} // namespace net
} // namespace acdk
} // namespace tests



