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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_TcpTimeout_Test.cpp,v 1.2 2005/04/30 18:57:16 kommer Exp $

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

BEGIN_DECLARE_TEST( TcpTimeout_Test )
  DECLARE_TEST( acceptTimeOut )
  DECLARE_TEST( connectTimeOut )
  DECLARE_TEST( acceptTimeOut2 )
END_DECLARE_TEST( TcpTimeout_Test  )

BEGIN_DEFINE_TEST( TcpTimeout_Test )
  ADD_TEST( TcpTimeout_Test, acceptTimeOut )
  ADD_TEST( TcpTimeout_Test, connectTimeOut )
  ADD_TEST( TcpTimeout_Test, acceptTimeOut2 )
  
END_DEFINE_TEST( TcpTimeout_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;

/*

int serverPortBase = 1234;

int getNextServerPort()
{
  return ++serverPortBase;
}
int getServerPort()
{
  return serverPortBase;
}

class ReceiveThread
: public ::acdk::lang::Thread
{
  int _port;
  RbyteArray _expect;

public:
  ReceiveThread(int port, RbyteArray exp)
    : Thread()
  , _port(port)
  , _expect(exp)
  {
  }
	void run()
	{
    try {
      Thread::sleep(200);
      //std::cout << "client starting " << std::endl;
      //std::cout << "client connecting" << std::endl;
 #if defined(ACDK_OS_BSD)
      RString localHost = "127.0.0.0";
#else
      RString localHost = InetAddress::getLocalHost()->toString();
#endif
      Socket socket(localHost, _port);
      //socket.setTcpNoDelay(true);
      //socket.setSoLinger(true, MYSOTIME); // wait for completion at close 5 seconds
      RReader in = socket.getInputStream();
      RWriter out = socket.getOutputStream();
      //std::cout << "client start reading" << std::endl;
      byteArray readed(_expect->length());

      int readcount = in->read(&readed);
      if (::acdk::util::Arrays::equals(RbyteArray(&readed), _expect) == false)
        {
        System::out->println(RString("read:\nexpected: ")
                             + _expect->toString()
                             + "\nreaded: "
                             + readed.toString());
        } else {
        System::out->println(RString("client: readed OK: ") + _expect->length());
      }
      out->write(&readed);
      Thread::sleep(100);
      //in->close();
      //out->close();
      //socket.close();
    } catch (RIOException ex) {
      System::out->println("Client IOException: " + ex->getMessage());
    } catch (RThrowable ex) {
      ex->printStackTrace();
      System::out->println("Client throwable: " + ex->getMessage());
    }


	}
};

class ServerHandleThread
: public ::acdk::lang::Thread
{
  RSocket socket;
  ServerHandleThread(IN(RSocket) socket_)
    : socket(socket_)
  {
  }
  void run()
  {
  }
};

*/

ACDK_DECL_CLASS(EchoThread);

class EchoThread
: public ::acdk::lang::Thread
{
  int _port;
  RbyteArray _expect;
  
public:
  bool shutdown;
  EchoThread(int port, IN(RbyteArray) exp = Nil)
  : _port(port)
  , _expect(exp)
  , shutdown(false)
  {
  }
	void run()
	{
    try {
      ServerSocket server(_port, 1, InetAddress::getLocalHost()/*InetAddress::getWildcardHost()*/);
      RSocket client;
      do {
        System::out->println("server.accept(300);");
        client = server.accept(300);
        if (shutdown == true)
          return;
      } while (client == Nil);
      System::out->println("server accepted!");
      RReader in = client->getInputStream();
      RWriter out = client->getOutputStream();
      int r = in->read();
      System::out->println(RString("readed: ") + (char)r);
      out->write(r);
      return;
    } catch (RIOException ex) {
      System::out->println("Server IOException: " + ex->getMessage());
   } catch (RThrowable ex) {
      ex->printStackTrace();
      System::out->println("Server throwable: " + ex->getMessage());
    }
  }
};



void 
TcpTimeout_Test::acceptTimeOut()
{
  ServerSocket ss(1246);
  RSocket s = ss.accept(1000);
  testAssert(s == Nil);
}

void
TcpTimeout_Test::connectTimeOut()
{
  {
    Socket socket(true);
    bool c = socket.connect("123.123.123.123", 1234, 300);
    testAssert(c == false);
  }
  {
    Socket socket(true);
    bool c = socket.connect("212.227.224.238", 80, 3000);
    testAssert(c == true);
  }

}

void
TcpTimeout_Test::acceptTimeOut2()
{
#if defined(ACDK_OS_BSD)
  RString localHost = "127.0.0.1";
#else
  RString localHost = InetAddress::getLocalHost()->toString();
#endif
  System::out->println("LocalHost: " + InetAddress::getDefaultHostName() + ": " + localHost);
  int acceptPort = 1235;
  REchoThread t = new EchoThread(acceptPort);
  t->start();
  Thread::sleep(1000);
  Socket socket(true);
  bool c = socket.connect(localHost, acceptPort, -1);
  testAssert(c == true);
  RReader reader = socket.getReader();
  RWriter writer = socket.getWriter();
  writer->write('A');
  int r = reader->read();
  testAssert(r == 'A');
  t->shutdown = true;
  t->join(1000);
  Thread::sleep(200);
  bool liv = t->isAlive();
  testAssert(t->isAlive() == false);
}

} // namespace net
} // namespace acdk
} // namespace tests



