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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_SendReceive.cpp,v 1.17 2005/02/05 10:45:30 kommer Exp $

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

BEGIN_DECLARE_TEST( SendReceive_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( SendReceive_Test  )

BEGIN_DEFINE_TEST( SendReceive_Test )
  ADD_TEST( SendReceive_Test, standard )
END_DEFINE_TEST( SendReceive_Test )

using namespace ::acdk::lang;
using namespace ::acdk::net;

#define MYSOTIME 1


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


class SendThread
: public ::acdk::lang::Thread
{
  int _port;
  RbyteArray _expect;

public:
  SendThread(int port, RbyteArray exp)
    : Thread()
  , _port(port)
  , _expect(exp)
  {
  }
	void run()
	{
    try {
    ServerSocket server(_port, 0);

    RSocket client = server.accept();
    
    //std::cout << "server accept OK" << std::endl;
    RReader in = client->getInputStream();
    RWriter out = client->getOutputStream();
    out->write(_expect);
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
    bool berg = ::acdk::util::Arrays::equals(RbyteArray(&readed), _expect);
    Thread::sleep(300);
    
    // don't close it, let client do that
      //in->close();
      //out->close();
      //client->close();
      //server.close();

    } catch (RIOException ex) {
      System::out->println("Server IOException: " + ex->getMessage());
   } catch (RThrowable ex) {
      ex->printStackTrace();
      System::out->println("Server throwable: " + ex->getMessage());
    }
  }
};

bool sendreceive(RbyteArray ba)
{
  int port = getNextServerPort();
  RThread serverthread = new SendThread(port, ba);
  serverthread->start();
  RThread clientthread = new ReceiveThread(port, ba);
  clientthread->start();

  serverthread->join();
  clientthread->join();
  
  return true;
}

void SendReceive_Test::standard()
{
#if defined(ACDK_OS_BSD)
  System::out->println("SendReceive doesn't work on BSD");
  return;
#endif
  int sleepBetweenTests = 300;
  sendreceive(String("This is a teststring\n").getBytes());
  //System::out->println(SBSTR("Sleep between test: " << sleepBetweenTests));
  //Thread::sleep(sleepBetweenTests);
  //System::out->println(SBSTR("Sleep between test: continue"));
  sendreceive(String("X").getBytes());
  StringBuffer buff;
  for (int i = 0; i < 500; ++i)
  {
    buff.append("asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf");
  }
  //System::out->println(SBSTR("Sleep between test: " << sleepBetweenTests));
  //Thread::sleep(sleepBetweenTests);
  //System::out->println(SBSTR("Sleep between test: continue"));
  sendreceive(buff.toString()->getBytes());
}


} // namespace net
} // namespace acdk
} // namespace tests



