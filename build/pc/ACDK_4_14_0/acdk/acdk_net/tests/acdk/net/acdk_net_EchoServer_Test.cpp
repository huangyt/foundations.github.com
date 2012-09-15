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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_EchoServer_Test.cpp,v 1.7 2005/02/05 10:45:30 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/net/ServerSocket.h>

namespace tests {
namespace acdk {
namespace net {

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::net;

class EchoServer
{
public:
  static int acdkmain(RStringArray args)
  {
    if (args.length() < 2) {
      System::out->println("Need port");
      return 1;
    }
    int port = Integer::parseInt(args[1]);
    try {
      ServerSocket server(port);
      RInetAddress iaddr = server.getInetAddress();
      if (iaddr == Nil)
        iaddr = new InetAddress(0);
      System::out->println("Login on " 
            + iaddr->toString() + ":" 
            + server.getLocalPort() 
            + ".\nExit with a single x");
      RSocket client = server.accept();
      RInputReader in = new InputReader(client->getInputStream());
      RPrintWriter out = new PrintWriter(client->getOutputStream());
      while (true) {
        RString str = in->readLine();
        System::out->println(str);
        if (str->equals("x") == true)
          break;
        out->println(str);
      }
    } catch (::acdk::io::RIOException ) {
    }
    return 0;
  }
};

} //namespace net 
} //namespace acdk 
} //namespace tests



int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdk::net::EchoServer::acdkmain, argc, argv, envptr);
}
