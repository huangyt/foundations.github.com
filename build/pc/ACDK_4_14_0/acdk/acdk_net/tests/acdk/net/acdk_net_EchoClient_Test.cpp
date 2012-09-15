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
// $Header: /cvsroot/acdk/acdk/acdk_net/tests/acdk/net/acdk_net_EchoClient_Test.cpp,v 1.9 2005/02/05 10:45:30 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/InputReader.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/net/Socket.h>
#include <acdk/net/SocketException.h>

namespace tests {
namespace acdk {
namespace net {

using namespace ::acdk::lang;
using namespace ::acdk::io;
using namespace ::acdk::net;

class EchoClient
{
public:
  static int acdkmain(RStringArray args)
  {
    if (args.length() < 3) {
      System::out->println("Need host and port");
      return 1;
    }
    RString host = args[1];
    int port = Integer::parseInt(args[2]);
    try {
    Socket socket(host, port);
    RInputReader in = new InputReader(socket.getInputStream());
    RPrintWriter out = new PrintWriter(socket.getOutputStream());
    while (true) {
      System::out->print("> "); System::out->flush();
      RString line = System::in->readLine();
      out->println(line);
      RString retline = in->readLine();
      System::out->println("< " + retline);
    }
    } catch (::acdk::net::RSocketException ex) {
      System::out->println(ex->getMessage());
    } catch (::acdk::io::RIOException ) {
    }
    return 0;
  }
};

} //namespace net 
} //namespace acdk 
} //  namespace tests



int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdk::net::EchoClient::acdkmain, argc, argv, envptr);
}

