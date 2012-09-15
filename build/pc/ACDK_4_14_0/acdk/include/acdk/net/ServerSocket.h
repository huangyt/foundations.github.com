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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ServerSocket.h,v 1.14 2005/04/30 14:06:54 kommer Exp $

#ifndef acdk_net_ServerSocket_h
#define acdk_net_ServerSocket_h

#include "net.h"
#include "SocketImplFactory.h"
#include "Socket.h"
#include "SocketImpl.h"
#include "InetAddress.h"
#include "TCPSocketFactory.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(ServerSocket);

/** 

  API: Java
  @author Roger Rene Kommer, Maximilian Thoran
  @version $Revision: 1.14 $
  @date $Date: 2005/04/30 14:06:54 $
*/


class ACDK_NET_PUBLIC ServerSocket 
: public acdk::lang::Object  
{
  ACDK_WITH_METAINFO(ServerSocket)
public:
  /**
    Creates a server socket on a specified port. 
    if port == -1 os should select a port
  */
  ServerSocket(int port = -1);
  virtual ~ServerSocket();
  /// Creates a server socket and binds it to the specified local port number. 
  ServerSocket(int port, int backlog);
  /// Create a server with the specified port, listen backlog, and local IP address to bind to. 
  ServerSocket(int port, int backlog, IN(RInetAddress) bindAddr);
  /**
    Listens for a connection to be made to this socket and accepts it. 
    @param timeOut in ms. if timeOut is -1 blocks forever
    @return return Nil if timeout is reached.
  */
  RSocket accept(int timeOut = -1);
  /// Closes this socket.           
  void close();
  /// Returns the local address of this server socket.           
  RInetAddress getInetAddress();
  /// Returns the port on which this socket is listening.           
  int getLocalPort();
  /// Retrive setting for SO_TIMEOUT.           
  int getSoTimeout();
  /// Sets the server socket implementation factory for the application.           
  static void setSocketFactory(IN(RSocketImplFactory) fac);
  /// Enable/disable SO_TIMEOUT with the specified timeout, in milliseconds.           
  void setSoTimeout(int timeout);
  /// get SO_REUSEADDR flag
  bool getReuseAddress() { return Socket(_theSocket).getReuseAddress(); }
  /// set SO_REUSEADDR flag
  void setReuseAddress(bool on) { Socket(_theSocket).setReuseAddress(on); }
  /// Tests if TCP_NODELAY is enabled. 
  bool getTcpNoDelay()  { return Socket(_theSocket).getTcpNoDelay(); }
  /// Enable/disable TCP_NODELAY (disable/enable Nagle's algorithm). 
  void setTcpNoDelay(bool on) { Socket(_theSocket).setTcpNoDelay(on); }
  
  /// Returns the implementation address and implementation port of this socket as a String.           
  RString toString();
  /// Subclasses of ServerSocket use this method to override accept() to return their own subclass of socket.           
protected:
  bool implAccept(IN(RSocket) s, int timeOut);

private:
  RSocketImpl _theSocket;
  int _backlog;
};

} // net
} // acdk

#endif // acdk_net_ServerSocket_h

