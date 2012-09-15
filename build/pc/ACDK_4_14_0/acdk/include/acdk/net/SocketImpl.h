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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/SocketImpl.h,v 1.12 2005/04/30 14:06:54 kommer Exp $

#ifndef acdk_net_SocketImpl_h
#define acdk_net_SocketImpl_h

#include <acdk.h>
#include "SocketOptions.h"
#include "InetAddress.h"

namespace acdk {
namespace net {


ACDK_DECL_CLASS(InetAddress);

ACDK_DECL_CLASS(SocketImpl);

/** 
  API: Java
  @author Roger Rene Kommer, Maximilian Thoran
  @version $Revision: 1.12 $
  @date $Date: 2005/04/30 14:06:54 $
*/
class ACDK_NET_PUBLIC SocketImpl 
: extends ::acdk::lang::Object,
  implements ::acdk::net::SocketOptions

{
  ACDK_WITH_METAINFO(SocketImpl)
protected:

  /// The file descriptor object for this socket.
  ::acdk::io::RFileDescriptor fd;

  /// The IP address of the remote end of this socket.
  RInetAddress address;

  /**
    The port number on the remote host to which this socket is connected.
    The is on local format (not network format!)
  */
  int port;

  /**
    The local port number to which this socket is connected
    The is on local format (not network format!)
  */
  int localport;

public:
  SocketImpl();
  virtual ~SocketImpl();
public:
  /**
    Accepts a connection. 
    @param s the socket implementation
    @param timeOut timeout in milliseconds
    @return true if a new connection was accepted
  */
  virtual bool accept(IN(RSocketImpl) s, int timeOut = -1) = 0;
    /// Returns the number of bytes that can be read from this socket without blocking. 
  virtual int available() = 0;
    /// Binds this socket to the specified port number on the specified host. 
  virtual void bind(IN(RInetAddress) host, int port) = 0;
    /// Closes this socket. 
  virtual  void close() = 0;
  /**
    Connects this socket to the specified port number on the specified host. 
    @param address the address to connect
    @param port the port to connect
    @param timeOut timeout in milliseconds. if timeOut == -1 block forever
    @return true if connected. false in case of timeout
  */
  virtual  bool connect(IN(RInetAddress) address, int port, int timeOut = -1) = 0;
  /**
    Connects this socket to the specified port on the named host. 
    @param address the address to connect
    @param port the port to connect
    @param timeOut timeout in milliseconds. if timeOut == -1 block forever
    @return true if connected. false in case of timeout
  */
  virtual  bool connect(IN(RString) host, int port, int timeOut = -1) = 0;
    /// Creates either a stream or a datagram socket. 
  virtual  void create(bool stream) = 0;
    /// Returns the value of this socket's fd field. 
  ::acdk::io::RFileDescriptor getFileDescriptor() { return fd; }
    /// Returns the value of this socket's address field. 
  virtual RInetAddress getInetAddress() { return address; }
    /// Returns an input stream for this socket. 
  virtual  RReader getInputStream() = 0;
    /// Returns the value of this socket's localport field. 
  virtual int getLocalPort() { return localport; }
    /// Returns an output stream for this socket. 
  virtual  RWriter getOutputStream() = 0;
    /// Returns the value of this socket's port field. 
  virtual int getPort() { return port; }
    /// Sets the maximum queue length for incoming connection indications (a request to connect) to the count argument. 
  virtual  void listen(int backlog) = 0;
    /// Places the input stream for this socket at "end of stream". 
  void shutdownInput(); 
    /// Disables the output stream for this socket. 
  void shutdownOutput(); 

/// from SocketOption
  virtual RObject getOption(int optID)
  {
    return getSockOption(fd, optID);
  }
  virtual void setOption(int optID, IN(RObject) value)
  {
    setSockOption(fd, optID, value);
  }

  static RObject getSockOption(IN(::acdk::io::RFileDescriptor) fd, int optId);
  static void setSockOption(IN(::acdk::io::RFileDescriptor) fd, int optID, IN(RObject) value);
public:
    /// Returns the address and port of this socket as a String. 
  RString toString(); 

  friend class Socket;
  friend class ServerSocket;

};

} // net
} // acdk

#endif // acdk_net_SocketImpl_h

