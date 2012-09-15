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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/Socket.h,v 1.17 2005/04/30 14:06:54 kommer Exp $

#ifndef acdk_net_Socket_h
#define acdk_net_Socket_h 

#include "SocketImplFactory.h"
#include "SocketImpl.h"
#include "InetAddress.h"
#include "TCPSocketFactory.h" 


#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS)
#  include <netinet/tcp.h>
#endif


namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(Socket);

/** 
  API: Java
  @author Roger Rene Kommer, Maximilian Thoran
  @version $Revision: 1.17 $
  @date $Date: 2005/04/30 14:06:54 $
*/
class ACDK_NET_PUBLIC Socket 
  : public acdk::lang::Object  
{
  ACDK_WITH_METAINFO(Socket)
public:
  virtual ~Socket();
protected: 
   /// Creates an unconnected Socket with a user-specified SocketImpl. 
  Socket(IN(RSocketImpl) impl); 

public:
  /**
    Creates an unconnected socket, with the system-default type of SocketImpl. 
    you have to use connect() to connect to socket
  */
  Socket(bool asStream = true); 
  
  /// Creates a stream socket and connects it to the specified port number at the specified IP address. 
    Socket(IN(RInetAddress) address, int port, bool asStream = true); 
  /// Creates a stream socket and connects it to the specified port number on the named host. 
  Socket(IN(RString) host, int port, bool asStream = true);
  /**
    connect a unconnected socket.
    if timeOut is != 0 this method may return false if socket cannot connect in the given time
  */
  bool connect(IN(RInetAddress) address, int port, int timeOut = -1);
  /**
    connect a unconnected socket.
    if timeOut is != 0 this method may return false if socket cannot connect in the given time
  */
  bool connect(IN(RString) host, int port, int timeOut = -1);
  
  /// Creates a socket and connects it to the specified remote host on the specified remote port. 
  //Socket(RString host, int port, RInetAddress localAddr, int localPort);

  /// Closes this socket. 
  void close();
  /// Returns the address to which the socket is connected. 
  RInetAddress getInetAddress(); // { return _theSocket->address(); }
   /// Returns an input stream for this socket. 
  RReader getInputStream() { return _theSocket->getInputStream(); }
  RReader getReader() { return getInputStream(); }
   /// Tests if SO_KEEPALIVE is enabled. 
  bool getKeepAlive();
   /// Gets the local address to which the socket is bound. 
  RInetAddress getLocalAddress();
   /// Returns the local port to which this socket is bound. 
  int getLocalPort() { return _theSocket->getLocalPort(); }
   /// Returns an output stream for this socket. 
  RWriter getOutputStream() { return _theSocket->getOutputStream(); }
  RWriter getWriter() { return getOutputStream(); }

  /// Returns the remote port to which this socket is connected. 
  int getPort() { return _theSocket->getPort(); }
  /// Get value of the SO_RCVBUF option for this socket, that is the buffer size used by the platform for input on the this Socket. 
  int getReceiveBufferSize();
    /// Get value of the SO_SNDBUF option for this socket, that is the buffer size used by the platform for output on the this Socket. 
  int getSendBufferSize();
  /// Returns setting for SO_LINGER. 
  int getSoLinger() ;
  /// Returns setting for SO_TIMEOUT. 
  int getSoTimeout() ;
  /// Tests if TCP_NODELAY is enabled. 
  bool getTcpNoDelay() ;
  /// Enable/disable SO_KEEPALIVE 
  void setKeepAlive(bool on) ;
  /// get SO_REUSEADDR flag
  bool getReuseAddress();
  /// set SO_REUSEADDR flag
  void setReuseAddress(bool on);

  /// Sets the SO_RCVBUF option to the specified value for this DatagramSocket. 
  void setReceiveBufferSize(int size) ;
  /// Sets the SO_SNDBUF option to the specified value for this DatagramSocket. 
  void setSendBufferSize(int size) ;
  /// Sets the client socket implementation factory for the application. 
  static void setSocketImplFactory(IN(RSocketImplFactory) fac);
  /// Enable/disable SO_LINGER with the specified linger time in seconds. 
  void setSoLinger(bool on, int linger) ;
  /// Enable/disable SO_TIMEOUT with the specified timeout, in milliseconds. 
  void setSoTimeout(int timeout) ;
  /// Enable/disable TCP_NODELAY (disable/enable Nagle's algorithm). 
  void setTcpNoDelay(bool on) ;
  /// Places the input stream for this socket at "end of stream". 
  void shutdownInput() ;
  /// Disables the output stream for this socket. 
  void shutdownOutput() ;
  /// Converts this socket to a String. 
  RString toString() ;
protected:
  static void _init();
  static bool _initialised;
  RSocketImpl _theSocket;
  static RSocketImplFactory _theFactory;
  static bool _factorychanged;
  bool _opened;
  bool _asStream;

  /// Noetig, damit ServerSocket den Konstruktor Socket( RSocketImpl ) benutzen kann
  friend class ServerSocket;

};

} // net
} // acdk

#endif // #define acdk_net_Socket_h

