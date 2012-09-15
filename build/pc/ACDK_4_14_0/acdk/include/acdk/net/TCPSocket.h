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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TCPSocket.h,v 1.20 2005/04/30 14:06:55 kommer Exp $

#ifndef acdk_net_TCPSocket_h
#define acdk_net_TCPSocket_h

#if defined(ACDK_OS_LINUX) || defined(ACDK_OS_SOLARIS) || defined(ACDK_OS_BSD)
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif // ACDK_OS_LINUX

#include "SocketImpl.h"
#include "DatagramPacket.h"

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

#if !defined(__BORLANDC__)
enum ::acdk::io::SeekPos;
#endif //!defined(__BORLANDC__)

ACDK_DECL_CLASS(TCPSocket);

/** 

  API: Java
  @author Maximilian Thoran, Roger Kommer
  @version $Revision: 1.20 $
  @date $Date: 2005/04/30 14:06:55 $
*/

class ACDK_NET_PUBLIC TCPSocket 
: extends SocketImpl
, implements acdk::io::Reader
, implements acdk::io::Writer
, implements acdk::io::Storage 
{
  ACDK_WITH_METAINFO(TCPSocket)
private:
  
  bool _connected;
  bool _eof;
  bool _eofReturned;
  bool _bound;
  foreign struct sockaddr_in _address;
public:
  TCPSocket();
  virtual ~TCPSocket();

   /// Accepts a connection. 
  virtual bool accept(IN(RSocketImpl) s, int timeOut = -1);
  
  /// Returns the number of bytes that can be read from this socket without blocking. 
  virtual int available();
  
  /// Binds this socket to the specified port number on the specified host. 
  virtual void bind(IN(RInetAddress) host, int port);
  
  /// Closes this socket. 
  virtual  void close();
  
  virtual  bool connect(IN(RInetAddress) address, int port, int timeOut = -1);
  
  /// Connects this socket to the specified port on the named host. 
  virtual  bool connect(IN(RString) host, int port, int timeOut = -1);
  
  /// Creates either a stream or a datagram socket. 
  virtual  void create(bool stream);
  
  RInetAddress getInetAddress();
  int getLocalPort();
  /// Returns an input stream for this socket. 
  virtual  RReader getInputStream();
  
  /// Returns an output stream for this socket. 
  virtual  RWriter getOutputStream();
  
  /// Sets the maximum queue length for incoming connection indications (a request to connect) to the count argument. 
  virtual  void listen(int backlog);

  // Methods inherited from Reader
  virtual jlong seek(::acdk::io::SeekPos seekrel, jlong seekpos);
  virtual jlong skip(jlong n);
  virtual int read();
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);  
  virtual void mark(int readAheadLimit)
  {
    THROW0(UnsupportedOperationException);
  }
  virtual bool markSupported() { return false; }
  virtual void reset();
  virtual bool ready();

  // Methods inherited from Writer
  virtual void flush();

  foreign virtual void write(const byte* cstr, int offset, int len);
  virtual void write(byte c);
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
  
  // Methods inherited from Storage
  
  virtual RString getDeviceName() { return toString(); }
  
  virtual bool isWriteable() { return _connected; }
  
  virtual bool isReadable() { return _connected; }
  virtual bool isConnected() { return _connected; }

  void send(IN(RDatagramPacket) packet);
  void receive(IN(RDatagramPacket) packet);

  void setSoTimeout(int timeOut);
  int getSoTimeout();
  bool isBound() { return _bound; }
  foreign static void inetSocketAddress2native(IN(RInetSocketAddress) isa, sockaddr_in& _address);
  foreign static RInetSocketAddress native2InetSocketAddress(const sockaddr_in& _address);
  //RInetAddress address, int portgetSockname
  /**
    return errno. if newErrno == -1 the errno will not be set
  */
  int getSetSocketErrno(int newErrno = 0);
private:
  void _resolveLocalPortAddress();
  /*
  int _setTimeOut(int timeOut);
  void _restoreTimeOut(int timeOut);
  */
};

} // net
} // acdk

#endif // acdk_net_TCPSocket_h

