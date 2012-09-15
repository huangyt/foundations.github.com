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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/DatagramSocket.h,v 1.2 2005/03/26 16:12:54 kommer Exp $

#ifndef acdk_net_DatagramSocket_h
#define acdk_net_DatagramSocket_h

#include "DatagramPacket.h"
#include "TCPSocket.h"

namespace acdk {
namespace net {


ACDK_DECL_CLASS(DatagramSocket);

/** 
  Implements UDP Server/Client
  API: Java, simplified

  Different to the Java class hierarchy this implementation
  does not has a DatagramSocketImpl, but relies directly on 
  the TCPSocket.

  @author Roger Rene Kommer
  @version $Revision: 1.2 $
  @date $Date: 2005/03/26 16:12:54 $
*/
class ACDK_NET_PUBLIC DatagramSocket 
: public acdk::lang::Object  
{
  ACDK_WITH_METAINFO(DatagramSocket)
protected:
  int _connected;
  acdk::io::RFileDescriptor _fd;
  RTCPSocket _socket;
  RInetSocketAddress _remoteAddress;
  RInetSocketAddress _localAddress;
  
public:
  DatagramSocket();
  
  DatagramSocket(int port);
  DatagramSocket(int port, IN(RInetAddress) laddr);
  DatagramSocket(IN(RInetSocketAddress) bindaddr);
  void bind(IN(RInetSocketAddress) addr);
  void close();
  void connect(IN(RInetAddress) address, int port);
  void connect(IN(RInetSocketAddress) addr);
  void disconnect();
  bool getBroadcast();
  void setBroadcast(bool on);
  
  RInetAddress getInetAddress();
  RInetAddress getLocalAddress();
  int getLocalPort();
  RInetSocketAddress getLocalSocketAddress();
  int getPort();
  int getReceiveBufferSize();
  RInetSocketAddress getRemoteSocketAddress();
  bool getReuseAddress();
  void setReuseAddress(bool on);
  
  int getSendBufferSize();
  void setSendBufferSize(int size);
  
  void setReceiveBufferSize(int size);
  
  int getSoTimeout();
  void setSoTimeout(int timeout);

  int getTrafficClass();
  void setTrafficClass(int tc);

  bool isBound();
  bool isClosed();
  bool isConnected();
  
  void receive(IN(RDatagramPacket) p);
  void send(IN(RDatagramPacket) p);
  //static void setDatagramSocketImplFactory(DatagramSocketImplFactory fac) 
  
  
};

} // net
} // acdk

#endif // acdk_net_DatagramSocket_h

