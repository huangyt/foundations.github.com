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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/DatagramSocket.cpp,v 1.1 2005/03/26 15:02:02 kommer Exp $


#include "DatagramSocket.h"

namespace acdk {
namespace net {

DatagramSocket::DatagramSocket()
: _connected(false)
, _localAddress(new InetSocketAddress(InetAddress::getAnyAddress(), 0))
{
  
}
  
DatagramSocket::DatagramSocket(int port)
: _connected(false)
, _localAddress(new InetSocketAddress(InetAddress::getAnyAddress(), 0))
{
  bind(_localAddress);
}

DatagramSocket::DatagramSocket(int port, IN(RInetAddress) laddr)
: _connected(false)
, _localAddress(new InetSocketAddress(laddr, port))
{
  bind(_localAddress);
}

DatagramSocket::DatagramSocket(IN(RInetSocketAddress) bindaddr)
: _connected(false)
, _localAddress(bindaddr)
{
  
  bind(_localAddress);
}

void 
DatagramSocket::bind(IN(RInetSocketAddress) addr)
{
  if (_socket == Nil)
  {
    _socket = new TCPSocket();
    _socket->create(false);
  }
  _socket->bind(_localAddress->getAddress(), _localAddress->getPort());
  //_socket->listen(1);
}

void 
DatagramSocket::close()
{
  _socket->close();

}

void 
DatagramSocket::connect(IN(RInetAddress) address, int port)
{
  if (_socket == Nil)
  {
    _socket = new TCPSocket();
    _socket->create(false);
  }
  //_socket->connect(address, port);
  _connected = true;
}

void 
DatagramSocket::connect(IN(RInetSocketAddress) addr)
{
  if (_socket == Nil)
  {
    _socket = new TCPSocket();
    _socket->create(false);
  }

  //_socket->connect(addr->getAddress(), addr->getPort());
  _connected = true;
}

void 
DatagramSocket::disconnect()
{
  if (_connected == false)
    return;
  _socket->shutdownInput();
  _socket->shutdownOutput();
  _socket->close();
}

bool 
DatagramSocket::getBroadcast()
{
  return RBoolean(_socket->getOption(_SO_BROADCAST))->booleanValue();
}

void 
DatagramSocket::setBroadcast(bool on)
{
  Boolean b(on);
  _socket->setOption(_SO_BROADCAST, &b);
}

  
RInetAddress 
DatagramSocket::getInetAddress()
{
  return _remoteAddress->getAddress();
}

RInetAddress 
DatagramSocket::getLocalAddress()
{
  return _localAddress->getAddress();
}

int 
DatagramSocket::getLocalPort()
{
  return _localAddress->getPort();
}

RInetSocketAddress 
DatagramSocket::getLocalSocketAddress()
{
  return _localAddress;
}

int 
DatagramSocket::getPort()
{
  return _remoteAddress->getPort();
}



RInetSocketAddress 
DatagramSocket::getRemoteSocketAddress()
{
  return _remoteAddress;
}

bool 
DatagramSocket::getReuseAddress()
{
  return RBoolean(_socket->getOption(_SO_REUSEADDR))->booleanValue();
}

void 
DatagramSocket::setReuseAddress(bool on)
{
  Boolean b(on);
  _socket->setOption(_SO_REUSEADDR, &b);
}
  
int 
DatagramSocket::getSendBufferSize()
{
  return RInteger(_socket->getOption(_SO_SNDBUF))->intValue();
}

void 
DatagramSocket::setSendBufferSize(int size)
{
  Integer it(size);
  _socket->setOption(_SO_SNDBUF, &it);
}

int 
DatagramSocket::getReceiveBufferSize()
{
  return RInteger(_socket->getOption(_SO_RCVBUF))->intValue();
}

void 
DatagramSocket::setReceiveBufferSize(int size)
{
  Integer it(size);
  _socket->setOption(_SO_RCVBUF, &it);
  
}

int 
DatagramSocket::getSoTimeout()
{
  return _socket->getSoTimeout();
}

void 
DatagramSocket::setSoTimeout(int timeout)
{
  _socket->setSoTimeout(timeout);
}

int 
DatagramSocket::getTrafficClass()
{
  return RInteger(_socket->getOption(_IP_TOS))->intValue();
}

void 
DatagramSocket::setTrafficClass(int tc)
{
  Integer it(tc);
  _socket->setOption(_IP_TOS, &it);
}

bool 
DatagramSocket::isBound()
{
  return _socket->isBound();
}

bool 
DatagramSocket::isClosed()
{
  return _socket == Nil || _socket->isConnected() == false; 
}

bool 
DatagramSocket::isConnected()
{
  return _socket != Nil && _socket->isConnected(); 
}

void 
DatagramSocket::receive(IN(RDatagramPacket) p)
{
  _socket->receive(p);
}

void 
DatagramSocket::send(IN(RDatagramPacket) p)
{
  if (_connected == false)
    connect(p->getSocketAddress());
  _socket->send(p);
}
  

} // net
} // acdk


