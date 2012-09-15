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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/ServerSocket.cpp,v 1.19 2005/04/30 14:06:54 kommer Exp $




#include "netsysincl.h"


#include "ServerSocket.h"
#include "SocketException.h"
#include "TCPSocketFactory.h"
#include "SocketOptions.h"

namespace acdk {
namespace net {

ServerSocket::ServerSocket(int port)
{
  Socket::_init();
  _theSocket = Socket::_theFactory->createSocketImpl();
  RInetAddress myAddress;
  if (port == 0 || port == -1)
    myAddress = InetAddress::getNullHost(); //INADDR_ANY 
  else
    myAddress = InetAddress::getLocalHost();
  _theSocket->create( true );
  _theSocket->setOption(_SO_REUSEADDR, &Boolean::getTRUE());
  _theSocket->bind( myAddress, port );
  _backlog = 50;
}

ServerSocket::~ServerSocket()
{
  close();
}

ServerSocket::ServerSocket(int port, int backlog)
{
  Socket::_init();
  _theSocket = Socket::_theFactory->createSocketImpl();
  _theSocket->create( true );
  RInetAddress myAddress = InetAddress::getLocalHost();
  _theSocket->setOption(_SO_REUSEADDR, &Boolean::getTRUE());
  _theSocket->bind( myAddress, port );
  _backlog = backlog;
}

ServerSocket::ServerSocket(int port, int backlog, IN(RInetAddress) bindAddr)
{
  Socket::_init();
  _theSocket = Socket::_theFactory->createSocketImpl();
  _theSocket->create( true );
  _theSocket->setOption(_SO_REUSEADDR, &Boolean::getTRUE());
  _theSocket->bind( bindAddr, port );
  _backlog = backlog;
}

RSocket 
ServerSocket::accept(int timeOut)
{
  RSocketImpl impl= Socket::_theFactory->createSocketImpl();
  _theSocket->listen(_backlog);
  if (_theSocket->accept(impl, timeOut) == false)
    return Nil;
  RSocket retval = new Socket(impl);
  return retval;
}

void 
ServerSocket::close()
{
  _theSocket->close();
}

RInetAddress 
ServerSocket::getInetAddress()
{
  
  return _theSocket->getInetAddress();
}

int 
ServerSocket::getLocalPort()
{
  return _theSocket->getLocalPort();
}

int 
ServerSocket::getSoTimeout()
{
  return ((RInteger)_theSocket->getOption((int)_SO_TIMEOUT))->intValue();
}

void 
ServerSocket::setSocketFactory(IN(RSocketImplFactory) fac)
{
  Socket::setSocketImplFactory( fac );
}

void 
ServerSocket::setSoTimeout(int timeout)
{
  RInteger value = new Integer( timeout );
  _theSocket->setOption((int)_SO_TIMEOUT, (RObject)value);
}

RString 
ServerSocket::toString()
{
  return _theSocket->toString();
}

bool
ServerSocket::implAccept(IN(RSocket) s, int timeOut)
{
  RSocketImpl impl;
  return _theSocket->accept(impl, timeOut);
}


} // net
} // acdk
