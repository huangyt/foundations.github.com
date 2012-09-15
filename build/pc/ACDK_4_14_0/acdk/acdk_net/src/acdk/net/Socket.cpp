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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/Socket.cpp,v 1.27 2005/04/30 16:46:25 kommer Exp $




#include "netsysincl.h"


#include "Socket.h"
#include "SocketException.h"
#include "TCPSocketFactory.h"


#include "SocketException.h"
#include <acdk/lang/sys/core_system.h>

namespace acdk {
namespace net {

Socket::Socket(bool asStream)
: _opened(false)
, _asStream(asStream)
{
  if(_initialised == false) 
    _init();
  _theSocket = _theFactory->createSocketImpl();
  _theSocket->create(asStream);
}

Socket::~Socket()
{
  /*
    do not close, because TCPSocket may still hold as Reader/Writer
    ~TCPSocket() will close it
  */
}

Socket::Socket(IN(RInetAddress) address, int port, bool asStream) 
: _opened(false)
, _asStream(asStream)
{
  if (_initialised == false) 
    _init();
  _theSocket = _theFactory->createSocketImpl();
  _theSocket->create( asStream );
  connect(address, port);
/*
  
  try {
    _theSocket->connect(address, port);
  } catch (RSocketException ) {
    _theSocket->close();
    throw;
  }
  */
}

Socket::Socket(IN(RString) host, int port, bool asStream) 
: _opened(false)
, _asStream(asStream)
{
  if (_initialised == false) 
    _init();
  RInetAddress address = InetAddress::getByName(host);
  _theSocket = _theFactory->createSocketImpl();
  _theSocket->create(asStream);
  connect(host, port);
  /*
  try {
    _theSocket->connect(address, port);
    _opened = true;
  } catch (RSocketException ex) {
    _theSocket->close();
    throw ex;
  }
  */
}

Socket::Socket(IN(RSocketImpl) impl)
{
  if(_initialised == false) 
    _init();

  _theSocket = impl;
}

 
void
Socket::_init()
{
  if (_initialised == false)
    _theFactory = new TCPSocketFactory();
  _initialised = true;
}

bool 
Socket::connect(IN(RInetAddress) address, int port, int timeOut)
{

  try {
    bool c = _theSocket->connect(address, port, timeOut);
    if (c == false)
      return false;
    _opened = true;
  } catch (RSocketException ex) {
    _theSocket->close();
    throw ex;
  }
  return true;
}

bool 
Socket::connect(IN(RString) host, int port, int timeOut)
{
  return connect(InetAddress::getByName(host), port, timeOut);
}


void
Socket::close()
{
  _opened = false;
  _theSocket->close();
}

RInetAddress 
Socket::getInetAddress()
{ 
  return _theSocket->getInetAddress(); 
}

bool
Socket::getKeepAlive() 
{
  RBoolean retval = (RBoolean) _theSocket->getOption( _SO_KEEPALIVE );
  bool ret = retval->booleanValue();
  return ret;
}

RInetAddress 
Socket::getLocalAddress() 
{
  RInetAddress retval=InetAddress::getLocalHost();
  return retval;
}

int
Socket::getReceiveBufferSize() 
{
  RInteger retval = ( RInteger ) _theSocket->getOption( _SO_RCVBUF );
  return retval->intValue();
}

int
Socket::getSendBufferSize() 
{
  RInteger retval = ( RInteger ) _theSocket->getOption( _SO_SNDBUF );
  return retval->intValue();
}

int 
Socket::getSoLinger() 
{
  RInteger retval = ( RInteger ) _theSocket->getOption( _SO_LINGER );
  return retval->intValue();
}

int 
Socket::getSoTimeout() 
{
  RInteger retval = ( RInteger ) _theSocket->getOption( _SO_TIMEOUT );
  return retval->intValue();

}
  
bool 
Socket::getTcpNoDelay() 
{
  RBoolean retval = ( RBoolean ) _theSocket->getOption( _TCP_NODELAY );
  return retval->booleanValue();
}

void 
Socket::setKeepAlive( bool on) 
{
  RBoolean value = Boolean::valueOf(on);
  _theSocket->setOption( _SO_KEEPALIVE, (RObject)value );
}

void 
Socket::setReceiveBufferSize(int size) 
{
  RInteger value = new Integer( size );
  _theSocket->setOption( _SO_RCVBUF, (RObject)value );
}

void 
Socket::setSendBufferSize(int size) 
{
  RInteger value = new Integer( size );
  _theSocket->setOption( _SO_SNDBUF, (RObject)value );
}

void 
Socket::setSocketImplFactory(IN(RSocketImplFactory) fac) 
{
  if( _factorychanged ) 
    THROW1( SocketException, "SocketImplFactory already set !");
  
  _theFactory = fac;
}

void 
Socket::setSoLinger(bool on, int linger) 
{
  RInteger value = new Integer( linger );
  _theSocket->setOption( _SO_LINGER, (RObject)value );
}

void 
Socket::setSoTimeout(int timeout) 
{
  RInteger value = new Integer( timeout );
  _theSocket->setOption( _SO_TIMEOUT, (RObject)value );
}

void 
Socket::setTcpNoDelay(bool on) 
{
  
  RBoolean value = Boolean::valueOf(on);
  //_theSocket->setOption( _SO_KEEPALIVE, (RObject)value );
  _theSocket->setOption( _TCP_NODELAY, (RObject)value );
  
}

bool 
Socket::getReuseAddress()
{
  RBoolean retval = ( RBoolean ) _theSocket->getOption(_SO_REUSEADDR);
  return retval->booleanValue();
}

void 
Socket::setReuseAddress(bool on)
{
  RBoolean value = Boolean::valueOf(on);
  _theSocket->setOption(_SO_REUSEADDR, (RObject)value );
}
 

void 
Socket::shutdownInput() 
{
  _theSocket->shutdownInput();
}

void 
Socket::shutdownOutput() 
{
  _theSocket->shutdownOutput();
}

RString 
Socket::toString() {
  return _theSocket->toString();
}

ACDK_NET_PUBLIC RSocketImplFactory 
Socket::_theFactory; //  = new TCPSocketFactory;

ACDK_NET_PUBLIC bool
Socket::_factorychanged = false;

ACDK_NET_PUBLIC bool
Socket::_initialised = false;

#ifdef ACDK_OS_WIN32
class _Initialiser
{
public:
  _Initialiser()
  {
    int wVersionRequested = MAKEWORD( 2, 0 ); 
    WSADATA wsaData;
    int err = WSAStartup( wVersionRequested, &wsaData );
    if ( err ) {
      ::acdk::lang::sys::coreout << "Error : Could'nt initialise Windows Sockets !" << acdk::lang::sys::eofl;
      return;
    }
  }


} _init_sockets;

#endif // ACDK_OS_WIN32


} // net
} // acdk
