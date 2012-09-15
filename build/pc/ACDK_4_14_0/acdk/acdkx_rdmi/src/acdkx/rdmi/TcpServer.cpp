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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/TcpServer.cpp,v 1.4 2005/04/30 14:46:33 kommer Exp $


#include "TcpServer.h"
#include "BinaryProtocol.h"

namespace acdkx {
namespace rdmi {

using namespace acdk::net;

 
//virtual 
RString 
TcpServerConnection::getRemoteServerId()
{
  return SBSTR("inet:/" << _socket->getInetAddress()->toString() << ":" << _socket->getPort());
}

bool 
TcpServerConnection::isClosed()
{
  return _socket->getReader()->ready() == false;
}
bool 
TcpServerConnection::dataAvailable()
{
  return _socket->getReader()->available() > 0;
}

//virtual 
RString 
TcpServerConnection::getLocalServerId()
{
   return SBSTR("inet:/" << _socket->getLocalAddress()->toString() << ":" << _socket->getLocalPort());
}

TcpServer::TcpServer(int portNo)
: _serverSocket(new ServerSocket(portNo))
, _port(portNo)
{
}

TcpServer::TcpServer(IN(RString) host, int portNo)
: _port(portNo)
, _host(host)
{
}
TcpServer::TcpServer(IN(acdk::net::RInetAddress) address, int portNo)
: _port(portNo)
, _address(address)
{
}

RString 
TcpServer::getLocalServerId()
{
  if (_serverSocket != Nil)
    return SBSTR("inet:/" << _serverSocket->getInetAddress()->toString() << ":" << _port);
  else if (_clientSocket != Nil)
    return SBSTR("inet:/" << _clientSocket->getLocalAddress()->toString() << ":" << _clientSocket->getLocalPort());
  else 
    return ""; //???
}

//virtual 
RConnection 
TcpServer::accept(int timeOut)
{
  RSocket sock = _serverSocket->accept(timeOut);
  if (sock == Nil)
    return Nil;
  return new TcpServerConnection(sock);
}

//virtual 
RConnection 
TcpServer::getClientConnection()
{
  if (_clientSocket == Nil)
  {
    if (_address != Nil)
      _clientSocket = new acdk::net::Socket(_address, _port);
    else
      _clientSocket = new acdk::net::Socket(_host, _port);
  }
  return new TcpServerConnection(_clientSocket);
}


RConnection 
TcpServer::getLocalServerConnection()
{
  if (_serverSocket == Nil)
    THROW1(Exception, "this is not running as server");
  return new TcpServerConnection(new acdk::net::Socket(_serverSocket->getInetAddress(), _port));
}

//virtual 
void 
TcpServer::shutdown()
{
  _serverSocket->close();
}
/*
void 
TcpServer::send(InvokeCmd cmd, acdk::lang::dmi::ScriptVarArray& args)
{
  if (_socket == Nil)
    _socket = _serverSocket->accept();

  _transport->send(_socket->getWriter(), cmd, args);
}

InvokeCmd 
TcpServer::receive(acdk::lang::dmi::ScriptVarArray& args)
{
  if (_socket == Nil)
    _socket = _serverSocket->accept();
  return _transport->receive(_socket->getReader(), args);
}
*/
} // namespace rdmi 
} // namespace acdkx

