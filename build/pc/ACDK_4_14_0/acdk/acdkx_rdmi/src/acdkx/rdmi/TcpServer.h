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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/TcpServer.h,v 1.3 2005/04/30 14:23:53 kommer Exp $

#ifndef acdkx_rdmi_TcpServer_h
#define acdkx_rdmi_TcpServer_h

#include "rdmi.h"
#include "ProtocolImpl.h"
#include "ServerImpl.h"
#include "Connection.h"

#include <acdk/net/ServerSocket.h>

namespace acdkx {
namespace rdmi {

ACDK_DECL_CLASS(TcpServerConnection);

class ACDKX_RDMI_LIB_PUBLIC TcpServerConnection
: extends acdk::lang::Object
, implements Connection
{
  ACDK_WITH_METAINFO(TcpServerConnection)
private:
  acdk::net::RSocket _socket;
public:
  TcpServerConnection(IN(acdk::net::RSocket) socket)
    : _socket(socket)
  {
  }
  virtual RString getRemoteServerId();
  virtual RString getLocalServerId();
  virtual acdk::io::RReader getReader()
  {
    return _socket->getReader();
  }
  virtual acdk::io::RWriter getWriter()
  {
    return _socket->getWriter();
  }
  /**
    may prepare a connection
    will be called before getWriter()
  */
  virtual void startWriteMessage()
  {
    // nothing
  }
  /**
    Message was written
    may flus/close Writer
  */
  virtual void endWriteMessage()
  {
    // nothing?
  }
  virtual void startReadMessage()
  {
  }
  virtual void endReadMessage()
  {
  }
  /**
    close the connection
  */
  virtual void close()
  {
    _socket->close();
  }
  virtual bool isClosed();
  virtual bool dataAvailable();
};

ACDK_DECL_CLASS(TcpServer);



class ACDKX_RDMI_LIB_PUBLIC TcpServer
: extends acdk::lang::Object
, implements ServerImpl
{
  ACDK_WITH_METAINFO(TcpServer)
private:
  acdk::net::RServerSocket _serverSocket;
  acdk::net::RSocket _clientSocket;
  int _port;
  RString _host;
  acdk::net::RInetAddress _address;
public:
  TcpServer(int portNo);
  TcpServer(IN(RString) host, int portNo);
  TcpServer(IN(acdk::net::RInetAddress) address, int portNo);
  virtual RString getLocalServerId();
  virtual RConnection accept(int timeOut = -1);
  virtual RConnection getClientConnection();
  virtual RConnection getLocalServerConnection();
  virtual void shutdown();
  virtual bool allowThreading() { return true; }
};


} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_TcpServer_h
