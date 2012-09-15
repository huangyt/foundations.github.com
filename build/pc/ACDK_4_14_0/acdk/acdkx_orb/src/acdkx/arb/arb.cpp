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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/arb.cpp,v 1.8 2005/02/05 10:45:39 kommer Exp $


#include <acdk.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/net/InetAddress.h>
#include <acdk/net/Socket.h>
#include <acdk/net/ServerSocket.h>
#include "arb.h"
#include "AObjectImpl.h"
#include "XMLDelegate.h"

namespace acdkx {
namespace arb {

USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, ServerSocket);
USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);

static ProxyClassesStruct* _proxyRoot = 0;
static ProxyClassesStruct* _proxyTail = 0;

RegisterProxyClass::RegisterProxyClass(ProxyClassesStruct* proxy)
: _proxy(proxy)
{
  if (_proxyRoot == 0) {
    _proxyTail = _proxyRoot = proxy;
  } else {
    _proxyTail->next = proxy;
    _proxyTail = proxy;
  }
}

RegisterProxyClass::~RegisterProxyClass()
{
  //### to implement
}

RADelegate 
AObjectImpl::delegater()
{
  return Nil; //### to fix
  /*
  if (_delegate != Nil)
    return _delegate;
  _delegate = ARB::getARB()->getDelegater(_objID);
  return _delegate;
  */
}

//static 
RARB ARB::_theARB;

//static 
RARB 
ARB::getARB()
{
  if (_theARB != Nil)
    return _theARB;
  _theARB = new ARB();
  System::registerStaticReference(_theARB);
  return _theARB;
}


RString 
ARB::object_to_string(IN(RObject) obj)
{
  if (instanceof(obj, AObjectImpl) == true) {
    RObjectID oaid = RAObjectImpl(obj)->objID();
    if (oaid != Nil)
      return RAObjectImpl(obj)->objID()->toString();
  }
  ObjectID objid("arb", acdk::net::InetAddress::getLocalHost()->toString(), _port, obj);
  return objid.toString();
  
  
}

bool
ARB::isLocalObject(IN(RObjectID) objid)
{
  if (_isServer == false)
    return false;
  if (Process::getProcessId() == objid->pid)
    return true;
  if (objid->pid == 0) {
    if (objid->port != _port)
      return false;
    RString address = acdk::net::InetAddress::getLocalHost()->toString();
    if (objid->network->equals(address) == false)
      return false;
    return true;
  }
  return false;
}

RObject 
ARB::string_to_object(IN(RString) str)
{
  RObjectID objid = new ObjectID(str);
  if (isLocalObject(objid) == true)
    return objid->getLocalObject();

  ProxyClassesStruct* pc = _proxyRoot;
  while (pc != 0) {
    RString fqname = RString(pc->clazz->ns) + "/" + pc->clazz->name;
    if (fqname->compareTo(objid->classname) == 0) {
      RObject ret = pc->creator();
      RAObjectImpl objimpl = (RAObjectImpl)ret;
      objimpl->setObjID(objid);
      return ret;
    }
    pc = pc->next;
  }
  return Nil;
}



bool
ARB::_doShutdown()
{
  return _shutdown;
}

void 
ARB::shutdown()
{
  _shutdown = true;
}

//virtual 
bool 
ARB::work_pending()
{
  return threadGroup()->activeCount() > 0;
}

RConnection 
ARB::connect(IN(RObjectID) objid)
{
  RConnection con = new Connection(objid->network, objid->port);
  //con->connect();
  return con;
}

ACDK_DECL_CLASS(ClientConnection);
class ClientConnection 
: public ::acdk::lang::Thread
{
  RARB _arb;
  RSocket _clientSocket;
public:
  ClientConnection(IN(RARB) arb, IN(RSocket) clsock)
  : Thread(arb->threadGroup()),
    _arb(arb),
    _clientSocket(clsock)
  {
  }
  virtual void run();
};

//virtual 
void 
ClientConnection::run()
{
  System::out->println("ClientConnection::run()");
  RReader in = _clientSocket->getInputStream();
  RWriter out = _clientSocket->getOutputStream();
  RXMLDelegate xmldelegate = new XMLDelegate();
  xmldelegate->dispatch(_arb,  in, out);
}

RObjectID
ARB::impl_is_ready(IN(RObject) obj)
{
  RString address = acdk::net::InetAddress::getLocalHost()->toString();
  RClass cls = obj->getClass(); //Class
  RObjectID objid = new ObjectID("arb", address, _port, obj);
  _objects->put((RObject)objid, obj);
  return objid;
  
}
/*
RADelegate
ARB::getDelegater(RObjectID objid)
{
  return new XMLDelegate();
}
*/
//virtual 
void 
ARB::run()
{
  RServerSocket theSocket = new ServerSocket(_port);
  _isServer = true;
  while (_doShutdown() == false) {
    RSocket clsock = theSocket->accept();
    RClientConnection client_thread = new ClientConnection(this, clsock);
    client_thread->start();
  }
  theSocket->close();
  _isServer = false;
}

} // namespace arb 
} // namespace acdkx 



