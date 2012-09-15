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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/arb.h,v 1.12 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_arb_arb_h
#define acdkx_arb_arb_h

#include <acdk.h>
ACDK_DECL_UNIT(acdkx_arb)

#include "../orb/Config.h"
#include "Config.h"

#include <acdk/lang/ThreadGroup.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/util/HashMap.h>
#include <acdk/net/Socket.h>
#include <acdk/net/InetAddress.h>

/* #include <org/omg/CORBA/ORB.h> */

namespace acdkx {
/** 
  An implementation of the simple ACDK Request Broker
  */
namespace arb {

USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::util::, HashMap);
USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, InetAddress);

#define DEFAULT_ARB_PORT 1423

ACDK_DECL_CLASS(ARB);
ACDK_DECL_CLASS(Connection);
ACDK_DECL_CLASS(ObjectID);
//ACDK_DECL_INTERFACE(ADelegate);

class ProxyClassesStruct
{
public:
  ::acdk::lang::dmi::ClazzInfo* clazz;
  ObjectCreator creator;
  ProxyClassesStruct* next;
};

class RegisterProxyClass
{
  ProxyClassesStruct* _proxy;
public:
  RegisterProxyClass(ProxyClassesStruct* proxy);
  ~RegisterProxyClass();
};

class ACDKX_ARB_PUBLIC Connection
: extends ::acdk::lang::Object
{
public:
  RString address;
  int port;
  RSocket _socket;
  Connection(IN(RString) adr, int theport)
  : address(adr),
    port(theport),
    _socket()
  {
  }
  virtual ~Connection()
  {
    System::out->println(" ~Connection()");
    if (_socket != Nil)
      _socket->close();
  }
  bool connect()
  {
    if (_socket != Nil)
      return true;
    _socket = new Socket(InetAddress::getByName(address), port);
    return true;
  }
  RReader in() { connect(); return _socket->getInputStream();   }
  RWriter out() { connect(); return _socket->getOutputStream(); }
  void close() 
  {
    if (_socket != Nil)
      _socket->close();
  }
};


class ACDKX_ARB_PUBLIC ARB 
: extends ::acdk::lang::Object//,
  //implements ::org::omg::CORBA::ORB
{
private:
  int _port;
  RHashMap _objects;
  RHashMap _objectsIds;
  bool _shutdown;
  RThreadGroup _treadGroup;
  static RARB _theARB;
  bool _isServer;
protected:
  ARB(int port = DEFAULT_ARB_PORT)
    : Object(),
    _port(port),
    _objects(new HashMap()),
    _objectsIds(new HashMap()),
    _shutdown(false),
    _treadGroup(new ThreadGroup("ARB")),
    _isServer(false)
  {
  }
public:
  static RARB getARB(); 
  
  virtual RString object_to_string(IN(RObject) obj);
  virtual RObject string_to_object(IN(RString) obj);
  virtual void run();
  

  virtual void shutdown();
  virtual bool work_pending();

  bool _doShutdown();
  
  RThreadGroup threadGroup() { return _treadGroup; }
  
  /** Server: register object */

  RObjectID impl_is_ready(IN(RObject) obj);

  /** Client: connect to the given client */
  RConnection connect(IN(RObjectID) objid);
  
  //RDelegate getDelegater(RObjectID objid);
  
  bool isLocalObject(IN(RObjectID) objid);
};

} // namespace arb 
} // namespace acdkx 
#endif //acdkx_arb_arb_h

