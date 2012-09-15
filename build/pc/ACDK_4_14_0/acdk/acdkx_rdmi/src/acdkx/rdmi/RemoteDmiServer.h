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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/RemoteDmiServer.h,v 1.4 2005/04/30 14:23:53 kommer Exp $

#ifndef acdkx_rdmi_RemoteDmiServer_h
#define acdkx_rdmi_RemoteDmiServer_h

#include "rdmi.h"
#include "ServerImpl.h"
#include "ProtocolImpl.h"

#include <acdk/lang/Thread.h>
#include <acdk/lang/dmi/DmiObject.h>

namespace acdkx {
namespace rdmi {

enum InvokeCmd;

ACDK_DECL_CLASS(RemoteObjectId);

ACDK_DECL_CLASS(RemoteDmiServer);

ACDK_DECL_CLASS(ThreadedConnectionHandler);

class ACDKX_RDMI_LIB_PUBLIC RemoteDmiServer
: extends acdk::lang::Thread
{
  ACDK_WITH_METAINFO(RemoteDmiServer)
public:
  RServerImpl _server;
  RProtocolImpl _protocol;
  bool _isServer;
  bool _shootDown;
  RemoteDmiServer(IN(RServerImpl) server, IN(RProtocolImpl) protocol)
    : _server(server)
    , _protocol(protocol)
    , _isServer(false)
    , _shootDown(false)
  {
  }
  /**
    run in this thread
    doesn't return until another
    thread call stopServer()
    or the remote requests to stop
  */
  void startInFront();
  
  void startInBackground();
  /**
    only valid if running as server
  */
  void shutdown();
  /**
    only valid if running as client
  */
  void shutdownRemote();

  foreign RObject createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface, const acdk::lang::dmi::ScriptVarArray& args);
  foreign RObject createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface)
  {
    acdk::lang::dmi::ScriptVarArray args(0);
    return createRemoteObject(remoteClass, localInterface, args);
  }
  foreign RObject createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface, IN(acdk::lang::dmi::ScriptVar) sv0)
  {
    acdk::lang::dmi::ScriptVarArray args(1);
    args[0] = sv0;
    return createRemoteObject(remoteClass, localInterface, args);
  }
  foreign RObject createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface, IN(acdk::lang::dmi::ScriptVar) sv0, IN(acdk::lang::dmi::ScriptVar) sv1)
  {
    acdk::lang::dmi::ScriptVarArray args(2);
    args[0] = sv0;
    args[1] = sv1;
    return createRemoteObject(remoteClass, localInterface, args);
  }
  foreign RObject createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface, 
                             IN(acdk::lang::dmi::ScriptVar) sv0, IN(acdk::lang::dmi::ScriptVar) sv1,
                             IN(acdk::lang::dmi::ScriptVar) sv2)
  {
    acdk::lang::dmi::ScriptVarArray args(3);
    args[0] = sv0;
    args[1] = sv1;
    args[2] = sv2;
    return createRemoteObject(remoteClass, localInterface, args);
  }
  RObject createRemote(IN(RString) remoteClass, IN(acdk::lang::dmi::RDmiObjectArray) rest)
  {
    acdk::lang::dmi::ScriptVarArray args(rest->length());
    for (int i = 0; i < args.size(); ++i)
      args[i] = *(rest[i]);
    return createRemoteObject(remoteClass, "", args);
  }
  RObject createRemoteAs(IN(RString) remoteClass, IN(RString) localInterface, IN(acdk::lang::dmi::RDmiObjectArray) rest)
  {
    acdk::lang::dmi::ScriptVarArray args(rest->length());
    for (int i = 0; i < args.size(); ++i)
      args[i] = *(rest[i]);
    return createRemoteObject(remoteClass, localInterface, args);
  }
  foreign acdk::lang::dmi::ScriptVar invokeStaticRemote(IN(RString) className, IN(RString) methodName, 
                                                        IN(acdk::lang::dmi::ScriptVarArray) rest, IN(RString) returnAsType = Nil, int returnAttr = 0);

  /**
    for scripting
    @internal
  */
  acdk::lang::dmi::RDmiObject invokeStaticRemote(IN(RString) className, IN(RString) methodName, IN(acdk::lang::dmi::RDmiObjectArray) rest)
  {
     acdk::lang::dmi::ScriptVarArray args(rest->length());
    for (int i = 0; i < args.size(); ++i)
      args[i] = *(rest[i]);
    return new acdk::lang::dmi::DmiObject(invokeStaticRemote(className, methodName, args));
  }
  /**
    for scripting
    @internal
  */
  acdk::lang::dmi::RDmiObject invokeStaticRemoteAs(IN(RString) className, IN(RString) methodName, IN(RString) returnAsType, int returnTypeFlags, IN(acdk::lang::dmi::RDmiObjectArray) rest)
  {
     acdk::lang::dmi::ScriptVarArray args(rest->length());
    for (int i = 0; i < args.size(); ++i)
      args[i] = *(rest[i]);
    return new acdk::lang::dmi::DmiObject(invokeStaticRemote(className, methodName, args, returnAsType, returnTypeFlags));
  }
  
  
   /**
    @param className server class
    @param interfaceName class of type to transfer. May be Nil
    @param flags acdk::lang::dmi::MiAiByval or acdk::lang::dmi::MiAiByref
           if flags = 0 selection will be done by type
    @param memberName name of static member
  */
  acdk::lang::dmi::ScriptVar peekStatic(IN(RString) className, IN(RString) memberName, IN(RString) interfaceName = Nil, int flags = 0);
  /**
    same as peekStatic, but used in scripting languages
  */
  acdk::lang::dmi::RDmiObject peekStaticRemote(IN(RString) className, IN(RString) memberName, IN(RString) interfaceName = Nil, int flags = 0)
  {
    return new acdk::lang::dmi::DmiObject(peekStatic(className, memberName, interfaceName, flags));
  }
 
  virtual void run();

  // Internal methods from now
  /**
    will use to run readWrite cyclus
    @internal
  */
  void readWrite(IN(RConnection) con, InvokeCmd initCmd = Unexpected);

  /**
    server receives message and translate it
    @internal
  */
  foreign InvokeCmd dispatch2Local(InvokeCmd cmd, IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  /**
    send a message and receive anwer
    @internal
  */
  foreign InvokeCmd dispatch2Remote(InvokeCmd cmd, IN(RRemoteArgArray) sendArgs, IN(RRemoteArgArray) recvArgs, IN(RConnection) con);

  foreign InvokeCmd dispatchNewObject(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchInvoke(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchInvokeStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchPeek(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchPoke(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchPeekStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign InvokeCmd dispatchPokeStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  foreign
  const acdk::lang::dmi::ClazzMethodInfo*  
    remoteStandardDispatch(IN(RRemoteArg) remoteThis, IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                      acdk::lang::dmi::ScriptVarArray& args, 
                      acdk::lang::dmi::DmiClient& dc,
                      IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                      int flags,
                      const acdk::lang::dmi::ClazzInfo* clazzinfo,
                      const acdk::lang::dmi::ClazzMethodInfo* methinf, 
                      IN(RRemoteArg) remoteId, IN(RConnection) con);
private:
  foreign acdk::lang::dmi::ScriptVar _remote2local(IN(RRemoteArg) ra, IN(RConnection) con);
  foreign RRemoteArg _local2remote(const acdk::lang::dmi::ScriptVar& ra, IN(RConnection) con);
  foreign RObject _resolveRemoteObject(IN(RRemoteArg) ra, IN(RConnection) con, bool forceLocal = false);
  /**
    return a remote id, which is located on this process
  */
  foreign RRemoteObjectId _getLocalRemoteObjectId(IN(RRemoteArg) ra);
  foreign RRemoteArg _createRemoteReference(IN(RObject) obj, IN(RString) interfaceName, IN(RConnection) con);
  foreign RRemoteArg _localObject2remoteObject(const acdk::lang::dmi::ScriptVar& sv, IN(RConnection) con);
  foreign void _send(InvokeCmd sendCmd, IN(RRemoteArgArray) sendArgs, IN(RConnection) con);
  /** if initCmd != Unexpected read also cmd */
  foreign InvokeCmd  _receive(IN(RRemoteArgArray) recvArgs, IN(RConnection) con, InvokeCmd initCmd);

  
  

};

ACDK_DECL_CLASS(ThreadedConnectionHandler);

class ACDKX_RDMI_LIB_PUBLIC ThreadedConnectionHandler
: extends acdk::lang::Thread
{
  ACDK_WITH_METAINFO(ThreadedConnectionHandler)
private:
  RConnection _connection;
  RRemoteDmiServer _server;
  InvokeCmd _initCmd;
public:
  ThreadedConnectionHandler(IN(RRemoteDmiServer) server, IN(RConnection) con)
  : _connection(con)
  , _server(server)
  , _initCmd(Unexpected)
  {
  }
  InvokeCmd readFirstCmd();
  virtual void run();
};

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_RemoteDmiServer_h
