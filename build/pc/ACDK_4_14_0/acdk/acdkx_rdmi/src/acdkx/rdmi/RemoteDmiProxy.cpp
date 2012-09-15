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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/RemoteDmiProxy.cpp,v 1.2 2005/03/07 10:04:18 kommer Exp $


#include "RemoteDmiProxy.h"

namespace acdkx {
namespace rdmi {

using namespace acdk::lang::dmi;

RemoteDmiProxy::RemoteDmiProxy(IN(RRemoteDmiServer) server, IN(RRemoteArg) remoteId, IN(RConnection) con)
: _server(server)
, _remoteId(remoteId)
, _connection(con)
, _interfaceClazzInfo(0)
{
  if (_remoteId->name != Nil && _remoteId->name->length() > 0)
    _interfaceClazzInfo = ClazzInfo::findClazzInfo(_remoteId->name, true);
}

RemoteDmiProxy::~RemoteDmiProxy()
{
  RemoteArgArray sendArgs(1);
  sendArgs[0] = _remoteId;
  RemoteArgArray recvArgs;
  InvokeCmd cmd = _server->dispatch2Remote(ReleaseRef, &sendArgs, &recvArgs, _connection);

}

bool  
RemoteDmiProxy::isDmiOverLoaded(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) funcname, const acdk::lang::dmi::ClazzMethodInfo* mi, acdk::lang::dmi::ClazzMethodArgInfo**const args)
{
  return true;
}

const acdk::lang::dmi::ClazzMethodInfo* 
RemoteDmiProxy::standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  
  return _server->remoteStandardDispatch(_remoteId, fname, ret, args, dc, namedArgs, flags, _interfaceClazzInfo, methinf, _remoteId, _connection);
}

const acdk::lang::dmi::ClazzMethodInfo*  
RemoteDmiServer::remoteStandardDispatch(IN(RRemoteArg) remoteThis, IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                      acdk::lang::dmi::ScriptVarArray& args, 
                      acdk::lang::dmi::DmiClient& dc,
                      IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                      int flags,
                      const acdk::lang::dmi::ClazzInfo* clazzinfo,
                      const acdk::lang::dmi::ClazzMethodInfo* methinf, 
                      IN(RRemoteArg) remoteId, IN(RConnection) con)
{
  if (clazzinfo != 0 && methinf == 0)
  {
    methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags, methinf);
    flags &= ~MiIvViaHash;
  }

  RRemoteArgArray sendArgs = new RemoteArgArray(4);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = remoteThis;
  sendArgs[2] = new RemoteArg(RemoteStringType, inOf(fname));
  sendArgs[3] = new RemoteArg(RemoteIntValueType, inOf(flags));
  if (methinf != 0)
  {
    sendArgs->append(new RemoteArg(RemoteIntValueType, methinf->getMethodSignatureHashValue()));
    int pcount = methinf->getArgumentCount();
    int argssize = args.size();
    if (pcount != argssize)
      THROW1(DmiException, "method called with not matching parameter count");
  
    for (int i = 0; i < argssize; ++i)
    {
      int aflags = methinf->methodArgs[i]->flags;
      if (aflags & MiAiIn)
        sendArgs->append(_local2remote(args[i], con));
    }
  }
  else
  {
    sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(WeakInvoke));
    for (int i = 0; i < args.size(); ++i)
    {
      sendArgs->append(_local2remote(args[i], con));
    }
  }
  RRemoteArgArray recvArgs = new RemoteArgArray(0);
  InvokeCmd cmd = dispatch2Remote(Invoke, sendArgs, recvArgs, con);
  if (cmd == ReturnException)
  {
    RThrowable ex = (RThrowable)recvArgs[0]->value.getObjectVar();
    THROW_INSTANCE(ex);
    return (const acdk::lang::dmi::ClazzMethodInfo*)1;
  } 
  int retRemoteFlags = _remote2local(recvArgs[0], con).getIntVar();
  int retArgOffset = 1;
  if (cmd == ReturnValue)
  {
    ret = _remote2local(recvArgs[retArgOffset], con);
    ++retArgOffset;
  }
  if (methinf != 0)
  {
    int argssize = args.size();
    int pcount = methinf->getArgumentCount();
    int outParamCount = 0;
    for (int i = 0; i < pcount; ++i)
    {
      int aflags = methinf->methodArgs[i]->flags;
      if (aflags & MiAiOut)
      {
        args[i] = _remote2local(recvArgs[outParamCount + retArgOffset], con);
        ++outParamCount;
      }
    }
  }
  return (const acdk::lang::dmi::ClazzMethodInfo*)1;
}


} // namespace rdmi 
} // namespace acdkx


