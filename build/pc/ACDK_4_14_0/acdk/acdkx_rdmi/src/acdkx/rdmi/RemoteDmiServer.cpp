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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/RemoteDmiServer.cpp,v 1.12 2005/04/30 20:17:36 kommer Exp $


#include "RemoteDmiServer.h"
#include "LocalObjectRepository.h"
#include "RemoteDmiProxy.h"
#include <acdk/util/logging/Log.h>

namespace acdkx {
namespace rdmi {

using namespace  acdk::lang::dmi;

void 
RemoteDmiServer::startInFront()
{
  _isServer = true;
  run();
}
  
void 
RemoteDmiServer::startInBackground()
{
  _isServer = true;
  start();
}

RObject
RemoteDmiServer::_resolveRemoteObject(IN(RRemoteArg) ra, IN(RConnection) con, bool forceLocal)
{
  RString localServerId = _server->getLocalServerId();
  if (localServerId->equals(ra->serverId) == true)
  {
    RRemoteObjectId roi = RemoteObjectId::getId(ra->value);
    if (LocalObjectRepository::get()->hasLocalRef(roi) == true)
      return LocalObjectRepository::get()->getLocalRef(roi);
  }
  if (forceLocal == true)
    THROW1(Exception, "Is not a local instance: " + ra->toString());
  RClass cls;
  if (ra->name == Nil || ra->name->length() == 0)
    cls = Object::GetClass();
  else
    cls = Class::forName(ra->name);

  RRemoteDmiProxy rdmiObject = new RemoteDmiProxy(this, ra, con);
  RObject prxy = cls->createDmiProxy(&rdmiObject);
  return prxy;
  /*
  // now create transparent proxy
  if (con->getRemoteServerId()->equals(ra->serverId) == true)
  {
    RObject tobj = 
  }
  return Nil;
  */
}

ScriptVar 
RemoteDmiServer::_remote2local(IN(RRemoteArg) ra, IN(RConnection) con)
{
  switch (ra->argType)
  {
  case RemoteIdType:
    return inOf(_resolveRemoteObject(ra, con));
  case RemoteArrayValueType:
  {
    RRemoteArgArray raa = (RRemoteArgArray)ra->value.getObjectVar();
    int len = raa->length();
    RObjectArray oa = (RObjectArray)Class::create_arrayInstance(Class::forName(ra->name), len);
    for (int i = 0; i < len; ++i)
    {
      oa[i] = _remote2local(raa[i], con).getObjectVar();
    }
    return inOf(oa);
  }
  default:
    return ra->value;
  }
  // ### throw here
  return ScriptVar();
}

RRemoteObjectId
RemoteDmiServer::_getLocalRemoteObjectId(IN(RRemoteArg) ra)
{
  if (ra->argType != RemoteIdType)
    THROW1(RuntimeException, "RemoteDmiServer::_getLocalRemoteObjectId unexpected type");
  if (_server->getLocalServerId()->equals(ra->serverId) == false)
    THROW1(RuntimeException, "RemoteDmiServer::_getLocalRemoteObjectId not a remote object from this server");
  return RemoteObjectId::getId(ra->value);
}

RRemoteArg 
RemoteDmiServer::_localObject2remoteObject(const acdk::lang::dmi::ScriptVar& sv, IN(RConnection) con)
{
  RObject obj = sv.getObjectVar();
  RClass cls;
  if (sv._clazzType != 0)
    cls = Class::getSingeltonClass(sv._clazzType);
  else
    cls = obj->getClass();
  RString clsName = cls->getName();
  if (sv.flags & MiAiByref)
  {
    return _createRemoteReference(obj, cls->getName(), con);
  } 
  else if (sv.flags & MiAiByval)
  {
    return new RemoteArg(RemoteSerializedType, inOf(obj));
  }
  
  if (instanceof(obj, String) == true)
    return new RemoteArg(RemoteStringType, inOf(obj));

  if (cls->isSerializable() == true || 
      Throwable::GetClass()->isAssignableFrom(cls) == true)
  {
    return new RemoteArg(RemoteSerializedType, inOf(obj), clsName);
  }
  if (cls->isArray() == true)
  {
    RClass cmp = cls->getArrayElementClass();
    if (cmp->isPrimitive() == true)
    {
      return new RemoteArg(RemoteSerializedType, inOf(obj), clsName);
    }
    RObjectArray oa = (RObjectArray)obj;
    RRemoteArgArray raa = new RemoteArgArray(oa->length());
    for (int i = 0; i < oa->length(); ++i)
    {
      raa[i] = _localObject2remoteObject(inOf(oa[i]), con);
    }
    return new RemoteArg(RemoteArrayValueType, inOf(raa), cmp->getName());
  }
  return _createRemoteReference(obj, cls->getName(), con);
}

RemoteArgType
svType2RaType(int tp)
{
  switch(tp)
  {
  case ScriptVar::BoolType:
  case ScriptVar::BoolRefType:
    return RemoteBoolValueType;
  case ScriptVar::IntType:
  case ScriptVar::IntRefType:
    return RemoteIntValueType;
  case ScriptVar::CharType:
  case ScriptVar::CharRefType:
    return RemoteCharValueType;
  case ScriptVar::UcCharType:
  case ScriptVar::UcCharRefType:
    return RemoteUcCharValueType;
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType:
    return RemoteByteValueType;
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType:
    return RemoteShortValueType;
  case ScriptVar::LongType:
  case ScriptVar::LongRefType:
    return RemoteLongValueType;
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType:
    return RemoteFloatValueType;
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType:
    return RemoteDoubleValueType;
  default:
    Object::_throwNotImplementedYet();
    return RemoteUnsetType;
    /*

    CharType,
    UcCharType,
    ByteType,
    ShortType,
    IntType,
    LongType, // i64
    FloatType,
    DoubleType,
    ObjectType,
    BoolRefType,
    CharRefType,
    UcCharRefType,
    ByteRefType,
    ShortRefType,
    IntRefType,
    LongRefType,
    FloatRefType,
    DoubleRefType,
    ObjectRefType
    */
  }
}

void 
RemoteArg::writeObject(IN(acdk::io::RObjectWriter) out)
{
  out->write((byte)argType);
  switch (argType)
  {
  case RemoteIdType:
    out->writeString(serverId);
    out->writeString(name);
    out->writeInt(value.getIntVar());
    break;
  case RemoteArrayValueType:
    out->writeString(name);
    // fall through
  case RemoteSerializedType:
    if (value._clazzType != 0)
      out->writeObject(Class::getSingeltonClass(value._clazzType), value.getObjectVar());
    else
      out->writeObject(value.getObjectVar());
    break;
  case RemoteStringType:
    out->writeString(value.getStringVar());
    break;
  case RemoteIntValueType:
    out->writeInt(value.getIntVar());
    break;
  case RemoteBoolValueType:
    out->writeBoolean(value.getBoolVar());
    break;
  case RemoteCharValueType:
    out->writeChar(value.getCharVar());
    break;
  case RemoteUcCharValueType:
    out->writeUcChar(value.getUcCharVar());
    break;
  case RemoteByteValueType:
    out->write(value.getByteVar());
    break;
  case RemoteShortValueType:
    out->writeShort(value.getShortVar());
    break;
  case RemoteLongValueType:
    out->writeLong(value.getLongVar());
    break;
  case RemoteFloatValueType:
    out->writeFloat(value.getFloatVar());
    break;
  case RemoteDoubleValueType:
    out->writeDouble(value.getDoubleVar());
    break;
  default:
    Object::_throwNotImplementedYet();
    break;
  }
}

RString 
RemoteArg::toString()
{
  if (argType == RemoteIdType)
    return SBSTR(getEnumValString(RemoteArgTypeMetaInf::GetEnumInfo(), argType) << ";" << serverId << ";" << name << ";" << value.toCode());
  const ClazzInfo* ci = value.getClazzInfo();
  StringBuffer sb;
  sb << getEnumValString(RemoteArgTypeMetaInf::GetEnumInfo(), argType) << ";";
  if (name != Nil)
    sb << name << "=";
  else
    if (ci != 0)
      sb << Class::getSingeltonClass(ci)->getName() << "=";
  sb << value.toCode();
  return sb.toString();
  
}

void 
RemoteArg::readObject(IN(acdk::io::RObjectReader) in)
{
  argType = (RemoteArgType)in->read();
  switch (argType)
  {
  case RemoteIdType:
    serverId = in->readString();
    name = in->readString();
    value = inOf(in->readInt());
    break;
  case RemoteArrayValueType:
    name = in->readString();
    // fall through
  case RemoteSerializedType:
    value = inOf(in->readObject());
    break;
   case RemoteStringType:
    value = inOf(in->readString());
    break;
  case RemoteIntValueType:
    value = inOf(in->readInt());
    break;
  case RemoteBoolValueType:
    value = inOf(in->readBoolean());
    break;
  case RemoteCharValueType:
    value = inOf(in->readChar());
    break;
  case RemoteUcCharValueType:
    value = inOf(in->readUcChar());
    break;
  case RemoteByteValueType:
    value = inOf(in->read());
    break;
  case RemoteShortValueType:
    value = inOf(in->readShort());
    break;
  case RemoteLongValueType:
    value = inOf(in->readLong());
    break;
  case RemoteFloatValueType:
    value = inOf(in->readFloat());
    break;
  case RemoteDoubleValueType:
    value = inOf(in->readDouble());
    break;
  default:
    Object::_throwNotImplementedYet();
  }
}

RRemoteArg 
RemoteDmiServer::_local2remote(const acdk::lang::dmi::ScriptVar& sv, IN(RConnection) con)
{
  if (sv.isObjectType() == true)
  {
    if (sv.isStringType() == true)
      return new RemoteArg(RemoteStringType, sv);
    return _localObject2remoteObject(sv, con);
  }
  return new RemoteArg(svType2RaType(sv.type), sv);
}

RRemoteArg 
RemoteDmiServer::_createRemoteReference(IN(RObject) obj, IN(RString) interfaceName, IN(RConnection) con)
{
  RRemoteArg ra = new RemoteArg(RemoteIdType);
  ra->value = inOf(LocalObjectRepository::get()->addCreateConnectionRef(obj, con)->_id);
  ra->serverId = _server->getLocalServerId();
  ra->name = interfaceName;
  return ra;
}


InvokeCmd 
RemoteDmiServer::dispatchNewObject(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  int remoteFlags = recvArgs[0]->value.getIntVar();
  RString className = recvArgs[1]->value.getStringVar();
  RString interfaceName = recvArgs[2]->value.getStringVar();
  int dmiFlags = recvArgs[3]->value.getIntVar();
  ScriptVarArray callArgs;
  for (int i = 4; i < recvArgs->length(); ++i)
  {
    callArgs.push_back(_remote2local(recvArgs[i], con));
  }
  RObject newObj = (RObject)Object::New(className, callArgs);
  sendArgs->resize(2);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = _createRemoteReference(newObj, interfaceName, con);
  return ReturnValue;
}

InvokeCmd 
RemoteDmiServer::dispatchInvoke(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  int remoteFlags = recvArgs[0]->value.getIntVar();
  RObject obj = _resolveRemoteObject(recvArgs[1], con, true);
  RString funcName = _remote2local(recvArgs[2], con).getStringVar();
  int dmiFlags = recvArgs[3]->value.getIntVar();
  
  if (remoteFlags & WeakInvoke)
  {
    ScriptVarArray callArgs;
    for (int i = 4; i < recvArgs->length(); ++i)
    {
      callArgs.push_back(_remote2local(recvArgs[i], con));
    }
    ScriptVar erg = obj->invokeMethod(funcName, callArgs);
    if (erg.isVoid() == true)
    {
      sendArgs->resize(1);
      sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
      return ReturnVoid;
    }
    sendArgs->resize(2);
    sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
    sendArgs[1] = _local2remote(erg, con);
    return ReturnValue;
  }
  else
  {
    int methodHash = recvArgs[4]->value.getIntVar();
    const ClazzInfo* ci = obj->getClazzInfo();
    const ClazzInfo* tclazz = ci;
    const ClazzMethodInfo* cmi = StdDispatch::lookupMethod(tclazz, methodHash, dmiFlags);
    if (cmi == 0)
      ClazzMethodInfo::throwMethodNotFound(ci, SBSTR(funcName << ", hash:" << methodHash), dmiFlags, 0);
    int pcount = cmi->getArgumentCount();
    ScriptVarArray callArgs(pcount);
    int i;
    for (i = 0; i < pcount; ++i)
    {
      int aflags = cmi->methodArgs[i]->flags;
      if (aflags & MiAiIn)
        callArgs[i] = _remote2local(recvArgs[i + 5], con);
    }
    ScriptVar ret;
    obj->standardDispatch(funcName, ret, callArgs, obj->getDmiClient(), Nil, dmiFlags, tclazz, cmi);
    sendArgs->append(new RemoteArg(RemoteIntValueType, inOf(0)));
    if (ret.isVoid() == false)
      sendArgs->append(_local2remote(ret, con));
    for (i = 0; i < pcount; ++i)
    {
      int aflags = cmi->methodArgs[i]->flags;
      if (aflags & MiAiOut)
        sendArgs->append(_local2remote(callArgs[i], con));
    }
    if (ret.isVoid() == true)
      return ReturnVoid;
    return ReturnValue;
  }
}

InvokeCmd
RemoteDmiServer::dispatchInvokeStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  int remoteFlags = recvArgs[0]->value.getIntVar();
  RString className  = _remote2local(recvArgs[1], con).getStringVar();
  RString funcName = _remote2local(recvArgs[2], con).getStringVar();
  int dmiFlags = recvArgs[3]->value.getIntVar();
  ScriptVarArray callArgs;
  for (int i = 4; i < recvArgs->length(); ++i)
  {
    callArgs.push_back(_remote2local(recvArgs[i], con));
  }
  ScriptVar erg = StdDispatch::invokeStaticMethod(className, funcName, callArgs);
  if (erg.isVoid() == true)
  {
    sendArgs->resize(1);
    sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
    return ReturnVoid;
  }
  sendArgs->resize(2);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = _local2remote(erg, con);
  return ReturnValue;
}

InvokeCmd 
RemoteDmiServer::dispatchPeek(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  Object::_throwNotImplementedYet();
  return ReturnVoid;
}

InvokeCmd 
RemoteDmiServer::dispatchPoke(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  Object::_throwNotImplementedYet();
  return ReturnVoid;
}

InvokeCmd 
RemoteDmiServer::dispatchPeekStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  int remoteFlags = recvArgs[0]->value.getIntVar();
  RString className  = _remote2local(recvArgs[1], con).getStringVar();
  RString memberName = _remote2local(recvArgs[2], con).getStringVar();
  int dmiFlags = recvArgs[3]->value.getIntVar();
  ScriptVar erg = StdDispatch::peek_static(className, memberName);
  sendArgs->resize(2);
  sendArgs[0] = _local2remote(erg, con);
  return ReturnValue;
}

InvokeCmd 
RemoteDmiServer::dispatchPokeStatic(IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  Object::_throwNotImplementedYet();
  return ReturnVoid;
}

InvokeCmd 
RemoteDmiServer::dispatch2Local(InvokeCmd cmd, IN(RRemoteArgArray) recvArgs, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  try {
    switch (cmd)
    {
    case AddRef:
    {
      RRemoteObjectId roid = _getLocalRemoteObjectId(recvArgs[0]);
      LocalObjectRepository::get()->addConnectionRef(roid, con);
      return ReturnVoid;
    }
    case ReleaseRef:
    {
      RRemoteObjectId roid = _getLocalRemoteObjectId(recvArgs[0]);
      LocalObjectRepository::get()->releaseConnectionRef(roid, con);
      return ReturnVoid;
    }
    case NewObject:
      return dispatchNewObject(recvArgs, sendArgs, con);
    case Invoke:
      return dispatchInvoke(recvArgs, sendArgs, con);
    case InvokeStatic:
      return dispatchInvokeStatic(recvArgs, sendArgs, con);
    case Peek:
      return dispatchPeek(recvArgs, sendArgs, con);
    case Poke:
      return dispatchPoke(recvArgs, sendArgs, con);
    case PeekStatic:
      return dispatchPeekStatic(recvArgs, sendArgs, con);
    case PokeStatic:
      return dispatchPokeStatic(recvArgs, sendArgs, con);
    default:
      Object::_throwNotImplementedYet();
      break;
    }
  } catch (RThrowable ex) {
    //ex->printStackTrace();
    sendArgs->resize(1);
    sendArgs[0] = _local2remote(ex, con);
    return ReturnException;
  }
  return Unexpected;
}

RString getEnumValString(const ClazzEnumInfo* ei, int val)
{
  ClazzEnumValueInfo** values = ei->values;
  for (int i = 0; values[i] != 0; ++i)
  {
    if (values[i]->value == val)
    {
      return values[i]->name;
    }
  }
  return "<unknown>";
}

RString cmdToString(InvokeCmd cmd)
{
  return getEnumValString(InvokeCmdMetaInf::GetEnumInfo(), cmd);

}

StringBuffer& operator<<(StringBuffer& sb, IN(RRemoteArgArray) args)
{
  sb << "count: " << args->length() << "; ";
  for (int i = 0; i < args->length(); ++i)
  {
    sb << "[" << args[i]->toString() << "] ";
  }
  return sb;
}

void
RemoteDmiServer::_send(InvokeCmd sendCmd, IN(RRemoteArgArray) sendArgs, IN(RConnection) con)
{
  ACDK_NLOG("acdkx.rdmi.transport", Debug, con->getRemoteServerId() << "; Start write message: " << cmdToString(sendCmd) << " " << sendArgs);
  con->startWriteMessage();
  acdk::io::RWriter writer = con->getWriter();
  _protocol->send(writer, sendCmd, sendArgs);
  con->endWriteMessage();
  ACDK_NLOG("acdkx.rdmi.transport", Debug, con->getRemoteServerId() << "; End write message: " << cmdToString(sendCmd));
}

InvokeCmd 
RemoteDmiServer::_receive(IN(RRemoteArgArray) recvArgs, IN(RConnection) con, InvokeCmd initCmd)
{
  ACDK_NLOG("acdkx.rdmi.transport", Debug, con->getRemoteServerId() << "; Start read message");
  if (con->isClosed() == true)
    return ConnectionClosed;

  if (initCmd == Unexpected)
    con->startReadMessage();

  acdk::io::RReader reader = con->getReader();
  InvokeCmd recvCmd = _protocol->receive(reader, recvArgs, initCmd);
  con->endReadMessage();
  ACDK_NLOG("acdkx.rdmi.transport", Debug, con->getRemoteServerId() << "; End read message: " << recvCmd << " " << recvArgs);
  return recvCmd;
}


//foreign 
InvokeCmd 
RemoteDmiServer::dispatch2Remote(InvokeCmd sendCmd, IN(RRemoteArgArray) sendArgs, IN(RRemoteArgArray) recvArgs, IN(RConnection) con)
{
  while (true)
  {
    _send(sendCmd, sendArgs, con);
    InvokeCmd recvCmd = _receive(recvArgs, con, Unexpected);
    
    if (recvCmd == ReturnValue || recvCmd == ReturnVoid || recvCmd == ReturnException)
      return recvCmd;
    if (recvCmd == ConnectionClosed)
    {
      /// ### throw ex
    }
    sendArgs->resize(0);
    sendCmd = dispatch2Local(recvCmd, recvArgs, sendArgs, con);
  }
  // never reach here
  return Unexpected;
}

void 
RemoteDmiServer::readWrite(IN(RConnection) con, InvokeCmd initCmd)
{
  while (true)
  {
    RRemoteArgArray recvArgs = new RemoteArgArray(0);
    InvokeCmd cmd = Unexpected;
    try {
      cmd = _receive(recvArgs, con, initCmd);
      if (cmd == ConnectionClosed)
        break;

    } catch (acdk::io::RIOException ex) {
      ACDK_NLOG("acdkx.rdmi.transport", Note, "Connection closed: " + ex->getMessage());
      break;
    }
    if (cmd == ShutDown)
    {
      ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Info, "received server shutdown in client connection");
      shutdown();
      break;
    }
    initCmd = Unexpected;
    RRemoteArgArray sendArgs = new RemoteArgArray(0);
    InvokeCmd returnCmd = dispatch2Local(cmd, recvArgs, sendArgs, con);
    if (returnCmd == ShutDown)
    {
      shutdown();
      break;
    }
    try {
      _send(returnCmd, sendArgs, con);
    } catch (acdk::io::RIOException ex) {
      ACDK_NLOG("acdkx.rdmi.transport", Note, "Connection closed: " + ex->getMessage());
      break;
    }
  }
}

RObject 
RemoteDmiServer::createRemoteObject(IN(RString) remoteClass, IN(RString) localInterface, const acdk::lang::dmi::ScriptVarArray& args)
{
  RRemoteArgArray sendArgs = new RemoteArgArray(args.size() + 4);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = new RemoteArg(RemoteStringType, inOf(remoteClass));
  sendArgs[2] = new RemoteArg(RemoteStringType, inOf(localInterface));
  sendArgs[3] = new RemoteArg(RemoteIntValueType, inOf(0)); // here MiMiPublic | MiIvConstructor
  RConnection con = _server->getClientConnection();

  for (int i = 0; i < args.size(); ++i)
  {
    sendArgs[i + 4] = _local2remote(args[i], con);
  }
  RRemoteArgArray recvArgs = new RemoteArgArray(0);
  InvokeCmd cmd = dispatch2Remote(NewObject, sendArgs, recvArgs, con);

  if (cmd == ReturnVoid)
  {
  }
  else if (cmd == ReturnValue)
  {
    int retRemoteFlags = _remote2local(recvArgs[0], con).getIntVar();
    ScriptVar ret = _remote2local(recvArgs[1], con);
    return ret.getObjectVar();
  }
  else if (cmd == ReturnException)
  {
    int retRemoteFlags = _remote2local(recvArgs[0], con).getIntVar();
    RThrowable ex = (RThrowable)recvArgs[1]->value.getObjectVar();
    THROW_INSTANCE(ex);
  } 
  else if (cmd == ConnectionClosed)
  {
    // #### oops
  }
  return Nil;
}

//foreign 
acdk::lang::dmi::ScriptVar 
RemoteDmiServer::invokeStaticRemote(IN(RString) className, IN(RString) methodName, IN(acdk::lang::dmi::ScriptVarArray) args, IN(RString) returnAsType, int returnAttr)
{
  // #### TODO handle returnAsType and attr
  RRemoteArgArray sendArgs = new RemoteArgArray(args.size() + 4);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = new RemoteArg(RemoteStringType, inOf(className));
  sendArgs[2] = new RemoteArg(RemoteStringType, inOf(methodName));
  sendArgs[3] = new RemoteArg(RemoteIntValueType, inOf(MiStatic));
  RConnection con = _server->getClientConnection();

  for (int i = 0; i < args.size(); ++i)
  {
    sendArgs[i + 4] = _local2remote(args[i], con);
  }
  RRemoteArgArray recvArgs = new RemoteArgArray(0);
  InvokeCmd cmd = dispatch2Remote(InvokeStatic, sendArgs, recvArgs, con);
  
  int retRemoteFlags = _remote2local(recvArgs[0], con).getIntVar();
  if (cmd == ReturnValue)
  {
    ScriptVar ret = _remote2local(recvArgs[1], con);
    return ret.getObjectVar();
  }
  else if (cmd == ReturnException)
  {
    RThrowable ex = (RThrowable)recvArgs[1]->value.getObjectVar();
    THROW_INSTANCE(ex);
  }
  return ScriptVar();
}
//foreign 
acdk::lang::dmi::ScriptVar 
RemoteDmiServer::peekStatic(IN(RString) className, IN(RString) memberName, IN(RString) interfaceName, int flags)
{
  // #### TODO handle returnAsType and attr
  RRemoteArgArray sendArgs = new RemoteArgArray(4);
  sendArgs[0] = new RemoteArg(RemoteIntValueType, inOf(0));
  sendArgs[1] = new RemoteArg(RemoteStringType, inOf(className));
  sendArgs[2] = new RemoteArg(RemoteStringType, inOf(Nil));
  sendArgs[3] = new RemoteArg(RemoteStringType, inOf(memberName));
  sendArgs[4] = new RemoteArg(RemoteIntValueType, MiStatic);
  RConnection con = _server->getClientConnection();
  RRemoteArgArray recvArgs = new RemoteArgArray(0);
  InvokeCmd cmd = dispatch2Remote(PeekStatic, sendArgs, recvArgs, con);

  int retRemoteFlags = _remote2local(recvArgs[0], con).getIntVar();
    
  if (cmd == ReturnValue)
  {
    ScriptVar ret = _remote2local(recvArgs[1], con);
    return ret.getObjectVar();
  }
  else if (cmd == ReturnException)
  {
    RThrowable ex = (RThrowable)recvArgs[1]->value.getObjectVar();
    THROW_INSTANCE(ex);
  }
  return ScriptVar();
}


void 
ThreadedConnectionHandler::run()
{
  _server->readWrite(_connection, _initCmd);
  LocalObjectRepository::get()->disconnetConnectionObjects(_connection);
}

InvokeCmd 
ThreadedConnectionHandler::readFirstCmd()
{
  _connection->startReadMessage();
  acdk::io::RReader reader = _connection->getReader();
  _initCmd = _server->_protocol->readCmd(reader);
  return _initCmd;
}

void 
RemoteDmiServer::run()
{
  while (true)
  {
    ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() << "; Accept new connections");
    RConnection con;
    do {
      ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() << "; Accept new connections");
      con = _server->accept(1000);
      if (_shootDown == true)
      {
        ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() << "; leave RemoteDmiServer::run() shutdown set");
        return;
      }
    } while (con == Nil);
    RString rsid = con->getRemoteServerId();
    ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() + "; Accepted new connections: " + rsid);
    RThreadedConnectionHandler conHandler = new ThreadedConnectionHandler(this, con);
    InvokeCmd cmd = conHandler->readFirstCmd();
    if (cmd == ShutDown)
    {
      ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Note, "received server shutdown");
      return;
    }
    conHandler->start();
  }
}

void 
RemoteDmiServer::shutdown()
{
  ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() + "; shutdown RemoteDmiServer now");
  _shootDown = true;
  /*
  RConnection con = _server->getLocalServerConnection();
  con->startWriteMessage();
  acdk::io::RWriter writer = con->getWriter();
  RemoteArgArray args(0);
  _protocol->send(writer, ShutDown, &args);
  ACDK_NLOG("acdkx.rdmi.RemoteDmiServer", Trace, _server->getLocalServerId() + "; shutdown RemoteDmiServer send");
  */
}


void 
RemoteDmiServer::shutdownRemote()
{
  RConnection con = _server->getClientConnection();
  con->startWriteMessage();
  acdk::io::RWriter writer = con->getWriter();
  RemoteArgArray args(0);
  _protocol->send(writer, ShutDown, &args);
  con->endWriteMessage();
  con->close();
}

} // namespace rdmi 
} // namespace acdkx

