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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/BinaryProtocol.cpp,v 1.2 2005/03/07 10:04:17 kommer Exp $

#include "BinaryProtocol.h"
#include <acdk/io/BinaryObjectWriter.h>
#include <acdk/io/BinaryObjectReader.h>

namespace acdkx {
namespace rdmi {

using namespace acdk::io;
/*
void 
BinaryProtocol_writeVar(BinaryObjectWriter& bout, RemoteArg& sv)
{
  bout.write((byte)sv.argType);
  switch (sv.argType)
  {
  case RemoteIdType:
    bout.writeString(sv.serverId);
    bout.writeString(sv.name);
    bout.writeInt(sv.value.getIntVar());
    break;
  case RemoteSerializedType:
    //bout.writeString(name);
    bout.writeObject(sv.value.getObjectVar());
    break;
  case RemoteStringType:
    bout.writeString(sv.value.getStringVar());
    break;
  case RemoteIntValueType:
    bout.writeInt(sv.value.getIntVar());
    break;
  case RemoteBoolValueType:
    bout.writeBoolean(sv.value.getBoolVar());
    break;
  case RemoteCharValueType:
    bout.writeChar(sv.value.getCharVar());
    break;
  case RemoteUcCharValueType:
    bout.writeUcChar(sv.value.getUcCharVar());
    break;
  case RemoteByteValueType:
    bout.write(sv.value.getByteVar());
    break;
  case RemoteShortValueType:
    bout.writeShort(sv.value.getShortVar());
    break;
  case RemoteLongValueType:
    bout.writeLong(sv.value.getLongVar());
    break;
  case RemoteFloatValueType:
    bout.writeFloat(sv.value.getFloatVar());
    break;
  case RemoteDoubleValueType:
    bout.writeDouble(sv.value.getDoubleVar());
    break;
  default:
    Object::_throwNotImplementedYet();
    break;
  }
}*/

void
BinaryProtocol_writeArgs(BinaryObjectWriter& bout, IN(RRemoteArgArray) args)
{
  int len = args->length();
  bout.writeInt(len);
  for (int i = 0; i < len; ++i)
    args[i]->writeObject(&bout);

    //BinaryProtocol_writeVar(bout, args[i]);
}

void 
BinaryProtocol::send(IN(acdk::io::RWriter) out, InvokeCmd cmd, IN(RRemoteArgArray) args)
{
  BinaryObjectWriter bout(out);
  bout.write((byte)cmd);
  BinaryProtocol_writeArgs(bout, args);
}
/*
void
BinaryProtocol_readArg(BinaryObjectReader& bin, RemoteArg& arg)
{
  arg.argType = (RemoteArgType)bin.read();
  switch (arg.argType)
  {
  case RemoteIdType:
    arg.serverId = bin.readString();
    arg.name = bin.readString();
    arg.value = inOf(bin.readInt());
    break;
  case RemoteSerializedType:
    arg.value = inOf(bin.readObject());
    break;
   case RemoteStringType:
    arg.value = inOf(bin.readString());
    break;
  case RemoteIntValueType:
    arg.value = inOf(bin.readInt());
    break;
  case RemoteBoolValueType:
    arg.value = inOf(bin.readBoolean());
    break;
  case RemoteCharValueType:
    arg.value = inOf(bin.readChar());
    break;
  case RemoteUcCharValueType:
    arg.value = inOf(bin.readUcChar());
    break;
  case RemoteByteValueType:
    arg.value = inOf(bin.read());
    break;
  case RemoteShortValueType:
    arg.value = inOf(bin.readShort());
    break;
  case RemoteLongValueType:
    arg.value = inOf(bin.readLong());
    break;
  case RemoteFloatValueType:
    arg.value = inOf(bin.readFloat());
    break;
  case RemoteDoubleValueType:
    arg.value = inOf(bin.readDouble());
    break;
  default:
    Object::_throwNotImplementedYet();
  }
}*/

void
BinaryProtocol_readArgs(BinaryObjectReader& bin, IN(RRemoteArgArray) args)
{
  int count = bin.readInt();
  args->resize(count);
  for (int i = 0; i < count; ++i)
  {
    //BinaryProtocol_readArg(bin, args[i]);
    RRemoteArg ra = new RemoteArg();
    ra->readObject(&bin);
    args[i] = ra;
  }
}


InvokeCmd 
BinaryProtocol::readCmd(IN(acdk::io::RReader) in)
{
  BinaryObjectReader bin(in);
  int readed = bin.read();
  if (readed == -1)
    return Unexpected;
  return (InvokeCmd)readed;
}

InvokeCmd 
BinaryProtocol::receive(IN(acdk::io::RReader) in, IN(RRemoteArgArray) args, InvokeCmd cmd)
{
  BinaryObjectReader bin(in);
  if (cmd == Unexpected)
  {
    int readed = bin.read();
    if (readed == -1)
      return Unexpected;
    cmd = (InvokeCmd)readed;
  }
  BinaryProtocol_readArgs(bin, args);
  return cmd;  
}

} // namespace rdmi 
} // namespace acdkx


