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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/ProtocolImpl.h,v 1.2 2005/03/07 10:04:18 kommer Exp $

#ifndef acdkx_rdmi_ProtocolImpl_h
#define acdkx_rdmi_ProtocolImpl_h

#include "Protocol.h"
#include <acdk/io/Serializable.h>
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/ObjectReader.h>

namespace acdkx {
namespace rdmi {

enum InvokeCmd;

RString getEnumValString(const acdk::lang::dmi::ClazzEnumInfo* ei, int val);

enum RemoteArgType
{
  RemoteUnsetType             = 0x0,
  /**
    streamed: RemoteIdType(byte) ServerId(String) Interface(String) ObjectId(int)
  */
  RemoteIdType                = 0x1,
  /**
    streamed: RemoteSerializedType(byte) ClassName(String) SerializedClass(byte[])
    first comes type name of serialized class
    deserialzed type is stored in value.object
  */
  RemoteSerializedType        = 0x2,
  /**
    streamed: RemoteStringType(byte) String(String)
    deserialized type is stored in value.object
  */
  RemoteStringType            = 0x11,
  /**
    streamed: RemoteArgType(byte) value(byte[])
    a basic type. 
  */
  RemoteIntValueType              = 0x12,
  RemoteBoolValueType             = 0x13,
  RemoteShortValueType            = 0x14,
  RemoteByteValueType             = 0x15,
  RemoteCharValueType             = 0x16,
  RemoteUcCharValueType           = 0x17,
  RemoteLongValueType             = 0x18,
  RemoteFloatValueType            = 0x19,
  RemoteDoubleValueType           = 0x1A,
  RemoteArrayValueType            = 0x1B
};
ACDK_DEF_LIB_ENUM(ACDKX_RDMI_LIB_PUBLIC, RemoteArgType);


ACDK_DECL_CLASS(RemoteArg);

class ACDKX_RDMI_LIB_PUBLIC RemoteArg
: extends acdk::lang::Object
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(RemoteArg)
public:
  RemoteArgType argType;

  acdk::lang::dmi::ScriptVar value;
  /** object id, private to server */
  RString serverId;
  /** contains class/interface name */
  RString name;
  RemoteArg(RemoteArgType type = RemoteUnsetType)
    : argType(type)
  {
  }
  foreign RemoteArg(RemoteArgType type, IN(acdk::lang::dmi::ScriptVar) val)
  : argType(type)
  , value(val)
  {
  }
  foreign RemoteArg(RemoteArgType type, IN(acdk::lang::dmi::ScriptVar) val, IN(RString) nam)
  : argType(type)
  , value(val)
  , name(nam)
  {
  }
  RString toString();
  virtual void writeObject(IN(acdk::io::RObjectWriter) out);
  virtual void readObject(IN(acdk::io::RObjectReader) in);
};


ACDK_DECL_INTERFACE(ProtocolImpl);

class ACDKX_RDMI_LIB_PUBLIC ProtocolImpl
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ProtocolImpl)
public:
 
  virtual void send(IN(acdk::io::RWriter) out, InvokeCmd cmd, IN(RRemoteArgArray) args) = 0;
  virtual InvokeCmd readCmd(IN(acdk::io::RReader) in) = 0;
  /**
    if cmd != Unexpected read also cmd, otherwise just return cmd
  */
  virtual InvokeCmd receive(IN(acdk::io::RReader) in, IN(RRemoteArgArray) args, InvokeCmd cmd) = 0;
};

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_ProtocolImpl_h
