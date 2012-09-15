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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/Protocol.h,v 1.3 2005/03/07 17:24:53 kommer Exp $

#ifndef acdkx_rdmi_Protocol_h
#define acdkx_rdmi_Protocol_h

#include "rdmi.h"

namespace acdkx {
namespace rdmi {

enum InvokeCmd 
{
  Unexpected        = 0x0,
  /**
    @param remoteObjectId
  */
  AddRef            = 0x02,
  /** 
    @param remoteObjectId 
    @return ReturnValue, int refCount after release
  */
  ReleaseRef        = 0x3,
  /** 
    @param ClassName   the Class must be known by the server
    @param InterfaceName The Interface must be implemented by 
           server ClassName
    @param RemoteFlags
    @param DmiFlags combination of acdk::lang::dmi::MetaInfoFlags
    @param Args see RemoteArgType
    @return ReturnValue
  */
  NewObject            = 0x4,
  /**
    @param remoteObjectId the this object
    @param methodName
    @param flags
    @param Args
    @return ReturnValue
    return with ReturnVoid, ReturnValue or ReturnException
  */
  Invoke           = 0x5,
  /**
    RemoteFlags className methodName DmiFlags Args 
    return with ReturnVoid, ReturnValue or ReturnException
   */
  InvokeStatic     = 0x6,
  /**
    RemoteFlags remoteObjectId memberName DmiFlags
    return with ReturnValue or ReturnException
  */
  Peek             = 0x7,
  /**
    RemoteFlags clasName memberName DmiFlags
    return with ReturnValue or ReturnException
  */
  PeekStatic       = 0x8,
  /**
    RemoteFlags remoteObjectId memberName DmiFlags
    return with ReturnVoid or ReturnException
  */
  Poke             = 0x9,
  /**
    RemoteFlags className memberName DmiFlags
    return with ReturnVoid or ReturnException
  */
  PokeStatic       = 0xA,
  /**
    RemoteFlags Arg
  */
  ReturnValue      = 0xB,
  /**
    RemoteFlags 
  */
  ReturnVoid       = 0xC,
  /*
    RemoteFlags Arg
  */
  ReturnException  = 0xD,
  /**
   keep reference alive on server identpendend
   if client will close its connection
  */
  AddPersistentRef     = 0xE,
  ReleasePersistentRef = 0xF,
  /**
    the argument is a ServerId (String) of the client
    This call return the ServerId from the server
    Normally this is the first call after connted to
    a server
  */
  SetServerId           = 0x10,
  ShutDown             = 0x11,

  /** used not in stream, but in request for polling for available data */
  NoData                = 0x1000,
  /** used not in stream, but in request for polling for available data */
  
  ConnectionClosed      = 0x2000
};
ACDK_DEF_LIB_ENUM(ACDKX_RDMI_LIB_PUBLIC, InvokeCmd);

/**
  combination of flags
*/
enum RemoteFlags
{
  /**
    it uses weak invoke. out parameter are not supported
  */
  WeakInvoke        = 0x000001
};
ACDK_DEF_LIB_ENUM(ACDKX_RDMI_LIB_PUBLIC, RemoteFlags);

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_Protocol_h
