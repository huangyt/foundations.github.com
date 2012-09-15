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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/ServerImpl.h,v 1.2 2005/04/30 14:23:53 kommer Exp $

#ifndef acdkx_rdmi_ServerImpl_h
#define acdkx_rdmi_ServerImpl_h

#include "Connection.h"

namespace acdkx {
namespace rdmi {


ACDK_DECL_INTERFACE(ServerImpl);

class ACDKX_RDMI_LIB_PUBLIC ServerImpl
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ServerImpl)
public:
  virtual RString getLocalServerId() = 0;
  /**
    creates a client connection
  */
  virtual RConnection getClientConnection() = 0;
  /** 
    only valid if is running as server
    returns a client connection to its own server
  */
  virtual RConnection getLocalServerConnection() = 0;
  /**
    server connection
    @param timeOut timeout in ms. if -1 the server blocks for ever
            if timeOut reached, it return Nil
  */
  virtual RConnection accept(int timeOut = -1) = 0;
  virtual void shutdown() = 0;
  virtual bool allowThreading() = 0;

};

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_ServerImpl_h
