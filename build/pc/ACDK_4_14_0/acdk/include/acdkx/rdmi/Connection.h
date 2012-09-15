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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/Connection.h,v 1.2 2005/03/07 17:24:53 kommer Exp $

#ifndef acdkx_rdmi_Connection_h
#define acdkx_rdmi_Connection_h

#include "Protocol.h"

namespace acdkx {
namespace rdmi {


ACDK_DECL_INTERFACE(Connection);
/**
  Connection is a representation of
  a statefull connection between one server and one client
*/
class ACDKX_RDMI_LIB_PUBLIC Connection
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Connection)
public:
  virtual RString getRemoteServerId() = 0;
  virtual RString getLocalServerId() = 0;
  virtual acdk::io::RReader getReader() = 0;
  virtual acdk::io::RWriter getWriter() = 0;
  /**
    may prepare a connection
    will be called before getWriter()
  */
  virtual void startWriteMessage() = 0;
  /**
    Message was written
    may flush/close Writer
  */
  virtual void endWriteMessage() = 0;
  virtual void startReadMessage() = 0;
  virtual void endReadMessage() = 0;
  /**
    close the connection
  */
  virtual void close() = 0;
  virtual bool isClosed() = 0;
  virtual bool dataAvailable() = 0;
};


} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_Connection_h
