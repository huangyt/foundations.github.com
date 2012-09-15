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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/BinaryProtocol.h,v 1.2 2005/03/07 10:04:18 kommer Exp $

#ifndef acdkx_rdmi_BinaryProtocol_h
#define acdkx_rdmi_BinaryProtocol_h

#include "ProtocolImpl.h"
#include <acdk/net/Socket.h>

namespace acdkx {
namespace rdmi {

ACDK_DECL_CLASS(BinaryProtocol);
/**
  BinaryProtocol implements binary read/write
*/
class ACDKX_RDMI_LIB_PUBLIC BinaryProtocol
: extends acdk::lang::Object
, implements ProtocolImpl
{
  ACDK_WITH_METAINFO(BinaryProtocol)
public:
  BinaryProtocol() {}
  
  foreign virtual void send(IN(acdk::io::RWriter) out, InvokeCmd cmd, IN(RRemoteArgArray) args);
  foreign InvokeCmd readCmd(IN(acdk::io::RReader) in);
  foreign virtual InvokeCmd receive(IN(acdk::io::RReader) in, IN(RRemoteArgArray) args, InvokeCmd cmd);
  
};

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_BinaryProtocol_h
