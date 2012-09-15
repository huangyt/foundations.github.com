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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbOutputStream.h,v 1.6 2005/02/05 10:45:39 kommer Exp $


#ifndef acdkx_orb_OrbOutputStream_h
#define acdkx_orb_OrbOutputStream_h

#include <acdk.h>
#include <acdk/io/MemWriter.h>
#include <acdk/net/Socket.h>
#include <acdk/net/InetAddress.h>


#include "AORB.h"
#include "ObjectKey.h"
#include "CDRObjectWriter.h"
#include "CDRObjectReader.h"
#include "GIOPMessage.h"

namespace acdkx {
namespace orb {

USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, InetAddress);
  
ACDK_DECL_CLASS(OrbOutputStream);



class ACDKX_ORB_PUBLIC OrbOutputStream
: extends ::acdkx::orb::CDRObjectWriter
{
  
protected:
  RObjectKey _objectKey;
  RWriter _realOut;
public:
  OrbOutputStream(IN(RObjectKey) objKey, IN(RWriter) out)
  : CDRObjectWriter(new ::acdk::io::MemWriter(), AORB::getORB()),
    _objectKey(objKey),
    _realOut(out)
  {
  }
  virtual ::org::omg::CORBA::portable::RInputStream  send() { return Nil; }
  void sendMessage();
  void setMessageSize(byte* buffer, int size);
  
};
} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_OrbOutputStream_h

