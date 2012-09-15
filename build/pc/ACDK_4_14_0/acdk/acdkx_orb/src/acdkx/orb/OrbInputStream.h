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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbInputStream.h,v 1.6 2005/02/05 10:45:39 kommer Exp $


#ifndef acdkx_orb_OrbInputStream_h
#define acdkx_orb_OrbInputStream_h

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
  
ACDK_DECL_CLASS(OrbInputStream);

class ACDKX_ORB_PUBLIC OrbInputStream
: extends ::acdkx::orb::CDRObjectReader
{
protected:
  RObjectKey _objectKey;
  
public:
  OrbInputStream(IN(RObjectKey) objKey, IN(RReader) in)
  : CDRObjectReader(in, AORB::getORB()),
    _objectKey(objKey)
  {
  }
};



} // namespace orb 
} // namespace acdkx 

#endif //acdkx_orb_OrbInputStream_h

