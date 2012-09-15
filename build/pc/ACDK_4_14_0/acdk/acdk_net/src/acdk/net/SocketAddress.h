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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/SocketAddress.h,v 1.1 2005/03/26 15:02:02 kommer Exp $

#ifndef acdk_net_SocketAddress_h
#define acdk_net_SocketAddress_h

#include "net.h"


namespace acdk {
namespace net {


ACDK_DECL_CLASS(SocketAddress);

/** 
  API: Java
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/26 15:02:02 $
*/
class ACDK_NET_PUBLIC SocketAddress 
: public acdk::lang::Object  
{
  ACDK_WITH_METAINFO(SocketAddress)
public:
  SocketAddress(){}

};

} // net
} // acdk

#endif // acdk_net_SocketAddress_h

