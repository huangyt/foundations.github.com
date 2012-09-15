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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/TCPSocketFactory.h,v 1.7 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_net_TCPSocketFactory_h
#define acdk_net_TCPSocketFactory_h

#include "net.h"
#include "SocketImplFactory.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(TCPSocketFactory);

/** 

  API: Java
  @author Roger Rene Kommer, Maximilian Thoran
  @version $Revision: 1.7 $
  @date $Date: 2005/04/08 10:53:20 $
*/
class ACDK_NET_PUBLIC TCPSocketFactory 
: extends ::acdk::lang::Object,  
  implements ::acdk::net::SocketImplFactory
{
public:
  TCPSocketFactory();
  virtual ~TCPSocketFactory();
  virtual RSocketImpl createSocketImpl();
};

} // net
} // acdk
#endif // acdk_net_TCPSocketFactory_h

