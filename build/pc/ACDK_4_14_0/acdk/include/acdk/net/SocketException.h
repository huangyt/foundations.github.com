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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/SocketException.h,v 1.9 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_SocketException_h
#define acdk_net_SocketException_h

#include <acdk/io/IOException.h>

#ifndef acdk_net_net_h
#  include "net.h"
#endif // acdk_net_net_h



namespace acdk {
namespace net {

using namespace acdk::lang;

ACDK_DECL_THROWABLE_FQ( SocketException, ::acdk::io::, IOException);

class ACDK_NET_PUBLIC SocketException 
: extends acdk::io::IOException
{
  ACDK_WITH_METAINFO(SocketException)  
public:
  SocketException();
  SocketException(IN(RString) what);
};


} // net
} // acdk

#endif //acdk_net_SocketException_h
