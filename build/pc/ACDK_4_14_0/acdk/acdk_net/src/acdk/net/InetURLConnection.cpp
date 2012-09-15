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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/InetURLConnection.cpp,v 1.5 2005/02/05 10:45:29 kommer Exp $


#include "InetURLConnection.h"

namespace acdk {
namespace net {

  
void 
InetURLConnection::connect()
{
  SYNCTHIS();
  if (_connected == true)
    return;
  _socket = new Socket(url->getHost(), url->getPort());
  _in = _socket->getInputStream();
  _out = _socket->getOutputStream();
  _connected  = true;
}
  

} // namespace acdk
} // namespace net



