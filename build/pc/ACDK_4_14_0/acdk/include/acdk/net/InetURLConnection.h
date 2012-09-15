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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/InetURLConnection.h,v 1.6 2005/02/05 10:45:29 kommer Exp $

#ifndef acdk_net_InetURLConnection_h
#define acdk_net_InetURLConnection_h

#include "URLConnection.h"
#include "Socket.h"

namespace acdk {
namespace net {

ACDK_DECL_CLASS(InetURLConnection);

class ACDK_NET_PUBLIC InetURLConnection
: extends URLConnection
{
  ACDK_WITH_METAINFO(InetURLConnection)
private:
  RSocket _socket;
  RWriter _out;
  RReader _in;
  bool _connected;
public:

  InetURLConnection(IN(RURL) url)
   : URLConnection(url)
   , _connected(false)
  {
  }

  void connect();
  
  RReader getInputStream()
  {
    if (_connected == false)
      connect();
    return _in;
  }
  virtual RWriter getOutputStream() 
  {
    if (_connected == false)
      connect();
    return _out;
  }
  void disconnect() 
  {
    if (_connected == true)
      _socket->close();
  }
};

} // namespace acdk
} // namespace net

#endif //acdk_net_InetURLConnection_h


