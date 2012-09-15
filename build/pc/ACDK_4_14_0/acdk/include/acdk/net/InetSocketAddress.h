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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/InetSocketAddress.h,v 1.1 2005/03/26 15:02:02 kommer Exp $

#ifndef acdk_net_InetSocketAddress_h
#define acdk_net_InetSocketAddress_h

#include "SocketAddress.h"
#include "InetAddress.h"

namespace acdk {
namespace net {


ACDK_DECL_CLASS(InetSocketAddress);

/** 
  API: Java
  @author Roger Rene Kommer
  @version $Revision: 1.1 $
  @date $Date: 2005/03/26 15:02:02 $
*/
class ACDK_NET_PUBLIC InetSocketAddress 
: extends SocketAddress
, implements acdk::io::Serializable 
{
  ACDK_WITH_METAINFO(InetSocketAddress)
protected:
  RInetAddress _inetAddress;
  int _port;
  bool _resolved;
public:
  InetSocketAddress(IN(RInetAddress) addr, int port) 
    : _inetAddress(addr)
    , _port(port)
    , _resolved(false)
  {
  }
  InetSocketAddress(int port) 
  : _inetAddress(InetAddress::getAnyAddress())
  , _port(0)
  {
  }

  InetSocketAddress(IN(RString) hostname, int port) 
    : _inetAddress(hostname)
    , _port(port)
  {
  }
  bool equals(IN(RObject) obj)
  {
    if (instanceof(obj, InetSocketAddress) == false)
      return false;
    return equals(RInetSocketAddress(obj));
  }
  bool equals(IN(RInetSocketAddress) other)
  {
    return other->getAddress()->equals(getAddress()) && 
           other->getPort() == getPort();
  }
  RInetAddress getAddress() { return _inetAddress; }
  RString getHostName() 
  {
    return _inetAddress->getHostName();
  }
          
  int getPort() { return _port; }
  int hashCode() { return _inetAddress->hashCode() * 31 + _port; }
  bool isUnresolved() { return _inetAddress != Nil; }
  RString toString() 
  {
    if (_inetAddress == Nil)
      return SBSTR("<unset inet address>:" << _port);
    return SBSTR(_inetAddress << ":" << _port);
  }
};

} // net
} // acdk

#endif // acdk_net_InetSocketAddress_h

