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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/InetAddress.h,v 1.13 2005/04/30 15:56:23 kommer Exp $

#ifndef acdk_net_InetAddress_h
#define acdk_net_InetAddress_h

#include "net.h"
#include <acdk/io/Serializable.h>

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_CLASS(InetAddress);

/** 
  Wrapper for Internet Address.
  API: Java
  @author Roger Rene Kommer, Maximilian Thoran
  @version $Revision: 1.13 $
  @date $Date: 2005/04/30 15:56:23 $

*/
class ACDK_NET_PUBLIC InetAddress 
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable

{
  ACDK_WITH_METAINFO(InetAddress)
protected:
  RbyteArray _ipNumber;
  RString _hostname;
public:
  /// liefert die Addresse in Network Byte Order zurueck ( benutzt htonl )
  InetAddress(IN(RbyteArray) ipaddr, IN(RString) hostname = Nil);
  InetAddress(int ipaddr, IN(RString) hostname = Nil);
  /// expects ip nummer in the format xxx.xxx.xxx.xxx
  //InetAddress(RString ipnummer);
  //InetAddress(int ip[4], RString hostname = Nil);
  virtual bool equals(IN(RObject) obj);
  virtual bool equals(IN(RInetAddress) other);
  virtual RbyteArray getAddress();
  virtual RString getHostAddress();
  virtual RString getHostName();
  virtual int hashCode();
  virtual RString toString();
  virtual bool isMulticastAddress();
  
  foreign byte* getData() { return _ipNumber->data(); }

  static RInetAddressArray getAllByName(IN(RString) host);
  static RInetAddress getByName(IN(RString) host);
  
  /**
    returns local or localhost
  */
  static RString getLocalHostName();
  /**
    return the inet  address of localhost
  */
  static RInetAddress getLocalHost();
  /**
    return '0.0.0.0'
  */
  static RInetAddress getNullHost();

  static RInetAddress getWildcardHost();
  /**
    returns INADDR_ANY 
  */
  static RInetAddress getAnyAddress();
  /**
    return the default host name of the local machine
  */
  static RString getDefaultHostName();
  /**
    return the default address of the local machine
  */
  static RInetAddress getDefaultHost()
  {
    return getByName(getDefaultHostName());
  }
protected:
 //  static RString _getHostByAddress(RIntArray _ipNumber) throw(RUnknownHostException, RThrowable);
};


} // namespace net 
} // namespace acdk 

#endif //acdk_net_InetAddress_h

