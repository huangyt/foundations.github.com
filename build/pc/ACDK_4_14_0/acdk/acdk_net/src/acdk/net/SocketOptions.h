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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/SocketOptions.h,v 1.15 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_net_SocketOptions_h
#define acdk_net_SocketOptions_h

#include "netsysincl.h"

namespace acdk {
namespace net {

using namespace acdk::lang;
using namespace acdk::io;

ACDK_DECL_INTERFACE(SocketOptions);

/** used in SocketOptions::getOption()/setOption */
enum SocketOption
{
  /// bool wrapped by Boolean
  _TCP_NODELAY,

  _IP_MULTICAST_IF,
  _SO_BINDADDR,
  /// bool wrapped by Boolean
  _SO_REUSEADDR,
  /// int wrapped by Integer
  _SO_LINGER,
  /// int wrapped by Integer
  _SO_SNDBUF,
  /// int wrapped by Integer
  _SO_RCVBUF,
  /// int wrapped by Integer
  _SO_TIMEOUT,
  /// int wrapped by Integer
  _SO_RCVTIMEO,
  /// int wrapped by Integer
  _SO_SNDTIMEO,
  /// bool wrapped by Boolean
  _SO_KEEPALIVE,
  /// bool wrapped by Boolean
  _SO_BROADCAST,
  /**
    int wrapped by Integer
    type of service. 
    May not supported by all platforms
  */
  _IP_TOS
};
ACDK_DEF_LIB_ENUM(ACDK_NET_PUBLIC, SocketOption);

/** 

  API: Java
  @author Maximilian Thoran, Roger Rene Kommer
  @version $Revision: 1.15 $
  @date $Date: 2005/04/08 10:53:20 $
*/
class ACDK_NET_PUBLIC SocketOptions
      ACDK_INTERFACEBASE 
{
  ACDK_WITH_METAINFO(SocketOptions)
public:
  
  /** Fetch the value of an option. */
  virtual RObject getOption(int optID) = 0;
  /** Enable/disable the option specified by optID. */
  virtual void setOption(int optID, IN(RObject) value) = 0;

};

} // net
} // acdk

#endif //acdk_net_SocketOptions_h


