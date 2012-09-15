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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbConnection.h,v 1.7 2005/02/05 10:45:39 kommer Exp $


#ifndef acdkx_orb_OrbConnection_h
#define acdkx_orb_OrbConnection_h

#include "orb.h"

#include <acdk/io/MemWriter.h>
#include <acdk/util/ArrayList.h>
#include <acdk/util/TreeSet.h>
#include <acdk/net/Socket.h>
#include <acdk/net/InetAddress.h>

#include "AORB.h"
#include "GIOPMessage.h"
#include "CDRObjectWriter.h"
#include "CDRObjectReader.h"

namespace acdkx {
namespace orb {

ACDK_DECL_CLASS(OrbConnection);
class ACDKX_ORB_PUBLIC OrbConnection
: extends acdk::lang::Object
, implements acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(OrbConnection)
public:
  /** ip number of ORB server */
  RString network;
  /** port number of ORB server */
  int port;
  /** socket to ORB server */
  RSocket socket;
  /**
    List of RServerDelegates which are active 
    on this OrbConnection
  */
  acdk::util::RArrayList objects;
private:
  int _nextRequestId;
public:  
  OrbConnection(RString net, int p)
  : Object()
  , network(net)
  , port(p)
  , _nextRequestId(0)
  {
  }

  /** used to hold them in a sorted collection */
  int compareTo(IN(RObject) obj)
  {
    return compareTo(ROrbConnection(obj));
  }

  /** used to hold them in a sorted collection */
  int compareTo(IN(ROrbConnection) other)
  {
    int i = network->compareTo(other->network);
    if (i != 0)
      return i;
    return port - other->port;
  }
  /** connect to server if needed */
  void connect(IN(RServerDelegate) sd);
  
  /** disconnects ds from this connection */
  void disconnect(IN(RServerDelegate) sd);

  /** disconnects from server */
  void disconnect();
  /**
    returns an request id individual to this connection
  */
  int nextRequestId() 
  { 
    SYNCTHIS();
    return ++_nextRequestId; 
  }
};

} // namespace orb
} // namespace acdkx

#endif //acdkx_orb_OrbConnection_h
