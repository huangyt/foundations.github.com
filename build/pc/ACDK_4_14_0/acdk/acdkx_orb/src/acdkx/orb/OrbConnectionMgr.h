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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbConnectionMgr.h,v 1.6 2005/02/05 10:45:39 kommer Exp $


#ifndef acdkx_orb_OrbConnectionMgr_h
#define acdkx_orb_OrbConnectionMgr_h

#include "orb.h"

#include <acdk/io/MemWriter.h>
#include <acdk/util/ArrayList.h>
#include <acdk/util/TreeMap.h>

#include "OrbConnection.h"

namespace acdkx {
namespace orb {

ACDK_DECL_CLASS(OrbConnectionMgr);

/**
  OrbConnectionMgr is a singleton which manages
  all Connections to other ORBs.
  It implements a  connection sharing of each peer connection
*/
class ACDKX_ORB_PUBLIC OrbConnectionMgr
: extends acdk::lang::Object
{
private:
  static ROrbConnectionMgr _connectionManager;
  /**
    Holds OrbConnections as key and value.
  */
  ::acdk::util::RTreeMap _connections;
  OrbConnectionMgr();
public:

  /** 
    returns a OrbConnection
    The connection (the socket) will be already connected
  */
  ROrbConnection getConnection(IN(RServerDelegate) sd);
  /**
    Will be called if given serverdelegate is not more needed
    If the containing connection is not needed by any ServerDelegate
    the connection itself will be closed
  */
  void disconnect(IN(RServerDelegate) sd);
  
  /** 
    OrbConnectionMgr is a singleton 
  */
  static ROrbConnectionMgr& getMgr();
};



} // namespace orb
} // namespace acdkx

#endif //acdkx_orb_OrbConnectionMgr_h
