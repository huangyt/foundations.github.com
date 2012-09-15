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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbConnectionMgr.cpp,v 1.6 2005/02/05 10:45:39 kommer Exp $



#include "OrbConnectionMgr.h"
#include "ServerDelegate.h"

namespace acdkx {
namespace orb {

OrbConnectionMgr::OrbConnectionMgr()
{
  _connections = new acdk::util::TreeMap();
}

//static 
ROrbConnectionMgr OrbConnectionMgr::_connectionManager;

//static 
ROrbConnectionMgr& 
OrbConnectionMgr::getMgr()
{
  if (_connectionManager == Nil)
    _connectionManager = new OrbConnectionMgr();
  return _connectionManager;
}


ROrbConnection 
OrbConnectionMgr::getConnection(IN(RServerDelegate) sd)
{
  SYNCTHIS();
  RString net = sd->objectKey()->network;
  int port = sd->objectKey()->port;
  OrbConnection obc(net, port);
  RObject obj = _connections->get(&obc);
  if (obj != Nil) 
    return (ROrbConnection)obj;
  ROrbConnection ncon = new OrbConnection(net, port);
  ncon->connect(sd);
  _connections->put(&ncon, &ncon);
  return ncon;
}

void 
OrbConnectionMgr::disconnect(IN(RServerDelegate) sd)
{
  SYNCTHIS();
  // ### to implement
}




} // namespace orb
} // namespace acdkx


