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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/OrbConnection.cpp,v 1.7 2005/02/05 10:45:39 kommer Exp $

#include <acdk.h>
#include <acdk/util/ArrayList.h>

#include "orb.h"
#include "OrbConnection.h"
#include "ServerDelegate.h"


namespace acdkx {
namespace orb {

void 
OrbConnection::connect(IN(RServerDelegate) sd)
{
  if (socket != Nil)
    return;
  socket = new Socket(InetAddress::getByName(network), port);
  if (objects == Nil)
    objects = new acdk::util::ArrayList();
  objects->add((::acdk::lang::RObject)sd);
}


void 
OrbConnection::disconnect(IN(RServerDelegate) sd)
{
  if (objects == Nil)
    return;// #### ex
  if (objects->remove(&sd) == false)
    return;// #### ex
}


void 
OrbConnection::disconnect()
{
  if (socket == Nil)
    return;
  socket->close();
}


} // namespace orb
} // namespace acdkx


